#!bin/bash
# This file sets up runs based on a common table of settings
# Christy Rollinson, crollinson@mortonarb.org

# Number of sites you want to run
n=8 # 8 is a full site right now

# Define constants & file paths for the scripts
file_base=/mnt/data/crollinson/MANDIFORE_modeling/ForestryTests_Ameriflux # whatever you want the base output file path to be
EDI_base=/home/models/ED_inputs/ # The location of basic ED Inputs for you
met_base=${file_base}/met_ed/

ed_exec=/home/crollinson/ED2/ED/build/ed_2.1-opt # Location of the ED Executable
file_dir=${file_base}/2_runs/ed_runs.v2 # Where everything will go
setup_dir=${file_base}/0_setup # Where some constant setup files are
site_file=${setup_dir}/ExperimentalDesign_Test.csv # # Path to list of ED sites w/ status
init_dir=${file_base}/1_spin/ed_spin.v2
lu_dir=${file_base}/lu_files # Where some constant setup files are

# Want to do a 25-year spin that ends in the year we want to starts
startyear=2006
finalyear=2101 #
finalfull=2100

# The range of met years to cycle for the transient spin
# We're just going to do the first year
metfirst=2006
metlast=2100

# Making the file directory if it doesn't already exist
mkdir -p $file_dir

# Extract the file names of sites that haven't been started yet
runs_all=($(awk -F ',' 'NR>1 {print $1}' ${site_file}))
sites_all=($(awk -F ',' 'NR>1 {print $2}' ${site_file}))
lat_all=($(awk -F ',' 'NR>1 {print $3}' ${site_file}))
lon_all=($(awk -F ',' 'NR>1 {print $4}' ${site_file}))

pft_all=($(awk -F ',' 'NR>1 {print $9}' ${site_file}))

GCM_all=($(awk -F ',' 'NR>1 {print $12}' ${site_file}))
scenario_all=($(awk -F ',' 'NR>1 {print $13}' ${site_file}))

fire_all=($(awk -F ',' 'NR>1 {print $18}' ${site_file}))
ianth_all=($(awk -F ',' 'NR>1 {print $15}' ${site_file}))
mgmt_all=($(awk -F ',' 'NR>1 {print $16}' ${site_file}))

# Get the list of what grid runs have already finished spinups
pushd $file_dir
	file_done=(*)
popd
file_done=(${file_done[@]/"*"/})

# Because we want to preserve the order of runs, I can't find away around doing a loop
# - This is slower than other options, but makes sure we still do our controls first
# - DO NOT imitate this with a large array
runs=()
sites=()
lat=()
lon=()
pft=()
GCM=()
scenario=()
fire=()
ianth=()
mgmt=()

for((i=0;i<${#runs_all[@]};i++)); do 
	RUN=${runs_all[i]}
    TEST=( ${file_done[@]/$RUN/} ) # Remove element from array

	# If the length of TEST is still the same, we haven't done it yet
    if [[ ${#TEST[@]} == ${#file_done[@]} ]]; then
		runs+=("$RUN")
		sites+=("${sites_all[i]}")
		lat+=("${lat_all[i]}")
		lon+=("${lon_all[i]}")
		pft+=("${pft_all[i]}")
		GCM+=("${GCM_all[i]}")
		scenario+=("${scenario_all[i]}")
		fire+=("${fire_all[i]}")
		ianth+=("${ianth_all[i]}")
		mgmt+=("${mgmt_all[i]}")
	fi    

done


n=$(($n<${#runs[@]}?$n:${#runs[@]}))


# for FILE in $(seq 0 (($n-1)))
for ((FILE=0; FILE<$n; FILE++)) # This is a way of doing it so that we don't have to modify N
do
	# Site Name and Lat/Lon
	RUN=${runs[FILE]}
	echo $RUN

	SITE=${sites[FILE]}

	GCM_now=${GCM[FILE]}
	GCM_now=${GCM_now/"-"/"_"}

	pft_now=${pft[FILE]}
	pft_now=${pft_now//"-"/","}

	# File Paths
    new_analy="'${file_dir}/${RUN}/analy/${RUN}'"
    new_histo="'${file_dir}/${RUN}/histo/${RUN}'"
    newbase=${file_dir}/$RUN
    oldbase=${file_dir}/TEST
	oldname=TESTinit
	met_path=${met_base}/${sites[FILE]}/${GCM_now}_${scenario[FILE]}_r1i1p1


	file_path=${file_dir}/${RUN}/

	mkdir -p ${file_path} 
	
	pushd ${file_path}
		# Creating the default file structure and copying over the base files to be modified
		mkdir -p histo analy
		ln -s $ed_exec
		cp ${init_dir}/${SITE}/ED2IN ED2IN
		cp ${init_dir}/${SITE}/PFTParams_MANDIFORE.xml .
		
		# ED2IN Changes	    
		sed -i "s/NL%EXPNME =.*/NL%EXPNME = 'MANDIFORE TESTS'/" ED2IN # change the experiment name

	    sed -i "s,$init_dir,$file_dir,g" ED2IN #change the baseline file path everywhere
		sed -i "s,/dummy/path,${file_dir},g" ED2IN # set the file path
		sed -i "s,NL%ED_MET_DRIVER_DB = .*,NL%ED_MET_DRIVER_DB = '${met_path}/ED_MET_DRIVER_HEADER',g" ED2IN # set the file path
		sed -i "s,NL%FFILOUT = .*,NL%FFILOUT = ${new_analy},g" ED2IN # set the file path
		sed -i "s,NL%SFILOUT = .*,NL%SFILOUT = ${new_histo},g" ED2IN # set the file path

	    sed -i "s/NL%RUNTYPE  = .*/NL%RUNTYPE  = 'HISTORY'/" ED2IN # change from .css/.pss to history
        sed -i "s/NL%IED_INIT_MODE   = .*/NL%IED_INIT_MODE   = 5/" ED2IN # change from bare ground to .css/.pss run
        sed -i "s,SFILIN   = .*,SFILIN   = '${init_dir}/${SITE}/histo/${SITE}',g" ED2IN # set initial file path to the SAS spin folder
        sed -i "s/NL%IYEARA   = .*/NL%IYEARA   = ${startyear}/" ED2IN # Set runs start year
        sed -i "s/NL%IMONTHA  = .*/NL%IMONTHA  = 01/" ED2IN # Set runs start month
        sed -i "s/NL%IDATEA   = .*/NL%IDATEA   = 01/" ED2IN # Set runs start day
        sed -i "s/NL%IYEARZ   = .*/NL%IYEARZ   = ${finalyear}/" ED2IN # Set runs last year
        sed -i "s/NL%IMONTHZ  = .*/NL%IMONTHZ  = 01/" ED2IN # Set runs last month
        sed -i "s/NL%IDATEZ   = .*/NL%IDATEZ   = 01/" ED2IN # Set runs last day
        sed -i "s/NL%IYEARH   = .*/NL%IYEARH   = ${startyear}/" ED2IN # Set histo year
        sed -i "s/NL%IMONTHH  = .*/NL%IMONTHH  = 01/" ED2IN # Set histo month
        sed -i "s/NL%IDATEH   = .*/NL%IDATEH   = 01/" ED2IN # Set histo day

        sed -i "s/NL%METCYC1     =.*/NL%METCYC1     = $metfirst/" ED2IN # Set met start
        sed -i "s/NL%METCYCF     =.*/NL%METCYCF     = $metlast/" ED2IN # Set met end


        sed -i "s/NL%INCLUDE_THESE_PFT =.*/NL%INCLUDE_THESE_PFT = $pft_now/" ED2IN # set possible PFTs
	    sed -i "s/NL%SM_FIRE         = .*/NL%SM_FIRE         = ${fire[FILE]}/" ED2IN # adjust fire threshold
	    sed -i "s/NL%IANTH_DISTURB   = .*/NL%IANTH_DISTURB   = ${ianth[FILE]}/" ED2IN # turn on disturbance
	    if [[ ${ianth[FILE]} == 2 ]]; then
	    	mgmt_path="${setup_dir}/${mgmt[FILE]}"
	    	sed -i "s,LANDUSEFILE,${mgmt_path},g" ED2IN # set the file path
	    fi

	popd	

	chmod -R a+rwx ${file_path}

done

# git stash # stash the pulled file so we don't get confilcts

