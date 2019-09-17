#!bin/bash
# This file sets up runs based on a common table of settings
# Christy Rollinson, crollinson@mortonarb.org

# Number of sites you want to run
n=4 # 4 = 1 site x 1 management x 1 GCM x 2 rcps x 2 CO2 scenarios

# Define constants & file paths for the scripts
file_base=/mnt/data/crollinson/MANDIFORE_modeling/MortonArb # whatever you want the base output file path to be
EDI_base=/home/models/ED_inputs/ # The location of basic ED Inputs for you
met_base=${file_base}/met_ed/

ed_exec=/home/crollinson/ED2/ED/build/ed_2.1-opt # Location of the ED Executable
file_dir=${file_base}/1_runs/MortonArb_ed_runs.v1 # Where everything will go
setup_dir=${file_base}/0_setup # Where some constant setup files are
site_file=${setup_dir}/MortonArb_CaseStudy_Experiment.csv # # Path to list of ED sites w/ status
# init_dir=${file_base}/1_spin/ed_spin.v3
lu_dir=${file_base}/lu_files # Where some constant setup files are

# Want to do a 25-year spin that ends in the year we want to starts
startyear=2018
finalyear=2101 #
finalfull=2100

# The range of met years to cycle for the transient spin
# We're just going to do the first year
metfirst=2018
metlast=2100

# Making the file directory if it doesn't already exist
mkdir -p $file_dir

# Extract the file names of sites that haven't been started yet
runs_all=($(awk -F ',' 'NR>1 {print $1}' ${site_file}))
#sites_all=($(awk -F ',' 'NR>1 {print $2}' ${site_file}))
#lat_all=($(awk -F ',' 'NR>1 {print $4}' ${site_file}))
#lon_all=($(awk -F ',' 'NR>1 {print $5}' ${site_file}))

clay_all=($(awk -F ',' 'NR>1 {print $6}' ${site_file}))
sand_all=($(awk -F ',' 'NR>1 {print $7}' ${site_file}))
depth_all=($(awk -F ',' 'NR>1 {print $8}' ${site_file}))

finit_all=($(awk -F ',' 'NR>1 {print $9}' ${site_file}))
#pft_all=($(awk -F ',' 'NR>1 {print $9}' ${site_file}))

GCM_all=($(awk -F ',' 'NR>1 {print $13}' ${site_file}))
scenario_all=($(awk -F ',' 'NR>1 {print $14}' ${site_file}))

#fire_all=($(awk -F ',' 'NR>1 {print $18}' ${site_file}))
#ianth_all=($(awk -F ',' 'NR>1 {print $15}' ${site_file}))
mgmt_all=($(awk -F ',' 'NR>1 {print $17}' ${site_file}))

co2_all=($(awk -F ',' 'NR>1 {print $15}' ${site_file}))

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
#lat=()
#lon=()
clay=()
sand=()
depth=()
finit=()
#pft=()
GCM=()
scenario=()
#fire=()
#ianth=()
mgmt=()
co2=()

for((i=0;i<${#runs_all[@]};i++)); do 
	RUN=${runs_all[i]}
    TEST=( ${file_done[@]/$RUN/} ) # Remove element from array

	# If the length of TEST is still the same, we haven't done it yet
    if [[ ${#TEST[@]} == ${#file_done[@]} ]]; then
		runs+=("$RUN")
		sites+=("${sites_all[i]}")
		#lat+=("${lat_all[i]}")
		#lon+=("${lon_all[i]}")
		clay+=("${clay_all[i]}")
		sand+=("${sand_all[i]}")
		depth+=("${depth_all[i]}")
		finit+=("${finit_all[i]}")
		#pft+=("${pft_all[i]}")
		GCM+=("${GCM_all[i]}")
		scenario+=("${scenario_all[i]}")
		#fire+=("${fire_all[i]}")
		#ianth+=("${ianth_all[i]}")
		mgmt+=("${mgmt_all[i]}")
		co2+=("${co2_all[i]}")		
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

	#lat_now=${lat[FILE]}
	#lon_now=${lon[FILE]}

	#pft_now=${pft[FILE]}
	#pft_now=${pft_now//"-"/","}

	# ---------------------------------------------
	# subsetting soil layers based on soil depth; deepest layer = soil_depth
	# ---------------------------------------------
	SLZ_BASE=(-4.00 -3.00 -2.17 -1.50 -1.10 -0.80 -0.60 -0.45 -0.30 -0.20 -0.12 -0.06)
	SLMSTR_BASE=(1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00)
	STGOFF_BASE=(0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00)
	NZG=${#SLZ_BASE[@]}
	depth_min=(-0.15) # Setting an artificial minimum soil depth of 15 cm; note: this gets us a min of 3 soil layers


	# If the actual soil depth is less than what we specified as the minimum, use our 
	# artificial minimum (default = 0.15 cm)
	BOTTOM=${depth[FILE]}
	if [[(("${BOTTOM}" < "${depth_min}"))]]
	then
		BOTTOM=$depth_min
	fi

	SLZ=()
	SLMSTR=()
	STGOFF=()
	for ((i=0; i<$NZG; i++));
	do
	if [[(("${SLZ_BASE[$i]}" < "${BOTTOM}"))]]
	then
		SLZ=(${SLZ[@]} ${SLZ_BASE[$i]},)
		SLMSTR=(${SLMSTR[@]} ${SLMSTR_BASE[$i]},)
		STGOFF=(${STGOFF[@]} ${STGOFF_BASE[$i]},)
	fi
	done

	# Defining some new index numbers
	NZG=${#SLZ[@]} # Number soil layers
	nz_last=$(($NZG - 1)) # index num of the last layer

	# Replace the deepest soil layer with soil depth
	SLZ=($BOTTOM, ${SLZ[@]:1:$nz_last})

	# Getting rid of trailing commas
	SLZ[$nz_last]=${SLZ[$nz_last]:0:5}
	SLMSTR[$nz_last]=${SLMSTR[$nz_last]:0:4}
	STGOFF[$nz_last]=${STGOFF[$nz_last]:0:4}

	# Flattening the array into a single "value"
	SLZ=$(echo ${SLZ[@]})
	SLMSTR=$(echo ${SLMSTR[@]})
	STGOFF=$(echo ${STGOFF[@]})
	echo ${SLZ}
	echo ${SLMSTR}
	echo ${STGOFF}
	# ---------------------------------------------

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
		cp ${setup_dir}/ED2IN_Base_MortonArb ED2IN
		# cp ${setup_dir}/PFTParams_MortonArb.xml .
		
		# ED2IN Changes	    
		#sed -i "s/NL%EXPNME =.*/NL%EXPNME = 'MANDIFORE TEST'/" ED2IN # change the experiment name

		sed -i "s,/dummy/path,${file_dir},g" ED2IN # set the file path
		sed -i "s,/met/path,${met_path},g" ED2IN # set the file path


	    sed -i "s,TEST,${SITE},g" ED2IN #change site ID
	    sed -i "s,METSITE,${sites[FILE]},g" ED2IN #change site ID

	    sed -i "s/NL%RUNTYPE  = .*/NL%RUNTYPE  = 'INITIAL'/" ED2IN # change from bare ground to .css/.pss run
        sed -i "s/NL%IED_INIT_MODE   = .*/NL%IED_INIT_MODE   = 6/" ED2IN # change from bare ground to .css/.pss run
        sed -i "s,SFILIN   = .*,SFILIN   = '${file_base}/init_files/${finit[FILE]}',g" ED2IN # set initial file path to the SAS spin folder

        sed -i "s/NL%IYEARA   = .*/NL%IYEARA   = ${startyear}/" ED2IN # Set first year
        sed -i "s/NL%IMONTHA  = .*/NL%IMONTHA  = 06/" ED2IN # Set first month
        sed -i "s/NL%IDATEA   = .*/NL%IDATEA   = 01/" ED2IN # Set first day
		sed -i "s/NL%IYEARH   = .*/NL%IYEARH   = ${startyear}/" ED2IN # Set first year
		sed -i "s/NL%IMONTHH  = .*/NL%IMONTHH  = 06/" ED2IN # Set first month
		sed -i "s/NL%IDATEH   = .*/NL%IDATEH   = 01/" ED2IN # Set first day

        sed -i "s/NL%IYEARZ   = .*/NL%IYEARZ   = ${finalyear}/" ED2IN # Set last year
        sed -i "s/NL%IMONTHZ  = .*/NL%IMONTHZ  = 01/" ED2IN # Set last month
        sed -i "s/NL%IDATEZ   = .*/NL%IDATEZ   = 01/" ED2IN # Set last day

		sed -i "s,NL%ED_MET_DRIVER_DB = .*,NL%ED_MET_DRIVER_DB = '${met_path}/ED_MET_DRIVER_HEADER',g" ED2IN # set the file path

		sed -i "s,NL%FFILOUT = .*,NL%FFILOUT = ${new_analy},g" ED2IN # set the file path
		sed -i "s,NL%SFILOUT = .*,NL%SFILOUT = ${new_histo},g" ED2IN # set the file path

        sed -i "s/NL%METCYC1     =.*/NL%METCYC1     = $metfirst/" ED2IN # Set met start
        sed -i "s/NL%METCYCF     =.*/NL%METCYCF     = $metlast/" ED2IN # Set met end

	    #sed -i "s/POI_LAT  =.*/POI_LAT  = $lat_now/" ED2IN # set site latitude
        #sed -i "s/POI_LON  =.*/POI_LON  = $lon_now/" ED2IN # set site longitude
        sed -i "s/SLXCLAY =.*/SLXCLAY = ${clay[FILE]}/" ED2IN # set fraction soil clay
        sed -i "s/SLXSAND =.*/SLXSAND = ${sand[FILE]}/" ED2IN # set fraction soil sand
        sed -i "s/NZG =.*/NZG = $NZG/" ED2IN # set number soil layers
        sed -i "s/SLZ     =.*/SLZ = $SLZ/" ED2IN # set soil depths
        sed -i "s/SLMSTR  =.*/SLMSTR = $SLMSTR/" ED2IN # set initial soil moisture
        sed -i "s/STGOFF  =.*/STGOFF = $STGOFF/" ED2IN # set initial soil temp offset

        #sed -i "s/NL%INCLUDE_THESE_PFT =.*/NL%INCLUDE_THESE_PFT = $pft_now/" ED2IN # set possible PFTs
        #sed -i "s/NL%IEDCNFGF   =.*/NL%IEDCNFGF   = 'PFTParams_MANDIFORE_${SITE}.xml'/" ED2IN # set possible PFTs

	    # sed -i "s/NL%SM_FIRE         = .*/NL%SM_FIRE         = ${fire[FILE]}/" ED2IN # adjust fire threshold
	    # sed -i "s/NL%IANTH_DISTURB   = .*/NL%IANTH_DISTURB   = ${ianth[FILE]}/" ED2IN # turn on disturbance
	    #if [[ ${ianth[FILE]} == 2 ]]; then
	    	mgmt_path="${lu_dir}/${mgmt[FILE]}"
	    	sed -i "s,LANDUSEFILE,${mgmt_path},g" ED2IN # set the file path
	    #fi
	    	    
	    # If we're turning of CO2, that requires using a met header that SHOULD use a constant CO2 of 380 ppm
	    if [[ ${co2[FILE]} == 380 ]]; then
	    	sed -i "s,NL%ED_MET_DRIVER_DB = .*,NL%ED_MET_DRIVER_DB = '${met_path}/ED_MET_DRIVER_HEADER_staticCO2',g" ED2IN # set the file path
	    fi

	popd	

	chmod -R a+rwx ${file_path}

done

# git stash # stash the pulled file so we don't get confilcts

