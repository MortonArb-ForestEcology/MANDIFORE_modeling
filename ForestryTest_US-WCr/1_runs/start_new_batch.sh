#!bin/bash
# This file sets up runs based on a common table of settings
# Christy Rollinson, crollinson@mortonarb.org


# Define constants & file paths for the scripts
file_base=/mnt/data/crollinson/MANDIFORE_modeling/ForestryTest_US-WCr/ # whatever you want the base output file path to be
EDI_base=/home/models/ED_inputs/ # The location of basic ED Inputs for you
met_base=${file_base}ED_MET/

ed_exec=/home/crollinson/ED2/ED/build/ed_2.1-opt # Location of the ED Executable
file_dir=${file_base}/1_runs/ed_runs.v1/ # Where everything will go
setup_dir=${file_base}/0_setup # Where some constant setup files are
site_file=${setup_dir}/ExperimentalDesign_AGU2018.csv # # Path to list of ED sites w/ status

# Some more constatns
finalyear=2032
finalfull=2031
n=2

# Making the file directory if it doesn't already exist
mkdir -p $file_dir

# Extract the file names of sites that haven't been started yet
runs_all=($(awk -F ',' 'NR>1 {print $1}' ${site_file}))
lat_all=($(awk -F ',' 'NR>1 {print $2}' ${site_file}))
lon_all=($(awk -F ',' 'NR>1 {print $3}' ${site_file}))

GCM_all=($(awk -F ',' 'NR>1 {print $7}' ${site_file}))
scenario_all=($(awk -F ',' 'NR>1 {print $8}' ${site_file}))

fire_all=($(awk -F ',' 'NR>1 {print $12}' ${site_file}))
ianth_all=($(awk -F ',' 'NR>1 {print $10}' ${site_file}))
mgmt_all=($(awk -F ',' 'NR>1 {print $11}' ${site_file}))

# Get the list of what grid runs have already finished spinups
pushd $file_dir
	file_done=(*-*)
popd
file_done=(${file_done[@]/"*-*"/})

# Because we want to preserve the order of runs, I can't find away around doing a loop
# - This is slower than other options, but makes sure we still do our controls first
# - DO NOT imitate this with a large array
runs=()
lat=()
lon=()
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
		lat+=("${lat_all[i]}")
		lon_=("${lon_all[i]}")
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
	SITE=${runID[FILE]}
	echo $SITE

	GCM_now=${GCM[FILE]}
	GCM_now=${GCM_now/"-"/"_"}
	# -----------------------------------------------------------------------------
	# What needs to change:
	# 1. Met Header -- needs to go to specific GCM
	# 2. File paths -- need to point to specific dirictory
	# -----------------------------------------------------------------------------
	# -----------------------------------------------------------------------------


	# File Paths
    new_analy="'${file_dir}${SITE}/analy/${SITE}'"
    new_histo="'${file_dir}${SITE}/histo/${SITE}'"
    old_analy="'${file_dir}TEST/analy/TEST'"
    old_histo="'${file_dir}TEST/histo/TEST'"
    newbase=${file_dir}/$SITE
    oldbase=${file_dir}/TEST
	oldname=TESTinit
	met_path=${met_base}/${GCM_now}_${scenario}_r1i1p1


	file_path=${file_dir}/${SITE}/

	mkdir -p ${file_path} 
	
	pushd ${file_path}
		# Creating the default file structure and copying over the base files to be modified
		mkdir -p histo analy
		ln -s $ed_exec
		cp ../../ED2IN_Base_WCr ED2IN
		cp ${setup_dir}/PFTParams_WCr.xml .
		
		# ED2IN Changes	    
		sed -i "s,/dummy/path,${file_dir},g" ED2IN # set the file path
		sed -i "s,/met/path,${met_path},g" ED2IN # set the file path
	    sed -i "s,TEST,${SITE},g" ED2IN #change site ID
	    sed -i "s/NL%INCLUDE_FIRE    = .*/NL%INCLUDE_FIRE    = ${fire[FILE]}/" ED2IN # turn on fire if run w/ fire on
	    sed -i "s/NL%IANTH_DISTURB   = .*/NL%IANTH_DISTURB   = ${ianth[FILE]}/" ED2IN # turn on disturbance
	    if [[ ${ianth[FILE]} == 2 ]]; then
	    	sed -i "s/NL%LU_DATABASE      = .*/NL%LU_DATABASE      = ${mgmt[FILE]}/" ED2IN # set fire intensity parameter
	    fi


		# spin spawn start changes -- 
		# Note: spins require a different first script because they won't have any 
		#       histo files to read
		cp ${setup_dir}spawn_startloops_spinstart.sh .
		sed -i "s/USER=.*/USER=${USER}/" spawn_startloops_spinstart.sh
		sed -i "s/SITE=.*/SITE=${SITE}/" spawn_startloops_spinstart.sh 		
		sed -i "s/finalyear=.*/finalyear=${finalfull}/" spawn_startloops_spinstart.sh 		
	    sed -i "s,/dummy/path,${file_path},g" spawn_startloops_spinstart.sh # set the file path
	    sed -i "s,sub_post_process.sh,sub_post_process_spininit.sh,g" spawn_startloops_spinstart.sh # set the file path

		# spawn restarts changes
		cp ${setup_dir}spawn_startloops.sh .
		sed -i "s/USER=.*/USER=${USER}/" spawn_startloops.sh
		sed -i "s/SITE=.*/SITE=${SITE}/" spawn_startloops.sh 		
		sed -i "s/finalyear=.*/finalyear=${finalfull}/" spawn_startloops.sh 		
	    sed -i "s,/dummy/path,${file_path},g" spawn_startloops.sh # set the file path
	    sed -i "s,sub_post_process.sh,sub_post_process_spininit.sh,g" spawn_startloops.sh # set the file path

		# adjust integration step changes
		cp ${setup_dir}adjust_integration_restart.sh .
		sed -i "s/USER=.*/USER=${USER}/" adjust_integration_restart.sh
		sed -i "s/SITE=.*/SITE=${SITE}/" adjust_integration_restart.sh 		
		
#  		sh spawn_startloops_spinstart.sh
	popd	

	chmod -R a+rwx ${file_path}

done

# git stash # stash the pulled file so we don't get confilcts

