#!/bin/bash

# This script cleans up all the spin initial & spin finish that happened before the
# automated file management was included in the run scripts
# file_base=MortonArb_ed_runs.v3 # whatever you want the base output file path to be



# ---------------------
# Clean up Runs
# ---------------------
runs_dir=MortonArb_ed_runs.v3
runsOut=COMPRESSED_MortonArb_ed_runs.v3/
mkdir ${runsOut}

pushd $runs_dir
	runs_done=(*)
popd

# # -------
# # Skip files that were already done
# # -------
# files_skip=(lat35.25lon-79.75 lat35.25lon-79.75 lat35.25lon-79.75 lat35.25lon-94.75 lat35.25lon-99.75 lat37.75lon-77.25 lat37.75lon-82.25 lat47.25lon-95.25 lat47.75lon-67.25 lat47.75lon-82.25 lat47.75lon-92.25 lat47.75lon-97.25) # Right now these are from Betsy and Ann

# for REMOVE in ${files_skip[@]}
# do
#	 init_done=(${init_done[@]/$REMOVE/})
# done
# -------

pushd $runs_dir
for SITE in ${runs_done[@]}
do
	tar -jcvf ../${runsOut}/${SITE}.tar.bz2 ${SITE}
	 	# popd
done
popd
# ---------------------
