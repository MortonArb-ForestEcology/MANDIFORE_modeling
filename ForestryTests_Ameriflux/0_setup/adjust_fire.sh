# For preservation runs, we need to lower the fire threshold after the spin has finished


sed -i "s/NL%RUNTYPE  = .*/NL%RUNTYPE  = 'HISTORY'/" ED2IN # change from bare ground to .css/.pss run
sed -i "s/NL%IED_INIT_MODE   = .*/NL%IED_INIT_MODE   = 5/" ED2IN # change from bare ground to .css/.pss run
sed -i "s,SFILIN   = .*,SFILIN   = HISTOPATH,g" ED2IN # set initial file path to the SAS spin folder
sed -i "s/NL%SM_FIRE         = .*/NL%SM_FIRE         = SMFIRE/" ED2IN # adjust fire threshold

sed -i "s/NL%IYEARA   = .*/NL%IYEARA   = 2006/" ED2IN # Set first year
sed -i "s/NL%IMONTHA  = .*/NL%IMONTHA  = 01/" ED2IN # Set first month
sed -i "s/NL%IDATEA   = .*/NL%IDATEA   = 01/" ED2IN # Set first day
sed -i "s/NL%IYEARH   = .*/NL%IYEARH   = 2006/" ED2IN # Set first year
sed -i "s/NL%IMONTHH  = .*/NL%IMONTHH  = 01/" ED2IN # Set first month
sed -i "s/NL%IDATEH   = .*/NL%IDATEH   = 01/" ED2IN # Set first day

sed -i "s/NL%IYEARZ   = .*/NL%IYEARZ   = 2101/" ED2IN # Set last year
sed -i "s/NL%IMONTHZ  = .*/NL%IMONTHZ  = 01/" ED2IN # Set last month
sed -i "s/NL%IDATEZ   = .*/NL%IDATEZ   = 01/" ED2IN # Set last day
