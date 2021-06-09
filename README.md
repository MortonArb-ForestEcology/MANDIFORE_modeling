# MANDIFORE_modeling




# Units

## Mandifore

Fix | Description 
-------|------------
MMEAN | Monthly Average
PY | Polygon-level averages
SI | Site-level averages
PA | Patch-level averages
CO | Cohort-level averages

Something level
 Column name   |   Mandifore Name  |  Plain text |  Unit   
---------------|-------------------|-------------|-------------|
soil.depth | SLZ | | m3/m3???

## Site Level

 Column name   |   Mandifore Name  |  Plain text |  Unit   
---------------|-------------------|-------------|-------------|
tair | MMEAN_ATM_TEMP_PY | Air Temperature |  K
precipf | MMEAN_PCPG_PY | Precipitation | kg/m2/mo
lwdown | MMEAN_ATM_RLONG_PY | Longwave radiation | W/m2
swdown | MMEAN_ATM_RSHORT_PY | Shortwave radiation | W/m2
qair | MMEAN_ATM_SHV_PY | |kg/kg
psurf | MMEAN_ATM_PRSS_PY | | Pa
wind | MMEAN_ATM_VELS_PY | | m/s
CO2 | MMEAN_ATM_TEMP_PY | | umol/umol
basal.area.tree | BASAL_AREA_PY | | cm2/m2??
density.tree | NPLANT_PY | | tree/m2??
agb | AGB_PY | | kg/m2??
gpp | MMEAN_GPP_PY | Gross Primary Product | kgC/m2/mo
npp | MMEAN_NPP_PY | | kgC/m2/mo
nee | MMEAN_NEP_PY | | kgC/m2/mo
cwd | MMEAN_CWD_C_PY | | kgC/m2
soil.c.fast | MMEAN_FAST_SOIL_C_PY | | kgC/m2
soil.c.slow | MMEAN_SLOW_SOIL_C_PY | | kgC/m2
soil.c.struc | MMEAN_STRUCT_SOIL_C_PY | | kgC/m2
albedo.short | MMEAN_ALBEDO_PY | | 
par.ground | MMEAN_PAR_GND_PY | |
soil.moist.surf | MMEEAN_SOIL_WATER_PY * SLZ | | m3/m3 
soil.moist.deep | MMEEAN_SOIL_WATER_PY | | m3/m3
soil.moistpot.surf | MMEAN_SOIL_MSTPOT_PY * SLZ | | 
soil.moistpot.deep | MMEAN_SOIL_MSTPOT_PY | | 
soil.temp.surf | MMEAN_SOIL_TEMP_PY * SLZ | | m3/m3
soil.temp.deep | MMEAN_SOIL_TEMP_PY | | m3/m3
runoff.surface | MMEAN_RUNOFF_PY | | kg/m2/mo?
runoff.subsurf | MMEAN_DRAINAGE_PY | | kg/m2/mo?
transp | MMEAN_TRANSP_PY | | kg/m2/mo
snow.depth | MMEAN_SFCW_DEPTH_PY | Depth of snow | m
swe | MMEAN_SFCW_MASS_PY | | kg/m2
albedo | MMEAN_ALBEDO_PY | | 
lai | MMEAN_LAI_PY | leaf area index | 

## Patch Level

 Column name   |   Mandifore Name  |  Plain text |  Unit   
---------------|-------------------|-------------|-------------|
age | AGE | | 
area.patch | AREA | | 
n.cohorts | PACO_N | Number of cohorts for each patch | 
patch.co.start | PACO_ID | Index of the first cohort of each patch | 

## Cohort Level

 Column name   |   Mandifore Name  |  Plain text |  Unit   
---------------|-------------------|-------------|-------------|
pft | PFT | Plant Functional Type | 5=grass; 9=early hard; 10=mid hard; 11=late hard
dbh | DBH | Diameter at Breast Height | cm
ba | BA_CO | Basal Area | cm2/tree
carbs | CB | Carbon Balance | scale 0 to 1; 1 = not stressed
stress | CBR_BAR | Running mean relative carbon balance | scale = -1 to 1
height | HITE | Height | m
lai | LAI_CO | leaf area index |
agb | AGB_CO | kgC/tree |
dens.pch | NPLANT | trees/m2 |

