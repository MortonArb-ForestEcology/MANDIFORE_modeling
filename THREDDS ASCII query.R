#Thredds query -- can't just access netcdf, but this look promising'
# http://dap.ceda.ac.uk/thredds/dodsC/badc/cmip5/data/cmip5/output1/BCC/bcc-csm1-1/rcp45/6hr/atmos/6hrLev/r1i1p1/latest/ta/ta_6hrLev_bcc-csm1-1_rcp45_r1i1p1_200601010000-200612311800.nc.html
# http://dap.ceda.ac.uk/thredds/dodsC/badc/cmip5/data/cmip5/output1/BCC/bcc-csm1-1/rcp45/6hr/atmos/6hrLev/r1i1p1/latest/ta/ta_6hrLev_bcc-csm1-1_rcp45_r1i1p1_200601010000-200612311800.nc.ascii?lat%5B0:1:63%5D
# Code is start:frequency:end ?lat%5B46:1:46%5D,lon%5B32:1:32%5D
#                                                                     ta grid: time   -     lev     - lat         - lon
# To get temperature for our grid cell: ?lat%5B46:1:46%5D,lon%5B32:1:32%5D,ta%5B0:1:0%5D%5B0:1:0%5D%5B46:1:46%5D%5B32:1:32%5D

tnc <- ncdf4::nc_open("http://dap.ceda.ac.uk/thredds/fileServer/badc/cmip5/data/cmip5/output1/BCC/bcc-csm1-1/rcp45/6hr/atmos/6hrLev/r1i1p1/latest/ta/ta_6hrLev_bcc-csm1-1_rcp45_r1i1p1_200601010000-200612311800.nc")
summary(tnc)


test <- ncdf4::nc_open("http://dap.ceda.ac.uk/thredds/dodsC/badc/cmip5/data/cmip5/output1/BCC/bcc-csm1-1/rcp45/6hr/atmos/6hrLev/r1i1p1/latest/ta/ta_6hrLev_bcc-csm1-1_rcp45_r1i1p1_200601010000-200612311800.nc")


test <- ncdf4::nc_open("http://dap.ceda.ac.uk/thredds/catalog/badc/cmip5/data/cmip5/output1/BCC/bcc-csm1-1/rcp45/3hr/atmos/3hr/r1i1p1/latest/tas/tas_3hr_bcc-csm1-1_rcp45_r1i1p1_228101010000-230012312100.nc")
test

test <- ncdf4::nc_open("http://dap.ceda.ac.uk/thredds/fileServer/badc/cmip5/data/cmip5/output1/BCC/bcc-csm1-1/rcp45/3hr/atmos/3hr/r1i1p1/latest/tas/tas_3hr_bcc-csm1-1_rcp45_r1i1p1_228101010000-230012312100.nc")

test <- ncdf4::nc_open("http://dap.ceda.ac.uk/thredds/dodsC/badc/cmip5/data/cmip5/output1/BCC/bcc-csm1-1/rcp45/6hr/atmos/6hrLev/r1i1p1/latest/ta/ta_6hrLev_bcc-csm1-1_rcp45_r1i1p1_200601010000-200612311800.nc?lat[0:1:63],lon[0:1:127]")


# ---------------------
# ESGF seems to be working; following combos with 
# Folder  cmip5.output1.NASA-GISS.GISS-E2-H.rcp45.3hr.atmos.3hr.r6i1p3.v20160512/
# Folder  cmip5.output2.NASA-GISS.GISS-E2-H.rcp45.3hr.atmos.3hr.r6i1p3.v20160512/
# Folder  cmip5.output1.NASA-GISS.GISS-E2-R.rcp45.3hr.atmos.3hr.r6i1p1.v20160513/	 	--
# Folder  cmip5.output1.NASA-GISS.GISS-E2-R.rcp45.3hr.atmos.3hr.r6i1p3.v20160513/
# Folder  cmip5.output1.NASA-GISS.GISS-E2-R.rcp85.3hr.atmos.3hr.r2i1p1.v20160513/	 	--
# Folder  cmip5.output1.NASA-GISS.GISS-E2-R.rcp85.3hr.atmos.3hr.r2i1p3.v20160513/
# Folder  cmip5.output2.NASA-GISS.GISS-E2-H.rcp85.3hr.atmos.3hr.r2i1p1.v20160512/	 	--
# Folder  cmip5.output2.NASA-GISS.GISS-E2-H.rcp85.3hr.atmos.3hr.r2i1p3.v20160512/
# Folder  cmip5.output2.NASA-GISS.GISS-E2-R.rcp85.3hr.atmos.3hr.r2i1p1.v20160513/	 	--
# Folder  cmip5.output2.NASA-GISS.GISS-E2-R.rcp85.3hr.atmos.3hr.r2i1p3.v20160513/



# https://dataserver.nccs.nasa.gov/thredds/catalog/CMIP5/ESGF/GISS/rcp45/E2-R_rcp45_r6i1p1_3hr/catalog.html?dataset=GISS/rcp45/E2-R_rcp45_r6i1p1_3hr/tas_3hr_GISS-E2-R_rcp45_r6i1p1_201101010300-202101010000.nc
test <- ncdf4::nc_open("https://dataserver.nccs.nasa.gov/thredds/dodsC/CMIP5/ESGF/GISS/rcp45/E2-R_rcp45_r6i1p1_3hr/tas_3hr_GISS-E2-R_rcp45_r6i1p1_201101010300-202101010000.nc")
summary(test$var)
test$var$time # Units = days since Jan 1

dat.time <- ncdf4::ncvar_get(test, "time")
dat.latbnds <- ncdf4::ncvar_get(test, "lat_bnds")
dat.lonbnds <- ncdf4::ncvar_get(test, "lon_bnds")
# First dim = lon (144); dim2 = lat (44), dim3 = time (29200)
dat.tas <- ncdf4::ncvar_get(test, "tas", start=c(36,66,1), count=c(1,1,length(dat.time)))
dim(dat.tas)
plot(dat.tas[1:24]-273.16,type="l")

# NC subset
# Website: https://dataserver.nccs.nasa.gov/thredds/ncss/grid/CMIP5/ESGF/GISS/rcp45/E2-R_rcp45_r6i1p1_3hr/tas_3hr_GISS-E2-R_rcp45_r6i1p1_201101010300-202101010000.nc/dataset.html
#Query URL: https://dataserver.nccs.nasa.gov/thredds/ncss/CMIP5/ESGF/GISS/rcp45/E2-R_rcp45_r6i1p1_3hr/tas_3hr_GISS-E2-R_rcp45_r6i1p1_201101010300-202101010000.nc?var=tas&north=41.84&west=88.03&east=88.10&south=41.80&disableProjSubset=on&horizStride=1&time_start=2011-01-01T03%3A00%3A00Z&time_end=2021-01-01T00%3A00%3A00Z&timeStride=1