# Get Experimental Design from Google Drive and save it as a csv as a key for our ED runs
library(googlesheets)

# Register the Experimental Design Google sheet
# Note: If doing this for the first time, it will open your browser and 
#       you'll need to enter a key
exp.design.wb <- gs_title("Test_Climate_x_Mgmt_ExperimentalDesign")
exp.design.wb # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
exp.design <- data.frame(gs_read(exp.design.wb, ws="Experimental Design"))
summary(exp.design)

# Not ready to do plantation yet, so we'll just do rows 1-8
# exp.design <- exp.design[1:8,]
exp.design

write.csv(exp.design, file="ExperimentalDesign_Test.csv", na="", row.names=F, quote = F, fileEncoding="utf8")
