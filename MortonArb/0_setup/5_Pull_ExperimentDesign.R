# Get Experimental Design from Google Drive and save it as a csv as a key for our ED runs

# Register the Experimental Design Google sheet
# Note: If doing this for the first time, it will open your browser and 
#       you'll need to enter a key

# Get the particular sheet & coerce it into a data frame rather than something special
exp.design <- googlesheets4::read_sheet(ss="1EROcBX_JI5828g6OLVcfUqz4wMu0cMMhCok0jSbRXD8", sheet="MortonArb_Experiment1", col_types = "c")
exp.design <- data.frame(exp.design)
summary(exp.design)
head(exp.design)

# Not ready to do plantation yet, so we'll just do rows 1-8
# exp.design <- exp.design[1:8,]
tail(exp.design)

write.csv(exp.design, file="MortonArb_CaseStudy_Experiment.csv", na="", row.names=F, quote = F, fileEncoding="utf8")
