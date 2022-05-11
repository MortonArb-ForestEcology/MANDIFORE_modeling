neon <- read.csv("NEON_Field_Site_Metadata_20220412.csv")
neon <- neon[neon$field_site_type %in% c("Core Terrestrial", "Gradient Terrestrial") & grepl("Forest", neon$field_dominant_nlcd_classes) & !neon$field_domain_id %in% c("D04", "D06", "D09", "D10", "D11", "D15", "D20"),] # Must be forest site; get rid of things like neotropics, prairies
names(neon); 
dim(neon)
unique(neon$field_domain_id)

# Pull out just the core sites to make our lives easy to start
neon.core <- neon[neon$field_site_type == "Core Terrestrial",]
dim(neon.core)

neon.core <- neon.core[order(neon.core$field_domain_id),]
neon.core[,c("field_domain_id", "field_site_id", "field_site_name", "field_site_type", "field_latitude", "field_longitude", "field_soil_subgroup", "field_dominant_nlcd_classes")]
write.csv(neon.core, "NEON_Field_Site_FOREST_CORE.csv", row.names=F)


# Now extractign the gradient sites as bonus-land territory
neon.extra <- neon[neon$field_site_type == "Gradient Terrestrial",]
dim(neon.extra)

neon.extra <- neon.extra[order(neon.extra$field_domain_id),]
neon.extra[,c("field_domain_id", "field_site_id", "field_site_name", "field_site_type", "field_latitude", "field_longitude", "field_soil_subgroup", "field_dominant_nlcd_classes")]
write.csv(neon.extra, "NEON_Field_Site_FOREST_GRADIENT.csv", row.names=F)
