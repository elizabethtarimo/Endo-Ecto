setwd("~/Desktop/Current Research/Endo-Ecto")
library(treeplyr)
library(dplyr)
library(ape)
library(rgbif)
library(splus2R)
library(countrycode)
library(CoordinateCleaner)
library(raster)
library(dplyr)
library(geiger)
library(treeplyr)
library(data.table)
library(ggplot2)
library(phytools)
library(tidyr)


#857 species (w/ Tb data)
# Create separate csv of enodtherm species and match with GBIF backbone
endo_dat <- readRDS("Tbdat.rds")
#View(endo_dat$dat)  #857
#View(data.frame(endo_dat$phy$tip.label))

endo_sp_list <- as.data.frame(endo_dat$phy$tip.label)
#View(endo_sp_list)

#substittute the _ for just space to later match
endo_sp_list <- as.character(sub("_", " ", endo_sp_list$`endo_dat$phy$tip.label`))

#View(endo_sp_list)
class(endo_sp_list)

#back to df
endo_sp_list <- as.data.frame(endo_sp_list)
class(endo_sp_list)
#View(endo_sp_list)

write.csv(endo_sp_list, "~/Desktop/Current Research/Endo&EctoFile/Mammals/endo_list.csv")
#View(endo_sp_list)

# Match endotherm species csv list file to GBIF backbone
# Species Name Match - GBIF
# Set directory to GBIF match csv file and get GBIF taxon keys

setwd("~/Desktop/Current Research/Endo&EctoFile/Mammals")
gbif_sp_lookup_endo <- read.csv("endo_gbif_match.csv", header = TRUE)
#View(gbif_sp_lookup_endo)  #857

gbif_taxon_keys_endo <- gbif_sp_lookup_endo %>%
  dplyr::filter(matchType =="EXACT" & status =="ACCEPTED") %>%
  dplyr::filter(kingdom == "Animalia" ) %>%
  pull(key)
print(gbif_taxon_keys_endo)
#View(data.frame(gbif_taxon_keys_endo))
#828 keys


#from the account that I already have of GBIF
user <- "elizadaniel"
pwd <- "Eliza6715"
email <- "delizabeth@vt.edu"


# Occ_download will send a download request for all the species in taxon key list
# The gbif website will download all of the occurrences.
?occ_download
occ_dl_endo <- occ_download(pred_in("taxonKey", gbif_taxon_keys_endo), 
                            format="SIMPLE_CSV", user=user, pwd=pwd, email=email)
# Occ_dl_endo has the download key 
occ_dl_endo    #0273398-210914110416597

# Get endotherm occurrences (this throws error)
gbif_endo <- occ_download_import(key = " 0273398-210914110416597")

# Try this method if occ_download_import throws error
#This is the one that worked for me. Took forever though and a risk of a crash
gbif_endo <- occ_download_get(key="0273398-210914110416597", overwrite = TRUE) %>%
  occ_download_import


#saveRDS(gbif_endo, file = "gbif_endo.RDS")
gbif_endo <- readRDS("gbif_endo.RDS")
#View(gbif_endo)


# Make sure scientificNames in gbif object match those in gbif_sp_lookup_endo
gbif_endo$matches <- 0     
match(gbif_endo$scientificName, gbif_sp_lookup_endo$scientificName)
gbif_endo$matches <- match(gbif_endo$scientificName, gbif_sp_lookup_endo$scientificName)
gbif_endo$matches
#View(gbif_endo)

# Remove NA's from matches column
gbif_endo %>% dplyr::filter(!is.na(gbif_endo$matches))

# Group sp_dat by species 
# The number of iteration should be equal to the # of taxon keys 
# (number of species requested for occurrence data)
sp_dat_dl_endo <- gbif_endo %>%
  group_by(species)%>%
  group_split()
#View(sp_dat_dl_endo)

# Save the sp_dat_dl_endo object (takes a while to download for each import)
#saveRDS(sp_dat_dl_endo, file = "sp_dat_dl_endo_save.RDS")
sp_dat_dl_endo <- readRDS("sp_dat_dl_endo_save.RDS")
#View(sp_dat_dl_endo)

#### Select species lat and long data
# skips rows with missing countryCode and scientificName



# use dplyr::select
sp_dat_all_endo <- list()
for (i in 1:length(sp_dat_dl_endo)) {
  if (is.null(sp_dat_dl_endo[[i]])) { next
  }
  if (is.missing(sp_dat_dl_endo[[i]]$countryCode)) { next
  }
  if (is.missing(sp_dat_dl_endo[[i]]$scientificName)) { next
  }
  sp_dat_all_endo[[i]] <- sp_dat_dl_endo[[i]] %>%
    dplyr::select(scientificName, decimalLongitude, decimalLatitude, species, countryCode)
} 
#View(sp_dat_all_endo)
class(sp_dat_all_endo)



#### Remove records without coordinates (NA) for lat and long
## Doing the filters separately works
# Filter NA's for Lat
sp_dat_1_endo <- list()
for (i in 1:length(sp_dat_all_endo)) {
  if (is.null(sp_dat_all_endo[[i]])) { next
  }
  sp_dat_1_endo[[i]] <- sp_dat_all_endo[[i]] %>%
    dplyr::filter(!is.na(sp_dat_all_endo[[i]]$decimalLatitude))
}
#View(sp_dat_1_endo)

# Filter NA's for Long
for (i in 1:length(sp_dat_1_endo)) {
  if (is.null(sp_dat_1_endo[[i]])) { next
  }
  sp_dat_1_endo[[i]] <- sp_dat_1_endo[[i]] %>%
    dplyr::filter(!is.na(sp_dat_1_endo[[i]]$decimalLongitude))
}
#View(sp_dat_1_endo)

# Save sp_dat_1_endo as a RDF (takes a long time to run previous code)
#saveRDS(sp_dat_1_endo, file = "sp_dat_1_endo.RDS")
sp_dat_1_endo <- readRDS("sp_dat_1_endo.RDS")
#View(sp_dat_1_endo)

#### Cleaning and cross-checking gbif data
unlist(sp_dat_1_endo) 
sp_dat_concat_endo <- dplyr::bind_rows(sp_dat_1_endo)
#View(sp_dat_concat_endo)
sp_dat_concat_endo$countryCode <- countrycode(sp_dat_concat_endo$countryCode, 
                                              origin = 'iso2c',destination = 'iso3c')
names(sp_dat_concat_endo)[2:3] <- c("decimallongitude", "decimallatitude")
#Looks better now


# Remove rows with NA values 
sp_dat_concat_endo <- na.omit(sp_dat_concat_endo)
#View(sp_dat_concat_endo)

# Removes rows without a "scientificName"
sp_dat_concat_endo <- sp_dat_concat_endo[sp_dat_concat_endo$scientificName != "",]
#View(sp_dat_concat_endo)

# THIS WORKS (USE THIS METHOD) (Takes a long time to run)
sp_dat_clean_concat_endo <- clean_coordinates(sp_dat_concat_endo)
#View(sp_dat_clean_concat_endo)

# filter out the rows with .summary FALSE (indicates at least one test is FALSE)
sp_dat_clean_endo <- data.frame()
class(sp_dat_clean_endo)
sp_dat_clean_endo <- sp_dat_clean_concat_endo[sp_dat_clean_concat_endo$.summary !=FALSE,]
#View(sp_dat_clean_endo)

# subset scientificName, decimallongitude, decimallatitude, species and cc
sp_dat_clean_endo <- subset(sp_dat_clean_endo, select = c(1,2,3,4,5))
#View(sp_dat_clean_endo)
sp_dat_clean_list_endo <- as.list(sp_dat_clean_endo)
#View(sp_dat_clean_list_endo)
sp_dat_clean_df_endo <- as.data.frame(sp_dat_clean_list_endo)
#View(sp_dat_clean_df_endo)


#I HAVE THIS
# Save sp_dat_clean_df as RDS file
# Save the sp_dat_clean_df object (takes a long time to filer coords)
#saveRDS(sp_dat_clean_df_endo, file = "sp_dat_clean_df_endotherms.RDS")
sp_dat_clean_df_endo <- readRDS("./Mammals/sp_dat_clean_df_endotherms.RDS")
#View(sp_dat_clean_df_endo)

# All presence data
#View(sp_dat_clean_df_endo)
sp_dat_grouped_endo <- sp_dat_clean_df_endo %>%
  group_by(species)%>%
  group_split()
#View(sp_dat_grouped_endo)
sp_dat_presence_endo <- sp_dat_grouped_endo
#View(sp_dat_presence_endo)

########################## Endotherm WorldClim Data ############################
#### Climate data from WorldClim
# Download 19 bioclimatic variables at the 10' resolution (Historical Climate Data)
# bio 10m

dir.create("clim_data")
clim_dat <- getData("worldclim", var="bio", res=10, download = T, path='clim_data')
#View(clim_dat)
plot(clim_dat) 

# gbif data
sp_dat_clean_df_endo



#JUST PULLING THE LONGITUDES AND LATITUDE
# worldclim data (bios)
clim_dat
class(sp_dat_clean_df_endo)
sp_dat_clean_df_endo[,c(2,3)]

### Combining clim_dat and sp_dat_clean_df_endo through sp object
values_endo <- raster::extract(clim_dat, SpatialPoints(sp_dat_clean_df_endo[,c(2,3)], 
                                                       proj4string = clim_dat@crs))
#View(values_endo)
class(values_endo)

# Subsetting annual temp and annual precip
temp_precip_vals_endo <- values_endo[,c(1,12)]
#View(temp_precip_vals_endo) 
temp_precip_vals_endo <- as.data.frame(temp_precip_vals_endo)
temp_precip_vals_endo <- mutate(temp_precip_vals_endo, tempC= bio1/10)
names(temp_precip_vals_endo)[2] <- "precip"
temp_precip_vals_endo <- subset(temp_precip_vals_endo, select = c(2,3))
#View(temp_precip_vals_endo)

# Add species column back
gbif_clim_dat_endo <- bind_cols(sp_dat_clean_df_endo, temp_precip_vals_endo)
#View(gbif_clim_dat_endo)

##RERUN FROM HERE WITH NEW GROUPING VARIABLE
# Remove duplicates 
enviro_sp_dat_endo <- distinct(gbif_clim_dat_endo) 
#View(enviro_sp_dat_endo)

# Summarize variables (median annual temp and precip)
enviro_sp_dat_grouped_endo <- enviro_sp_dat_endo %>% group_by(species) %>% 
  group_split()
#View(enviro_sp_dat_grouped_endo)
enviro_sp_dat_grouped_endo
enviro_sp_dat_md_endo <- list()
for (i in 1:length(enviro_sp_dat_grouped_endo)) {
  enviro_sp_dat_md_endo[[i]] <- enviro_sp_dat_grouped_endo[[i]] %>%
    summarise(enviro_sp_dat_grouped_endo[[i]], tempC = median(tempC), 
              precip = median(precip)) 
}
#View(enviro_sp_dat_md_endo)

# Subset species, temp and precip (data for SLOUCH model)

sp_values_endo <- list()
for (i in 1:length(enviro_sp_dat_md_endo)) {
  sp_values_endo[[i]] <- enviro_sp_dat_md_endo[[i]] %>%
    dplyr::select(species, precip, tempC)
}
#View(sp_values_endo)
sp_values_df_endo <- do.call(rbind, lapply(sp_values_endo, data.frame))
#View(sp_values_df_endo)
sp_values_df_endo <- distinct(sp_values_df_endo)
#View(sp_values_df_endo)


# Remove any leftover NAs
sp_values_df_endo <- na.omit(sp_values_df_endo)

# Save as RDS 
#saveRDS(sp_values_df_endo, file = "sp_values_df_endo.RDS")
sp_values_df_endo <- readRDS("sp_values_df_endo.RDS")
#View(sp_values_df_endo)
ncol(sp_values_df_endo)
nrow(sp_values_df_endo)
#4187

#ALOT OF RECURRING  SPECIES THOUGH
####################### SLOUCH Analysis for Endotherms (mammals) ###############

# Mammal thermo data
endo_dat <- readRDS("~/Desktop/Current Research/Endo&EctoFile/Tbdat.rds")
#View(endo_dat$dat)
endo_dat$dat$species <- endo_dat$phy$tip.label
#View(endo_dat$dat)
nrow(endo_dat$dat)
#857

endo_temp <- endo_dat$dat
endo_dat$phy$tip.label
#View(endo_temp)

# Endotherm species list
#View(endo_sp_list)

# Combine temp data with endotherm species list
#endo_dat_new <- cbind(endo_temp, endo_sp_list)
#View(endo_dat_new)
#class(endo_dat_new)

#THE OTHER WAY ROUND
#endo_dat_new <- cbind(endo_sp_list,endo_temp)
#View(endo_dat_new)
#class(endo_dat_new)


# Make endo_dat_new class df
#endo_dat_df <- as.data.frame(endo_dat_new)
#class(endo_dat_df)
#View(endo_dat_df)

# Rename column names
#View(endo_sp_list)
#names(endo_dat_df)[names(endo_dat_df) == "endo_sp_list"] <- "species"
#View(endo_dat_df)

# Endotherm enviro data
endo_dat_df <-  data.frame(endo_dat$dat)
endo_dat_df$species <- gsub("_", " ", endo_dat_df$species)
#View(sp_values_df_endo)
endo_predictors <- sp_values_df_endo %>% rename(precipmm = precip)
#View(endo_predictors)
endo_predictors <- na.omit(endo_predictors)
#View(endo_predictors)

# Merge endotherm body temp and endotherm enviro predictors
#View(endo_predictors)
#View(endo_dat_df)  #Tb

endo_complete <- merge.data.frame(endo_predictors, endo_dat_df, by='species')
#View(endo_complete)
nrow(endo_complete) #4184

#######Start of merging with Diel
Diel=read.csv("diel1 - Sheet1.csv")
Diel<-rename(Diel, species=Binomial)

Dielendo<-left_join(endo_complete,Diel,by="species")

View(Dielendo)
nrow(Dielendo)
#4184

#DIURNAL=1, NOCTURNAL=0, ARRYTHMIC=1, CRESPUCULAR=0
Dielendo['Diel'][Dielendo['Diel'] == 'DIU'] <- '1'
Dielendo['Diel'][Dielendo['Diel'] == 'NOC'] <- '0'
Dielendo['Diel'][Dielendo['Diel'] == 'ARR'] <- '1'
Dielendo['Diel'][Dielendo['Diel'] == 'CRE'] <- '0'

Dielendo$species <- gsub(" ", "_", Dielendo$species)
.td_endo <- make.treedata(endo_dat$phy,Dielendo)


unloadNamespace("tidyverse")


library(treeplyr)
source("imputeDiscrete.R")

set.seed(1)
#impDiel <- impute_discrete(.td_endo$phy, .td_endo[["Diel"]])
impDielendo <-impute_discrete(.td_endo$phy, setNames(.td_endo$dat$Diel, .td_endo$phy$tip.label))


impDielendo1<-data.frame(.td_endo$phy$tip.label,.td_endo$dat$Diel , impDielendo)
.td_endo$dat$iDiel <- impDielendo
td_endo <- .td_endo
#View(td_endo$dat)


##End of merging with Diel

#max(branching.times(endo_tree))

#plot(td_endo$phy, show.tip.label = FALSE)



# Remove "_" in between genus and species for tip labels
#td_endo$phy$tip.label <- gsub("_", " ", td_endo$phy$tip.label)
#td_endo$phy$tip.label
#class(td_endo$phy)

# Compare taxa in data and tree
#name.check(td_endo$phy$tip.label,td_endo$dat$species)



#td_endo$dat$species == td_endo$phy$tip.label

td_endo$phy$tip.label
td_endo[["Tb"]]
td_endo$dat


#View(td_endo$dat)
nrow(td_endo$dat)
#813

# Taking into account measurement error with Tb
# Trying to add ME into slouch.fit
ME_endo <- td_endo$dat$ME
#View(ME_endo)
# Remove NA's from list in ME_endo
ME_endo <- na.omit(ME_endo)
#View(ME_endo)
#View(data.frame(ME_endo))
class(ME_endo)

# Take median of values
med_endo <- median(ME_endo)
med_endo # 0.1643168

# Replace NA's in orginal ME list with med_endo value
library(tidyr)
ME_endo_replace <- td_endo$dat$ME
ME_endo_replace <- ME_endo_replace %>% replace_na( 0.1643168)  
#View(ME_endo_replace)
#View(data.frame(ME_endo_replace))


# Filter out NA's within Tb                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                for species 
td_endo <- dplyr::filter(td_endo$dat, !is.na(Tb))
#View(td_endo$phy$tip.label)
#View(td_endo$dat)
nrow(td_endo$dat)
#813

# Save as RDS 
getwd()

#saveRDS(td_endo, file = "/td_endo_2_23.RDS")
td_endo<-readRDS("td_endo_9_14.RDS")
list.files()
