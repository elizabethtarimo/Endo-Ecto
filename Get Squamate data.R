setwd("~/Desktop/Current Research/Endo&EctoFile")
#18 to 404
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

gbif_sp_lookup <- read.csv("gbif_squamates_match.csv", header = TRUE)
#View(gbif_sp_lookup)

gbif_taxon_keys <- gbif_sp_lookup %>%
  dplyr::filter(matchType =="EXACT" & status =="ACCEPTED") %>%
  dplyr::filter(kingdom == "Animalia" ) %>%
  pull(key)
print(gbif_taxon_keys)
#View(gbif_taxon_keys)


gbif_taxon_keys <- gbif_sp_lookup %>%
  dplyr::filter(matchType =="EXACT" & status =="ACCEPTED") %>%
  dplyr::filter(kingdom == "Animalia" ) %>%
  dplyr::filter(class == "Reptilia" ) %>%
  pull(key)
print(gbif_taxon_keys)
#View(gbif_taxon_keys)



#### Different method for getting occurrences (need GBIF account)
# Make objects for GBIF account info
user <- "elizadaniel"
pwd <- "Eliza6715"
email <- "delizabeth@vt.edu"

# Occ_download will send a download request for all the species in taxon key list
# The gbif website will download all of the occurrences.
#occ_dl <- occ_download(pred_in("taxonKey", gbif_taxon_keys),format="SIMPLE_CSV", user=user, pwd=pwd, email=email)

# Occ_dl has the download key (manually insert as key for next function)
#occ_dl

# Different method for importing download from gbif (This works too)
#occ_df <- occ_download_get(key=" 0173121-210914110416597", overwrite = TRUE) %>%
#  occ_download_import

#unzip(0011178-210914110416597.zip, files = NULL, overwrite = TRUE, junkpaths = FALSE, exdir = ".")

#View(occ_df)


# Save the occ_df object (takes a while to download for each import)
#saveRDS(occ_df, file = "occ_df_save.RDS")

## On restart, start here ================
occ_df <- readRDS("occ_df_save.RDS") 

#View(occ_df)

# Make sure scientificNames in gbif object match those in gbif_sp_lookup csv
occ_df$matches <- 0     
match(occ_df$scientificName, gbif_sp_lookup$scientificName)
occ_df$matches <- match(occ_df$scientificName, gbif_sp_lookup$scientificName)
#View(occ_df)


# Remove NA's from matches column
occ_df %>% dplyr::filter(!is.na(occ_df$matches))
#View(occ_df)
colnames(occ_df)

# Group sp_dat by species key
sp_dat_dl <- occ_df %>%
  group_by(species)%>%
  group_split()
#View(sp_dat_dl)
# 9006 species


#### Select species lat and long data
# skips rows with missing countryCode and scientificName

sp_dat_all <- list()
for (i in 1:length(sp_dat_dl)) {
  if (is.null(sp_dat_dl[[i]])) { next
  }
  if (is.missing(sp_dat_dl[[i]]$countryCode)) { next 
  }
  if (is.missing(sp_dat_dl[[i]]$scientificName)) { next
  }
  sp_dat_all[[i]] <- sp_dat_dl[[i]] %>%
    dplyr::select(.,scientificName, decimalLongitude, decimalLatitude, species, countryCode)
} 


#View(sp_dat_all)


#### Remove records without coordinates (NA) for lat and long
## Doing the filters separately works
# Filter NA's for Latitude
sp_dat_1 <- list()
for (i in 1:length(sp_dat_all)) {
  if (is.null(sp_dat_all[[i]])) { next
  }
  sp_dat_1[[i]] <- sp_dat_all[[i]] %>%
    dplyr::filter(!is.na(sp_dat_all[[i]]$decimalLatitude))
}

#View(sp_dat_1)
# Filter NA's for Longitude
for (i in 1:length(sp_dat_1)) {
  if (is.null(sp_dat_1[[i]])) { next
  }
  sp_dat_1[[i]] <- sp_dat_1[[i]] %>%
    dplyr::filter(!is.na(sp_dat_1[[i]]$decimalLongitude))
}
#View(sp_dat_1)

#### Cleaning and cross-checking gbif data
unlist(sp_dat_1) 
sp_dat_concat <- dplyr::bind_rows(sp_dat_1)
#View(sp_dat_concat)
sp_dat_concat$countryCode <- countrycode(sp_dat_concat$countryCode, 
                                         origin = 'iso2c',destination = 'iso3c')
names(sp_dat_concat)[2:3] <- c("decimallongitude", "decimallatitude")



# Remove rows with NA values 
sp_dat_concat <- na.omit(sp_dat_concat)
#View(sp_dat_concat)


# Removes rows without a "scientificName"
sp_dat_concat <- sp_dat_concat[sp_dat_concat$scientificName != "",]
#View(sp_dat_concat)

# Clean coordinates (Takes a long time to run)
sp_dat_clean_concat <- clean_coordinates(sp_dat_concat)
#View(sp_dat_clean_concat)

# filter out the rows with .summary FALSE (indicates at least one test is FALSE)
sp_dat_clean <- data.frame()
class(sp_dat_clean)
sp_dat_clean <- sp_dat_clean_concat[sp_dat_clean_concat$.summary !=FALSE,]
#View(sp_dat_clean)


# subset scientificName, decimallongitude, decimallatitude, species and cc
sp_dat_clean <- subset(sp_dat_clean, select = c(1,2,3,4,5))
#View(sp_dat_clean)

sp_dat_clean_list <- as.list(sp_dat_clean)
#View(sp_dat_clean_list)

sp_dat_clean_df <- as.data.frame(sp_dat_clean_list)
#View(sp_dat_clean_df)

# Save sp_dat_clean_df as RDS file
# Save the sp_dat_clean_df object (takes a long time to filer coords)
#saveRDS(sp_dat_clean_df, file = "sp_dat_clean_df_squamates.RDS")
sp_dat_clean_df <- readRDS("sp_dat_clean_df_squamates.RDS")
#View(sp_dat_clean_df)

# All presence data (roughly 7774 squamate species)
#I got 8208?
#View(sp_dat_clean_df)
sp_dat_grouped <- sp_dat_clean_df %>%
  group_by(species)%>%
  group_split()
#View(sp_dat_grouped)
sp_dat_presence <- sp_dat_grouped
#View(sp_dat_presence)



########################## Squamate WorldClim Data ##############################
#### Climate data from worldclim
# download 19 bioclimatic variables at the 10' resolution 
dir.create("clim_data")
clim_dat <- getData("worldclim", var="bio", res=10, download = T, path='clim_data')
#View(clim_dat)
plot(clim_dat) 

#...................
#got it from Emma tho...but use the top function
#setwd("~/Desktop/Eliza R/Current Research")
# gbif data
sp_dat_clean_df
# worldclim data
clim_dat
class(sp_dat_clean_df)
sp_dat_clean_df[,c(2,3)]
#....................


### Combining clim_dat and sp_dat_clean_concat through sp object
values <- raster::extract(clim_dat, SpatialPoints(sp_dat_clean_df[,c(2,3)], 
                                                  proj4string = clim_dat@crs))
#View(values)
class(values)

# Subsetting annual temp and annual precip
#???how did  she knoww  if they are temp and prec values?
temp_precip_vals <- values[,c(1,12)]
#View(temp_precip_vals) 
temp_precip_vals <- as.data.frame(temp_precip_vals)
class(temp_precip_vals)
temp_precip_vals <- mutate(temp_precip_vals, tempC= bio1/10)
names(temp_precip_vals)[2] <- "precip"
temp_precip_vals <- subset(temp_precip_vals, select = c(2,3))
#View(temp_precip_vals)

# Add species column back
gbif_clim_dat <- bind_cols(sp_dat_clean_df, temp_precip_vals)
#View(gbif_clim_dat)

# Remove duplicates 
enviro_sp_dat <- distinct(gbif_clim_dat) 
#View(enviro_sp_dat)

# Summarize variables (median annual temp and precip)
enviro_sp_dat_grouped <- enviro_sp_dat %>% group_by(species) %>% 
  group_split()
#View(enviro_sp_dat_grouped)

enviro_sp_dat_md <- list()
for (i in 1:length(enviro_sp_dat_grouped)) {
  enviro_sp_dat_md[[i]] <- enviro_sp_dat_grouped[[i]] %>%
    summarise(enviro_sp_dat_grouped[[i]], tempC = median(tempC), 
              precip = median(precip)) 
}
#View(enviro_sp_dat_md)


# Subset species, temp and precip (data for SLOUCH model)

sp_values <-list()
for (i in 1:length(enviro_sp_dat_md)) {
  sp_values[[i]] <- enviro_sp_dat_md[[i]] %>%
    dplyr::select(species, precip, tempC)
}
#View(sp_values)
sp_values_df <- do.call(rbind, lapply(sp_values, data.frame))
#View(sp_values_df)
sp_values_df <- distinct(sp_values_df)
#View(sp_values_df)

#---------------------START HERE
# Save as RDS 
#saveRDS(sp_values_df, file = "sp_values_df_squa.RDS")

sp_values_df <- readRDS("sp_values_df_squa.RDS")
#View(sp_values_df)
length(sp_values_df)
nrow(sp_values_df)
#8000

####################### SLOUCH Analysis for Squamates ###########################
# Squamate thermo data
getwd()
squamate_dat1 = read.csv("PreliminaryTb_forJosef.csv")
#View(squamate_dat1)
#705 spp

# Squamate enviro data
#View(sp_values_df)
squamate_predictors <- sp_values_df %>% rename(precipmm = precip)
#View(squamate_predictors)
squamate_predictors <- na.omit(squamate_predictors)
#View(squamate_predictors)
length(squamate_predictors)
#8000

# Merge squamate thermo dat and squamate enviro predictors
#make sure they match 1st
head(squamate_predictors)
head(squamate_dat1)

#change the column name to match
colnames(squamate_dat1)[2] <- "species"
#View(squamate_dat1)

# now merge
#which elements in df x are not found in y?
squa_complete1<-left_join(squamate_dat1,squamate_predictors,by="species")
#View(squa_complete1)

setdiff(squamate_dat1$species, squamate_predictors$species)

#View(squa_complete1)

#saveRDS(squa_complete1, file = "squa_complete1.RDS")
squa_complete1 <- readRDS("squa_complete1.RDS")
#View(squa_complete1)
nrow(squa_complete1)
#1279
#THIS IS ALL SQUA YOU WANT/HAVE/NEED
#MATCHING IT WITH DIEL ACTIVITY

Diel=read.csv("diel1 - Sheet1.csv")
#View(Diel)
nrow(Diel)
#1721

#######Start of merging with Diel
Dielsqua<-left_join(squa_complete1,Diel,by="species")

#View(Dielsqua)
nrow(Dielsqua)
#1279

#DIURNAL=1, NOCTURNAL=0, ARRYTHMIC=1, CRESPUCULAR=0
Dielsqua['Diel'][Dielsqua['Diel'] == 'DIU'] <- '1'
Dielsqua['Diel'][Dielsqua['Diel'] == 'NOC'] <- '0'
Dielsqua['Diel'][Dielsqua['Diel'] == 'ARR'] <- '1'
Dielsqua['Diel'][Dielsqua['Diel'] == 'CRE'] <- '0'

# Squamate tree 
squa_tree <- read.tree("squa.tre.txt")
squa_tree$tip.label
class(squa_tree)

max(branching.times(squa_tree))
plot(squa_tree, show.tip.label = FALSE)

# Remove "_" in between genus and species for tip labels
squa_tree$tip.label <- gsub("_", " ", squa_tree$tip.label)
squa_tree$tip.label
class(squa_tree)

.td_squa <- make.treedata(squa_tree,Dielsqua)
source("imputeDiscrete.R")
set.seed(1)
#impDielsqua <- impute_discrete(.td_squa$phy, .td_squa[["Diel"]])
impDielsqua <- impute_discrete(.td_squa$phy, setNames(.td_squa$dat$Diel, .td_squa$phy$tip.label))

.td_squa$dat$iDiel <- impDielsqua
td_squa1 <- .td_squa
#View(td_squa1$dat)
nrow(td_squa1$dat)
#566

#453 with Diel

td_squa1[["Tb"]]
td_squa1$phy$tip.label
td_squa1$dat


# Filter out NA's within Tb for each species (do for each variable before
# each SLOUCH analysis)
td_squa1 <- dplyr::filter(td_squa1, !is.na(Tb))
#View(td_squa1$dat)
nrow(td_squa1$dat)
#539


# Replace NA's in orginal ME list with med_squa value
td_squa <- filter(td_squa1$dat, !is.na(precipmm), !is.na(tempC), Tb > 14)
nrow(td_squa)
#500

# Taking into account measurement error with Tb
# Trying to add ME into slouch.fit
ME_squa1 <- td_squa1$dat$Tb.err
#View(ME_squa1)


# Remove NA's from list in ME_squa
ME_squa1 <- na.omit(ME_squa1)
#View(ME_squa1)


# Take median of values
med_squa1 <- median(ME_squa1)
med_squa1 #0.5
#View(ME_squa1)

ME_squa_replace1 <- td_squa1$dat$Tb.err
ME_squa_replace1 <- ME_squa_replace1 %>% replace_na(0.5)
#View(ME_squa_replace1)
#View(data.frame(ME_squa_replace1))

#View(data.frame(td_squa1$dat))
nrow(td_squa1$dat)#500

td_squa1

# Save as RDS 
#saveRDS(td_squa1, file = "td_squa1.RDS")

td_squa1 <- readRDS("td_squa1.RDS")
