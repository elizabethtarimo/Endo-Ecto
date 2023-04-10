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
library(slouch)

??matchType
getwd()
setwd("~/Desktop/Current Research/Endo&EctoFile")

gbif_sp_lookup <- read.csv("gbif_squamates_match.csv", header = TRUE)
View(gbif_sp_lookup)

gbif_taxon_keys <- gbif_sp_lookup %>%
  dplyr::filter(matchType =="EXACT" & status =="ACCEPTED") %>%
  dplyr::filter(kingdom == "Animalia" ) %>%
  dplyr::filter(class == "Reptilia" ) %>%
  pull(key)
print(gbif_taxon_keys)
View(gbif_taxon_keys)



#### Different method for getting occurrences (need GBIF account)
# Make objects for GBIF account info
user <- "elizadaniel"
pwd <- "Eliza6715"
email <- "delizabeth@vt.edu"

# Occ_download will send a download request for all the species in taxon key list
# The gbif website will download all of the occurrences.
occ_dl <- occ_download(pred_in("taxonKey", gbif_taxon_keys),format="SIMPLE_CSV", user=user, pwd=pwd, email=email)

# Occ_dl has the download key (manually insert as key for next function)
occ_dl

# Different method for importing download from gbif (This works too)
occ_df <- occ_download_get(key=" 0173121-210914110416597", overwrite = TRUE) %>%
  occ_download_import

#unzip(0011178-210914110416597.zip, files = NULL, overwrite = TRUE, junkpaths = FALSE, exdir = ".")

View(occ_df)


# Save the occ_df object (takes a while to download for each import)
saveRDS(occ_df, file = "occ_df_save.RDS")

## On restart, start here ================
occ_df <- readRDS("occ_df_save.RDS") 

View(occ_df)

# Make sure scientificNames in gbif object match those in gbif_sp_lookup csv
occ_df$matches <- 0     
match(occ_df$scientificName, gbif_sp_lookup$scientificName)
occ_df$matches <- match(occ_df$scientificName, gbif_sp_lookup$scientificName)
View(occ_df)


# Remove NA's from matches column
occ_df %>% dplyr::filter(!is.na(occ_df$matches))
View(occ_df)
colnames(occ_df)

# Group sp_dat by species key
sp_dat_dl <- occ_df %>%
  group_by(species)%>%
  group_split()
View(sp_dat_dl)
# 9006 species
#??????why

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


View(sp_dat_all)


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

View(sp_dat_1)
# Filter NA's for Longitude
for (i in 1:length(sp_dat_1)) {
  if (is.null(sp_dat_1[[i]])) { next
  }
  sp_dat_1[[i]] <- sp_dat_1[[i]] %>%
    dplyr::filter(!is.na(sp_dat_1[[i]]$decimalLongitude))
}
View(sp_dat_1)

#### Cleaning and cross-checking gbif data
unlist(sp_dat_1) 
sp_dat_concat <- dplyr::bind_rows(sp_dat_1)
View(sp_dat_concat)
sp_dat_concat$countryCode <- countrycode(sp_dat_concat$countryCode, 
                                         origin = 'iso2c',destination = 'iso3c')
names(sp_dat_concat)[2:3] <- c("decimallongitude", "decimallatitude")



# Remove rows with NA values 
sp_dat_concat <- na.omit(sp_dat_concat)
View(sp_dat_concat)


# Removes rows without a "scientificName"
sp_dat_concat <- sp_dat_concat[sp_dat_concat$scientificName != "",]
View(sp_dat_concat)

# Clean coordinates (Takes a long time to run)
sp_dat_clean_concat <- clean_coordinates(sp_dat_concat)
View(sp_dat_clean_concat)

# filter out the rows with .summary FALSE (indicates at least one test is FALSE)
sp_dat_clean <- data.frame()
class(sp_dat_clean)
sp_dat_clean <- sp_dat_clean_concat[sp_dat_clean_concat$.summary !=FALSE,]
View(sp_dat_clean)
#???????Why

# subset scientificName, decimallongitude, decimallatitude, species and cc
sp_dat_clean <- subset(sp_dat_clean, select = c(1,2,3,4,5))
View(sp_dat_clean)

sp_dat_clean_list <- as.list(sp_dat_clean)
View(sp_dat_clean_list)

sp_dat_clean_df <- as.data.frame(sp_dat_clean_list)
View(sp_dat_clean_df)

# Save sp_dat_clean_df as RDS file
# Save the sp_dat_clean_df object (takes a long time to filer coords)
saveRDS(sp_dat_clean_df, file = "sp_dat_clean_df_squamates.RDS")
sp_dat_clean_df <- readRDS("sp_dat_clean_df_squamates.RDS")
View(sp_dat_clean_df)

# All presence data (roughly 7774 squamate species)
#I got 8208?
View(sp_dat_clean_df)
sp_dat_grouped <- sp_dat_clean_df %>%
  group_by(species)%>%
  group_split()
View(sp_dat_grouped)
sp_dat_presence <- sp_dat_grouped
View(sp_dat_presence)



########################## Squamate WorldClim Data ##############################
#### Climate data from worldclim
# download 19 bioclimatic variables at the 10' resolution 
dir.create("clim_data")
clim_dat <- getData("worldclim", var="bio", res=10, download = T, path='clim_data')
View(clim_dat)
plot(clim_dat) 

#...................
#got it from Emma tho...but use the top function
setwd("~/Desktop/Eliza R/Current Research")
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
View(values)
class(values)

# Subsetting annual temp and annual precip
#???how did  she knoww  if they are temp and prec values?
temp_precip_vals <- values[,c(1,12)]
View(temp_precip_vals) 
temp_precip_vals <- as.data.frame(temp_precip_vals)
class(temp_precip_vals)
temp_precip_vals <- mutate(temp_precip_vals, tempC= bio1/10)
names(temp_precip_vals)[2] <- "precip"
temp_precip_vals <- subset(temp_precip_vals, select = c(2,3))
View(temp_precip_vals)

# Add species column back
gbif_clim_dat <- bind_cols(sp_dat_clean_df, temp_precip_vals)
View(gbif_clim_dat)

# Remove duplicates 
enviro_sp_dat <- distinct(gbif_clim_dat) 
View(enviro_sp_dat)

# Summarize variables (median annual temp and precip)
enviro_sp_dat_grouped <- enviro_sp_dat %>% group_by(species) %>% 
  group_split()
View(enviro_sp_dat_grouped)

enviro_sp_dat_md <- list()
for (i in 1:length(enviro_sp_dat_grouped)) {
  enviro_sp_dat_md[[i]] <- enviro_sp_dat_grouped[[i]] %>%
    summarise(enviro_sp_dat_grouped[[i]], tempC = median(tempC), 
              precip = median(precip)) 
}
View(enviro_sp_dat_md)


# Subset species, temp and precip (data for SLOUCH model)

sp_values <-list()
for (i in 1:length(enviro_sp_dat_md)) {
  sp_values[[i]] <- enviro_sp_dat_md[[i]] %>%
    dplyr::select(species, precip, tempC)
}
View(sp_values)
sp_values_df <- do.call(rbind, lapply(sp_values, data.frame))
View(sp_values_df)
sp_values_df <- distinct(sp_values_df)
View(sp_values_df)

#---------------------START HERE
# Save as RDS 
saveRDS(sp_values_df, file = "sp_values_df_squa.RDS")

sp_values_df <- readRDS("sp_values_df_squa.RDS")
View(sp_values_df)
length(sp_values_df)
nrow(sp_values_df)
#8000

####################### SLOUCH Analysis for Squamates ###########################
# Squamate thermo data
getwd()
squamate_dat1 = read.csv("PreliminaryTb_forJosef.csv")
View(squamate_dat1)
#705 spp

# Squamate enviro data
View(sp_values_df)
squamate_predictors <- sp_values_df %>% rename(precipmm = precip)
View(squamate_predictors)
squamate_predictors <- na.omit(squamate_predictors)
View(squamate_predictors)
length(squamate_predictors)
nrow(squamate_predictors)
#7935

# Merge squamate thermo dat and squamate enviro predictors
#make sure they match 1st
head(squamate_predictors)
head(squamate_dat1)

#change the column name to match
colnames(squamate_dat1)[2] <- "species"
View(squamate_dat1)

# now merge
#which elements in df x are not found in y?
squa_complete1<-left_join(squamate_dat1,squamate_predictors,by="species")
View(squa_complete1)

setdiff(squamate_dat1$species, squamate_predictors$species)

View(squa_complete1)

saveRDS(squa_complete1, file = "squa_complete1.RDS")
squa_complete1 <- readRDS("squa_complete1.RDS")
View(squa_complete1)
nrow(squa_complete1)
#1279
#THIS IS ALL SQUA YOU WANT/HAVE/NEED
#MATCHING IT WITH DIEL ACTIVITY

Diel=read.csv("diel1 - Sheet1.csv")
View(Diel)
nrow(Diel)
#1721

#######Start of merging with Diel
Dielsqua<-left_join(squa_complete1,Diel,by="species")

View(Dielsqua)
nrow(Dielsqua)
#1279

#DIURNAL=1, NOCTURNAL=0, ARRYTHMIC=1, CRESPUCULAR=0
Dielsqua['Diel'][Dielsqua['Diel'] == 'DIU'] <- '1'
Dielsqua['Diel'][Dielsqua['Diel'] == 'NOC'] <- '0'
Dielsqua['Diel'][Dielsqua['Diel'] == 'ARR'] <- '1'
Dielsqua['Diel'][Dielsqua['Diel'] == 'CRE'] <- '0'

# Squamate tree 
library(treeplyr)
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
View(td_squa1$dat)
nrow(td_squa1$dat)
#566

#453 with Diel

td_squa1[["Tb"]]
td_squa1$phy$tip.label
td_squa1$dat


# Filter out NA's within Tb for each species (do for each variable before
# each SLOUCH analysis)
td_squa1 <- dplyr::filter(td_squa1, !is.na(Tb))
View(td_squa1$dat)
nrow(td_squa1$dat)
#539


library(tidyr)
library(treeplyr)
# Replace NA's in orginal ME list with med_squa value
td_squa1 <- filter(td_squa1, !is.na(precipmm), !is.na(tempC), Tb > 14)
nrow(td_squa1$dat)
#500

# Taking into account measurement error with Tb
# Trying to add ME into slouch.fit
ME_squa1 <- td_squa1$dat$Tb.err
View(ME_squa1)


# Remove NA's from list in ME_squa
ME_squa1 <- na.omit(ME_squa1)
View(ME_squa1)


# Take median of values
med_squa1 <- median(ME_squa1)
med_squa1 #0.5
View(ME_squa1)

ME_squa_replace1 <- td_squa1$dat$Tb.err
ME_squa_replace1 <- ME_squa_replace1 %>% replace_na(0.5)
View(ME_squa_replace1)
View(data.frame(ME_squa_replace1))

View(data.frame(td_squa1$dat))
nrow(td_squa1$dat)#500

td_squa1

# Save as RDS 
saveRDS(td_squa1, file = "td_squa1.RDS")
td_squa1 <- readRDS("td_squa1.RDS")
nrow(td_squa1$dat)  #500
View(td_squa1$dat)
td_squa1$phy$node.label




#TREE WITH TB AS THE CONTINOUS TRAIT
objSqua<-contMap(td_squa1$phy,td_squa1[["Tb"]])
plot(objSqua,type="fan",lwd=1) 

contMap(td_squa1$phy,td_squa1[["Tb"]] , res=100, fsize=0.7, ftype="off", lwd=4, legend=NULL,
        lims=NULL, outline=TRUE, sig=3, type="fan", direction="rightwards", 
        plot=TRUE)

contMap(td_squa1$phy,td_squa1[["Tb"]] , res=100, fsize=NULL, ftype=NULL, lwd=4, legend=NULL,
        lims=NULL, outline=TRUE, sig=3, type="phylogram", direction="rightwards", 
        plot=TRUE)

?contMap



# SLOUCH Analysis for Ectotherms
library(ape)
library(slouch)

# Predictors = tempC, precipmm
# Response Variables = Tb 
# Model 1 (response = Tb)
#?what is mv responce
?slouch.fit

model1 <- slouch.fit(phy = td_squa1$phy, 
                     species = td_squa1$phy$tip.label,
                     response = td_squa1[["Tb"]],
                     mv.response = ME_squa_replace1,
                     random.cov = td_squa1[c("precipmm","tempC")])
model1
summary(model1)


plot(td_squa1[["tempC"]], td_squa1[["Tb"]]) 
plot(td_squa1[["precipmm"]], td_squa1[["Tb"]])
slouch::hillclimbplot(model1)

model1$beta_evolutionary$coefficients

model1$beta_primary$coefficients_bias_corr
model1$beta_evolutionary$coefficients_bias_corr


#####WITH DIEL
tree_squa_diel <- td_squa1$phy
diel_asr <- ace(x=setNames(factor(td_squa1$dat$iDiel+1), tree_squa_diel$tip.label), phy=multi2di(tree_squa_diel), type="discrete")
tree_squa_diel$node.label <- unname(apply(diel_asr$lik.anc, 1, function(x) which(x==max(x))))-1

plot(tree_squa_diel, show.tip.label = FALSE, type="fan")
tiplabels(pch=21, bg=td_squa1$dat$iDiel+1, col=td_squa1$dat$iDiel+1, cex=0.5)
nodelabels(pch=21, bg=tree_squa_diel$node.label+1, col=tree_squa_diel$node.label+1)

model1D <- slouch.fit(phy = tree_squa_diel, 
                      species = tree_squa_diel$tip.label,
                      response = td_squa1[["Tb"]],
                      mv.response = ME_squa_replace1,
                      random.cov = td_squa1[c("precipmm","tempC")], 
                      fixed.fact = td_squa1[['iDiel']])
model1D
summary(model1D)

model1D$modfit$AICc    #temp, Precip & Diel
model1$modfit$AICc      #temp & Precip
model2a$modfit$AICc    #temp
model3a$modfit$AICc   #Precip
######

#model1Dgrid <- slouch.fit(phy = tree_squa_diel, 
##                      species = tree_squa_diel$tip.label,
#                      hl_values=seq(1,100,length.out=8),
#                      vy_values=seq(1,30, length.out=8),
#                      response = td_squa1[["Tb"]],
##                      mv.response = ME_squa_replace1,
#                    random.cov = td_squa1[c("precipmm","tempC")], 
#                      fixed.fact = td_squa1[['iDiel']], 
#                      hillclimb=FALSE)
#slouch:::plot(model1Dgrid)

# SLOUCH Analysis (Predictor = Temp)
# Model 2 (response = Tb)
model2a <- slouch.fit(phy = td_squa1$phy,
                      species = td_squa1$phy$tip.label,
                      response = td_squa1[["Tb"]],
                      random.cov = td_squa1[["tempC"]])
model2a
summary(model2a)
plot(td_squa1[["tempC"]], td_squa1[["Tb"]])
slouch::hillclimbplot(model2)

model2a$beta_evolutionary$coefficients

# SLOUCH Analysis (predictor = precip)
# Model 3 (response = Tb)
model3a <- slouch.fit(phy = td_squa1$phy,
                      species = td_squa1$phy$tip.label,
                      response = td_squa1[["Tb"]],
                      random.cov = td_squa1[["precipmm"]])
model3a
summary(model3a)
plot(td_squa1[["precipmm"]], td_squa1[["Tb"]])
slouch::hillclimbplot(model3a)

model3a$beta_evolutionary$coefficients

### Repeat Qu and Wiens 2020 wiith our dataset?
#diel <- read.csv("diel1-Sheet1.csv", header = TRUE)
#View(diel)


#library(phylolm)
ndat <- td_squa$dat
rownames(ndat) <- td_squa$phy$tip.label
lm1 <- phylolm(Tb ~ tempC,  data=ndat, phy=td_squa$phy)






################### Endotherm (mammals & birds) Presence Data ##################
setwd("~/Desktop/Current Research/Endo&EctoFile/Mammals")

#Birds
install.packages("dplyr")
install.packages("readxl")
library(readxl)
birds <- read_excel("AllcladesTb.xlsx")
View(birds)

library(dplyr)
birds<-birds %>% dplyr::select(1,4,5)

birds1<-birds[birds$Class == 'Aves', ]
View(birds1)
nrow(birds1) #474

#Mammals
#857 species (w/ Tb data)
# Create separate csv of enodtherm species and match with GBIF backbone
endo_dat <- readRDS("Tbdat.rds")
View(endo_dat$dat)  #857
nrow(endo_dat$dat)
View(data.frame(endo_dat$phy$tip.label))

#env data 807
td_endo <- readRDS("td_endo.RDS")
View(td_endo$dat)
nrow(td_endo$dat)

endo_sp_list <- as.data.frame(endo_dat$phy$tip.label)
View(endo_sp_list)

#substittute the _ for just space to later match
endo_sp_list <- as.character(sub("_", " ", endo_sp_list$`endo_dat$phy$tip.label`))

View(endo_sp_list)
class(endo_sp_list)

#back to df
endo_sp_list <- as.data.frame(endo_sp_list)
class(endo_sp_list)
View(endo_sp_list)

write.csv(endo_sp_list, "~/Desktop/Current Research/Endo&EctoFile/endo_list.csv")
endo_sp_list<-read.csv("endo_list.csv", header = TRUE)

View(endo_sp_list)
nrow(endo_sp_list) #857
#mammal list

# Match endotherm species csv list file to GBIF backbone
# Species Name Match - GBIF
# Set directory to GBIF match csv file and get GBIF taxon keys

gbif_sp_lookup_endo <- read.csv("endo_gbif_match.csv", header = TRUE)
View(gbif_sp_lookup_endo) #857
nrow(gbif_sp_lookup_endo)

gbif_taxon_keys_endo <- gbif_sp_lookup_endo %>%
  dplyr::filter(matchType =="EXACT" & status =="ACCEPTED") %>%
  dplyr::filter(kingdom == "Animalia" ) %>%
  pull(key)
print(gbif_taxon_keys_endo)
View(data.frame(gbif_taxon_keys_endo))
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


saveRDS(gbif_endo, file = "gbif_endo.RDS")
gbif_endo <- readRDS("gbif_endo.RDS")
View(gbif_endo)
nrow(gbif_endo)


# Make sure scientificNames in gbif object match those in gbif_sp_lookup_endo
gbif_endo$matches <- 0     
match(gbif_endo$scientificName, gbif_sp_lookup_endo$scientificName)
gbif_endo$matches <- match(gbif_endo$scientificName, gbif_sp_lookup_endo$scientificName)
gbif_endo$matches
View(gbif_endo)

# Remove NA's from matches column
gbif_endo %>% dplyr::filter(!is.na(gbif_endo$matches))

# Group sp_dat by species 
# The number of iteration should be equal to the # of taxon keys 
# (number of species requested for occurrence data)
sp_dat_dl_endo <- gbif_endo %>%
  group_by(species)%>%
  group_split()
View(sp_dat_dl_endo)

# Save the sp_dat_dl_endo object (takes a while to download for each import)
saveRDS(sp_dat_dl_endo, file = "sp_dat_dl_endo_save.RDS")
sp_dat_dl_endo <- readRDS("sp_dat_dl_endo_save.RDS")
View(sp_dat_dl_endo)

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
View(sp_dat_all_endo)
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
View(sp_dat_1_endo)

# Filter NA's for Long
for (i in 1:length(sp_dat_1_endo)) {
  if (is.null(sp_dat_1_endo[[i]])) { next
  }
  sp_dat_1_endo[[i]] <- sp_dat_1_endo[[i]] %>%
    dplyr::filter(!is.na(sp_dat_1_endo[[i]]$decimalLongitude))
}
View(sp_dat_1_endo)

# Save sp_dat_1_endo as a RDF (takes a long time to run previous code)
saveRDS(sp_dat_1_endo, file = "sp_dat_1_endo.RDS")
sp_dat_1_endo <- readRDS("sp_dat_1_endo.RDS")
View(sp_dat_1_endo)

#### Cleaning and cross-checking gbif data
unlist(sp_dat_1_endo) 
sp_dat_concat_endo <- dplyr::bind_rows(sp_dat_1_endo)
View(sp_dat_concat_endo)
sp_dat_concat_endo$countryCode <- countrycode(sp_dat_concat_endo$countryCode, 
                                              origin = 'iso2c',destination = 'iso3c')
names(sp_dat_concat_endo)[2:3] <- c("decimallongitude", "decimallatitude")
#Looks better now


# Remove rows with NA values 
sp_dat_concat_endo <- na.omit(sp_dat_concat_endo)
View(sp_dat_concat_endo)

# Removes rows without a "scientificName"
sp_dat_concat_endo <- sp_dat_concat_endo[sp_dat_concat_endo$scientificName != "",]
View(sp_dat_concat_endo)

# THIS WORKS (USE THIS METHOD) (Takes a long time to run)
sp_dat_clean_concat_endo <- clean_coordinates(sp_dat_concat_endo)
View(sp_dat_clean_concat_endo)

# filter out the rows with .summary FALSE (indicates at least one test is FALSE)
sp_dat_clean_endo <- data.frame()
class(sp_dat_clean_endo)
sp_dat_clean_endo <- sp_dat_clean_concat_endo[sp_dat_clean_concat_endo$.summary !=FALSE,]
View(sp_dat_clean_endo)

# subset scientificName, decimallongitude, decimallatitude, species and cc
sp_dat_clean_endo <- subset(sp_dat_clean_endo, select = c(1,2,3,4,5))
View(sp_dat_clean_endo)
sp_dat_clean_list_endo <- as.list(sp_dat_clean_endo)
View(sp_dat_clean_list_endo)
sp_dat_clean_df_endo <- as.data.frame(sp_dat_clean_list_endo)
View(sp_dat_clean_df_endo)


#I HAVE THIS
# Save sp_dat_clean_df as RDS file
# Save the sp_dat_clean_df object (takes a long time to filer coords)
saveRDS(sp_dat_clean_df_endo, file = "sp_dat_clean_df_endotherms.RDS")
sp_dat_clean_df_endo <- readRDS("sp_dat_clean_df_endotherms.RDS")
View(sp_dat_clean_df_endo)
nrow(sp_dat_clean_df_endo)   #5272308

# All presence data
View(sp_dat_clean_df_endo)
sp_dat_grouped_endo <- sp_dat_clean_df_endo %>%
  group_by(species)%>%
  group_split()
View(sp_dat_grouped_endo)
sp_dat_presence_endo <- sp_dat_grouped_endo
View(sp_dat_presence_endo)

########################## Endotherm WorldClim Data ############################
#### Climate data from WorldClim
# Download 19 bioclimatic variables at the 10' resolution (Historical Climate Data)
# bio 10m

dir.create("clim_data")
clim_dat <- getData("worldclim", var="bio", res=10, download = T, path='clim_data')
View(clim_dat)
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
View(values_endo)
class(values_endo)

# Subsetting annual temp and annual precip
temp_precip_vals_endo <- values_endo[,c(1,12)]
View(temp_precip_vals_endo) 
temp_precip_vals_endo <- as.data.frame(temp_precip_vals_endo)
temp_precip_vals_endo <- mutate(temp_precip_vals_endo, tempC= bio1/10)
names(temp_precip_vals_endo)[2] <- "precip"
temp_precip_vals_endo <- subset(temp_precip_vals_endo, select = c(2,3))
View(temp_precip_vals_endo)

# Add species column back
gbif_clim_dat_endo <- bind_cols(sp_dat_clean_df_endo, temp_precip_vals_endo)
View(gbif_clim_dat_endo)

##RERUN FROM HERE WITH NEW GROUPING VARIABLE
# Remove duplicates 
enviro_sp_dat_endo <- distinct(gbif_clim_dat_endo) 
View(enviro_sp_dat_endo)

# Summarize variables (median annual temp and precip)
enviro_sp_dat_grouped_endo <- enviro_sp_dat_endo %>% group_by(species) %>% 
  group_split()
View(enviro_sp_dat_grouped_endo)
enviro_sp_dat_grouped_endo
enviro_sp_dat_md_endo <- list()
for (i in 1:length(enviro_sp_dat_grouped_endo)) {
  enviro_sp_dat_md_endo[[i]] <- enviro_sp_dat_grouped_endo[[i]] %>%
    summarise(enviro_sp_dat_grouped_endo[[i]], tempC = median(tempC), 
              precip = median(precip)) 
}
View(enviro_sp_dat_md_endo)

# Subset species, temp and precip (data for SLOUCH model)

sp_values_endo <- list()
for (i in 1:length(enviro_sp_dat_md_endo)) {
  sp_values_endo[[i]] <- enviro_sp_dat_md_endo[[i]] %>%
    dplyr::select(species, precip, tempC)
}
View(sp_values_endo)
sp_values_df_endo <- do.call(rbind, lapply(sp_values_endo, data.frame))
View(sp_values_df_endo)
sp_values_df_endo <- distinct(sp_values_df_endo)
View(sp_values_df_endo)


# Remove any leftover NAs
sp_values_df_endo <- na.omit(sp_values_df_endo)

# Save as RDS 
saveRDS(sp_values_df_endo, file = "sp_values_df_endo.RDS")
sp_values_df_endo <- readRDS("sp_values_df_endo.RDS")
View(sp_values_df_endo)
ncol(sp_values_df_endo)
nrow(sp_values_df_endo)
#4187
#?Only mammals? No birds?

#ALOT OF RECURRING  SPECIES THOUGH
####################### SLOUCH Analysis for Endotherms (mammals) ###############

# Mammal thermo data
endo_dat <- readRDS("Tbdat.rds")
View(endo_dat)
endo_dat$dat$species <- endo_dat$phy$tip.label
View(endo_dat$dat)
nrow(endo_dat$dat)
#857

endo_temp <- endo_dat$dat
endo_dat$phy$tip.label
View(endo_temp)

# Endotherm species list
View(endo_sp_list)

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
View(sp_values_df_endo)
endo_predictors <- sp_values_df_endo %>% rename(precipmm = precip)
View(endo_predictors)
endo_predictors <- na.omit(endo_predictors)
View(endo_predictors)

# Merge endotherm body temp and endotherm enviro predictors
View(endo_predictors)
View(endo_dat_df)  #Tb

endo_complete <- merge.data.frame(endo_predictors, endo_dat_df, by='species')
View(endo_complete)
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
View(td_endo$dat)


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


View(td_endo$dat)
nrow(td_endo$dat)
#813

# Taking into account measurement error with Tb
# Trying to add ME into slouch.fit
ME_endo <- td_endo$dat$ME
View(ME_endo)
# Remove NA's from list in ME_endo
ME_endo <- na.omit(ME_endo)
View(ME_endo)
View(data.frame(ME_endo))
class(ME_endo)

# Take median of values
med_endo <- median(ME_endo)
med_endo # 0.1643168

# Replace NA's in orginal ME list with med_endo value
library(tidyr)
ME_endo_replace <- td_endo$dat$ME
ME_endo_replace <- ME_endo_replace %>% replace_na( 0.1643168)
View(ME_endo_replace)
View(data.frame(ME_endo_replace))


# Filter out NA's within Tb                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                for species 
td_endo <- dplyr::filter(td_endo$dat, !is.na(Tb))
View(td_endo$phy$tip.label)
View(td_endo$dat)
nrow(td_endo$dat)
#813



# Save as RDS 
getwd()
setwd("/Users/elizabethdaniel/Desktop/Current Research/Endo&EctoFile")
#saveRDS(td_endo, file = "/td_endo_6_20.RDS")
td_endo <- readRDS("td_endo_6_20.RDS")

class(td_endo)
View(td_endo$phy$tip.label)


# Endotherm species list
endo_sp_list <- as.data.frame(td_endo$phy$tip.label)

View(endo_sp_list)



td_endo$dat <- cbind(endo_sp_list,td_endo$dat)
View(td_endo$dat)
nrow(td_endo$dat)


# Match species in endo_complete with endo_tree phylogeny tips

#setnames(td_endo$dat, "td_endo$phy$tip.label", "species")

#endo_match <- td_endo[match(endo_tree_diel$tip.label,
#                             td_endo$species), ]

#endo_match$species == endo_tree_diel$tip.label


#View(endo_match)


# SLOUCH Analysis 
# Predictors = tempC, precipmm
# Response Variables = Tb 
# Model 4 (response = Tb)
library(slouch)


#####WITH DIEL
endo_tree_diel <-  td_endo$phy
diel_asr <- ace(x=setNames(factor( td_endo$dat$iDiel+1), endo_tree_diel$tip.label), phy=multi2di(endo_tree_diel), type="discrete")
endo_tree_diel$node.label <- unname(apply(diel_asr$lik.anc, 1, function(x) which(x==max(x))))-1

plot(endo_tree_diel, show.tip.label = FALSE, type="fan")
tiplabels(pch=21, bg= td_endo$dat$iDiel+1, col= td_endo$dat$iDiel+1, cex=0.5)
nodelabels(pch=21, bg=endo_tree_diel$node.label+1, col=endo_tree_diel$node.label+1)

model4a <- slouch.fit(phy = endo_tree_diel, 
                      species = endo_tree_diel$tip.label,
                      response = td_endo[["Tb"]],
                      mv.response = ME_endo_replace,
                      random.cov = td_endo[c("precipmm","tempC")], 
                      fixed.fact = td_endo[['iDiel']])
model4a
summary(model4a)

model4a$modfit$AICc    #temp, Precip & Diel
model4$modfit$AICc      #temp & Precip
model5$modfit$AICc    #temp
model6$modfit$AICc   #Precip

####
model4 <- slouch.fit(phy = td_endo$phy, 
                     species = td_endo$phy$tip.label,
                     response = td_endo[["Tb"]],
                     mv.response = ME_endo_replace,
                     random.cov = td_endo[c("precipmm","tempC")])
model4
summary(model4)
plot(td_endo[["tempC"]], td_endo[["Tb"]]) 
plot(td_endo[["precipmm"]], td_endo[["Tb"]])
slouch::hillclimbplot(model4)

model4$beta_evolutionary$coefficients


# SLOUCH Analysis (Predictor = Temp)
# Model 5 (response = Tb)
model5 <- slouch.fit(phy = td_endo$phy,
                     species = td_endo$phy$tip.label,
                     response = td_endo[["Tb"]],
                     random.cov = td_endo[["tempC"]])
model5
summary(model5)
plot(td_endo[["tempC"]], td_endo[["Tb"]])
slouch::hillclimbplot(model5)

model5$beta_evolutionary$coefficients

# SLOUCH Analysis (predictor = precip)
# Model 6 (response = Tb)
model6 <- slouch.fit(phy = td_endo$phy,
                     species = td_endo$phy$tip.label,
                     response = td_endo[["Tb"]],
                     random.cov = td_endo[["precipmm"]])
model6
summary(model6)
plot(td_endo[["precipmm"]], td_endo[["Tb"]])
slouch::hillclimbplot(model6)

model6$beta_evolutionary$coefficients


################

unloadNamespace("treeio")
unloadNamespace("ggtree")
library(treeplyr)
install.packages("circlize")
library(circlize)
library(phytools)


View(td_endo)

td_endo$phy$edge.length <- td_endo$phy$edge.length/max(branching.times(td_endo$phy))
#td_endo$dat$iDiel <- rbinom(length(td_endo$phy$tip.label), 1, prob=0.5)

cmap <- contMap(td_endo$phy, setNames(td_endo$dat$Tb, td_endo$phy$tip.label), plot=FALSE)

plotSimmapCircle <- function(smap, df, vars, track.height=0.05, var.pal=viridis::viridis, map.pal=viridis::viridis, plot.heights=TRUE, treepad=0, ...){
  continuousColorScale <- function(x, pal=viridis::viridis){
    ix <- findInterval((x - min(x, na.rm=TRUE))/diff(range(x, na.rm=TRUE))*100, seq(0,100,length.out=101))  
    ret <- pal(101)[ix]
    return(ret)
  }
  
  continuousInterval <- function(x, interval=c(0,1)){
    x <- as.numeric(x)
    ix <- findInterval((x - min(x, na.rm=TRUE))/diff(range(x, na.rm=TRUE)), seq(0,1,length.out=101))  
    (ix/100*diff(interval))+min(interval)
  }
  TH <- max(branching.times(smap$tree))
  circos.clear()
  trackh <- track.height
  nvar <- length(vars)
  ntips <- length(smap$tree$tip.label)
  if(length(var.pal)==1){
    var.pal <- rep(var.pal, nvar)
  }
  if(length(plot.heights)==1){
    plot.heights <- rep(plot.heights, nvar)
  }
  circos.par(start.degree=360/(2*ntips), track.height=trackh*TH, track.margin=c(0,0),canvas.xlim=c(-1,1))
  circos.initialize(sectors=c("a"), xlim=c(ntips,0), ring=TRUE, )
  for(j in 1:nvar){
    circos.track(ylim=c(0,1),cell.padding=c(0,0,0,0), bg.border=NULL)
    .cols <- continuousColorScale(df[[vars[j]]], pal=var.pal[[j]])
    if(plot.heights[j]){
      .heights <- continuousInterval(df[[vars[j]]], c(0,1))
    } else {
      .heights <- rep(1, ntips)
    }
    for(i in 1:ntips){
      circos.rect(i-.95, 0, (i-1)+0.95, .heights[i],  col=.cols[i], border=.cols[i])
    }
  }
  smap <- setMap(smap, map.pal(100))
  plotSimmap(smap$tree, colors=smap$cols, type="fan", xlim=c(1/(-1 + trackh*TH*nvar+treepad), 1/(1 - trackh*TH*nvar - treepad)), add=TRUE, ...)
  
}


plotSimmapCircle(cmap, td_endo$dat, vars=c("precipmm", "tempC",  "iDiel"), var.pal = c(viridis::viridis, viridis::inferno, colorRampPalette(c("#34495e", "yellow")) ), 
                 map.pal = viridis::inferno, plot.heights=c(TRUE, TRUE, FALSE), ftype="off", treepad=0.05, track.height=0.08)
###############
View(td_squa1) 
getwd()

td_squa1$phy$edge.length <- td_squa1$phy$edge.length/max(branching.times(td_squa1$phy))
#td_endo$dat$iDiel <- rbinom(length(td_endo$phy$tip.label), 1, prob=0.5)

cmapsqua <- contMap(td_squa1$phy, setNames(td_squa1$dat$Tb, td_squa1$phy$tip.label), plot=FALSE)

plotSimmapCirclesqua <- function(smap, df, vars, track.height=0.05, var.pal=viridis::viridis, map.pal=viridis::viridis, plot.heights=TRUE, treepad=0, ...){
  continuousColorScale <- function(x, pal=viridis::viridis){
    ix <- findInterval((x - min(x, na.rm=TRUE))/diff(range(x, na.rm=TRUE))*100, seq(0,100,length.out=101))  
    ret <- pal(101)[ix]
    return(ret)
  }
  
  continuousInterval <- function(x, interval=c(0,1)){
    x <- as.numeric(x)
    ix <- findInterval((x - min(x, na.rm=TRUE))/diff(range(x, na.rm=TRUE)), seq(0,1,length.out=101))  
    (ix/100*diff(interval))+min(interval)
  }
  TH <- max(branching.times(smap$tree))
  circos.clear()
  trackh <- track.height
  nvar <- length(vars)
  ntips <- length(smap$tree$tip.label)
  if(length(var.pal)==1){
    var.pal <- rep(var.pal, nvar)
  }
  if(length(plot.heights)==1){
    plot.heights <- rep(plot.heights, nvar)
  }
  circos.par(start.degree=360/(2*ntips), track.height=trackh*TH, track.margin=c(0,0),canvas.xlim=c(-1,1))
  circos.initialize(sectors=c("a"), xlim=c(ntips,0), ring=TRUE, )
  for(j in 1:nvar){
    circos.track(ylim=c(0,1),cell.padding=c(0,0,0,0), bg.border=NULL)
    .cols <- continuousColorScale(df[[vars[j]]], pal=var.pal[[j]])
    if(plot.heights[j]){
      .heights <- continuousInterval(df[[vars[j]]], c(0,1))
    } else {
      .heights <- rep(1, ntips)
    }
    for(i in 1:ntips){
      circos.rect(i-.95, 0, (i-1)+0.95, .heights[i],  col=.cols[i], border=.cols[i])
    }
  }
  smap <- setMap(smap, map.pal(100))
  plotSimmap(smap$tree, colors=smap$cols, type="fan", xlim=c(1/(-1 + trackh*TH*nvar+treepad), 1/(1 - trackh*TH*nvar - treepad)), add=TRUE, ...)
  
}


plotSimmapCirclesqua(cmapsqua, td_squa1$dat, vars=c("precipmm", "tempC",  "iDiel"), var.pal = c(viridis::viridis, viridis::inferno, colorRampPalette(c("#34495e", "#f1c40f")) ), 
                     map.pal = viridis::inferno, plot.heights=c(TRUE, TRUE, FALSE), ftype="off", treepad=0.05, track.height=0.08)



###############

############### Figure Comparison of Tb in Endotherms and Ectotherms ############
#saveRDS(model1D, file="./modelfit_squam.RDS")
#saveRDS(model4a, file="./modelfit_mamm.RDS")
library(cowplot)
model1D <- readRDS("./modelfit_squam.RDS")
model4a <- readRDS("./modelfit_mamm.RDS")
modelbird <- readRDS("./modelfit_birdTDiel.RDS")

par(mfrow=c(1,2))

# TempC vs Tb (Squamates) 
col1 <-"#34495e"
col2 <- "#f1c40f"
col3 <- "#e67e22"
col4 <- "#2c3e50"
model2plot <- model1D
temp_squa_reg <- 
  ggplot(td_squa1$dat,aes(x=td_squa1$dat$tempC, y=td_squa1$dat$Tb)) +
  geom_point(aes(colour=factor(td_squa1$dat$iDiel)), size=2) +
  labs(title = "Squamates (Ectotherms)", x="Annual Mean Temperature (°C)",
       y="Field Body Temperature (°C)") +
  theme_minimal() + 
  scale_color_manual(values=c("0"=col1, "1"=col2), labels=c("Nocturnal", "Diurnal"),name="Diel Activity")+
  scale_linetype_manual(values = c(2,1), name="Regression", 
                        guide = guide_legend(override.aes = list(color = "black") ) )+
  geom_abline(aes(slope=model2plot$beta_primary$coefficients_bias_corr["tempC",1],
                  intercept = model2plot$beta_primary$coefficients_bias_corr[1,1],
                  linetype="Optimal"), size=1, colour=col4) +
  geom_abline(aes(slope=model2plot$beta_primary$coefficients_bias_corr["tempC",1],
                  intercept = model2plot$beta_primary$coefficients_bias_corr[2,1],
                  linetype="Optimal"), size=1, colour=col3) +
  geom_abline(aes(slope=model2plot$beta_evolutionary$coefficients_bias_corr["tempC",1],
                  intercept = model2plot$beta_evolutionary$coefficients_bias_corr[1,1],
                  linetype="Evolutionary"), size=1, colour=col4) +
  geom_abline(aes(slope=model2plot$beta_evolutionary$coefficients_bias_corr["tempC",1],
                  intercept = model2plot$beta_evolutionary$coefficients_bias_corr[2,1],
                  linetype="Evolutionary"), size=1, colour=col3) +
  scale_x_continuous(limit=c(-10,35), breaks=c(-10,-5, 0, 5,10,15,20,25,30,35,40),
                     labels=c("-10", "-5", "0", "5","10","15","20","25","30","35","40"),
                     expand = c(0,0)) +
  scale_y_continuous(limit=c(13,42), breaks=c(5,10,15,20,25,30,35,40,
                                              45,50,55,60),
                     labels = c("5","10","15","20","25","30","35",
                                "40","45","50","55","60"),
                     expand = c(0,0)) +
  theme(#plot.title = element_text(hjust=0.5, size=20, face="plain"),
    panel.border=element_rect(colour="black", fill=NA, 
                              size=0.5), 
    legend.position = "bottom",
    legend.box="vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(),
    axis.title = element_text(size = 14, face = "plain"),
    axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y.left = element_text(margin = margin(0, 15, 0, 0)),
    plot.title = element_text(margin = margin(0,0,10,0))) 
temp_squa_reg


#MAMMALS
model2plot2 <- model4a
temp_mamm_reg <- 
  ggplot(td_endo$dat,aes(x=td_endo$dat$tempC, y=td_endo$dat$Tb)) +
  geom_point(aes(colour=factor(td_endo$dat$iDiel)), size=2) +
  labs(title = "Mammals (Endotherms)", x="Annual Mean Temperature (°C)",
       y="Field Body Temperature (°C)") +
  theme_minimal() + 
  scale_color_manual(values=c("0"=col1, "1"=col2), labels=c("Nocturnal", "Diurnal"),name="Diel Activity")+
  scale_linetype_manual(values = c(2,1), name="Regression", 
                        guide = guide_legend(override.aes = list(color = "black") ) )+
  geom_abline(aes(slope=model2plot2$beta_primary$coefficients_bias_corr["tempC",1],
                  intercept = model2plot2$beta_primary$coefficients_bias_corr[1,1],
                  linetype="Optimal"), size=1, colour=col4) +
  geom_abline(aes(slope=model2plot2$beta_primary$coefficients_bias_corr["tempC",1],
                  intercept = model2plot2$beta_primary$coefficients_bias_corr[2,1],
                  linetype="Optimal"), size=1, colour=col3) +
  geom_abline(aes(slope=model2plot2$beta_evolutionary$coefficients_bias_corr["tempC",1],
                  intercept = model2plot2$beta_evolutionary$coefficients_bias_corr[1,1],
                  linetype="Evolutionary"), size=1, colour=col4) +
  geom_abline(aes(slope=model2plot2$beta_evolutionary$coefficients_bias_corr["tempC",1],
                  intercept = model2plot2$beta_evolutionary$coefficients_bias_corr[2,1],
                  linetype="Evolutionary"), size=1, colour=col3) +
  scale_x_continuous(limit=c(-10,35), breaks=c(-10,-5, 0, 5,10,15,20,25,30,35,40),
                     labels=c("-10", "-5", "0", "5","10","15","20","25","30","35","40"),
                     expand = c(0,0)) +
  scale_y_continuous(limit=c(13,42), breaks=c(5,10,15,20,25,30,35,40,
                                              45,50,55,60),
                     labels = c("5","10","15","20","25","30","35",
                                "40","45","50","55","60"),
                     expand = c(0,0)) +
  theme(#plot.title = element_text(hjust=0.5, size=20, face="plain"),
    panel.border=element_rect(colour="black", fill=NA, 
                              size=0.5), 
    legend.position = "bottom",
    legend.box="vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks = element_line(),
    axis.title = element_text(size = 14, face = "plain"),
    axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y.left = element_text(margin = margin(0, 15, 0, 0)),
    plot.title = element_text(margin = margin(0,0,10,0))) 
temp_mamm_reg

pdf("./poster_regressions1.pdf", height=6, width=6)
temp_squa_reg
dev.off()
pdf("./poster_regressions2.pdf", height=6, width=6)
temp_mamm_reg
dev.off()
#####################

##BIRDS
model2plot <- modelbird
temp_bird_reg <- ggplot(td_bird$dat,
                        aes(x=td_bird$dat$tempC, y=td_bird$dat$Tb)) +
  geom_point(aes(colour=factor(td_bird$dat$iDiel)), size=2) +
  labs(title = "Bird (Endotherms)", x="Annual Mean Temperature (°C)",
       y="Body Temperature (°C)") +
  theme_minimal() + 
  scale_color_manual(values=c("0"=col1, "1"=col2), labels=c("Nocturnal", "Diurnal"),name="Diel Activity")+
  theme(plot.title = element_text(hjust=0.5, size=20, face="plain")) +
  geom_abline(slope=model2plot$beta_primary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_primary$coefficients_bias_corr[1,1],
              size=1, colour=col4) +
  geom_abline(slope=model2plot$beta_primary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_primary$coefficients_bias_corr[2,1],
              size=1, colour=col3) +
  geom_abline(slope=model2plot$beta_evolutionary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_evolutionary$coefficients_bias_corr[1,1],
              linetype="dashed", size=1, colour=col4) +
  geom_abline(slope=model2plot$beta_evolutionary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_evolutionary$coefficients_bias_corr[2,1],
              linetype="dashed", size=1, colour=col3) +
  scale_x_continuous(limit=c(-10,35), breaks=c(-10,-5, 0, 5,10,15,20,25,30,35,40),
                     labels=c("-10", "-5", "0", "5","10","15","20","25","30","35","40"),
                     expand = c(0,0)) +
  scale_y_continuous(limit=c(13,45), breaks=c(5,10,15,20,25,30,35,40,
                                              45,50,55,60),
                     labels = c("5","10","15","20","25","30","35",
                                "40","45","50","55","60"),
                     expand = c(0,0)) +
  theme(panel.border=element_rect(colour="black", fill=NA, 
                                  size=0.5), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(),
        axis.title = element_text(size = 14, face = "plain"),
        axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y.left = element_text(margin = margin(0, 15, 0, 0)),
        plot.title = element_text(margin = margin(0,0,10,0))) 
temp_bird_reg

########
#SIMILAR TO ABOVE
#####
# TempC vs Tb (Squamates) 
col1 <-"#34495e"
col2 <- "#f1c40f"
col3 <- "#e67e22"
col4 <- "#2c3e50"
model2plot <- model1D
temp_squa_reg <- ggplot(td_squa1$dat,
                        aes(x=td_squa1$dat$tempC, y=td_squa1$dat$Tb)) +
  geom_point(aes(colour=factor(td_squa1$dat$iDiel)), size=2) +
  labs(title = "Squamates (Ectotherms)", x="Annual Mean Temperature (°C)",
       y="Field Body Temperature (°C)") +
  theme_minimal() + 
  scale_color_manual(values=c("0"=col1, "1"=col2), labels=c("Nocturnal", "Diurnal"),name="Diel Activity")+
  theme(plot.title = element_text(hjust=0.5, size=20, face="plain")) +
  geom_abline(slope=model2plot$beta_primary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_primary$coefficients_bias_corr[1,1],
              size=1, colour=col4) +
  geom_abline(slope=model2plot$beta_primary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_primary$coefficients_bias_corr[2,1],
              size=1, colour=col3) +
  geom_abline(slope=model2plot$beta_evolutionary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_evolutionary$coefficients_bias_corr[1,1],
              linetype="dashed", size=1, colour=col4) +
  geom_abline(slope=model2plot$beta_evolutionary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_evolutionary$coefficients_bias_corr[2,1],
              linetype="dashed", size=1, colour=col3) +
  scale_x_continuous(limit=c(-10,35), breaks=c(-10,-5, 0, 5,10,15,20,25,30,35,40),
                     labels=c("-10", "-5", "0", "5","10","15","20","25","30","35","40"),
                     expand = c(0,0)) +
  scale_y_continuous(limit=c(13,42), breaks=c(5,10,15,20,25,30,35,40,
                                              45,50,55,60),
                     labels = c("5","10","15","20","25","30","35",
                                "40","45","50","55","60"),
                     expand = c(0,0)) +
  theme(panel.border=element_rect(colour="black", fill=NA, 
                                  size=0.5), 
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_text(size = 14, face = "plain"),
        axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y.left = element_text(margin = margin(0, 15, 0, 0)),
        plot.title = element_text(margin = margin(0,0,10,0))) 
temp_squa_reg

#Repeat plot, but with mammals
model2plot <- model4a
temp_endo_reg <- ggplot(td_endo$dat,
                        aes(x=td_endo$dat$tempC, y=td_endo$dat$Tb)) +
  geom_point(aes(colour=factor(td_endo$dat$iDiel)), size=2) +
  labs(title = "Mammals (Endotherms)", x="Annual Mean Temperature (°C)",
       y="Body Temperature (°C)") +
  theme_minimal() + 
  scale_color_manual(values=c("0"=col1, "1"=col2), labels=c("Nocturnal", "Diurnal"),name="Diel Activity")+
  theme(plot.title = element_text(hjust=0.5, size=20, face="plain")) +
  geom_abline(slope=model2plot$beta_primary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_primary$coefficients_bias_corr[1,1],
              size=1, colour=col4) +
  geom_abline(slope=model2plot$beta_primary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_primary$coefficients_bias_corr[2,1],
              size=1, colour=col3) +
  geom_abline(slope=model2plot$beta_evolutionary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_evolutionary$coefficients_bias_corr[1,1],
              linetype="dashed", size=1, colour=col4) +
  geom_abline(slope=model2plot$beta_evolutionary$coefficients_bias_corr["tempC",1],
              intercept = model2plot$beta_evolutionary$coefficients_bias_corr[2,1],
              linetype="dashed", size=1, colour=col3) +
  scale_x_continuous(limit=c(-10,35), breaks=c(-10,-5, 0, 5,10,15,20,25,30,35,40),
                     labels=c("-10", "-5", "0", "5","10","15","20","25","30","35","40"),
                     expand = c(0,0)) +
  scale_y_continuous(limit=c(13,42), breaks=c(5,10,15,20,25,30,35,40,
                                              45,50,55,60),
                     labels = c("5","10","15","20","25","30","35",
                                "40","45","50","55","60"),
                     expand = c(0,0)) +
  theme(panel.border=element_rect(colour="black", fill=NA, 
                                  size=0.5), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(),
        axis.title = element_text(size = 14, face = "plain"),
        axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y.left = element_text(margin = margin(0, 15, 0, 0)),
        plot.title = element_text(margin = margin(0,0,10,0))) 
temp_endo_reg

pdf("./poster_regressions.pdf", height=12, width=6)
plot_grid(temp_squa_reg, temp_endo_reg, ncol=1)
dev.off()


# Precipmm vs Tb (squamates) 
precip_squa_reg <- ggplot(td_squa1$dat,
                          aes(x=td_squa1$dat$precipmm, y=td_squa1$dat$Tb)) +
  geom_point(size=2) +
  labs(title = "Squamates (Ectotherms)", x="Annual Precipitation (mm)",
       y="Field Body Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20, face="plain")) +
  geom_abline(slope=model1$beta_primary$coefficients_bias_corr[2,1],
              intercept = model1$beta_primary$coefficients_bias_corr[1,1],
              size=1, colour="lightgreen") +
  geom_abline(slope=model1$beta_evolutionary$coefficients_bias_corr[2,1],
              intercept = model1$beta_evolutionary$coefficients_bias_corr[1,1],
              linetype="dashed", size=1, colour="lightblue") +
  scale_x_continuous(limit=c(0,4000), breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000),
                     labels=c("0","500","1000","1500","2000","2500","3000","3500","4000"),
                     expand = c(0,0)) +
  scale_y_continuous(limit=c(0,50), breaks=c(5,10,15,20,25,30,35,40,
                                             45,50),
                     labels = c("5","10","15","20","25","30","35",
                                "40","45","50"),
                     expand = c(0,0)) +
  theme(panel.border=element_rect(colour="black", fill=NA, 
                                  size=0.5), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_text(size = 14, face = "plain"),
        axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y.left = element_text(margin = margin(0, 15, 0, 0)),
        plot.title = element_text(margin = margin(0,0,10,0))) 


precip_squa_reg

# TempC vs Tb (Mammals) 
temp_endo_reg <- ggplot(td_endo$dat,
                        aes(x=td_endo$dat$tempC, y=td_endo$dat$Tb)) +
  geom_point(size=2) +
  labs(title = "Mammals (Endotherms)*", x="Annual Mean Temperature (°C)",
       y="Field Body Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20, face="plain")) +
  geom_abline(slope=model4$beta_primary$coefficients_bias_corr[3,1],
              intercept = model4$beta_primary$coefficients_bias_corr[1,1],
              size=1, colour="lightgreen") +
  geom_abline(slope=model4$beta_evolutionary$coefficients_bias_corr[3,1],
              intercept = model4$beta_evolutionary$coefficients_bias_corr[1,1],
              linetype="dashed", size=1, colour="lightblue") +
  scale_x_continuous(limit=c(0,40), breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40),
                     labels=c("-25","-20","-15","-10","-5","0","5","10","15","20","25","30","35","40"),
                     expand = c(0,0)) +
  scale_y_continuous(limit=c(13,45), breaks=c(25,30,35,40,
                                              45),
                     labels = c("25","30","35",
                                "40","45"),
                     expand = c(0,0)) +
  theme(panel.border=element_rect(colour="black", fill=NA, 
                                  size=0.5), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_text(size = 14, face = "plain"),
        axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y.left = element_text(margin = margin(0, 15, 0, 0)),
        plot.title = element_text(margin = margin(0,0,10,0)))

temp_endo_reg

# Precipmm vs Tb (Mammals) --> add geom_smooth? remove family grouping
precip_endo_reg <- ggplot(td_endo$dat,
                          aes(x=td_endo$dat$precipmm, y=td_endo$dat$Tb)) +
  #geom_point(size=2) +
  geom_point(aes(colour=factor(td_endo$dat$iDiel)), size=2) +
  labs(title = "Mammals (Endotherms)", x="Annual Precipitation (mm)",
       y="Field Body Temperature (°C)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20, face="plain")) +
  geom_abline(slope=model4a$beta_primary$coefficients_bias_corr[2,1],
              intercept = model4a$beta_primary$coefficients_bias_corr[1,1],
              size=1, colour="lightgreen") +
  geom_abline(slope=model4a$beta_evolutionary$coefficients_bias_corr[2,1],
              intercept = model4a$beta_evolutionary$coefficients_bias_corr[1,1],
              linetype="dashed", size=1, colour="lightblue") +
  scale_x_continuous(limit=c(0,4500), breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000,
                                               4500),
                     labels=c("0","500","1000","1500","2000","2500","3000","3500","4000",
                              "4500"),
                     expand = c(0,0)) +
  scale_y_continuous(limit=c(20,50), breaks=c(20,25,30,35,40,
                                              45,50),
                     labels = c("20","25","30","35",
                                "40","45","50"),
                     expand = c(0,0)) +
  theme(panel.border=element_rect(colour="black", fill=NA, 
                                  size=0.5), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_text(size = 14, face = "plain"),
        axis.title.x.bottom = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y.left = element_text(margin = margin(0, 10, 0, 0)),
        plot.title = element_text(margin = margin(0,0,10,0))) 

precip_endo_reg

View(td_endo$dat)
View(td_squa$dat)

# All graphs together
ggarrange(temp_squa_reg, precip_squa_reg, temp_endo_reg,
          precip_endo_reg, 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)

?ggarrange
##################### Run Brownian Motion Model for Tb #########################
#### Check diffusion variances with BM model (sigma squared values)
###### Squamates
# Brownina Motion for Tb in Squamates
squa_phy <- td_squa1$phy
squa_Tb <- c(td_squa1$phy$tip.label, td_squa1$dat$Tb) 
species <- squa_Tb[1:1000]
View(species)
Tb <- squa_Tb[501:1000]
View(Tb)
squa_Tb <- cbind(species, Tb)
squa_Tb <- as.data.frame(squa_Tb)
squa_Tb <- setNames(as.numeric(squa_Tb$Tb), squa_Tb$species)

ME_squa1 <- cbind(species, ME_squa_replace1)
ME_squa1

ME_squa1 <- as.data.frame(ME_squa1)
ME_squa1 <- setNames(as.numeric(ME_squa1$ME), ME_squa1$species)
ME_squa1

fitBM_squa <- fitContinuous(squa_phy, squa_Tb, model = "BM", SE=ME_squa1)
fitBM_squa
summary(fitBM_squa)

# fitContinuous for temp and precip in Squamates
# Enviro Temp for Squamates
temp <- td_squa1$dat$tempC
squa_temp <-cbind(species, temp)
View(squa_temp)

squa_temp <- as.data.frame(squa_temp)
squa_temp <- setNames(as.numeric(squa_temp$temp), squa_temp$species)

fitContinuous(phy=squa_phy, squa_temp, model = "BM")

# Envrio Precip for Squamates
precip <- td_squa$dat$precipmm
squa_precip <- cbind(species, precip)

squa_precip <- as.data.frame(squa_precip)
squa_precip <- setNames(as.numeric(squa_precip$precip), squa_precip$species)

fitContinuous(phy=squa_phy, squa_precip, model = "BM")

##### Endotherms
# Brownian Motion for Tb in Mammals
endo_phy <- td_endo$phy
endo_Tb <- c(td_endo$phy$tip.label, td_endo$dat$Tb)
View(endo_Tb)

species_endo <- endo_Tb[1:807]
View(species_endo)
Tb_endo <- endo_Tb[808:1614]
View(Tb_endo)

endo_Tb <- cbind(species_endo, Tb_endo)
View(endo_Tb)
endo_Tb <- as.data.frame(endo_Tb)
View(endo_Tb)

endo_Tb$species_endo
endo_Tb$Tb_endo

endo_Tb <- setNames(as.numeric(endo_Tb$Tb_endo), endo_Tb$species_endo)

ME_endo <- cbind(species_endo, ME_endo_replace)
ME_endo

ME_endo <- as.data.frame(ME_endo)
ME_endo <- setNames(as.numeric(ME_endo$ME), ME_endo$species)
ME_endo

endo_phy
class(endo_phy)
is.binary(endo_phy)
endo_phy <- multi2di(endo_phy)

fitBM_endo <- fitContinuous(endo_phy, endo_Tb, model = "BM", SE=ME_endo)
fitBM_endo

# fitContinuous for precip and temp in Mammals
# Enviro Temp for Mammals
temp2 <- td_endo$dat$tempC
endo_temp <- cbind(species_endo, temp2)
endo_temp <- as.data.frame(endo_temp)
endo_temp <- setNames(as.numeric(endo_temp$temp2), endo_temp$species_endo)
fitContinuous(endo_phy, endo_temp, model = "BM")

# Enviro Precip for Mammals
precip2 <- td_endo$dat$precipmm
endo_precip <- cbind(species_endo, precip2)
endo_precip <- as.data.frame(endo_precip)
endo_precip <- setNames(as.numeric(endo_precip$precip2), endo_precip$species_endo)
fitContinuous(endo_phy, endo_precip, model = "BM")

############# Brownian Motion Model Bar Plots for Tb, tempc, precipmm in squamates and mammals ############
#precip
df2 <- data.frame(Group=rep(c("Squamates","Mammals"), each=1),
                  EvolutionaryRate=c(9625.271,49565.84))
View(df2)

precip_endoexo_bg <- ggplot(data=df2, aes(x='Precip', y=EvolutionaryRate, fill=Group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c('lightblue','lightgreen')) +
  xlab("Precip") +
  ylab(expression(paste("Evolutionary Rate ", (sigma^2)))) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
precip_endoexo_bg

#temp
df3 <- data.frame(Group=rep(c("Squamates","Mammals"), each=1),
                  EvolutionaryRate=c(0.285271, 5.329388))
View(df3)

temp_endoexo_bg <- ggplot(data=df3, aes(x='Temp', y=EvolutionaryRate, fill=Group)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values = c('lightblue', 'lightgreen')) +
  xlab("Precip") +
  ylab(expression(paste("Evolutionary Rate ", (sigma^2)))) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
temp_endoexo_bg

#Tb
df4 <- data.frame(Group=rep(c("Squamates","Mammals"), each=1),
                  EvolutionaryRate=c(0.255942, 0.092254))
View(df4)

Tb_endoexo_bg <- ggplot(data=df4, aes(x='Tb', y=EvolutionaryRate, fill=Group)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values = c('lightblue', 'lightgreen')) +
  xlab("Tb") +
  ylab(expression(paste("Evolutionary Rate  ", (sigma^2)))) +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  scale_x_discrete(labels=c(expression(T["b"])))

Tb_endoexo_bg


# All graphs together
Install.packages(ggpubr)
library(ggpubr)

ggarrange(Tb_endoexo_bg, temp_endoexo_bg, precip_endoexo_bg,
          ncol = 3, nrow = 1, widths=c(0.45,0.4,0.55))







