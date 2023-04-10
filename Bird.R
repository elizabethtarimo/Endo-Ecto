setwd("~/Desktop/Current Research/Endo&EctoFile")
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

###################################
#reading in the raw data and the tree data from species 360
getwd()
sp360 <- readRDS("sp360Tb.RDS")
#View(sp360)

tdsp360 <- readRDS("tdsp360.RDS")
#View(tdsp360$dat)
nrow(tdsp360$dat)

#match first with the species list.
allsp360list<-as.data.frame(tdsp360$phy$tip.label)
#View(allsp360list)

tdallsp360<-cbind(allsp360list,tdsp360$dat)
#View(tdallsp360)
nrow(tdallsp360)


tdbirds<-allsp360[tdallsp360$class == 'Aves', ]
#View(tdbirds) #140
nrow(tdbirds)


#substittute the _ for just space to later match
tdbirds$`tdsp360$phy$tip.label` <- sub("_", " ", tdbirds$`tdsp360$phy$tip.label`)

#Renaming
tdbirds <- tdbirds %>% dplyr::rename(Species='tdsp360$phy$tip.label')
#View(tdbirds)   #sp360   #140

#View(birds1)    #Dr. Weins data  #474

#merging sp360 with birds1(this retains only the common spp from both)
totalbirds <- merge(tdbirds,birds1,by="Species")
#View(totalbirds) #21


#with this you get all the rows from each df
a<-bind_rows(tdbirds, birds1) 
nrow(a)#614


#what I want(21 species are duplicated, should drop them)
#593


#to drop the duplicated species and assign them to either Tb.360 or Tb.W
Allb <- cbind(a, Tb.360=NA,Tb.W=NA)
#View(Allb)
nrow(Allb)
    
names <- unique(Allb$Species)
library(dplyr)

temp <- list()
new_df <- data.frame(names, rep(NA,length(names)), rep(NA,length(names)), rep(NA,length(names)), rep(NA,length(names)), rep(NA,length(names)), rep(NA,length(names)), rep(NA,length(names)), rep(NA,length(names)),rep(NA,length(names)),rep(NA,length(names)))
colnames(new_df)<-colnames(Allb)

for(i in 1:length(names)) {
df_all <- filter(Allb, Allb$Species==names[i])

if (length(df_all$Species)==2) {
  
  new_df[i,1:9] <- df_all[1,1:9]
  new_df[i,10] <- df_all$Tb[1]
  new_df[i,11] <- df_all$Tb[2]
  
} else {
  new_df[i,1:9] <- df_all[1,1:9]
  new_df[i,10]<-df_all$Tb[1]
  new_df[i,11]<-NA
}
}
       
for (i in 1:10) {
  if (!i %% 2){
    next
  }
  print(i)
}
  

nrow(new_df) #593


#Add a column to have our TB (Avarage)
new_df <- cbind(new_df, TB=NA)
#View(new_df)


#Calculate the mean 
new_df[new_df == 0] <- NA

new_df$TB <- rowMeans(new_df[ , c(10,11)], na.rm=TRUE)

#USE THE Tb
new_df<-rename(new_df,Tbody=Tb)
new_df<-rename(new_df,Tb=TB)

#rearranging the colummns 
new_df <- new_df[, c(1, 12, 3, 4, 5,6,7,8,9,10,11,2)]
View(new_df)
#drop some columnns
New_df = select(new_df, c(-6,-7,-8,-9,-10,-11,-12))
#View(New_df)
nrow(New_df)

Birds<-New_df
#View(Birds)   #593


#Env. Temperature data from Wein, 
WeinbirdsTa<-read.xlsx("NewDF.xlsx",4)
#View(WeinbirdsTa)
nrow(WeinbirdsTa)  #636

#This just joins every available species from the 2 data frames
joined_df <- merge(WeinbirdsTa, Birds, by.x = "Species.name.in.tree", 
                   by.y = "Species", all.x = TRUE, all.y = TRUE)

#View(joined_df)
nrow(joined_df)  #891

# Query OTL for otl names
ott_Birds <- tnrs_match_names(Birds$Species)
ott_Birds$unique_name[is.na(ott_Birds$unique_name)] <- Birds$Species[is.na(ott_Birds$unique_name)]
ott_Weinbirds <- tnrs_match_names(WeinbirdsTa$Species.name.in.tree)

#make otl name columns
Birds$ottNames <- ott_Birds$unique_name
WeinbirdsTa$ottNames <- ott_Weinbirds$unique_name
#Joining with the Birds data to drop the species that will not be in the tree.  
NewB<-left_join(Birds, WeinbirdsTa, by=c("ottNames" = "ottNames"))
nrow(NewB) #593
#View(NewB)

colnames(NewB)[10] <- "tempC"
colnames(NewB)[11] <- "TMax"
colnames(NewB)[12] <- "TMin"
colnames(NewB)[6] <- "Species"

Bird_data <- NewB[, c(6,2,3,4,5,10,11,12,1,7,8,9,13)]
#View(Bird_data)

#drop the NA tempC
Bird_dat<-Bird_data %>% drop_na(tempC)
View(Bird_dat)
nrow(Bird_dat)  #478

saveRDS(Bird_dat, file = "Bird_dat.RDS")
Bird_dat<-readRDS("Bird_dat.RDS")
View(Bird_dat)

################
#######Start of merging with Diel
Diel=read.csv("diel1 - Sheet1.csv")
Diel<-rename(Diel, species=Binomial)
colnames(Bird_dat)[1] <- "species"


Dielbird<-left_join(Bird_dat,Diel,by="species")

View(Dielbird)
nrow(Dielbird) #478


#DIURNAL=1, NOCTURNAL=0, ARRYTHMIC=1, CRESPUCULAR=0
Dielbird['Diel'][Dielbird['Diel'] == 'DIU'] <- '1'
Dielbird['Diel'][Dielbird['Diel'] == 'NOC'] <- '0'
Dielbird['Diel'][Dielbird['Diel'] == 'ARR'] <- '1'
Dielbird['Diel'][Dielbird['Diel'] == 'CRE'] <- '0'


saveRDS(Dielbird, file = "Dielbird.RDS")
Dielbird<-readRDS("Dielbird.RDS")
View(Dielbird)

#read in the tree
bird_tree <- read.tree("bird.tre")
plot(bird_tree,show.tip.label = FALSE)
bird_tree$tip.label


# Remove "_" in between genus and species for tip labels
bird_tree$tip.label <- gsub("_", " ", bird_tree$tip.label)
bird_tree$tip.label
class(bird_tree)

td_bird <- make.treedata(bird_tree,Dielbird)


plot(td_bird$phy,show.tip.label = FALSE)

nrow(as.data.frame(td_bird$phy$tip.label))   #476


source("imputeDiscrete.R")

set.seed(1)

impDielbird <-impute_discrete(td_bird$phy, setNames(td_bird$dat$Diel, td_bird$phy$tip.label))

impDielbird1<-data.frame(td_bird$phy$tip.label,td_bird$dat$Diel , impDielbird)
td_bird$dat$iDiel <- impDielbird
View(td_bird$dat)

...
# Compare taxa in data and tree
name.check(td_bird$phy$tip.label,td_bird$dat$species)


td_bird$dat$species == td_bird$phy$tip.label
td_bird$dat$tip.label <- td_bird$phy$tip.label
#cbind(td_bird$phy$tip.label, td_bird$dat)[td_bird$dat$species != td_bird$phy$tip.label,]


td_bird$phy$tip.label
td_bird[["Tb"]]
td_bird$dat


nrow(td_bird$dat)
#476

# Taking into account measurement error with Tb
# Trying to add ME into slouch.fit
ME_bird <- td_bird$dat$ME
#View(ME_bird)

# Remove NA's from list in ME_endo
ME_bird <- na.omit(ME_bird)
#View(ME_bird)
#View(data.frame(ME_bird))
class(ME_bird)

# Take median of values
med_bird <- median(ME_bird)
med_bird #0.6681981

# Replace NA's in orginal ME list with med_bird value

ME_bird_replace <- td_bird$dat$ME
ME_bird_replace <- ME_bird_replace %>% replace_na( 0.6681981)
#View(ME_bird_replace)
#View(data.frame(ME_bird_replace))


#View(td_bird$phy$tip.label)
#View(td_bird$dat)
nrow(td_bird$dat)
View(td_bird$dat)

# Save as RDS 
getwd()

#saveRDS(td_bird, file="td_bird_8_31.RDS")
#td_bird<-readRDS("td_bird_8_31.RDS")

Aquat<-read.csv("Aquat.csv")
View(Aquat)
class(Aquat)



nrow(td_bird$dat)   #476
length(which(td_bird$dat$iDiel==1))
#439

length(which(td_bird$dat$iDiel==0))
#37

