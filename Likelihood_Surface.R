library(slouch)

getwd()
#setwd("~/Desktop/Current Research/Endo&EctoFile")

#td_endo<-readRDS("td_endo_9_14.RDS")
#modelS <- readRDS("./modelfit_squam.RDS")
modelM <- readRDS("./modelfit_mamm.RDS")
#modelB <- readRDS("./modelfit_birdTDiel.RDS")


#1 mammals
#look for how deep is the tree
node.depth.edgelength(td_endo$phy)


# Read in the phylogenetic tree and data
tree <- td_endo$phy
data <- td_endo$dat

## Using the seq function
h <- seq(from = 0.001, to = 100, length.out = 15)
vy <- seq(from = 0.001, to = 5, length.out = 15)


model_grid_M1 <- slouch.fit(phy = tree,
                           hl_values = seq(0.001, 12, length.out = 20),
                           vy_values = seq(0.1, 1, length.out = 20),
                           species = td_endo$phy$tip.label,
                           response = td_endo$dat$Tb,
                           hillclimb = FALSE)


plot(model_grid_M1)
#No hill here coz your h1/2 and Vy values are off

model_grid_M2 <- slouch.fit(phy = tree,
                           hl_values = seq(30, 65, length.out = 20),
                           vy_values = seq(3, 6, length.out = 20),
                           species =  td_endo$phy$tip.label,
                           response = td_endo$dat$Tb,
                           mv.response = ME_endo_replace,
                           random.cov = td_endo[c("precipmm","tempC")],
                           hillclimb = FALSE)



plot(model_grid_M2)
#A hill


#Trying to fine grid the hill
model_grid_M3 <- slouch.fit(phy = tree,
                            hl_values = seq(30, 65, length.out = 50),
                            vy_values = seq(3, 6, length.out = 50),
                            species =  td_endo$phy$tip.label,
                            response = td_endo$dat$Tb,
                            mv.response = ME_endo_replace,
                            random.cov = td_endo[c("precipmm","tempC")],
                            hillclimb = FALSE)



plot(model_grid_M3)






#2 Squamates

#h <- seq(from = 0.001, to = 100, length.out = 15)
#vy <- seq(from = 0.001, to = 5, length.out = 15)

#Matching the tip labels with species names
td_squa1$dat$species == phy$tip.label


model_grid_S1 <- slouch.fit(phy = tree,
                            hl_values = seq(0.001, 100, length.out = 20),
                            vy_values = seq(0.001, 5, length.out = 20),
                            species = td_squa1$phy$tip.label,
                            response = td_squa1$dat$Tb,
                            mv.response = ME_squa_replace1,
                            random.cov = td_squa1[c("precipmm","tempC")],
                            hillclimb = FALSE)

plot(model_grid_S1 )

