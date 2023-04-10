library(ape)
library(slouch)

setwd("~/Desktop/Current Research/Endo&EctoFile")

td_endo <- readRDS("td_endo_9_14.RDS")

class(td_endo)
#View(td_endo$phy$tip.label)

# Endotherm species list
endo_sp_list <- as.data.frame(td_endo$phy$tip.label)

#View(endo_sp_list)

td_endo$dat <- cbind(endo_sp_list,td_endo$dat)

#View(td_endo$dat)
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
