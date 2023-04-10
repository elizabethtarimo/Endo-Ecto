

setwd("~/Desktop/Current Research/Endo&EctoFile")


#TREE WITH TB AS THE CONTINOUS TRAIT
objSqua<-contMap(td_squa1$phy,td_squa1[["Tb"]])
plot(objSqua,type="fan",lwd=1) 

contMap(td_squa1$phy,td_squa1[["Tb"]] , res=100, fsize=0.7, ftype="off", lwd=4, legend=NULL,
        lims=NULL, outline=TRUE, sig=3, type="fan", direction="rightwards", 
        plot=TRUE)

contMap(td_squa1$phy,td_squa1[["Tb"]] , res=100, fsize=NULL, ftype=NULL, lwd=4, legend=NULL,
        lims=NULL, outline=TRUE, sig=3, type="phylogram", direction="rightwards", 
        plot=TRUE)





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

