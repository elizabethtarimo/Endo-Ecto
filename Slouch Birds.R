library(ape)
library(slouch)


td_bird <- readRDS("td_bird_8_31.RDS")
View(td_bird$dat)
class(td_bird)
#View(td_bird$phy$tip.label)

nrow(td_bird$dat)


#TREE WITH TB AS THE CONTINOUS TRAIT
objbird<-contMap(td_bird$phy,td_bird[["Tb"]])
plot(objbird,type="fan",lwd=1) 

contMap(td_bird$phy,td_bird[["Tb"]] , res=100, fsize=0.7, ftype="off", lwd=4, legend=NULL,
        lims=NULL, outline=TRUE, sig=3, type="fan", direction="rightwards", 
        plot=TRUE)

contMap(td_bird$phy,td_bird[["Tb"]] , res=100, fsize=NULL, ftype=NULL, lwd=4, legend=NULL,
        lims=NULL, outline=TRUE, sig=3, type="phylogram", direction="rightwards", 
        plot=TRUE)



# Predictors = tempC, precipmm
# Response Variables = Tb 
# Model 1 (response = Tb)
#?what is mv responce
?slouch.fit

modela <- slouch.fit(phy = td_bird$phy, 
                     species = td_bird$phy$tip.label,
                     response = td_bird[["Tb"]],
                     mv.response = ME_bird_replace,
                     random.cov = td_bird[['tempC']])
modela

saveRDS(modelb, file = "modelfit_birdT.RDS")
summary(modela)


plot(td_bird[["tempC"]], td_bird[["Tb"]]) 

slouch::hillclimbplot(modela)

modela$beta_evolutionary$coefficients

modela$beta_primary$coefficients_bias_corr
modela$beta_evolutionary$coefficients_bias_corr


#####WITH DIEL
  tree_bird_diel <- td_bird$phy
  diel_asr <- ace(x=setNames(factor(td_bird$dat$iDiel+1), tree_bird_diel$tip.label), phy=multi2di(tree_bird_diel), type="discrete")
  tree_bird_diel$node.label <- unname(apply(diel_asr$lik.anc, 1, function(x) which(x==max(x))))-1

plot(tree_bird_diel, show.tip.label = FALSE, type="fan")
tiplabels(pch=21, bg=td_bird$dat$iDiel+1, col=td_bird$dat$iDiel+1, cex=0.5)
nodelabels(pch=21, bg=tree_bird_diel$node.label+1, col=tree_bird_diel$node.label+1)

modelb <- slouch.fit(phy = tree_bird_diel, 
                      species = tree_bird_diel$tip.label,
                      response = td_bird$dat$Tb,
                      mv.response = ME_bird_replace,
                      random.cov = td_bird[['tempC']], 
                      fixed.fact = td_bird[['iDiel']])
modelb
summary(modelb)
saveRDS(modelb, file = "modelfit_birdTDiel.RDS")


modela$modfit$AICc    #temp
modelb$modfit$AICc      #temp & Diel

######
#this is if I had precip data for birds
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



#library(phylolm)
ndat <- td_squa$dat
rownames(ndat) <- td_squa$phy$tip.label
lm1 <- phylolm(Tb ~ tempC,  data=ndat, phy=td_squa$phy)


td_bird$dat$iDiel==1



