setwd("~/Desktop/Current Research/Endo&EctoFile")

library(dplyr)
library(slouch)


td_endo <- readRDS("td_endo_9_14.RDS")
View(td_endo$dat)
nrow(td_endo$dat)

#Code for ruminants

#c("Bovidae","Antilocapridae","Moschidae","Cervidae","Giraffidae") 

#rename
#td_endo$dat<-rename(td_endo$dat, Rumi='R/F')
#td_endo$dat<-td_endo$dat %>% dplyr::select(-new_col)


#td_endo$dat <- mutate(td_endo$dat, TRD = paste0(AdTerr,Rumi,iDiel))


td_endo$dat['TRD'][td_endo$dat['TRD']=="111"] <- "1"
td_endo$dat['TRD'][td_endo$dat['TRD']=="110"] <- "2"
td_endo$dat['TRD'][td_endo$dat['TRD']=="100"] <- "3"
td_endo$dat['TRD'][td_endo$dat['TRD']=="101"] <- "4"
td_endo$dat['TRD'][td_endo$dat['TRD']=="000"] <- "5"
td_endo$dat['TRD'][td_endo$dat['TRD']=="011"] <- "5"
td_endo$dat['TRD'][td_endo$dat['TRD']=="001"] <- "5"
td_endo$dat['TRD'][td_endo$dat['TRD']=="010"] <- "5"

library(corHMM)
mkTRD <- matrix(nrow = 5, ncol = 5, byrow = T, data = c(0,1,0,0,4,
                                                        2,0,0,0,4,    
                                                        0,3,0,2,4,
                                                        3,0,1,0,4,
                                                        0,0,0,0,0)
)

diag(mkTRD) <- 0

mkTRD_Model <- castor::fit_mk(tree = td_endo$phy, tip_states = setNames(as.integer(td_endo$dat$TRD),td_endo$phy$tip.label), Nstates = 5, rate_model = mkTRD)

mkTRD[mkTRD==0] <- NA
p <- c(mkTRD_Model$transition_matrix[1,2], mkTRD_Model$transition_matrix[2,1], mkTRD_Model$transition_matrix[3,2], mkTRD_Model$transition_matrix[1,5])
.tree <- td_endo$phy
.tree$node.label <- NULL
mkTRD_asr <- ancRECON(.tree, data=data.frame("Genus_sp"=td_endo$phy$tip.label,model="ARD", "TRD"=td_endo$dat$TRD),p=p, method="joint", 
                      rate.cat=1,  rate.mat=mkTRD, root.p=c(0,0,0.5, 0.5, 0))

#mkTRD_asr <- ape::ace(td_endo$dat$TRD, multi2di(td_endo$phy), type="discrete", model=mkTRD)
#plot(td_endo$phy, show.tip.label = FALSE)
#tiplabels(pch=21, bg=td_endo$dat$TRD, col=td_endo$dat$TRD)
#nodelabels(pie = mkTRD_asr$lik.anc, piecol=1:5, cex=0.35)
#legend(0, 800, legend = c("TRD", "TRN", "TNN", "TND", "Aq"), pch=22, pt.bg=1:5)

.tree$node.label <- mkTRD_asr$lik.anc.states
plot(td_endo$phy, show.tip.label = FALSE)
tiplabels(pch=21, bg=td_endo$dat$TRD, col=td_endo$dat$TRD)
nodelabels(pch=21, bg = mkTRD_asr$lik.anc.states)
legend(0, 800, legend = c("TRD", "TRN", "TNN", "TND", "Aq"), pch=22, pt.bg=1:5)

tree_endo_Terr <-  td_endo$phy


tree_endo_Terr$node.label <- mkTRD_asr$lik.anc.states
modelTRD <- slouch.fit(phy = tree_endo_Terr, 
                       species = tree_endo_Terr$tip.label,
                       response = td_endo[["Tb"]],
                       mv.response = ME_endo_replace,
                       random.cov = td_endo[c("precipmm","tempC")], 
                       fixed.fact = as.integer(td_endo[['TRD']]))
modelTRD
summary(modelTRD1)



#saveRDS(modelTRD, file = "modelTRD.RDS")
modelTRD1 <- readRDS("modelTRD.RDS")

View(td_endo$dat$Rumi==1)

View(td_endo)
length(which(td_endo$dat$Rumi==1))
#89

length(which(td_endo$dat$Rumi==0))
#724

