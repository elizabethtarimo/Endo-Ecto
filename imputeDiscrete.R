library(geiger)
#library(treeplyr)
library(castor)
#library(Rphylopars)

impute_discrete <- function(tree, tip_states){
  
  get_simmap_tips <- function(simmap){
    ntips <- length(simmap$tip.label)
    tip_index <- match(1:ntips, simmap$edge[,2])
    state <- sapply(simmap$maps[tip_index], function(x) names(x)[length(x)])
    state <- setNames(state, simmap$tip.label)
    return(state)
  }
  #tip_states <- td$dat$ecomorph
  nstates <- length(na.omit(unique(tip_states)))
  Ntips <- length(tree$tip.label)
  # Make a matrix of tip priors
  tippriors <- matrix(0, ncol=nstates, nrow=Ntips)
  for(i in 1:nrow(tippriors)){
    if(is.na(tip_states[i])){
      tippriors[i,] <- 1/nstates
    } else {
      tippriors[i, tip_states[i]+1] <- 1.0
    }
  }  
  rowSums(tippriors) #should all be 1
  rownames(tippriors) <- tree$tip.label
  
  
  asr1 <- castor::hsp_mk_model(tree, tip_states=NULL, tip_priors=tippriors, Nstates=nstates, rate_model="ARD")
  .QQ <- asr1$transition_matrix
  rownames(.QQ) <- colnames(.QQ) <- colnames(tippriors) <-  c("0", "1")
  simmaps <- make.simmap(tree, tippriors, model = "ARD", Q=.QQ, nsim=20)
  smap_recons <- sapply(simmaps, function(x) as.numeric(get_simmap_tips(x)))
  asr_tips <- apply(smap_recons, 1, function(x) round(mean(x),0))
  
  #lik_max <- apply(asr2$ancestral_likelihoods, 1, function(x) which(x==max(x))[1])
  #asr_tips <- lik_max[1:Ntips]
  #asr_nodes <- lik_max[(Ntips+1):length(lik_max)]
  
  #tree$node.label <- asr_nodes
  
  #plot(tree, show.tip.label=FALSE)
  #tiplabels(pch=21, bg=asr_tips)
  #nodelabels(pch=21, bg=asr_nodes)
  return(asr_tips)
}
#impute_discrete(tree, tip_states)