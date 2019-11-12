# Simulation functions
simulation_generation <- function(trans,N){
  transition <- function(char,trans){
    sample(colnames(trans),1,prob=trans[char,])
  }
  
  sim <- character(N)
  sim[1] <- sample(colnames(trans),1)
  for (i in 2:N){
    sim[i] <- transition(sim[i-1],trans)
  }
  return(sim)
}

simulate_observed_states <- function(hmm, 
                                     hidden_states){
  observed_path <- character()
  for(i in 1:length(hidden_states)){
    observed_path[i] <- hmm$hmm$Symbols[sample(length(hmm$hmm$Symbols), 
                                               1, 
                                               prob = as.numeric(hmm$hmm$emissionProbs[hidden_states[i],]))]
  }
  return(observed_path)
}
