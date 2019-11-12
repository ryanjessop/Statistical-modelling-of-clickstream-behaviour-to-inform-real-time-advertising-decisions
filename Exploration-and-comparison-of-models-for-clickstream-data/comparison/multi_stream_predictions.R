# Multi stream predictions

# Start to split clickstreams up

# multi_obvs_test
# paste(multi_obvs_test,collapse="")
# str_split(paste(multi_obvs_test,collapse=""), 'E')
# aaa = str_split(paste(multi_obvs_test,collapse=""), 'E')[[1]]
# 
# for (i in 2:length(aaa)-1){
#   print(str_split(aaa[i], '')[[1]])
#   print(predict_hmm(recursive_gold_hmm_5, str_split(aaa[i], '')[[1]], conversion_state = 'Y'))
#   print(which(str_split(aaa[i], '')[[1]] == 'Y'))
# }

#--------------------------------------------------------
# Look at simulation picture

sampling_hidden_states <- simulation_generation(trans=gold_hmm_5$hmm$transProbs,
                                                N=100) 

sample_observed_states <- simulate_observed_states(hmm=gold_hmm_5, 
                                                   hidden_states=sampling_hidden_states)


viterbi(gold_hmm_5$hmm, sample_observed_states)

# Find difference between 'conversion' hidden or observed states from simualtion to predictions from model