predict_first_order_mc <- function(markov_chain, 
                                   new_observation, 
                                   conversion_state='X'){
  first_order_conv_probs <- numeric()
  for (i in 1:length(new_observation)){
    first_order_conv_probs[i] <- markov_chain@transitions$`1`[conversion_state, new_observation[i]]
  }
  return(first_order_conv_probs)
}

predict_second_order_mc <- function(markov_chain, 
                                    new_observation, 
                                    conversion_state='X'){
  second_order_conv_probs <- numeric()
  for (i in 2:length(new_observation)){
    second_order_conv_probs[i] <- markov_chain[paste(new_observation[i-1], new_observation[i], sep=""), conversion_state]
  }
  return(second_order_conv_probs)
}

predict_hmm <- function(hmm, 
                        new_observation, 
                        conversion_state='X'){
  hmm_predictions <- numeric()
  for (i in 1:length(new_observation)){
    hmm_predictions[i] <- sum(sweep_matrix_cols(exp(forward(hmm$hmm, new_observation)))[,i]*hmm$hmm$emissionProbs[,conversion_state])
  }
  return(hmm_predictions)
}

predict_hmm_state <- function(hmm, 
                              new_observation, 
                              conversion_state){
  hmm_predictions <- numeric()
  for (i in 1:length(new_observation)){
    hmm_predictions[i] <- sweep_matrix_cols(exp(forward(hmm$hmm, new_observation)))[conversion_state,i]
  }
  return(hmm_predictions)
}

predict_hmm_state_general <- function(hmm, 
                                      new_observation, 
                                      conversion_observed_state='Y'){
  
  conversion_hidden_states <- rownames(hmm$hmm$emissionProbs)[which(hmm$hmm$emissionProbs[,conversion_observed_state]!=0)]
  
  hmm_predictions <- numeric()
  for (i in 1:length(new_observation)){
    hmm_predictions[i] <- sum(sweep_matrix_cols(exp(forward(hmm$hmm, new_observation)))[conversion_hidden_states,i])
  }
  return(hmm_predictions)
}

predict_glm <- function(model,
                        start_data, 
                        end_data,
                        new_obs_sequence){
  glm_prediction_val_start = predict(model, newdata = start_data, type="response")
  glm_prediction_val_end = predict(model, newdata = end_data, type="response")
  glm_prediction <- c(glm_prediction_val_start, 
                      rep(NA, length(new_obs_sequence) - 2), 
                      glm_prediction_val_end)
  return(glm_prediction)
}

filter_audience_by_state <- function(initial_audience,
                                     hmm,
                                     hidden_state_target,
                                     hidden_state_count_target=1){
  filtered_pids = initial_audience %>% 
    mutate(browsing_list=str_split(browsing_str, "")) %>% 
    unnest(browsing_list) %>%
    mutate(hidden_state=viterbi(hmm$hmm, browsing_list)) %>%
    group_by(pid, hidden_state) %>%
    summarise(hidden_state_count=n()) %>%
    filter(hidden_state == hidden_state_target & hidden_state_count > hidden_state_count_target) %>%
    distinct(pid)
  return(filtered_pids)
}

hidden_state_dist <- function(hmm, 
                              new_observation){
    return(sweep_matrix_cols(exp(forward(hmm$hmm, new_observation))))
}

