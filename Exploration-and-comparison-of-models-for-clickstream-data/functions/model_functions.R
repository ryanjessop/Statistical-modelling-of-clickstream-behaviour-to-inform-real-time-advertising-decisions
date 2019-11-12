second_order_mc <- function(data){
  add_triad = data %>% 
    mutate(triad = lapply(browsing_str, 
                          function(browsing_str) {
                            substring(browsing_str, 
                                      first = 1:(nchar(browsing_str) - 2), 
                                      last = 3:nchar(browsing_str)
                            )})) 
  
  triad_probs = add_triad %>%
    unnest(triad) %>%
    group_by(triad) %>%
    summarize(total_counts = n()) %>%
    mutate(probs = total_counts / sum(total_counts))
  
  split_states = triad_probs %>% 
    mutate(split_triad = lapply(triad, 
                                function(triad) {
                                  list(substr(triad, 1, 2),
                                       substr(triad, 3, 3))
                                })) %>%
    mutate(previous_states = sapply(split_triad, "[[", 1), 
           next_state = sapply(split_triad, "[[", 2)) %>%
    select(previous_states, next_state, probs)
  
  matrix = spread(split_states, next_state, probs)
  
  RS <- rowSums(matrix[2:ncol(matrix)], na.rm=TRUE)
  
  normalised_probs <- matrix %>%
    mutate_if(is.numeric, funs(. / RS)) # Soft depricated in 0.8 dplyr
  
  second_order_matrix <- data.matrix(normalised_probs[,2:ncol(normalised_probs)])
  rownames(second_order_matrix) <- normalised_probs$previous_states
  return(second_order_matrix)
}

second_order_mc_joint <- function(data){
  add_triad = data %>% 
    mutate(triad = lapply(browsing_str, 
                          function(browsing_str) {
                            substring(browsing_str, 
                                      first = 1:(nchar(browsing_str) - 2), 
                                      last = 3:nchar(browsing_str)
                            )})) 
  
  triad_probs = add_triad %>%
    unnest(triad) %>%
    group_by(triad) %>%
    summarize(total_counts = n()) %>%
    mutate(probs = total_counts / sum(total_counts))
  
  split_states = triad_probs %>% 
    mutate(split_triad = lapply(triad, 
                                function(triad) {
                                  list(substr(triad, 1, 2),
                                       substr(triad, 3, 3))
                                })) %>%
    mutate(previous_states = sapply(split_triad, "[[", 1), 
           next_state = sapply(split_triad, "[[", 2)) %>%
    select(previous_states, next_state, probs)
  
  matrix = spread(split_states, next_state, probs)
  
  second_order_matrix <- data.matrix(matrix[,2:ncol(matrix)])
  rownames(second_order_matrix) <- matrix$previous_states
  return(second_order_matrix)
}

train_hmm <- function(num_hidden_states, 
                      observable_states=c("H", "S", "I", "L", "C", "X", "R", "E"), 
                      initial_transition=matrix(runif(num_hidden_states*num_hidden_states), 
                                                nrow=num_hidden_states) + runif(1,1,2)*diag(num_hidden_states), 
                      init_emission=matrix(runif(num_hidden_states*length(observable_states)), 
                                           nrow=num_hidden_states), 
                      observations=clickstreams,
                      training_method=viterbiTraining,
                      start_sample_size=0.1,
                      end_sample_size=0.9,
                      replace_threshold=0,
                      replace_value=0){
  initial_transition <- replace(initial_transition, initial_transition < replace_threshold, replace_value)
  init_emission <- replace(init_emission, init_emission < replace_threshold, replace_value)
  initial_transition <- sweep_matrix(initial_transition)
  init_emission <- sweep_matrix(init_emission)
  observations <- observations[
    (which(observations == 'E')[round(length(which(observations == 'E'))*start_sample_size)] + 1):
      which(observations == 'E')[round(length(which(observations == 'E'))*end_sample_size)]]
  training_method(
    initHMM(States = LETTERS[1:num_hidden_states],
            Symbols = observable_states,
            transProbs=initial_transition,
            emissionProbs=init_emission), 
    observations)
}