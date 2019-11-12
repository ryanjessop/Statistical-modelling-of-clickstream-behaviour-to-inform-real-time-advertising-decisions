# Model selection functions 

generate_single_obvs <- function(data, seed_val=1){
  exits <- which(data == 'E')
  set.seed(seed_val); click_start <- sample(exits, 1)
  end <- which(exits == click_start) + 1
  click_end <- exits[end]
  return(data[(click_start + 1):(click_end-1)])
}

generate_multi_obvs <- function(data, sample_size=0.5){
  break_point <- length(data) / (1/sample_size)
  cut_data <- data[1:floor(break_point)]
  last_exit <- max(which(cut_data == 'E'))
  return(cut_data[1:(last_exit)])
}

likelihood_hmm <- function(hmm,
                           observation){
  final_col <- forward(hmm, observation)[,length(observation)] 
  return(sum(final_col*is.finite(final_col), na.rm = TRUE))
}