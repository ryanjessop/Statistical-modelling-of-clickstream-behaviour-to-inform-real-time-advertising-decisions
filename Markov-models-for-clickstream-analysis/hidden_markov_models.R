# Hidden Markov Model functions

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

sweep_matrix <- function(x){
  sweep(x = x, 
        MARGIN = 1,
        STATS = rowSums(x),
        FUN = "/")
}

sweep_matrix_cols <- function(x){
  sweep(x = x, 
        MARGIN = 2,
        STATS = colSums(x),
        FUN = "/")
}

plot_hmm <- function(hmm){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.08, text="Emission probability",showarrow=F)
  
  plot_ly(x = hmm$hmm$Symbols, 
          y = hmm$hmm$States,
          z = hmm$hmm$emissionProbs, 
          colors = palette(100),
          type = "heatmap",
          zauto = FALSE, zmin = 0, zmax = max(hmm$hmm$emissionProbs, na.rm = TRUE)) %>%
    layout(xaxis = list(title='Observed States'),
           yaxis = list(title='Hidden States'),
           title = "5 hidden state Markov model",
           annotations=legendtitle)
}

plot_hmm_logs <- function(hmm){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.08, text="Emission Probability (log)",showarrow=F)
  
  plot_ly(x = hmm$hmm$Symbols, 
          y = hmm$hmm$States,
          z = log10(hmm$hmm$emissionProbs) %>% rationalize(), 
          colors = palette(100),
          type = "heatmap",
          zauto = FALSE, zmin = min(log10(hmm$hmm$emissionProbs) %>% rationalize(), zmax = 0, na.rm = TRUE)) %>%
    layout(xaxis = list(title='Observed States'),
           yaxis = list(title='Hidden States'),
           title = "5 hidden state Markov model",
           annotations=legendtitle)
}


# Hidden Markov Model run

clickstream_df <- read_csv("hidden-markov-data/cleanshark-hmm.csv", col_names = FALSE)
clickstreams <- strsplit(clickstream_df$X1, split=c())[[1]]

gold_hmm_3 = train_hmm(
  num_hidden_states=3, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.1,
  end_sample_size=0.8)
plot_hmm(gold_hmm_3)

gold_hmm_4 = train_hmm(
  num_hidden_states=4, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.1,
  end_sample_size=0.9)
plot_hmm(gold_hmm_4)

gold_hmm_5 = train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.1,
  end_sample_size=0.55)
plot_hmm(gold_hmm_5)

recursive_gold_hmm_5 = train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  initial_transition=gold_hmm_5$hmm$transProbs,
  init_emission=gold_hmm_5$hmm$emissionProbs,
  start_sample_size=0.7,
  end_sample_size=0.9)
plot_hmm(recursive_gold_hmm_5)
plot_hmm_logs(recursive_gold_hmm_5)

full_gold_hmm_5 = train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.05,
  end_sample_size=0.95)
plot_hmm(full_gold_hmm_5)

gold_hmm_6 = train_hmm(
  num_hidden_states=6, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.2,
  end_sample_size=0.8)
plot_hmm(gold_hmm_6)


(table(viterbi(recursive_gold_hmm_5$hmm, clickstreams)) / length(clickstreams)) * 100
