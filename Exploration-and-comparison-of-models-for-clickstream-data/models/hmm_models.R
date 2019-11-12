# Hidden Markov Model run

palette <- colorRampPalette(c("white", "yellow", "red", "dark red"))

clickstream_df <- read_csv("data/hidden-markov-data/cleanshark-hmm.csv", col_names = FALSE)
clickstreams <- strsplit(clickstream_df$X1, split=c())[[1]]

gold_hmm_3 = train_hmm(
  num_hidden_states=3, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.1,
  end_sample_size=0.8)
plot_hmm(gold_hmm_3, title_header = '3 hidden state HMM')
plot_hmm_logs(gold_hmm_3, title_header = '3 hidden state HMM')
plot_matrix_logs(gold_hmm_3,
                 gold_hmm_3$hmm$transProbs, 
                 title_header = '3 hidden state HMM - Transition matrix',
                 legend_text = 'Transition probability (log)')

gold_hmm_4 = train_hmm(
  num_hidden_states=4, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.1,
  end_sample_size=0.9)
plot_hmm(gold_hmm_4, title_header = '4 hidden state HMM')
plot_hmm_logs(gold_hmm_4, title_header = '4 hidden state HMM')
plot_matrix_logs(gold_hmm_4,
                 gold_hmm_4$hmm$transProbs, 
                 title_header = '4 hidden state HMM - Transition matrix',
                 legend_text = 'Transition probability (log)')

recursive_gold_hmm_5 = train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  initial_transition=gold_hmm_5$hmm$transProbs,
  init_emission=gold_hmm_5$hmm$emissionProbs,
  start_sample_size=0.7,
  end_sample_size=0.9)
plot_hmm(recursive_gold_hmm_5, title_header = '5 hidden state HMM')
plot_hmm_logs(recursive_gold_hmm_5, title_header = '5 hidden state HMM')
plot_matrix_logs(recursive_gold_hmm_5,
                 recursive_gold_hmm_5$hmm$transProbs, 
                 title_header = '5 hidden state HMM - Transition matrix',
                 legend_text = 'Transition probability (log)')

gold_hmm_6 = train_hmm(
  num_hidden_states=6, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.2,
  end_sample_size=0.8)
plot_hmm(gold_hmm_6, title_header = '6 hidden state HMM')
plot_hmm_logs(gold_hmm_6, title_header = '6 hidden state HMM')
plot_matrix_logs(gold_hmm_6,
                 gold_hmm_6$hmm$transProbs, 
                 title_header = '6 hidden state HMM - Transition matrix',
                 legend_text = 'Transition probability (log)')

# Proportion of time spent in each hidden state
(table(viterbi(gold_hmm_6$hmm, clickstreams)) / length(clickstreams)) * 100

# Likelihoods
observations <- clickstreams[
  (which(clickstreams == 'E')[round(length(which(clickstreams == 'E'))*0.25)] + 1):
    which(clickstreams == 'E')[round(length(which(clickstreams == 'E'))*0.75)]]
likelihood_hmm(gold_hmm_6$hmm, observations)

