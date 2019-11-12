# Initial values 

# Change function to produced initial values
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
  hmm <- training_method(
    initHMM(States = LETTERS[1:num_hidden_states],
            Symbols = observable_states,
            transProbs=initial_transition,
            emissionProbs=init_emission), 
    observations)
  return(list('hmm' = hmm, 'init_trans' = initial_transition, 'init_emis' = init_emission))
}

plot_hmm_logs <- function(hmm, title_header='HMM'){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.08, text="Emission Probability (log)",showarrow=F)
  
  plot_ly(x = hmm$hmm$Symbols, 
          y = hmm$hmm$States,
          z = log10(hmm$hmm$emissionProbs) %>% rationalize(), 
          colors = palette(100),
          type = "heatmap",
          zauto = FALSE, zmin = min(log10(hmm$hmm$emissionProbs) %>% rationalize(), zmax = 0, na.rm = TRUE)) %>%
    layout(xaxis = list(title='Observed States'),
           yaxis = list(title='Hidden States'),
           title = title_header,
           annotations=legendtitle)
}

plot_matrix_logs <- function(hmm, matrix, title_header='HMM', legend_text = 'Emission Probability (log)'){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.08, text=legend_text, showarrow=F)
  
  plot_ly(x = hmm$hmm$Symbols, 
          y = hmm$hmm$States,
          z = log10(matrix) %>% rationalize(), 
          colors = palette(100),
          type = "heatmap",
          zauto = FALSE, zmin = min(log10(matrix) %>% rationalize(), zmax = 0, na.rm = TRUE)) %>%
    layout(xaxis = list(title='Hidden States'),
           yaxis = list(title='Hidden States'),
           title = title_header,
           annotations=legendtitle)
}

#-----------------------------------------------------------------------------------------

gold_hmm_5 = train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.1,
  end_sample_size=0.55)
plot_hmm(gold_hmm_5, title_header = '5 hidden state HMM')
plot_hmm_logs(gold_hmm_5, title_header = '5 hidden state HMM')

full_gold_hmm_5 = train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.05,
  end_sample_size=0.95)
plot_hmm(full_gold_hmm_5, title_header = '5 hidden state HMM')
plot_hmm_logs(full_gold_hmm_5, title_header = '5 hidden state HMM')


plot_matrix_logs(gold_hmm_5, 
                 gold_hmm_5$hmm$transProbs, 
                 title_header = '5 hidden state HMM - Initial transition matrix',
                 legend_text = 'Transition probability (log)')
plot_matrix_logs(recursive_gold_hmm_5, 
                 recursive_gold_hmm_5$hmm$transProbs, 
                 title_header = '5 hidden state HMM - Transition matrix',
                 legend_text = 'Transition probability (log)')

plot_hmm_logs(gold_hmm_5,
              title_header = '5 hidden state HMM - Initial emission matrix')
plot_hmm_logs(recursive_gold_hmm_5, 
              title_header = '5 hidden state HMM - Emission matrix')


# Outputs:
# Likelihood
# Initial values

initial_val_hmm <- train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.5,
  end_sample_size=0.95,
  replace_threshold = 0.4,
  replace_value = 0)

plot_matrix_logs(initial_val_hmm$hmm, 
                 initial_val_hmm$init_trans, 
                 title_header = '5 hidden state HMM - Initial transition matrix',
                 legend_text = 'Transition probability (log)')
plot_matrix_logs(initial_val_hmm$hmm, 
                 initial_val_hmm$hmm$hmm$transProbs, 
                 title_header = '5 hidden state HMM - Transition matrix',
                 legend_text = 'Transition probability (log)')

plot_matrix_logs(initial_val_hmm$hmm, 
                 initial_val_hmm$init_emis, 
                 title_header = '5 hidden state HMM - Initial emission matrix')
plot_hmm_logs(initial_val_hmm$hmm, 
              title_header = '5 hidden state HMM - Emission matrix')


# Fails to train...
initial_val_hmm_ones <- train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.5,
  end_sample_size=0.95,
  replace_threshold = 0.9,
  replace_value = 1)

plot_hmm_logs(initial_val_hmm$hmm, title_header = '3 hidden state HMM')
plot_matrix_logs(initial_val_hmm$hmm, initial_val_hmm$init_trans, title_header = '3 hidden state HMM')
plot_matrix_logs(initial_val_hmm$hmm, initial_val_hmm$init_trans, title_header = '3 hidden state HMM')



#----------------------------------------------------------

initial_val_hmm <- train_hmm(
  num_hidden_states=5, 
  observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
  observations=clickstreams,
  start_sample_size=0.5,
  end_sample_size=0.95,
  replace_threshold = 0.4,
  replace_value = 0)


initial_val_hmm$init_trans
initial_val_hmm$hmm$hmm$transProbs

initial_val_hmm$init_emis
initial_val_hmm$hmm$hmm$emissionProbs

dist(initial_val_hmm$init_trans, initial_val_hmm$hmm$hmm$transProbs)
?dist
