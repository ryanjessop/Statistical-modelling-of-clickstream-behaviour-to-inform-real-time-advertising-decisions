# Distribution of hidden states probabilities 

# One model, one seq, forward probs distribution at each obs

hidden_state_dist(gold_hmm_5, unseen_new_obvs)


plot_hidden_state_dist <- function(hmm,
                                   new_observation){
  forward_probs <- sweep_matrix_cols(exp(forward(hmm$hmm, new_observation)))
  
  predictions <- data.frame(
    new_observation = new_observation,
    page_visit_id = seq(0,length(new_observation)-1))
  
  #legendtitle <- list(yref='paper', xref="paper",
  #                    y=1.03, x=1.09, 
  #                    text="Forward probability", showarrow=F)
  
  plot_ly(
    y = rownames(forward_probs),
    z = forward_probs, 
    type = "heatmap",
    colors = palette(100),
    colorbar=list(title='Forward \n Probability', 
                  titlefont=list(size=20), 
                  tickfont=list(size=20))) %>%
    layout(xaxis = list(title="Observed state",
                        ticktext = predictions$new_observation, 
                        tickvals = predictions$page_visit_id,
                        tickmode = "array", 
                        titlefont=list(size=30), 
                        tickfont=list(size=20)),
           yaxis = list(title="Hidden state", 
                        titlefont=list(size=30), 
                        tickfont=list(size=20)))
  
}



fig = plot_hidden_state_dist(gold_hmm_4, unseen_new_obvs)
fig = plot_hidden_state_dist(recursive_gold_hmm_5, unseen_new_obvs)
fig = plot_hidden_state_dist(gold_hmm_6, unseen_new_obvs)

orca(fig, file = "images/real_seq_5_state_dist.pdf")

#--------------------------------

fig = plot_hidden_state_dist(gold_hmm_4, seen_obvs)
fig = plot_hidden_state_dist(recursive_gold_hmm_5, seen_obvs)
fig = plot_hidden_state_dist(gold_hmm_6, seen_obvs)
