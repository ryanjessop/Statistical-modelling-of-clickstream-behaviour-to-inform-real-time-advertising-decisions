# 1st order MC
predict_first_order_mc(first_mc, unseen_new_obvs, conversion_state = 'Y')
predict_first_order_mc(first_mc, seen_obv, conversion_state = 'Y')
# 2nd order MC
predict_second_order_mc(smooth_second_mc, unseen_new_obvs, conversion_state = 'Y')
predict_second_order_mc(smooth_second_mc, seen_obv, conversion_state = 'Y')
# HMM 
predict_hmm(recursive_gold_hmm_5, unseen_new_obvs, conversion_state = 'Y')
predict_hmm(recursive_gold_hmm_5, seen_obv, conversion_state = 'Y')

predict_hmm_state(recursive_gold_hmm_5, unseen_new_obvs, conversion_state='D')
predict_hmm_state_general(recursive_gold_hmm_5, unseen_new_obvs, conversion_observed_state='Y')
# GLM
predict_glm(fit, unseen_new_glm_obvs_start, unseen_new_glm_obvs_end, unseen_new_obvs)
predict_glm(fit, unseen_new_glm_obvs_start, unseen_new_glm_obvs_end, seen_obv)

#--------------------------------------------------------------------------------------
# Plot predictions 
# comparison_graphic(new_observation=unseen_new_obvs,
#                    first_mc=first_mc,
#                    second_mc=smooth_second_mc,
#                    hmm=recursive_gold_hmm_5,
#                    conversion_state='Y') %>% 
#   add_trace(y=~as.numeric(predict_hmm(gold_hmm_4, unseen_new_obvs, 'Y')),
#             mode="lines+text",
#             text=~viterbi(gold_hmm_4$hmm, unseen_new_obvs),
#             textfont = list(color = 'purple', size = 16),
#             name="4 state HMM") %>% 
#   add_trace(y=~as.numeric(predict_hmm(gold_hmm_5, unseen_new_obvs, 'Y')),
#             mode="lines+text",
#             text=~viterbi(gold_hmm_5$hmm, unseen_new_obvs),
#             textfont = list(color = 'brown', size = 16),
#             name="5 state HMM (Half sample)") %>% 
#   add_trace(y=~as.numeric(predict_hmm(gold_hmm_6, unseen_new_obvs, 'Y')),
#             mode="lines+text",
#             text=~viterbi(gold_hmm_6$hmm, unseen_new_obvs),
#             textfont = list(color = 'pink', size = 16),
#             name="6 state HMM")

comparison_graphic_v2 <- function(new_observation,
                                  first_mc,
                                  second_mc,
                                  hmms,
                                  hmm_names,
                                  glm_new_data,
                                  glm_model,
                                  conversion_observed_state){
  
  #legendtitle <- list(yref='paper', xref="paper", 
  #                    y=1.03, x=1.1, 
  #                    text="Types of model", showarrow=F)
  
  predictions <- data.frame(
    new_observation = new_observation,
    page_visit_id = seq(1,length(new_observation)),
    first_order = 
      as.numeric(
        predict_first_order_mc(first_mc,
                               new_observation, 
                               conversion_observed_state)), 
    second_order = 
      as.numeric(
        predict_second_order_mc(second_mc,
                                new_observation,
                                conversion_observed_state))
  )
  
  p <- plot_ly(data=predictions, 
               x=predictions$page_visit_id,
               y=~first_order,
               type="scatter", 
               mode="lines",
               name="First order MC") %>%
    add_trace(
      x=predictions$page_visit_id,
      y=~second_order,
      type='scatter',
      mode="lines",
      name="Two-step MC")
  
  for(i in 1:length(hmms)){
    df <- data.frame(
      hmm_predict <- as.numeric(predict_hmm(hmms[[i]],
                                            new_observation,
                                            conversion_observed_state)),
      hmm_hidden <- viterbi(hmms[[i]]$hmm, new_observation),
      hmm_name <- hmm_names[[i]]
      
    )
    p <- add_trace(p,
                   y=hmm_predict,
                   data=df,
                   type='scatter',
                   mode="lines+text",
                   text=hmm_hidden,
                   name=hmm_name,
                   textfont = list(size = 20)
    )
  }
  
  glm_predictions <- predict_glm(glm_model, 
                                 glm_new_data[[1]],
                                 glm_new_data[[2]],
                                 new_observation)
  
  glm_predictions[1] <- 0
  
  p <- add_trace(p,
                 y=glm_predictions,
                 mode="lines",
                 #marker=list(size=10),
                 name="Logistic Regression",
                 connectgaps = TRUE)
  l <- list(
    font = list(size = 10)
    )
  
  p %>% layout(xaxis=list(title="Page visits",
                          ticktext = predictions$new_observation,
                          tickvals = predictions$page_visit_id,
                          tickmode = "array", 
                          titlefont=list(size=20), 
                          tickfont=list(size=20)),
               yaxis=list(title="Transition probability from current to conversion state", 
                          titlefont=list(size=16), 
                          tickfont=list(size=20)),
               legend = l
               #annotations=legendtitle
               )
}


obv_test
unseen_new_obvs
seen_obv


names(step_model$coefficients)


comparison_graphic_hidden_states <- function(new_observation,
                                             hmms,
                                             hmm_names,
                                             conversion_observed_state){
  
  #legendtitle <- list(yref='paper', xref="paper", 
  #                    y=1.03, x=1.12, 
  #                    text="Types of Markov model", showarrow=F)
  
  predictions <- data.frame(
    new_observation = new_observation,
    page_visit_id = seq(1,length(new_observation))
  )
  
  p <- plot_ly()
  
  for(i in 1:length(hmms)){
    df <- data.frame(
      hmm_predict <- as.numeric(predict_hmm_state_general(hmms[[i]],
                                                          new_observation,
                                                          conversion_observed_state)),
      hmm_hidden <- viterbi(hmms[[i]]$hmm, new_observation),
      hmm_name <- hmm_names[[i]]
    )
    p <- add_trace(p,
                   y=hmm_predict,
                   data=df,
                   type='scatter',
                   mode="lines+text",
                   text=hmm_hidden,
                   name=hmm_name,
                   textfont = list(size = 20)
    )
  }
  
  l <- list(
    font = list(size = 10)
  )
  
  p %>% layout(xaxis=list(title="Page visits",
                          ticktext = predictions$new_observation, 
                          tickvals = c(predictions$page_visit_id) - 1,
                          tickmode = "array", 
                          titlefont=list(size=20), 
                          tickfont=list(size=20)),
               yaxis=list(title="Transition probability to any conversion hidden state", 
                          titlefont=list(size=16), 
                          tickfont=list(size=20)),
               legend = l
               #annotations=legendtitle
               )
}



fig = comparison_graphic_v2(new_observation=unseen_new_obvs,
                      first_mc=first_mc,
                      second_mc=smooth_second_mc,
                      hmms=list(gold_hmm_4, recursive_gold_hmm_5, gold_hmm_6),
                      hmm_names=list('4-state HMM', '5-state HMM', '6-state HMM'),
                      glm_new_data=list(unseen_new_glm_obvs_start, unseen_new_glm_obvs_end),
                      glm_model=step_model,
                      conversion_observed_state='Y')

orca(fig, file = "images/real_seq_hidden_conv_predict_graph.pdf")

fig = comparison_graphic_hidden_states(new_observation=unseen_new_obvs,
                                 hmms=list(gold_hmm_4, recursive_gold_hmm_5, gold_hmm_6),
                                 hmm_names=list('4-state HMM', '5-state HMM', '6-state HMM'),
                                 conversion_observed_state='Y')

#---------------------------------------------------

fig = comparison_graphic_v2(new_observation=seen_obvs,
                      first_mc=first_mc,
                      second_mc=smooth_second_mc,
                      hmms=list(gold_hmm_4, recursive_gold_hmm_5, gold_hmm_6),
                      hmm_names=list('4-state HMM', '5-state HMM', '6-state HMM'),
                      glm_new_data=list(seen_new_glm_obvs_start, seen_new_glm_obvs_end),
                      glm_model=step_model,
                      conversion_observed_state='Y')

fig = comparison_graphic_hidden_states(new_observation=seen_obvs,
                                 hmms=list(gold_hmm_4, recursive_gold_hmm_5, gold_hmm_6),
                                 hmm_names=list('4-state HMM', '5-state HMM', '6-state HMM'),
                                 conversion_observed_state='Y')
