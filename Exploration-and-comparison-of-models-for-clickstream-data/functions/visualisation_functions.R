plot_first_order_mc <- function(markov_chain){
  hmPlot(markov_chain, title="First order Markov Chain transition matrix")
}

plot_first_order_mc_v2 <- function(matrix){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.11, text="Transition Probability",showarrow=F)
  
  plot_ly(
    x = colnames(matrix),
    y = colnames(matrix),
    z = matrix, 
    colors = palette(100),
    type = "heatmap",
    zauto = FALSE, zmin = 0, zmax = max(matrix, na.rm = TRUE)) %>%
    layout(xaxis = list(title="To"),
           yaxis = list(title="From"),
           title = "First order Markov chain transition matrix",
           annotations=legendtitle)
}

plot_network_first_order_mc_v3 <- function(markov_chain,
                                           lower_bound=0.001,
                                           state_hits=c(127,830,557,9800,4749,862,620,65,6323,998,66,572,634,235),
                                           node_size_factor=0.06,
                                           edge_width_factor=10,
                                           edge_arrow_size=.8,
                                           edge_curved=.6,
                                           layout_size=1.2,
                                           legend_x=-2.0,
                                           legend_y=-0.5){
  
  transitions_df <- as.data.frame(t(markov_chain@transitions$`1`))
  previous_state <- rownames(transitions_df)
  df <- cbind(previous_state, transitions_df)
  tidy_df <- gather(df, 
                    c(rownames(transitions_df)),
                    key='next_state', 
                    value='transition_probability') %>% 
    filter(transition_probability > lower_bound) %>% 
    mutate(mapping_values = round(transition_probability, 2))
  
  color_map <- data_frame(
    greyscale_color = gray.colors(81, start=0, end=0, alpha=seq(0,0.80,0.01)), 
    mapping_values = seq(0,0.80,0.01))
  
  edges <- inner_join(tidy_df, color_map, b="mapping_values")
  
  nodes <- data_frame(
    previous_state = previous_state,
    hit_counts = state_hits,
    state_types = factor(c("Content", "Purchasing", "Content", "Products", "Artificial", 
                           "Home", "Site Information", "Profile", "Products", "Artificial",
                           "Products", "Profile", "Purchasing", "Purchasing")))
  
  net <- graph_from_data_frame(d=edges, 
                               vertices=nodes)
  
  V(net)$size <- log(V(net)$hit_counts)*node_size_factor
  E(net)$width <- E(net)$transition_probability*edge_width_factor
  V(net)$color <- c("green", "orange", "green", "gold", "gray50", 
                    "tomato", "white", "light blue", "gold", "gray50", 
                    "gold", "light blue", "orange", "orange")
  colrs <- c("gray50", "green", "tomato", "gold", "light blue", "orange", "white")
  
  l <- layout_with_fr(net)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  plot(net, 
       edge.arrow.size=edge_arrow_size, 
       edge.curved=edge_curved,
       edge.color=E(net)$greyscale_color,
       rescale=F, 
       layout=l*layout_size)
  
  legend(x=legend_x, 
         y=legend_y, 
         levels(nodes$state_types), 
         pch=21,
         col="#777777", 
         pt.bg=colrs, 
         pt.cex=2, 
         cex=.8, 
         bty="n", 
         ncol=1)
}

plot_second_order_mc <- function(matrix){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.09, text="Transition Probability",showarrow=F)
  
  plot_ly(
    x = colnames(matrix),
    y = rownames(matrix),
    z = matrix, 
    type = "heatmap") %>%
    layout(xaxis = list(title="Next state"),
           yaxis = list(title="Previous/Current State"),
           title = "Two-step Markov chain transition matrix with smoothing",
           annotations=legendtitle)
}

smooth_transition_matrix <- function(transition_matrix){
  transition_matrix[is.na(transition_matrix)] <- 0
  add_noise <- transition_matrix + runif(ncol(transition_matrix)*nrow(transition_matrix),0,0.005)
  smooth_transition_matrix <- sweep_row_matrix(add_noise)
  return(smooth_transition_matrix)
}

zoom_second_order_matrix <- function(transition_matrix=second_mc,
                                     target_state='X'){
  target_column = data.frame(
    states=colsplit(rownames(transition_matrix), "", names = c("first", "second")),
    probs=as.numeric(transition_matrix[,target_state]))
  matrix = spread(target_column, states.second, probs)
  result = data.matrix(matrix[,2:ncol(matrix)])
  return(result)
}

plot_second_order_mc_zoom <- function(matrix){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.09, text="Transition Probability to Y",showarrow=F)
  
  plot_ly(
    x = colnames(matrix),
    y = colnames(matrix),
    z = matrix, 
    type = "heatmap") %>%
    layout(xaxis = list(title="Current State"),
           yaxis = list(title="Previous State"),
           title = "Two-step Markov chain transition matrix",
           annotations=legendtitle)
}

plot_hmm <- function(hmm, title_header='HMM'){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.08, text="Emission probability",showarrow=F)
  
  plot_ly(x = hmm$hmm$Symbols, 
          y = hmm$hmm$States,
          z = hmm$hmm$emissionProbs, 
          colors = palette(100),
          type = "heatmap",
          zauto = FALSE, zmin = 0, zmax = max(hmm$hmm$emissionProbs, na.rm = TRUE)) %>%
    layout(xaxis = list(title='Observed States'),
           yaxis = list(title='Hidden States'),
           title = title_header,
           annotations=legendtitle)
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

plot_hidden_state_dist <- function(hmm,
                                   new_observation){
   forward_probs <- sweep_matrix_cols(exp(forward(hmm$hmm, new_observation)))
   
   predictions <- data.frame(
     new_observation = new_observation,
     page_visit_id = seq(0,length(new_observation)-1))
   
   legendtitle <- list(yref='paper', xref="paper",
                       y=1.03, x=1.09, 
                       text="Forward probability", showarrow=F)
   plot_ly(
     y = rownames(forward_probs),
     z = forward_probs, 
     type = "heatmap",
     colors = "YlOrRd") %>%
     layout(xaxis = list(title="Observed state",
                         ticktext = predictions$new_observation, 
                         tickvals = predictions$page_visit_id,
                         tickmode = "array"),
            yaxis = list(title="Hidden state"),
            title = "Distribution of hidden states",
            annotations=legendtitle)
}

comparison_graphic <- function(new_observation,
                               first_mc,
                               second_mc,
                               hmm,
                               glm_start,
                               glm_end,
                               glm_model,
                               conversion_state='X'){
  page_visit_id <- seq(1,length(new_observation))
  
  predictions <- data.frame(
    first_order = as.numeric(predict_first_order_mc(first_mc, 
                                                    new_observation, 
                                                    conversion_state = conversion_state)), 
    second_order = as.numeric(predict_second_order_mc(second_mc, 
                                                      new_observation,
                                                      conversion_state = conversion_state)),
    hidden_states = viterbi(hmm$hmm, new_observation), 
    hmm_predictions = as.numeric(predict_hmm(hmm, 
                                             new_observation,
                                             conversion_state = conversion_state)),
    new_observation = new_observation,
    zeros = rep(-0.01, length(new_observation)),
    page_visit_id = page_visit_id)
  
  #predictions[is.na(predictions)] <- 0
  
  #glm_predictions <- predict_glm(glm_model, 
  #                               glm_start, 
  #                               glm_end, 
  #                               new_observation)
  
  #predictions = cbind(predictions, glm_predictions)
  
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.2, text="Types of Markov model",showarrow=F)
  
  plot_ly(data=predictions, 
          x=~page_visit_id,
          y=~first_order,
          type="scatter", 
          mode="lines",
          name="First order MC") %>%
    add_trace(y=~second_order, 
              mode="lines",
              name="Two-step MC") %>% 
    add_trace(y=~zeros,
              mode="text",
              text=~new_observation,
              textfont = list(color = '#000000', size = 16),
              name="Observed page types") %>%
    add_trace(y=~hmm_predictions,
              mode="lines+text",
              text=~hidden_states,
              textfont = list(color = 'rgb(205, 12, 24)', size = 16),
              name="5 state recursive HMM") %>%
    #    add_trace(y=~glm_predictions,
    #              mode="lines+markers",
    #              marker=list(size=10),
    #              name="GLM",
    #              connectgaps = TRUE) %>%
    layout(xaxis=list(title="Page visits"),
           yaxis=list(title="Transition probability from current state to conversion state Y"),
           title="Predicting a successful conversion with Markov models",
           annotations=legendtitle)
}

comparison_graphic_v2 <- function(new_observation,
                                  first_mc,
                                  second_mc,
                                  hmms,
                                  hmm_names,
                                  glm_new_data,
                                  glm_model,
                                  conversion_observed_state){
  
  legendtitle <- list(yref='paper', xref="paper", 
                      y=1.03, x=1.1, 
                      text="Types of model", showarrow=F)
  
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
                   name=hmm_name
                   )
  }
  
  glm_predictions <- predict_glm(glm_model, 
                                 glm_new_data[[1]],
                                 glm_new_data[[2]],
                                 new_observation)

  glm_predictions[1] <- 0
  
  p <- add_trace(p,
                 y=glm_predictions,
                 mode="lines+markers",
                 marker=list(size=10),
                 name="GLM",
                 connectgaps = TRUE)

  p %>% layout(xaxis=list(title="Page visits",
                          ticktext = predictions$new_observation,
                          tickvals = predictions$page_visit_id,
                          tickmode = "array"),
               yaxis=list(title="Transition probability from current state to conversion state Y"),
               title="Predicting a successful conversion with clickstream models",
               annotations=legendtitle)
}

comparison_graphic_hidden_states <- function(new_observation,
                                             hmms,
                                             hmm_names,
                                             conversion_observed_state){
  
  legendtitle <- list(yref='paper', xref="paper", 
                      y=1.03, x=1.12, 
                      text="Types of Markov model", showarrow=F)
  
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
                   name=hmm_name
    )
  }
  
  p %>% layout(xaxis=list(title="Page visits",
                          ticktext = predictions$new_observation, 
                          tickvals = c(predictions$page_visit_id) - 1,
                          tickmode = "array"),
               yaxis=list(title="Transition probability to any 'conversion' hidden state"),
               title="Predicting a successful conversion with HMMs",
               annotations=legendtitle)
}
