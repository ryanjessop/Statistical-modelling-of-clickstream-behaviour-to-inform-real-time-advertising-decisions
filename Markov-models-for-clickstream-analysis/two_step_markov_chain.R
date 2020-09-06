# Two-step Markov chain functions
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
    mutate_if(is.numeric, funs(. / RS))
  
  second_order_matrix <- data.matrix(normalised_probs[,2:ncol(normalised_probs)])
  rownames(second_order_matrix) <- normalised_probs$previous_states
  return(second_order_matrix)
}

plot_second_order_mc <- function(matrix){
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.11, text="Transition Probability",showarrow=F)
  
  plot_ly(
    x = colnames(matrix),
    y = rownames(matrix),
    z = matrix, 
    colors = palette(100),
    colorbar=list(title='Transition \n Probability', titlefont=list(size=20), tickfont=list(size=20)),
    type = "heatmap",
    zauto = FALSE, zmin = 0, zmax = max(matrix, na.rm = TRUE)) %>%
    layout(xaxis = list(title="Next state", titlefont=list(size=30), tickfont=list(size=20)),
           yaxis = list(title="Previous/Current State", titlefont=list(size=30)))
}

sweep_row_matrix <- function(x){
  sweep(x = x, 
        MARGIN = 1,
        STATS = rowSums(x),
        FUN = "/")
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
  legendtitle <- list(yref='paper',xref="paper",y=1.03,x=1.12, text="Transition probability to state Y",showarrow=F)
  
  plot_ly(
    x = colnames(matrix),
    y = colnames(matrix),
    z = matrix, 
    colors = palette(100),
    colorbar=list(title='Transition \n Probability', titlefont=list(size=20), tickfont=list(size=20)),
    type = "heatmap",
    zauto = FALSE, zmin = 0, zmax = max(matrix, na.rm = TRUE)) %>%
    layout(xaxis = list(title="Current State", titlefont=list(size=30), tickfont=list(size=20)),
           yaxis = list(title="Previous State", titlefont=list(size=30), tickfont=list(size=20)))
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

# Two-step Markov chain run
clickstreams_trbl = read_csv("second-order-data/cleanshark-no-gap-mc.csv")
second_mc <- second_order_mc(clickstreams_trbl)
fig = plot_second_order_mc(second_mc)

smooth_second_mc <- smooth_transition_matrix(second_mc)
plot_second_order_mc(smooth_second_mc)

# Joint distribution transition matrix
second_mc_joint = second_order_mc_joint(clickstreams_trbl)
plot_second_order_mc(second_mc_joint)

# Focus in on prediction column
second_mc_x = zoom_second_order_matrix(second_mc_joint, 'X')
fig = plot_second_order_mc_zoom(second_mc_x)

second_mc_y = zoom_second_order_matrix(second_mc_joint, 'Y')
fig = plot_second_order_mc_zoom(second_mc_y)

# Shall we add an X state before every Y state?

orca(fig, file = "images/two_step_matrix_to_Y.pdf")


