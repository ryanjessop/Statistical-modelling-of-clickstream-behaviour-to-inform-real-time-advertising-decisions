hidden_state_sim <- simulation_generation(recursive_gold_hmm_5$hmm$transProbs, 10000)

observed_state_sim <- simulate_observed_states(recursive_gold_hmm_5, hidden_state_sim)



data <- data.frame(text=paste(observed_state_sim, sep=",", collapse="")) 

data2 <- data.frame(text=unlist(strsplit(as.character(data$text), "E")))

write.csv(data2, file = "5_state.csv")



cls <- readClickstreams(file='5_state.csv', sep = ",", header = TRUE)
first_mc <- fitMarkovChain(cls)

first_mc_trans <- t(as.matrix(first_mc@transitions[[1]]))

plot_first_order_mc(first_mc)
plot_first_order_mc_v2(first_mc_trans)

plot_network_first_order_mc_v2(first_mc, layout_size = 1.2, node_size_factor=2)
plot_network_first_order_mc_v3(first_mc, layout_size = 1, node_size_factor=2)



occurences_sim <- table(observed_state_sim)

transition_matrix_sim <- markovchainFit(data=observed_state_sim)

transition_matrix_sim$estimate

plot_first_order_mc_v2(transition_matrix_sim$estimate@transitionMatrix)

plot_network_first_order_mc_v4 <- function(transition_matrix,
                                           lower_bound=0.001,
                                           state_hits=c(127,830,557,9800,4749,862,620,65,6323,998,66,572,634,235),
                                           node_size_factor=0.06,
                                           edge_width_factor=10,
                                           edge_arrow_size=.8,
                                           edge_curved=.6,
                                           layout_size=1.2,
                                           legend_x=-2.0,
                                           legend_y=-0.5){
  
  transitions_df <- as.data.frame(t(transition_matrix))
  previous_state <- rownames(transitions_df)
  df <- cbind(previous_state, transitions_df)
  tidy_df <- gather(df, 
                    c(rownames(transitions_df)),
                    key='next_state', 
                    value='transition_probability') %>% 
    filter(transition_probability > lower_bound) %>% 
    mutate(mapping_values = round(transition_probability, 2))
  
  color_map <- data_frame(
    greyscale_color = gray.colors(91, start=0, end=0, alpha=seq(0,0.90,0.01)), 
    mapping_values = seq(0,0.90,0.01))
  
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

plot_network_first_order_mc_v4(transition_matrix = transition_matrix_sim$estimate@transitionMatrix, 
                               layout_size = 1, 
                               node_size_factor=2,
                               state_hits=as.numeric(occurences_sim))

#-------------------------------------------------------------------------------


hidden_state_sim_4 <- simulation_generation(gold_hmm_4$hmm$transProbs, 10000)

observed_state_sim_4 <- simulate_observed_states(gold_hmm_4, hidden_state_sim_4)

occurences_sim_4 <- table(observed_state_sim_4)

transition_matrix_sim_4 <- markovchainFit(data=observed_state_sim_4)

transition_matrix_sim_4$estimate

plot_first_order_mc_v2(transition_matrix_sim_4$estimate@transitionMatrix)

plot_network_first_order_mc_v4(transition_matrix = transition_matrix_sim_4$estimate@transitionMatrix, 
                               layout_size = 1, 
                               node_size_factor=2,
                               state_hits=as.numeric(occurences_sim_4))

#-------------------------------------------------------------------------------


hidden_state_sim_6 <- simulation_generation(gold_hmm_6$hmm$transProbs, 10000)

observed_state_sim_6 <- simulate_observed_states(gold_hmm_6, hidden_state_sim_6)

occurences_sim_6 <- table(observed_state_sim_6)

transition_matrix_sim_6 <- markovchainFit(data=observed_state_sim_6)

transition_matrix_sim_6$estimate

plot_first_order_mc_v2(transition_matrix_sim_6$estimate@transitionMatrix)

plot_network_first_order_mc_v4(transition_matrix = transition_matrix_sim_6$estimate@transitionMatrix, 
                               layout_size = 1, 
                               node_size_factor=2,
                               state_hits=as.numeric(occurences_sim_4))
