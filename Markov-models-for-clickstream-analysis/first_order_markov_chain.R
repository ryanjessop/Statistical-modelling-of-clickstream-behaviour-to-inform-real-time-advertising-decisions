library(tibble)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tidyverse)
library(reshape2)
library(readr)
library(clickstream)
library("arulesSequences")
library(plotly)
library(HMM)
library(hablar)
library(readr)
library(broom)
library(ndtv)
library(networkD3)
library(threejs)
library(visNetwork)
library(igraph)
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(RColorBrewer)

palette <- colorRampPalette(c("white", "yellow", "red", "dark red"))

# First order Markov chain functions

plot_first_order_mc <- function(markov_chain){
  hmPlot(markov_chain, 
         title="First order Markov Chain transition matrix",
         lowColor = 'yellow',
         highColor = 'dark red')
}

plot_first_order_mc_v2 <- function(matrix){
  plot_ly(
    x = colnames(matrix),
    y = colnames(matrix),
    z = matrix, 
    colors = palette(100),
    colorbar=list(title='Transition \n Probability', titlefont=list(size=20), tickfont=list(size=20)),
    type = "heatmap",
    zauto = FALSE, zmin = 0, zmax = max(matrix, na.rm = TRUE)) %>%
    layout(xaxis = list(title="To", titlefont=list(size=30), tickfont=list(size=20)),
           yaxis = list(title="From", titlefont=list(size=30), tickfont=list(size=20)))
}

plot_network_first_order_mc_v2 <- function(markov_chain,
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
    greyscale_color = gray.colors(56, start=1, end=0), 
    mapping_values = seq(0.01,0.56,0.01))
  
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


plot_network_first_order_mc_v3 <- function(markov_chain,
                                           lower_bound=0.01,
                                           state_hits=c(127,830,557,9800,4749,862,620,65,6323,998,66,572,634,235),
                                           node_size_factor=0.06,
                                           edge_width_factor=10,
                                           edge_arrow_size=.8,
                                           edge_curved=.6,
                                           layout_size=1.5,
                                           legend_x=-2.0,
                                           legend_y=-0.5, 
                                           edges=full_edges){
  
  transitions_df <- as.data.frame(t(markov_chain@transitions$`1`))
  previous_state <- rownames(transitions_df)
  df <- cbind(previous_state, transitions_df)
  tidy_df <- gather(df, 
                    c(rownames(transitions_df)),
                    key='next_state', 
                    value='transition_probability') %>% 
    filter(transition_probability > lower_bound) %>% 
    mutate(mapping_values = round(transition_probability, 2))
  #print(tidy_df)
  color_map <- data_frame(
    greyscale_color = gray.colors(81, start=0, end=0, alpha=seq(0,0.80,0.01)), 
    mapping_values = seq(0,0.80,0.01))
  # print(color_map)
  #edges <- left_join(tidy_df, color_map, b="mapping_values")
  #print(edges)
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
         pt.cex=3.5, 
         cex=1.4, 
         bty="n", 
         ncol=1)
}

# First order Markov chain run

cls <- readClickstreams(file='first-order-data/cleanshark-comma-mc.csv', sep = ",", header = TRUE)
first_mc <- fitMarkovChain(cls)

first_mc_trans <- t(as.matrix(first_mc@transitions[[1]]))

plot_first_order_mc(first_mc)
fig = plot_first_order_mc_v2(first_mc_trans)

plot_network_first_order_mc_v2(first_mc, layout_size = 1.2, node_size_factor=2)
plot_network_first_order_mc_v3(first_mc, layout_size = 1, node_size_factor=2)

orca(fig, file = "images/first_order_markov_chain_matrix.pdf")
