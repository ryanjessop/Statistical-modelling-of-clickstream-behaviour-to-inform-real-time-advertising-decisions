# First order Markov chain run

cls <- readClickstreams(file='data/first-order-data/cleanshark-comma-mc.csv', sep = ",", header = TRUE)
first_mc <- fitMarkovChain(cls)
plot_first_order_mc(first_mc)
plot_network_first_order_mc_v3(first_mc, layout_size = 1.1, node_size_factor=2)

# Two-step Markov chain run
clickstreams_trbl = read_csv("data/second-order-data/cleanshark-no-gap-mc.csv")
second_mc <- second_order_mc(clickstreams_trbl)
plot_second_order_mc(second_mc)
smooth_second_mc <- smooth_transition_matrix(second_mc)
plot_second_order_mc(smooth_second_mc)

# Joint distribution transition matrix
second_mc_joint = second_order_mc_joint(clickstreams_trbl)
plot_second_order_mc(second_mc_joint)

# Focus in on prediction column
second_mc_x = zoom_second_order_matrix(second_mc_joint, 'X')
plot_second_order_mc_zoom(second_mc_x)

second_mc_y = zoom_second_order_matrix(second_mc_joint, 'Y')
plot_second_order_mc_zoom(second_mc_y)

# Shall we add an X state before every Y state?