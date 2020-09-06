# Input data size different HMMs

# Trade off
start_sizes <- c(0.15,0.25,0.35,0.45,0.55,0.65,0.75)
end_sizes <- c(  0.85,0.85,0.85,0.85,0.85,0.85,0.85)
sizes <- end_sizes - start_sizes

observations <- clickstreams[
  (which(clickstreams == 'E')[round(length(which(clickstreams == 'E'))*0.75)] + 1):
    which(clickstreams == 'E')[round(length(which(clickstreams == 'E'))*0.85)]]

const_transition <- matrix(runif(5*5), 
       nrow=5) + runif(1,1,2)*diag(5)

const_emission <- matrix(runif(5*length(14)), 
       nrow=5)

likelihoods <- c()
timings <- c()

# Need to stabilise the initial values 
# Cut down on the likelihood ?

for (i in 1:length(start_sizes)){
  start_time <- Sys.time()
  moment_hmm <- train_hmm(
    num_hidden_states=5, 
    observable_states = c("A", "B", "C", "D", "E", "H", "I", "L", "P", "R", "S", "U", "X", "Y"),
    observations=clickstreams,
    #initial_transition = const_transition,
    #init_emission = const_emission,
    start_sample_size=start_sizes[i],
    end_sample_size=end_sizes[i])
  end_time <- Sys.time()
  print(as.numeric(end_time - start_time))
  likelihoods[i] <- as.numeric(end_time - start_time)
  print(likelihood_hmm(moment_hmm$hmm$hmm, observations))
  timings[i] <- likelihood_hmm(moment_hmm$hmm$hmm, observations)
}

timings_1 <- c(14.64838, 13.26821, 8.00259, 9.214347, 7.244868, 9.809826, 2.162215)
likelihoods_1 <- c(-17661.12, -8728.096, -16976.16, -12886.27, -21528, -16968.39, -12663.7)

timings_2 <- c(25.19986, 13.06691, 16.47567, 11.3523, 8.584958, NA, NA)
likelihoods_2 <- c(-17560.25, -12658.29, -12675.35, -12935.92, -12621.47, NA, NA)

timings_3 <- c(13.25127, 21.46121, 15.54075, 13.26633, 5.126895, 3.13473, 1.533902)
likelihoods_3 <- c(-12983.37, -16951.35, -16904.34, -17210.28, -12690.7, -17258.5, -8444.126)

timings_4 <- c(32.14699, 25.5164, 18.69039, 11.97849, 10.52402, NA, NA)
likelihoods_4 <- c(-8410.374, -12661.05, -13188.67, -12824.79, -17507.18, NA, NA)

timings_5 <- c(32.50982, 11.2769, 13.36749, 8.57606, 4.220708, 5.420259, 0.9933429)
likelihoods_5 <- c(-8578.386, -12800.58, -17249.75, -8496.763, -12995.88, -12852.01, -12998.28)


data_likelihoods_df <- tibble(
  size = sizes
)

data_timings_df <- tibble(
  size = sizes
)

data_likelihoods_df <- data_likelihoods_df %>% 
  add_column(likelihoods_1) %>%
  add_column(likelihoods_2) %>%
  add_column(likelihoods_3) %>%
  add_column(likelihoods_4) %>%
  add_column(likelihoods_5)

data_timings_df <- data_timings_df %>% 
  add_column(timings_1) %>%
  add_column(timings_2) %>%
  add_column(timings_3) %>% 
  add_column(timings_4) %>% 
  add_column(timings_5)


data_likelihoods_df <- gather(data_likelihoods_df, 
                              'likelihoods_1', 'likelihoods_2',
                              'likelihoods_3', 'likelihoods_4', 'likelihoods_5',
                              key = 'variable', value='score')

data_timings_df <- gather(data_timings_df, 
                         'timings_1', 'timings_2',
                         'timings_3', 'timings_4', 'timings_5',
                         key = 'variable', value='score')

boxplot(score~size,
        data=data_likelihoods_df,
        main="",
        xlab="Sample size %", 
        ylab="Log-likelihood of sequence",
        cex.axis=2,
        cex.lab=2.5)

boxplot(score~size,
        data=data_timings_df,
        main="",
        xlab="Sample size %", 
        ylab="Computation time (s)",
        cex.axis=2.5,
        cex.lab=2.5)

# Outputs:
# Likelihood 
# Computation time
# Data size 

plot(data_size_df$size, 
     data_size_df$likelihoods_1,
     xlab = 'Sample size %',
     ylab = 'HMM Likelihood',
     pch = 19,
     type = "b")
plot(data_size_df$size, 
     data_size_df$timings_1,
     xlab = 'Sample size %',
     ylab = 'Computation time (s)',
     pch = 19,
     type = "b")
