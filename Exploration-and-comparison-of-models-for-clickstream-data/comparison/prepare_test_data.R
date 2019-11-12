# Clickstream data

clickstream_df_test <- read_csv("data/hidden-markov-data/cleanshark-hmm-test.csv", col_names = FALSE)
clickstreams_test <- strsplit(clickstream_df_test$X1, split=c())[[1]]

# Create a new clickstream
unseen_new_obvs <- c("H","P","D","C","D","B","X","Y")

seen_obvs <- c("P","D","B","X","P","P","D","P","D","R","D")

# Single observation from training set
seen_obv <- generate_single_obvs(clickstreams, seed_val = 10010)
seen_obv <- generate_single_obvs(clickstreams, seed_val = 77494)
seen_obv

# Single observation from test set
obv_test <- generate_single_obvs(clickstreams_test, seed_val = 100220)
obv_test

# Multiple observations from training set
multi_obvs <- generate_multi_obvs(clickstreams, sample_size = 0.01)
multi_obvs

# Multiple observations from test set
multi_obvs_test <- generate_multi_obvs(clickstreams_test, sample_size = 0.01)
multi_obvs_test

#-----------------------------------------------------------------
# Browsing features GLM data

# Create a new browsing features GLM observation
# unseen_new_glm_obvs_start <- data.frame(num_page_visits=5, 
#                                         session_duration=400,
#                                         dev_type="SmartPhone",
#                                         intent_score=0.45,
#                                         local_total_duration=0,
#                                         hour="17")
# unseen_new_glm_obvs_end <- data.frame(num_page_visits=11, 
#                                       session_duration=520,
#                                       dev_type="SmartPhone",
#                                       intent_score=0.55,
#                                       local_total_duration=400,
#                                       hour="18")

unseen_new_glm_obvs_start <- data.frame(num_page_visits=0, 
                                        session_duration=0,
                                        dev_type="SmartPhone",
                                        intent_score=0.45,
                                        local_num_prev_sess=0,
                                        local_total_page_visits=5,
                                        local_total_duration=100,
                                        local_prev_sess_length=0,
                                        total_conversions_lag=0)

unseen_new_glm_obvs_end <- data.frame(num_page_visits=9, 
                                      session_duration=300,
                                      dev_type="SmartPhone",
                                      intent_score=0.55,
                                      local_num_prev_sess=1,
                                      local_total_page_visits=14,
                                      local_total_duration=400,
                                      local_prev_sess_length=100,
                                      total_conversions_lag=0)

#------------------------------------------------------------------

join_df <- trim_data %>%
  inner_join(clickstreams_trbl, by='pid')

seen_new_glm_obvs_start <- data.frame(num_page_visits=0, 
                                        session_duration=0,
                                        dev_type="SmartPhone",
                                        intent_score=0,
                                        local_num_prev_sess=0,
                                        local_total_page_visits=0,
                                        local_total_duration=0,
                                        local_prev_sess_length=0,
                                        total_conversions_lag=0)

seen_new_glm_obvs_end <- data.frame(num_page_visits=8, 
                                    session_duration=417,
                                    dev_type="SmartPhone",
                                    intent_score=0.48,
                                    local_num_prev_sess=0,
                                    local_total_page_visits=0,
                                    local_total_duration=0,
                                    local_prev_sess_length=0,
                                    total_conversions_lag=0)
