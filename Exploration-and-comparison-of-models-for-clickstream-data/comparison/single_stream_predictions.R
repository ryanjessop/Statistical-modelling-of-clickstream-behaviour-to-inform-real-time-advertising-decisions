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


obv_test
unseen_new_obvs
seen_obv


names(step_model$coefficients)


comparison_graphic_v2(new_observation=unseen_new_obvs,
                      first_mc=first_mc,
                      second_mc=smooth_second_mc,
                      hmms=list(gold_hmm_4, gold_hmm_5, gold_hmm_6),
                      hmm_names=list('4 State HMM', '5 State HMM', '6 State HMM'),
                      glm_new_data=list(unseen_new_glm_obvs_start, unseen_new_glm_obvs_end),
                      glm_model=step_model,
                      conversion_observed_state='Y')

comparison_graphic_hidden_states(new_observation=unseen_new_obvs,
                                 hmms=list(gold_hmm_4, gold_hmm_5, gold_hmm_6),
                                 hmm_names=list('4 State HMM', '5 State HMM', '6 State HMM'),
                                 conversion_observed_state='Y')

#---------------------------------------------------

comparison_graphic_v2(new_observation=seen_obvs,
                      first_mc=first_mc,
                      second_mc=smooth_second_mc,
                      hmms=list(gold_hmm_4, gold_hmm_5, gold_hmm_6),
                      hmm_names=list('4 State HMM', '5 State HMM', '6 State HMM'),
                      glm_new_data=list(seen_new_glm_obvs_start, seen_new_glm_obvs_end),
                      glm_model=step_model,
                      conversion_observed_state='Y')

comparison_graphic_hidden_states(new_observation=unseen_new_obvs,
                                 hmms=list(gold_hmm_4, gold_hmm_5, gold_hmm_6),
                                 hmm_names=list('4 State HMM', '5 State HMM', '6 State HMM'),
                                 conversion_observed_state='Y')
