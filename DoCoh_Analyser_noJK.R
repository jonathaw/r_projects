source('/Users/jonathan/Documents/bin/general_functions/functions.R')
filters = c('a_ddg', 'a_packstat', 'a_rms', 'a_sasa', 'a_shape', 'a_stability_score_full')

PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/ct_doc_48s/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/ct_doc11b/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/cc_doc5a/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/rf_doc44b/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/ct_cipa/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/af_doc/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/bc_doc48a/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/rf_scaa/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/rf_scab/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/ac_scaa/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/bc_scaa/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/ac_scab/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/rotation_designs/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/crys_14/results/'

PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/all_results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/4ums_modeling/again_only_ed/4ums_VS_all/'
PATH_DATA = '/Users/jonathan/eden/no_backup/postdiction/temp_threshold/cc_a1_VS_cc_5a/'
PATH_DATA = '/Users/jonathan/eden/no_backup/prediction/lizi/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/prediction/olga/results/'

PATH_DATA = '/Users/jonathan/eden/no_backup/designs/1ohz_bb_30.12/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/chain_flip_6.1/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/seqprofcons_7.1/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/pido_met/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/pido_met_2/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/design_prediction/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/design_prediction/results_2nd/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/pssm_fix_21.1/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/design_prediciton_21.1/results/'
PATH_DATA = '/Users/jonathan/eden/no_backup/designs/prediction_redo_25.1/results/'

file.list = list.files(PATH_DATA, pattern="*.score$")
file.list
file = file.list[5]
file
par(mfrow=c(1, 1))
par(mfrow=c(5, 9))
par(mfrow=c(4, 4))
for (file in file.list){
  file.df <- read_n_clean_scores(PATH_DATA, file)
  has_sd2 = FALSE
  ### plot something Vs. ddG:
  what1_and_what2_single_df(file.df, 'a_sasa','a_ddg', file)
  #   what1_and_what2(file.df, sd.df, sd.df2, has_sd2, 'a_rms','a_stability_score_full')
  #   what1_and_what2(file.df, sd.df, sd.df2, has_sd2, 'a_rms','a_sasa')
  #   what1_and_what2(file.df, sd.df, sd.df2, has_sd2, 'a_rms','a_packstat')
  #   jk.best <- choose_best(file.df, 0.2)
  #   summarize_filters(jk.best, filters)
  #   color_by_filters_single_df(file.df, 'a_sasa', 'a_ddg', ddg = -16, pack = 0.5, sasa = 1000, shape = 0.3, colour = 'red')
  #   color_by_filters_single_df(file.df, 'a_sasa', 'a_ddg', ddg = -18, pack = 0.6, sasa = 1100, shape = 0.4, colour = 'blue')
  color_by_filters_single_df(file.df, 'a_sasa', 'a_ddg')
  #   bb_barplots(file.df, bb = 'coh', ddg = -18, pack = 0.6, sasa = 1100, shape = 0.4)
  print(c(file, count_by_filters_single_df(DF = file.df)))
  #   desc.parts <- ldply(strsplit(file.df$description, '_'))
  #   colnames(desc.parts) <- c('coh_query', 'doc_query', 'on', 'coh_temp', 'A','doc_temp', 'num', 'num2')  
  #   file.df$coh_temp <- desc.parts$coh_temp
  #   file.df$doc_temp <- desc.parts$doc_temp
  #   count_by_filters_single_df(file.df)
  #   plot_identities(file.df, file)
}

squeezed_best <- squeeze_df_subset(file.df)#, ddg = -16, sasa = 1000, pack = 0.55)
squeezed_best <- squeezed_best[with( squeezed_best, order(a_shape, decreasing = T)), ]
print_pdb_names(squeezed_best$description[1:10])
View(squeezed_best)


file.df[identify(file.df$a_sasa, file.df$a_ddg), ]

only.4ums <- squeeze_df(subset(file.df, coh_temp == '4ums'))


### 6.1.15 an attempt at refining the threshplds to optimise results
{
  ct_a3_ct_48s.purples <- squeeze_df_subset(file.df)
  ct_a3_cc_5a.purples <- squeeze_df_subset(file.df)
  ct_ob4_ct_cipa.purples <- squeeze_df_subset(file.df)
  ac_d1_ac_scaa.purples <- (squeeze_df_subset(file.df))
  
  summary(as.numeric(ct_a3_cc_5a.purples$a_shape))
  summary(as.numeric(ct_a3_ct_48s.purples$a_shape))
  summary(as.numeric(ct_ob4_ct_cipa.purples$a_shape))
  summary(as.numeric(ac_d1_ac_scaa.purples$a_shape))
  
  summary(as.numeric(ct_a3_cc_5a.purples$a_packstat))
  summary(as.numeric(ct_a3_ct_48s.purples$a_packstat))
  summary(as.numeric(ct_ob4_ct_cipa.purples$a_packstat))
  summary(as.numeric(ac_d1_ac_scaa.purples$a_packstat))
  
  
  binders.purples <- squeeze_df_subset(master.binders)
  non.binders.purples <- squeeze_df_subset(master.non.binders)
  summary(as.numeric(binders.purples$a_sasa))
  summary(as.numeric(non.binders.purples$a_sasa))
  count_by_filters_single_df(DF = ct_a3_cc_5a.purples, shape = 0.53, sasa = 1320)
  count_by_filters_single_df(DF = ct_a3_ct_48s.purples, shape = 0.53)
  count_by_filters_single_df(DF = ct_ob4_ct_cipa.purples, shape = 0.53, sasa=1320)
  
  plot(as.numeric(binders.purples$a_shape), runif(length(binders.purples$a_shape), 1, 100))
  points(as.numeric(non.binders.purples$a_shape), runif(length(non.binders.purples$a_shape), 1, 100), col = 'red')
  plot(as.numeric(binders.purples$a_packstat), runif(length(binders.purples$a_packstat), 1, 100))
  points(as.numeric(non.binders.purples$a_packstat), runif(length(non.binders.purples$a_packstat), 1, 100), col = 'red')
  plot(as.numeric(binders.purples$a_sasa), runif(length(binders.purples$a_sasa), 1, 100))
  points(as.numeric(non.binders.purples$a_sasa), runif(length(non.binders.purples$a_sasa), 1, 100), col = 'red')
}
### 7.1.15: looking at designs:
designs <- subset(file.df, a_ddg < DDG & a_packstat > PACK & a_sasa > SASA & a_shape > SHAPE)
#designs <- designs[with( designs, order(a_shape, decreasing = T)), ]
designs <- designs[with( designs, order(a_ddg, decreasing = F)), ]
cat(print_pdb_names(designs$description[1:10]))
View(designs)


ROC <- filter_result(num_purples_pass = 10, shape = 0.6)

