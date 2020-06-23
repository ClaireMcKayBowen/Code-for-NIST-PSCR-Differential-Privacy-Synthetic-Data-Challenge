#################################################################################
# P-value Utility Functions
#################################################################################

# Function for marginal distances based on p-values
# INPUTS:
#     orig_data: original data (df)
#     data_list: synthetic data (list of df)
#     factor_var: variables treated as categorical
#     cont_var: variables treated as continuous
#     m: number of synthetic replicates
# OUTPUTS:
#     out_summary: distance (p-value) from original distribution for each variable for each synthetic data set (num)

## summarize categorical variables using chi-sq test p-value
categorical_summary_function = function(vari, m, data_list){
  chisq_test = chisq.test(table(select(data_list, 'id', vari)))
  out = tibble('variable' = vari, 
               'chisq_stat' = chisq_test$statistic, 'chisq_pval' = chisq_test$p.value)
  return(out)
}

## summarize continuous variables using ks test or t-test p-value
continuous_summary_function = function(vari, m, data_list){
  ## id = 0 is original data, id = 1 is synthetic data
  t_test = t.test(filter(data_list, id == 0) %>% 
      pull(vari), filter(data_list, id == 1) %>% 
      pull(vari))
  ks_test = ks.test(filter(data_list, id == 0) %>% 
      pull(vari), filter(data_list, id == 1) %>% 
      pull(vari))
  out = tibble('variable' = vari, 
               'ks_stat' = ks_test$statistic, 'ks_pval' = ks_test$p.value,
               't_stat' = t_test$statistic, 't_pval' = t_test$p.value)
  return(out)
}

## summarize all variables
summarize_whole_dataset = function(orig_data, data_list, factor_var, cont_var, m){
  comb_summary = vector('list', m)
  ## generate summaries for each m synthetic replicate
  for(a in 1:m){
    ## combine original data and synthetic
    comb_data = bind_rows(orig_data %>% 
      mutate('id' = 0), data_list[[a]] %>% 
      mutate('id' = 1)) 
    ## get summaries for given categorical and continuous variables
    test_cat = lapply(factor_var, categorical_summary_function, m = m, data_list = comb_data)
    test_cont = lapply(cont_var, continuous_summary_function, m = m, data_list = comb_data)
    ## combine
    comb_summary[[a]] = bind_rows(test_cat, test_cont) %>% 
      mutate('id' = a)
  }
  ## make combined data frame with results for all synthetic replicates
  out_summary = bind_rows(comb_summary)
  return(out_summary)
}

