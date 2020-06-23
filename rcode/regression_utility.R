#################################################################################
# Regression Utility Functions
#################################################################################

# Function for marginal distances based on p-values
# INPUTS:
#     orig_glm: output from a fitted glm object (df: summary(object)$coef)
#     syn_glm: list of outputs from fitted glm objects (list of df: summary(object)$coef)
#     m: number of synthetic replicates
# OUTPUTS:
#     regression_utility: mean and median standardized coefficient difference and confidence interval overlap for all regression coefficients (num)


regression_summary_function = function(orig_glm, syn_glm, m){
  ## orig with names
  orig_tibble = as_tibble(orig_glm) %>% 
    mutate('names' = rownames(orig_glm))
  ## combine all synthetic data outputs
  for(a in 1:m){
    if(a == 1){
      ## initialize names
      syn_tibble = as_tibble(syn_glm[[a]]) %>% 
        mutate('names' = rownames(syn_glm[[a]]))
    } else{
      ## bind other df
      temp = as_tibble(syn_glm[[a]]) %>% 
        mutate('names' = rownames(syn_glm[[a]]))
      syn_tibble = bind_rows(syn_tibble, temp)
    }
  }

  ## helper to collapse variances
  var_mean = function(varr){sum(varr) / 5}
  ## collapse estimates from each synthetic data set
  syn_tibble = syn_tibble %>% 
    mutate(Var = `Std. Error` ^ 2) %>% 
    group_by(names) %>% 
    summarize_all(funs(var_mean)) %>%
    mutate(`Adj Std. Error` = sqrt(Var * (1 + (1 / m)))) %>% 
    select(Estimate, `Adj Std. Error`, names)
  combined_analyses = left_join(selects(orig_tibble, names, Estimate, `Std. Error`), 
                                syn_tibble, 
                                by = 'names', suffix = c('_orig', '_syn'))
  
  ## now compute std. diff and ci overlap
  regression_utility = combined_analyses %>% 
    mutate('std. coef diff' = abs(Estimate_orig - Estimate_syn) / `Std. Error`,
           'orig_lower' = Estimate_orig - 1.96 * `Std. Error`, 'orig_upper' = Estimate_orig + 1.96 * `Std. Error`,
           'syn_lower' = Estimate_syn - 1.96 * `Adj Std. Error`, 'syn_upper' = Estimate_orig + 1.96 * `Adj Std. Error`) %>%
    mutate('ci_overlap' = 0.5 * (((pmin(orig_upper, syn_upper) - pmax(orig_lower, syn_lower)) / (orig_upper - orig_lower)) + 
                                   ((pmin(orig_upper, syn_upper) - pmax(orig_lower, syn_lower)) / (syn_upper - syn_lower)))) %>%
    select(names, 'std. coef diff', 'ci_overlap') %>% 
    filter(names != '(Intercept)') %>%
    summarize('mean std. coef diff' = mean(`std. coef diff`), 'median std. coef diff' = median(`std. coef diff`),
              'mean ci overlap' = mean(ci_overlap), 'median ci overlap' = median(ci_overlap))
  
  return(regression_utility)
}

