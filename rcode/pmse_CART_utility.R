#################################################################################
# CART pMSE Utility Functions
#################################################################################

# Functions for estimating pmse and null pmse using CART models
# INPUTS:
#     orig_data: original data (df)
#     data_list: synthetic data (df)
#     cp: CART model complexity parameter
#     nperm: number of permutations for estimating the null
# OUTPUTS:
#     pmse: joint distributional utility metric (num)
#     mean_null_pmse: joint distributional utility metric under the null of two data sets drawn from the same distribution (num)



## function to compute observed pmse for a single synthetic data set
pmse_compute = function(syn_data, orig_data, cp){
  
  ## combine orignal and synthetic data and add group indicator
  comb_data = bind_rows(orig_data, select(syn_data, colnames(orig_data))) %>% 
    mutate(id = c(rep(0, nrow(orig_data)), rep(1, nrow(syn_data))))
  
  ## fit model and predict prop scores
  prop_cart = rpart(id ~ ., data = comb_data, cp = cp, method = 'class')
  pred_prob = predict(prop_cart)[, 2]
  
  ## estimate obesrved pmse
  pmse = mean((pred_prob - (nrow(syn_data) / nrow(comb_data))) ^ 2)
  
  return(pmse)
}


## function to calculate null pmse
compute_null_pmse = function(orig_data, nperm, cp){
  
  pmse_null = rep(NA, nperm)
  for(a in 1:nperm){
    ## bootstrap 2 times the rows
    samp_row = sample(1:nrow(orig_data), (nrow(orig_data) * 2), replace = TRUE)
    
    ## combine and add indicators
    comb_data = orig_data[samp_row, ] %>% 
      mutate(id = rep(0:1, each = nrow(orig_data)))
    
    ## fit model and predict prop scores
    prop_cart = rpart(id ~ ., data = comb_data, cp = cp, method = 'class')
    pred_prob = predict(prop_cart)[, 2]
    
    ## calculate pmse
    pmse_null[a] = mean((pred_prob - 0.5) ^ 2)
  }

  mean_null_pmse = mean(pmse_null)
  return(mean_null_pmse)
}


