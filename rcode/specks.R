#################################################################################
#	SPECKS Function
#################################################################################

# Function for SPECKS
# INPUTS:
#     dat_ori: original data (df)
#   dat_synth: synthetic data (df)
# OUTPUTS:
#          ks: SPECKS output, ks dist. (num)

specks <- function(dat_ori, dat_synth) {
  N <- nrow(dat_ori)				# Number of observations
  
  # Start propensity score analysis --------------------------------------------------------	
  # Combining the original and synthetic datasets together
  dat_comb <- rbind(dat_ori, dat_synth)
  
  # Creating a test data set that gives 0 for original and 1 for synthetic
  dat_test <- data.frame(cbind(c(rep(0, N), rep(1, N)), dat_comb))
  colnames(dat_test)[1] <- "Treat"
  
  dat_test$Treat <- as.factor(dat_test$Treat)
  mod <- suppressWarnings(glm(Treat ~ .^2, dat_test, family = binomial(link = "logit")))
  dat_pscore <- predict(mod, dat_test, type = "response")
  
  # Compare propensity score by comparing ECDFs using KS Distance --------------------------------------------------------	
  # Generate ECDF for original and synthetic data
  ori_cdf <- ecdf(dat_pscore[1:N])
  synth_cdf <- ecdf(dat_pscore[(N + 1):(2 * N)])
  
  # Sequence of values to extract CDFs
  z <- seq(0, 1, 1e-4)
  
  # Calculate KS Distance of the original and synthetic CDFS
  ks <- suppressWarnings(ks.test(ori_cdf(z), synth_cdf(z)))$statistic	
  
  return(ks)
}
