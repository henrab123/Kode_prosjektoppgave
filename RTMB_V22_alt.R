library(RTMB)

# Setting seed for reproducibility
set.seed(1)

#Check if there are NAs in the dataset:
if (any(is.na(RTMB_V22))) {
  stop("NA values found in RTMB_V22, please check the dataset")
}

# Define the number of ordinal categories for each response
N_Questions_V22 <- 20

# Initialize an empty vector to store the number of unique categories
N_categories_V22 <- c()

# Use a for loop to calculate the number of unique categories for each question Q1 to Q20
for (i in 1:N_Questions_V22) {
  N_categories_V22 <- c(N_categories_V22, length(unique(RTMB_V22[[paste0("Q", i)]])))
}

# Print the result
print(N_categories_V22)

# Create a unique mapping from "kandidatnummer" to indices
unique_kandidater <- unique(RTMB_V22$kandidatnummer)
kandidat_index    <- match(RTMB_V22$kandidatnummer, unique_kandidater)

###########################################
## CALCULATING THRESHOLDS FOR PARAMETERS ##
###########################################

theta_true_values_V22 <- list()

for (i in 1:1){#N_Questions_V22) {
  
  y <- RTMB_V22[paste0("Q",i)]
  N_y <- length(y[[paste0("Q",i)]])
  N_i <- rep(0, length(unique(y[[paste0("Q",i)]])))
  
  for (k in 1: N_y){
    
  }
  print(N_i)
  print(N_y)
   
}


#########################
## CREATING PARAMETERS ##
#########################

parameters_V22 <- list(
  # Random effects
  s       = rep(0, length(unique(RTMB_V22$kommisjon))),      # Examiner random effects
  gamma   = rep(0, length(unique(RTMB_V22$kandidatnummer))),  # Student random effects
  
  log_sd_gamma  = 1,   # Std deviation for students
  log_std_s     = 1    # Std deviation for examiners
)

# Thresholds:
for (i in 1:N_Questions_V22) {
  parameters_V22[[paste0("alpha", i)]] <- c(seq(0, 1, length.out = N_categories_V22[i] - 1))
}

###########################
## CREATING THE FUNCTION ##
###########################

f_V22 <- function(parms) {
  # Make variables local from the parameter list
  getAll(RTMB_V22, parms, warn = FALSE)
  
  # Initialize negative log-likelihood
  nll <- 0
  
  # Loop over all 20 questions
  for (q in 1:N_Questions_V22){

    # Linear predictor: student and examiner effects
    eta <- gamma[kandidat_index] + s[RTMB_V22$kommisjon]
    
    # Extract the number of categories and corresponding thresholds for this question
    thresholds     <-  parameters_V22[[paste0("alpha", q)]]
    theta          <-  rep(0,length(thresholds))
    
    # Rescaling the thresholds:
    theta[1] <- thresholds[1]
    
    if (length(theta) > 1){
      for (i in 2:(length(thresholds))){
        theta[i] <- theta[i-1] + exp(thresholds[i])
      }
    }
    
    # Loop over categories to compute log-likelihood
    for (k in 1:length(theta)) {
      if (k == 1){
        nll <- nll - sum(log(dlogis(eta, location = theta[k], scale = 1)))
      } else {
        print(dlogis(eta, location = theta[k], scale = 1) - dlogis(eta, location = theta[k-1], scale = 1))
        nll <- nll - sum(log(dlogis(eta, location = theta[k], scale = 1) - dlogis(eta, location = theta[k-1], scale = 1) + 0.1))
      }
    }
  }
  
  # Add random effects for students (gamma) and examiners (s) to the likelihood
  nll <- nll - sum(dnorm(gamma, mean = 0, sd = exp(parms$log_sd_gamma), log = TRUE))
  nll <- nll - sum(dnorm(s    , mean = 0, sd = exp(parms$log_std_s)   , log = TRUE))
  
  # Get predicted eta
  ADREPORT(eta)
  
  #Returning the negative log-likelihood
  return(nll)
}

####################################################
## CREATE THE OBJECTIVE FUNCTION FOR OPTIMIZATION ##
####################################################

obj <- MakeADFun(
  func = f_V22,                  # Function for the negative log-likelihood
  parameters = parameters_V22,   # Parameters to be estimated
  random = c("s", "gamma"),      # Random effects for examiners and students
)

# FIT THE MODEL USING OPTIMIZATION (e.g., nlminb)
fit <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)

# GET STANDARD ERRORS FOR THE PARAMETERS
sdr <- sdreport(obj)
summary(sdr)
