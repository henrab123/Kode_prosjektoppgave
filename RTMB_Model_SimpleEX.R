library(RTMB)

# Setting seed for reproducibility
set.seed(1)

# Small correction
epsilon <- 1E-6

#Check if there are NAs in the dataset:
if (any(is.na(RTMB_V22))) {
  stop("NA values found in RTMB_V22, please check the dataset")
}

################################
## DEFINING GLOBAL PARAMETERS ##
################################

# Define the number of questions for the given exam
N_Questions_V22 <- 20

# Define the number of candidates for the given exam
N_Candidates_V22 <- length(RTMB_V22$kommisjon)

# Initialize an empty vector to store the number of unique categories
N_categories_V22 <- c()

# Use a for loop to calculate the number of unique categories for each question Q1 to Q20
for (i in 1:N_Questions_V22) {
  N_categories_V22 <- c(N_categories_V22, length(unique(RTMB_V22$Y[,i])))
}

# Creating a sorted list with all the unique values in each category
Values_categories_V22 <- list()

for (i in 1:N_Questions_V22){
  Values_categories_V22[[paste0("Q",i)]] <- sort(as.double(unique(RTMB_V22$Y[,i])))
}

###########################################
## CALCULATING THRESHOLDS FOR PARAMETERS ##
###########################################

# Initialize an empty vector to store the
alpha_true_values_V22 <- list()

count_unique_values <- function(matrix_data, column_index) {
  
  column_data <- matrix_data[, column_index]
  result <- as.data.frame(table(column_data))
  colnames(result) <- c("value", "count")
  
  return(result)
}

# Creating the true alpha values based on the number of responses in each category
for (q in 1:N_Questions_V22) {
  
  # Counting the responses in each category
  y <- count_unique_values(RTMB_V22$Y, q)
  print(y)
  
  # Initialize two empty vector to store the thresholds
  alphas  <- rep(0, length(y$count))
  thetas  <- rep(0, length(y$count))
  
  # Initialize the cumulative count in the categories looped through 
  y_cum <- 0
  
  # 
  print(Values_categories_V22[[paste0("Q",q)]])
  for (k in 1:length(unique(Values_categories_V22[[paste0("Q",q)]]))){
    
    # Updating the cumulative count
    y_cum <- y_cum + y[k, "count"]
    
    # Calculating the thetas for the probabilities to be correct
    thetas[k] <- as.double(-log(1/(y_cum/N_Candidates_V22)-1))
    
    # Transforming the thetas to alphas
    if (k == 1){
      alphas[k] <- thetas[k]
    } else {
      alphas[k] <- log(thetas[k]-thetas[k-1])
    }  
  }
  
  #Adding the true values to the vector
  print(alphas)
  alpha_true_values_V22[[paste0("Q",q)]] <- alphas
}

#########################
## CREATING PARAMETERS ##
#########################

parameters_V22C <- list(
  # Random effects
  s       = rep(0, length(unique(RTMB_V22$kommisjon))),       # Examiner random effects
  gamma   = rep(0, length(RTMB_V22$kommisjon)),               # Student random effects
  
  # Standard deviation
  log_sd_gamma  = log(1),   # Std deviation for students
  log_std_s     = log(1),   # Std deviation for examiners
  
  alpha = rep(0, 18)
)

# Thresholds
#for (i in 1:18) {
#  parameters_V22C[[paste0("alpha", i)]] <- alpha_true_values_V22[[paste0("Q",i)]][-length(alpha_true_values_V22[[paste0("Q",i)]])]
#}

###########################
## CREATING THE FUNCTION ##
###########################

f <- function(parms) {
  # Make variables local from the parameter list
  getAll(RTMB_V22, parms, warn = FALSE)

  # Initialize negative log-likelihood
  nll <- 0
  
  # Loop over all 20 questions
  for (q in 1:18){
    
    # Extract the number of categories and corresponding thresholds for this question
    theta <- c(alpha[q])
    
    # The data vector
    Y_q  <- as.integer(Y[,q])
    
    for (i in 1:length(Y_q)){
    
      if (Y_q[i] == 0) {
        nll  <- nll - sum(log(pnorm(gamma[i] + theta[Y_q[i]]) - 0))
      } else if (Y_q[i] == 1) {
        # Code block executes if condition1 is FALSE and condition2 is TRUE
      } else {
        # Code block executes if neither condition1 nor condition2 is TRUE
      }
      qnll <- sum(log(plogis(gamma + theta[Y_q+2]) - plogis(gamma + theta[Y_q+1]) + epsilon))
      nll  <- nll - sum(log(plogis(gamma + theta[Y_q+2]) - plogis(gamma + theta[Y_q+1]) + epsilon))
    }
    
  }
  
  # Add random effects for students (gamma) and examiners (s) to the likelihood
  nll <- nll - sum(dnorm(gamma, mean = 0, sd = exp(log_sd_gamma), log = TRUE))
  #nll <- nll - sum(dnorm(s    , mean = 0, sd = exp(log_std_s)   , log = TRUE))
  
  #Returning the negative log-likelihood
  return(nll)
}

###############################################
## CREATE THE OBJECTIVE FUNCTION AND FITTING ##
###############################################

obj_V22C <- MakeADFun(
  func = f,                     # Function for the negative log-likelihood
  parameters = parameters_V22C, # Parameters to be estimated
  random = c("gamma")          # Random effects for examiners and students
)

# FIT THE MODEL
fit_V22C <- nlminb(start = obj_V22C$par, objective = obj_V22C$fn, gradient = obj_V22C$gr)

# CALCULATING THE MODEL OUTPUT
sdr_V22C <- sdreport(obj_V22C)
summary(sdr_V22C)
