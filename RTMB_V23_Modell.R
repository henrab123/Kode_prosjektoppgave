library(RTMB)

# Setting seed for reproducibility
set.seed(1)

#Check if there are NAs in the dataset:
if (any(is.na(RTMB_V23))) {
  stop("NA values found in RTMB_V23, please check the dataset")
}

################################
## DEFINING GLOBAL PARAMETERS ##
################################

# Define the number of questions for the given exam
N_Questions_V23 <- 17

# Define the number of candidates for the given exam
N_Candidates_V23 <- length(RTMB_V23$kandidatnummer)

# Initialize an empty vector to store the number of unique categories
N_categories_V23 <- c()

# Use a for loop to calculate the number of unique categories for each question Q1 to Q20
for (i in 1:N_Questions_V23) {
  N_categories_V23 <- c(N_categories_V23, length(unique(RTMB_V23[[paste0("Q", i)]])))
}

# Creating a sorted list with all the unique values in each category
Values_categories_V23 <- list()

for (i in 1:N_Questions_V23){
  Values_categories_V23[[paste0("Q",i)]] <- sort(as.double(unique(RTMB_V23[[paste0("Q", i)]])))
}

# Create a unique mapping from "kandidatnummer" to indices
unique_kandidater <- unique(RTMB_V23$kandidatnummer)
kandidat_index    <- match(RTMB_V23$kandidatnummer, unique_kandidater)

###########################################
## CALCULATING THRESHOLDS FOR PARAMETERS ##
###########################################

# Initialize an empty vector to store the
alpha_true_values_V23 <- list()

# Function to count the amount of each unique value in a column
count_unique_values <- function(data, column_name) {
  
  result <- data %>%
    group_by(!!sym(column_name)) %>%
    summarise(count = n()) %>%
    ungroup()
  
  return(result)
}

# Creating the true alpha values based on the number of responses in each category
for (i in 1:N_Questions_V23) {
  
  # Counting the responses in each category
  y <- count_unique_values(RTMB_V23, paste0("Q",i))
  
  # Initialize two empty vector to store the thresholds
  alphas  <- rep(0, length(y$count))
  thetas  <- rep(0, length(y$count))
  
  # Initialize the cumulative count in the categories looped through 
  y_cum <- 0
  
  # 
  for (k in 1:length(unique(RTMB_V23[[paste0("Q",i)]]))){
    
    # Updating the cumulative count
    y_cum <- y_cum + y[k, "count"]
    
    # Calculating the thetas for the probabilities to be correct
    thetas[k] <- as.double(-log(1/(y_cum/N_Candidates_V23)-1))
    
    # Transforming the thetas to alphas
    if (k == 1){
      alphas[k] <- thetas[k]
    } else {
      alphas[k] <- log(thetas[k]-thetas[k-1])
    }  
  }
  
  #Adding the true values to the vector
  alpha_true_values_V23[[paste0("Q",i)]] <- alphas
}

#########################
## CREATING PARAMETERS ##
#########################

parameters_V23 <- list(
  # Random effects
  s       = rep(0, length(unique(RTMB_V23$kommisjon))),       # Examiner random effects
  gamma   = rep(0, length(unique(RTMB_V23$kandidatnummer))),  # Student random effects
  
  # Standard deviation
  log_sd_gamma  = 1,   # Std deviation for students
  log_std_s     = 1    # Std deviation for examiners
)

# Thresholds
for (i in 1:N_Questions_V23) {
  parameters_V23[[paste0("alpha", i)]] <- alpha_true_values_V23[[paste0("Q",i)]][-length(alpha_true_values_V23[[paste0("Q",i)]])]
}

###########################
## CREATING THE FUNCTION ##
###########################

f_V23 <- function(parms) {
  # Make variables local from the parameter list
  getAll(RTMB_V23, parms, warn = FALSE)
  
  # Initialize negative log-likelihood
  nll <- 0
  
  # Linear predictor: student and examiner effects
  eta_auto <- gamma[kandidat_index]
  eta_manu <- gamma[kandidat_index] + s[kommisjon]
  
  # Loop over all 20 questions
  for (q in 1:N_Questions_V23){
    
    # Extract the number of categories and corresponding thresholds for this question
    thresholds     <-  get(paste0("alpha", q))
    theta          <-  rep(0,length(thresholds))
    
    # Rescaling the thresholds:
    theta[1] <- thresholds[1]
    
    if (length(theta) > 1){
      for (i in 2:(length(thresholds))){
        theta[i] <- theta[i-1] + exp(thresholds[i])
      }
    }
    
    # Loop over students to compute log-likelihood
    for (i in 1:length(kandidatnummer)) {
      
      # Finding the response category for each student
      response_QI <- as.double(get(paste0("Q",q))[i])
      
      # Finding the index in the theta vector
      index <- which(Values_categories_V23[[paste0("Q",q)]] == response_QI)
      
      # Check if the question is automatic corrected
      if (length(theta) == 1){
        
        # Check if the response is in the first category
        if (index == 1){
          nll <- nll - sum(log(plogis(eta_auto + theta[index])))
        }
        
        # Check if the response is in the last category
        else if (index == N_categories_V23[q]){
          nll <- nll - sum(log(1 - plogis(eta_auto + theta[index-1])))
        }
        
        # Check if the response is in neither the first or the last category
        else {
          nll <- nll - sum(log(plogis(eta_auto + theta[index]) - plogis(eta_auto + theta[index-1])))
        }
      } 
      
      # Check if the question is manually corrected
      else {
        
        # Check if the response is in the first category
        if (index == 1){
          nll <- nll - sum(log(plogis(eta_manu + theta[index])))
        }
        
        # Check if the response is in the last category
        else if (index == N_categories_V23[q]){
          nll <- nll - sum(log(1 - plogis(eta_manu + theta[index-1])))
        }
        
        # Check if the response is in neither the first or the last category
        else {
          nll <- nll - sum(log(plogis(eta_manu + theta[index]) - plogis(eta_manu + theta[index-1])))
        }
      }
    }
  }
  
  ## Get eta uncertainties
  ADREPORT(eta_manu) 
  
  # Add random effects for students (gamma) and examiners (s) to the likelihood
  nll <- nll - sum(dnorm(gamma, mean = 0, sd = exp(log_sd_gamma), log = TRUE))
  nll <- nll - sum(dnorm(s    , mean = 0, sd = exp(log_std_s)   , log = TRUE))
  
  #Returning the negative log-likelihood
  return(nll)
}

###################################
## CREATE THE OBJECTIVE FUNCTION ##
###################################

obj_V23 <- MakeADFun(
  func = f_V23,                  # Function for the negative log-likelihood
  parameters = parameters_V23,   # Parameters to be estimated
  random = c("s", "gamma"),      # Random effects for examiners and students
)

# FIT THE MODEL
fit_V23 <- nlminb(start = obj_V23$par, objective = obj_V23$fn, gradient = obj_V23$gr)

# CALCULATING THE MODEL OUTPUT
sdr_V23 <- sdreport(obj_V23)
summary(sdr_V23)
