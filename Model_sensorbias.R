library(RTMB)
set.seed(1)

################################
## DEFINING GLOBAL PARAMETERS ##
################################

RTMB_V22
RTMB_V23

###########################################
## CALCULATING THRESHOLDS FOR PARAMETERS ##
###########################################

calculating_true_thresholds <- function(data){
  
  # List to store thresholds
  true_thresholds <- list()
  
  # Looping over each question
  for (q in 1:ncol(data$Y)) {
    
    # Counting the responses
    n_thresholds_q <- length(unique(data$Y[,q])) - 1
    
    # Vectors to store the thresholds
    alphas  <- rep(0, n_thresholds_q)
    thetas  <- rep(0, n_thresholds_q)
    
    # Cumulative counting
    y_cum <- 0
    
    for (k in 1:n_thresholds_q){
      
      # Updating the cumulative count
      y_cum <- y_cum + sum(data$Y[,q] == k-1)
      
      # Calculating the thetas
      thetas[k] <- qnorm(y_cum/nrow(data$Y))
      
      # Transforming the thetas to alphas
      if (k == 1){
        alphas[k] <- thetas[k]
      } else {
        alphas[k] <- log(thetas[k]-thetas[k-1])
      }  
    }
    
    #Adding the true values to the vector
    true_thresholds[[paste0("Q",q)]] <- alphas
  }
  
  return(true_thresholds)
}

# Calculating the thresholds
alpha_true_values_V22 <- calculating_true_thresholds(RTMB_V22)
alpha_true_values_V23 <- calculating_true_thresholds(RTMB_V23)

#########################
## CREATING PARAMETERS ##
#########################

par_V22 <- list(
  # Random effects
  s     = rep(0, length(unique(RTMB_V22$kommisjon))),  # Examiner random effects
  gamma = rep(0, length(RTMB_V22$kommisjon)),          # Student random effects
  
  # Standard deviation
  log_std_s     = log(1),   # Std deviation for examiners
  log_sd_gamma  = log(1)    # Std deviation for students
)

# Thresholds par_V22
for (i in 1:ncol(RTMB_V22$Y)) {
  par_V22[[paste0("alpha", i)]] <- alpha_true_values_V22[[paste0("Q",i)]]
}

par_V23 <- list(
  # Random effects
  s       = rep(0, length(unique(RTMB_V23$kommisjon))),       # Examiner random effects
  gamma   = rep(0, length(RTMB_V23$kommisjon)),               # Student random effects
  
  # Standard deviation
  log_std_s     = log(1),   # Std deviation for examiners
  log_sd_gamma  = log(1)    # Std deviation for students
)

# Thresholds par_V23
for (i in 1:ncol(RTMB_V23$Y)) {
  par_V23[[paste0("alpha", i)]] <- alpha_true_values_V23[[paste0("Q",i)]]
}

############################################
## CREATING THE FUNCTION TO CALCULATE NLL ##
############################################

f <- function(parms){
  
  # Making the variable local
  getAll(data, parms, warn = FALSE)
  
  #Initializing the nll:
  nll <- 0
  
  # Loop over all the questions
  for (q in 1:ncol(Y)){
    
    # Extracting the thresholds for question q:
    alpha_q <- parms[[paste0("alpha",q)]]
    
    # Transforming from alphas to thetas
    for (j in 1:length(alpha_q)){
      if (j == 1){
        theta <- alpha_q[1]
      } else {
        theta <- c(theta, theta[j-1] + exp(alpha_q[j]))
      }
    }
    
    # Loop over each student:
    for (i in 1:nrow(Y)){
      
      # Checking for automatic or manual q
      if (length(theta) == 1){
        eta = gamma[i]                             # Automatic corrected q
      } else {
        eta = gamma[i] + s[kommisjon[i]]           # Manually  corrected q
      } 
      
      # Updating the nll:
      if (Y[i,q] == 0) { 
        nll  <- nll - log(pnorm(-eta + theta[Y[i,q]+1]) - 0)                          # Response in the first category
      } else if (Y[i,q] == length(theta)) {
        nll  <- nll - log(1 - pnorm(-eta + theta[Y[i,q]]))                            # Response in the last category
      } else {
        nll  <- nll - log(pnorm(-eta + theta[Y[i,q]+1]) - pnorm(-eta + theta[Y[i,q]])) # Response in the Y_iq-1 category
      }
    }
  }
  
  # Adding random effects 
  nll <- nll - sum(dnorm(gamma, mean = 0, sd = exp(log_sd_gamma), log = TRUE))
  nll <- nll - sum(dnorm(s,     mean = 0, sd = exp(log_std_s)    , log = TRUE))
  
  #returning nll:
  return(nll)
}

###############################################
## CREATE THE OBJECTIVE FUNCTION AND FITTING ##
###############################################

creating_obj <- function(f, data, parameter, random_effects){
  obj <- MakeADFun(func = f, parameters = parameter, random = random_effects)
  fit <- nlminb(start = obj$par, objective = obj$fn, gradient = obj$gr)
  return(obj)
}

# Calculating V22
data <- RTMB_V22
obj_V22 <- creating_obj(f, RTMB_V22, par_V22, c("gamma", "s"))
sdr_V22 <- sdreport(obj_V22)

# Calcuclating V23
data <- RTMB_V23
obj_V23 <- creating_obj(f, RTMB_V23, par_V23, c("gamma", "s"))
sdr_V23 <- sdreport(obj_V23)

# Summary
summary(sdr_V22)
summary(sdr_V23)


#LRT

#The coed was first run with the s and then without the s that gave ut the different variables
L1V22 <-obj_V22$fn(obj_V22$par)
L1V23 <-obj_V23$fn(obj_V23$par)
L0V22 #<-obj_V22$fn(obj_V22$par)
L0V23 #<-obj_V23$fn(obj_V23$par)

df1=1
df2=2

lambda_V22 <- 2*(L1V22-L0V22)
lambda_V23 <- 2*(L1V23-L0V23)

p_value_V22 <- 0.5*pchisq(lambda_V22, 1, lower.tail = FALSE) + 0.5*pchisq(lambda_V22, 2, lower.tail = FALSE)
p_value_V23 <- 0.5*pchisq(lambda_V23, 1, lower.tail = FALSE) + 0.5*pchisq(lambda_V23, 2, lower.tail = FALSE)

p_value_V22
p_value_V23
