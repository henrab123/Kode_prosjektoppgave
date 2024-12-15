#####################################
## DEFINING FUNCTIONS TO USE AFTER ##
#####################################

alpha_to_theta <- function(alphas){
  for (j in 1:length(alphas)){
    if (j == 1){
      thetas <- alphas[1]
    } else {
      thetas <- c(thetas, thetas[j-1] + exp(alphas[j]))
    }
  }
  return(thetas)
}

theta_to_alpha <- function(thetas){
  for (k in 1: length(thetas)){
    if (k == 1){
      alphas <- thetas[k]
    } else {
      alphas <- c(alphas, log(thetas[k]-thetas[k-1]))
    }
  }
  return(alphas)
}

calculating_num_of_alphas <- function(Y){
  
  n_alpha <- c()
  
  for (i in 1:ncol(Y)){
    
    num_cat <- length(unique(Y[,i]))
    n_alpha <- c(n_alpha, num_cat - 1)
  }
  
  return(n_alpha)
}

a <- calculating_num_of_alphas(RTMB_V22$Y)

######################################
## GETTING THE ESTIMASTES FROM RTMB ##
######################################


summary_V22 <- summary(sdr_V22, select = c("all", "fixed", "random", "report"), p.value = TRUE)
summary_V23 <- summary(sdr_V23, select = c("all", "fixed", "random", "report"), p.value = TRUE)

ordering_estimators <- function(summary_XX, data){
  
  log_sd_s     <- summary_XX[1,]
  log_sd_gamma <- summary_XX[2,]
  
  num_alpha  <- calculating_num_of_alphas(data$Y) 
  
  alpha      <- list()
  end_index  <- 3
  
  for (i in 1: length(num_alpha)){
    start_index <- end_index
    end_index   <- end_index + num_alpha[i]
    alpha[[i]]  <- summary_XX[start_index:(end_index-1),]
  }
  
  start_index  <- end_index
  
  s            <- summary_XX[start_index:(end_index+length(unique(data$kommisjon))-1),]
  start_index  <- end_index + length(unique(data$kommisjon))
  
  gamma        <- summary_XX[start_index:length(summary_XX[,1]),]
  
  return(list(
    log_sd_gamma = log_sd_gamma,
    log_sd_s     = log_sd_s,
    alpha        = alpha,
    s            = s,
    gamma        = gamma
    ))
}

#debugonce(ordering_estimators)
ordering_estimators(summary_V22, RTMB_V22)


# HERE!!
estimated_parameters_V22 <- ordering_estimators(summary_V22, RTMB_V22)
estimated_parameters_V23 <- ordering_estimators(summary_V23, RTMB_V23)

#############################
## PREDICTING THE RESPONSE ##
#############################

deterministic_prediction <- function(estimated_par, data, s=TRUE){
  
  eta           <- rep(0, length(data$kandidatnummer))
  eta_without_s <- rep(0, length(data$kandidatnummer))
  
  for (i in 1:length(data$kandidatnummer)){
    eta[i]           <- -estimated_par$gamma[i,1] - estimated_par$s[data$kommisjon[i],1]
    eta_without_s[i] <- -estimated_par$gamma[i,1] 
  }
  
  predictor <- eta_without_s

  Y_predicted             <- matrix(0, ncol=ncol(data$Y), nrow=nrow(data$Y))
  
  n_alpha <- calculating_num_of_alphas(data$Y)
  
  for (q in 1:ncol(data$Y)){
    points_q <- sort(unique(data$Y_m[,q]))
    
    if (n_alpha[q] == 1){
      theta_q   <- c(alpha_to_theta(estimated_par$alpha[[q]][1]), Inf)
      predictor <- eta_without_s
      for (i in 1:nrow(data$Y)) {
        for (yij in 1:(n_alpha[q]+1)) {
          if (predictor[i] < theta_q[yij]){
            Y_predicted[i,q] <- yij-1
            break
          }
        }
      }
    } else {
      theta_q <- c(alpha_to_theta(estimated_par$alpha[[q]][,1]), Inf)
      if (s){
        predictor <- eta
      }
      for (i in 1:nrow(data$Y)) {
        for (yij in 1:(n_alpha[q])) {
          Y_predicted[i,q] <- Y_predicted[i,q] + (pnorm(theta_q[yij+1]+predictor[i])-pnorm(theta_q[yij]+predictor[i]))*points_q[yij+1]
          a <- Y_predicted[i,q]
        }
      }
    }
  }
  
  return(Y_predicted)
  
}

#debugonce(deterministic_prediction)
deterministic_prediction(estimated_parameters_V23, RTMB_V23, s=FALSE)

pred_to_dataframe <- function(predicted_Y, data){
  df <- data.frame(kommisjon = data$kommisjon,
                   kandidatnummer = data$kandidatnummer,
                   Y = predicted_Y)
  return(df)
}

