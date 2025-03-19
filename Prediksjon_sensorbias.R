######################################
## GETTING THE ESTIMASTES FROM RTMB ##
######################################

summary_V22_m1 <- summary(sdr_V22_m1, select = c("all", "fixed", "random", "report"), p.value = TRUE)
summary_V22_m2 <- summary(sdr_V22_m2, select = c("all", "fixed", "random", "report"), p.value = TRUE)
summary_V23_m1 <- summary(sdr_V23_m1, select = c("all", "fixed", "random", "report"), p.value = TRUE)
summary_V23_m2 <- summary(sdr_V23_m2, select = c("all", "fixed", "random", "report"), p.value = TRUE)

################################
## ORDERING OF THE ESTIMATORS ##
################################

ordering_estimators <- function(summary_XX, data, QA=FALSE, QM=FALSE){
  
  #Calculating number of alphas:
  num_alpha  <- calculating_num_of_alphas(data$Y)
  
  #Calculating the number of QA and QM:
  if(QA==FALSE && QM==FALSE){
    QM  <- sum(num_alpha > 1)
    QA  <- length(num_alpha)-QM
  }
  
  #Including log_std_s:
  log_std_s   <- tryCatch({
    summary_XX["log_std_s",, drop=FALSE]
  }, error = function(e){
    c(0,0,0,0)
  })

  #Including log_std_gamma:
  log_std_gamma   <- tryCatch({
    summary_XX["log_std_gamma",, drop = FALSE]
  }, error = function(e){
    c(0,0,0,0)
  })
  
  #Including lambda:
  lambda   <- tryCatch({
    lambda_rows <- grep("^lambda", rownames(summary_XX))
    if(length(lambda_rows) == 0) stop()
    summary_XX[lambda_rows, , drop=FALSE]
  }, error = function(e){
    lambda <- matrix(rep(c(1,0,0,0),each = QA+QM), QA+QM, 4)
  })
  
  #Including kappa:
  kappa   <- tryCatch({
    kappa_rows <- grep("^kappa", rownames(summary_XX))
    if(length(kappa_rows) == 0) stop()
    summary_XX[kappa_rows, , drop=FALSE]
  }, error = function(e){
    kappa <- matrix(rep(c(1,0,0,0),each = QM), QM, 4)
  })
  
  #Including alpha:
  alpha   <- tryCatch({
    alpha_rows <- grep("^alpha", rownames(summary_XX))
    if(length(alpha_rows) == 0) stop()
    summary_XX[alpha_rows, , drop=FALSE]
  }, error = function(e){
    alpha <- matrix(rep(c(1,0,0,0),each = QM), QM, 4)
  })
  
  
  #Including s:
  s   <- tryCatch({
    s_rows <- grep("^s", rownames(summary_XX))
    if(length(s_rows) == 0) stop()
    summary_XX[s_rows, , drop=FALSE]
  }, error = function(e){
    message("s is not working!!")
  })
  

  #Including gamma:
  gamma   <- tryCatch({
    gamma_rows <- grep("^gamma", rownames(summary_XX))
    if(length(gamma_rows) == 0) stop()
    summary_XX[gamma_rows, , drop=FALSE]
  }, error = function(e){
    message("gamma is not working!!")
  })

  #Returning included estimates:
  return(list(
    log_std_gamma = log_std_gamma,
    log_std_s     = log_std_s,
    alpha        = alpha,
    s            = s,
    gamma        = gamma,
    lambda       = lambda,
    kappa        = kappa
    ))
}

estimated_parameters_V22_m1 <- ordering_estimators(summary_V22_m1, RTMB_V22)
estimated_parameters_V22_m2 <- ordering_estimators(summary_V22_m2, RTMB_V22)

estimated_parameters_V23_m1 <- ordering_estimators(summary_V23_m1, RTMB_V23)
estimated_parameters_V23_m2 <- ordering_estimators(summary_V23_m2, RTMB_V23)

#############################
## PREDICTING THE RESPONSE ##
#############################

naive_prediction <- function(estimated_par, data, s=TRUE){
  
  #Creating the predictors:
  eta           <- rep(0, length(data$kandidatnummer))
  eta_without_s <- rep(0, length(data$kandidatnummer))
  
  #Creating an empty matrix to store the predicted results:
  Y_predicted   <- matrix(0, ncol=ncol(data$Y), nrow=nrow(data$Y))
  n_alpha       <- calculating_num_of_alphas(data$Y)
  
  for (q in 1:ncol(data$Y)){
    
    # Automatically corrected predictor:
    for (i in 1:length(data$kandidatnummer)){
      eta_without_s[i] <- -estimated_par$gamma[i,1]*estimated_par$lambda[q,1] 
    }
    
    # Each unique possible score on question q:
    points_q <- sort(unique(data$Y_m[,q]))
    
    # Automatically corrected question q
    if (n_alpha[q] == 1){
      
      #Finding the thresholds:
      theta_q   <- c(alpha_to_theta(estimated_par$alpha[[q]][1]), Inf)
      
      #Updating predictor:
      predictor <- eta_without_s
      
      #Predicting response
      for (i in 1:nrow(data$Y)) {
        for (yij in 1:(n_alpha[q]+1)) {
          if (predictor[i] < theta_q[yij]){
            Y_predicted[i,q] <- yij-1
            break
          }
        }
      }
    # Manually corrected q:
    } else {
      
      #Finding the thresholds:
      theta_q <- c(alpha_to_theta(estimated_par$alpha[[q]][,1]), Inf)
      
      # Updating predictor:
      if (s){
        for (i in 1:length(data$kandidatnummer)){
          eta[i]  <- -estimated_par$gamma[i,1]*estimated_par$lambda[q,1] - estimated_par$s[data$kommisjon[i],1]*estimated_par$kappa[q-(length(estimated_parameters_V22_m1$lambda[,1])-length(estimated_parameters_V22_m1$kappa[,1])),1]
          
        }
        predictor <- eta
      }
      
      #Predicting response
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

#debugonce(naive_prediction)

predicted_V22_m1 <- naive_prediction(estimated_parameters_V22_m1,RTMB_V22, s=TRUE)
predicted_V22_m2 <- naive_prediction(estimated_parameters_V22_m2,RTMB_V22, s=TRUE)
predicted_V23_m1 <- naive_prediction(estimated_parameters_V23_m1,RTMB_V23, s=TRUE)
predicted_V23_m2 <- naive_prediction(estimated_parameters_V23_m2,RTMB_V23, s=TRUE)

#########################################
## CREATING A DATAFRAME OF THE RESULTS ##
#########################################

pred_to_dataframe <- function(predicted_Y, data){
  num_alpha <- calculating_num_of_alphas(data$Y)
  num_auto  <- sum(unlist(num_alpha) == 1)
  num_man   <- sum(unlist(num_alpha) != 1)
  df <- data.frame(kommisjon = data$kommisjon,
                   kandidatnummer = data$kandidatnummer,
                   Y = predicted_Y,
                   Y_sum_auto = rowSums(predicted_Y[, 1:num_auto], na.rm = TRUE),
                   Y_sum_manu = rowSums(predicted_Y[, (1+num_auto):(num_man+num_auto)], na.rm = TRUE))
  return(df)
}

# Predicting:
#debugonce(pred_to_dataframe)
Predicted_points_V22_m1 <- pred_to_dataframe(predicted_V22_m1,RTMB_V22)
Predicted_points_V22_m2 <- pred_to_dataframe(predicted_V22_m2,RTMB_V22)
Predicted_points_V23_m1 <- pred_to_dataframe(predicted_V23_m1,RTMB_V23)
Predicted_points_V23_m2 <- pred_to_dataframe(predicted_V23_m2,RTMB_V23)
