# Setting seed:
set.seed(1)

#Defining function:
simulate_data <- function(N=100, 
                          K=2, 
                          K_f=FALSE, 
                          s=c(0,0), 
                          gamma=rep(0,100), 
                          QA=2, AT=c(-0.5, 1), 
                          QM = 2, 
                          MT=matrix(c(-1,-0.5,3,4,5,6),nrow=2,ncol = 3, byrow = TRUE), 
                          model = 1){
  
 "DESCRIPTION OF THE FUNCTION:
  Input variables:
    N: Number of students
    K: Number of commissions [N must be divisible by K]
    K_f: The partition of the comissions [Must be divisible by N and K]
    SB: The mean of each of the comissions
    QA: Number of automatic corrected questions
    QM: Number of manually questions
    model: The type of model used
  
  This function simulates resualts from exams based on the model we want to evaluate. 
  "
  
  
  #Checking the inputs:
  if (length(K_f) > 1 & length(K_f) != K) {
    warning("Dimensions of K_f do not fit the value of K!")
  }
  if (length(s) != K) {
    warning("Dimensions of s do not fit the value of K!")
  }
  if (length(AT) != QA) {
    warning("Dimensions of AT do not fit the value of QA!")
  }
  if (N %% K != 0) {  # Check if N is not divisible by K
    warning("N is not evenly divisible by K!")
  }
  
  #Implementation:
  simulated_data <- NULL
  
  #Commission
  if (length(K_f) > 1){
    commission  <- c()
    for (i in 1:length(K_f)){
      commission <- c(commission, rep(i, K_f[i]*N))
    }
    simulated_data$kommisjon <- commission
  } else {
    simulated_data$kommisjon= rep(1:K, N/K)
  }

  #Candidate number:
  simulated_data$kandidatnummer <- seq(1, N)
  
  #Data:
  simulated_data$Y <- matrix(0, nrow = N, ncol = (QA + QM))
  
  #Defining models:
  if (model == 1){
    eta_a <- function(i) gamma[i]
    eta_m <- function(i) gamma[i] + s[simulated_data$kommisjon[i]]
  }
  
  for (q in 1:QA){
    for (i in 1:N){
      eta_pred <- eta_a(i)
      if (eta_pred > AT[q]){
        simulated_data$Y[i,q] <- 1
      }
    }
  }
  
  for (q in 1:QM){
    for (i in 1:N){
      eta_pred <- eta_m(i)
      for (j in 1:length(MT[1,])){
        if(eta_pred > MT[q,j]){
          simulated_data$Y[i,q+QA] <- j
        }
      }
    }
  }


  return(data.frame(kommisjon = simulated_data$kommisjon,
                    kandidatnummer = simulated_data$kandidatnummer,
                    gamma = gamma,
                    Y = I(simulated_data$Y)))
}

#debugonce(simulate_data)
#SD_2022 <- simulate_data()
#SD_2022B <- simulate_data(N=100, K=4, K_f=c(1/4, 2/4, 1/4, 0/4), QA = 15, QM = 2, model = 0)

#Lets test it:
N = 100
K = 2

mu_gamma = 0
mu_s     = 0

log_std_gamma = log(1)
log_std_s     = log(1)

QA = 10
QM = 4
Lvl = 20

Theta_a <- c(-1.5, seq(-1, 1, ((1 - -1)/(QA - 3))), 1.5)
Theta_m <- t(apply(matrix(runif(QM*Lvl, -3, 3), QM, Lvl), 1 , sort))
  
gamma <- rnorm(N, mu_gamma, exp(log_std_gamma))
s     <- rnorm(K, mu_s    , exp(log_std_s    ))

SD_1_par   <- list(N=N, K=K, mu_gamma=mu_gamma, mu_s=mu_s, log_std_gamma=log_std_gamma, log_std_s=log_std_s, QA=QA, QM=QM, Theta_a=Theta_a, Theta_m=Theta_m, gamma=gamma, s=s)
SD_1_data  <- simulate_data(N=N, 
                          K=K, 
                          K_f=FALSE, 
                          s=s, 
                          gamma=gamma, 
                          QA=QA, 
                          AT=Theta_a, 
                          QM=QM, 
                          MT=Theta_m, 
                          model = 1)
