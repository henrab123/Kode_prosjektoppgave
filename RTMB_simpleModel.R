#Formaterer data for ?? passe inn...
RTMB_test_data_V23$i    <- 0
RTMB_test_data_V23$ii   <- 0
RTMB_test_data_V23$iii  <- 0
RTMB_test_data_V23$iv   <- 0
RTMB_test_data_V23$v    <- 0
RTMB_test_data_V23$vi   <- 0
RTMB_test_data_V23$vii  <- 0
RTMB_test_data_V23$viii <- 0
RTMB_test_data_V23$ix   <- 0
RTMB_test_data_V23$x    <- 0
RTMB_test_data_V23$xi   <- 0

for (i in 1:nrow(RTMB_test_data_V23)) {
  if (RTMB_test_data_V23$kommisjon[i] == 1) {
    RTMB_test_data_V23$i[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 2) {
    RTMB_test_data_V23$ii[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 3) {
    RTMB_test_data_V23$iii[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 4) {
    RTMB_test_data_V23$iv[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 5) {
    RTMB_test_data_V23$v[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 6) {
    RTMB_test_data_V23$vi[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 7) {
    RTMB_test_data_V23$vii[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 8) {
    RTMB_test_data_V23$viii[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 9) {
    RTMB_test_data_V23$ix[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 10) {
    RTMB_test_data_V23$x[i] <- 1
  }
  if (RTMB_test_data_V23$kommisjon[i] == 11) {
    RTMB_test_data_V23$xi[i] <- 1
  }
}


#######################
####   RTMB-test   ####
#######################

RTMB_V23_simple_parameters <- list(
  mua = 0,
  sda = 1,
  sdeps = 1,
  a = rep(0, 11)
)

f <- function(params){
  #Accessing the parameters
  getAll(RTMB_test_data_V23, params, warn=FALSE)
  
  #Defining response
  score <- OBS(scoreDiff)
  
  #Initialize joint negative log likelihood
  nll <- 0
  
  #Random intercepts:
  nll <- nll - sum(dnorm(a, mean=mua, sd=sda, log=TRUE))
  
  #Data
  predkomm <- a[kommisjon] 
  nll <- nll - sum(dnorm(score, predkomm, sd=sdeps, log=TRUE))
  
  #Get predicted weight uncertainties:
  ADREPORT(predkomm)
  
  #Returning:
  nll
}

#Processing function:
obj <- MakeADFun(f, RTMB_V23_simple_parameters, random = c("a"))

#Fitting the model
opt <- nlminb(obj$par, obj$fn, obj$gr)

#Calculating uncertainties
sdr <- sdreport(obj)
sdr

#Simulating for checking the model:
set.seed(1)
chk <- checkConsistency(obj)
chk