#Formatted data for ?? passe inn...
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
  mu1 = 0,
  sd1 = 1,
  mu2 = 0,
  sd2 = 1,
  mu3 = 0,
  sd3 = 1,
  mu4 = 0,
  sd4 = 1,
  mu5 = 0,
  sd5 = 1,
  mu6 = 0,
  sd6 = 1,
  mu7 = 0,
  sd7 = 1,
  mu8 = 0,
  sd8 = 1,
  mu9 = 0,
  sd9 = 1,
  mu10 = 0,
  sd10 = 1,
  mu11 = 0,
  sd11 = 1,
  sdeps = 1,
  a =    rep(0,11),
  ri =    rep(0, 11),
  rii =   rep(0, 11),
  riii =  rep(0, 11),
  riv =   rep(0, 11),
  rv =    rep(0, 11),
  rvi =   rep(0, 11),
  rvii =  rep(0, 11),
  rviii = rep(0, 11),
  rix =   rep(0, 11),
  rx =    rep(0, 11),
  rxi =   rep(0, 11)
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
  
  #Random slopes:
  nll <- nll - sum(dnorm(ri,    mean=mu1,  sd=sd1,  log=TRUE))
  nll <- nll - sum(dnorm(rii,   mean=mu2,  sd=sd2,  log=TRUE))
  nll <- nll - sum(dnorm(riii,  mean=mu3,  sd=sd3,  log=TRUE))
  nll <- nll - sum(dnorm(riv,   mean=mu4,  sd=sd4,  log=TRUE))
  nll <- nll - sum(dnorm(rv,    mean=mu5,  sd=sd5,  log=TRUE))
  nll <- nll - sum(dnorm(rvi,   mean=mu6,  sd=sd6,  log=TRUE))
  nll <- nll - sum(dnorm(rvii,  mean=mu7,  sd=sd7,  log=TRUE))
  nll <- nll - sum(dnorm(rviii, mean=mu8,  sd=sd8,  log=TRUE))
  nll <- nll - sum(dnorm(rix,   mean=mu9,  sd=sd9,  log=TRUE))
  nll <- nll - sum(dnorm(rx,    mean=mu10, sd=sd10, log=TRUE))
  nll <- nll - sum(dnorm(rxi,   mean=mu11, sd=sd11, log=TRUE))
  
  #Data
  predkomm <- a[kommisjon] + ri[kommisjon]     * RTMB_test_data_V23$i
                           + rii[kommisjon]    * RTMB_test_data_V23$ii
                           + riii[kommisjon]   * RTMB_test_data_V23$iii
                           + riv[kommisjon]    * RTMB_test_data_V23$iv
                           + rv[kommisjon]     * RTMB_test_data_V23$v
                           + rvi[kommisjon]    * RTMB_test_data_V23$vi
                           + rvii[kommisjon]   * RTMB_test_data_V23$vii
                           + rviii[kommisjon]  * RTMB_test_data_V23$viii
                           + rix[kommisjon]    * RTMB_test_data_V23$ix
                           + rx[kommisjon]     * RTMB_test_data_V23$x
                           + rxi[kommisjon]    * RTMB_test_data_V23$xi
  
  nll <- nll - sum(dnorm(score, predkomm, sd=sdeps, log=TRUE))
  
  #Get predicted weight uncertainties:
  ADREPORT(predkomm)
  
  #Returning:
  nll
}

#Processing function:
obj <- MakeADFun(f, RTMB_V23_simple_parameters, random = c("a", "ri", "rii", "riii", "riv", "rv", "rvi", "rvii", "rviii", "rxi", "rx", "rxi"))

#Fitting the model
opt <- nlminb(obj$par, obj$fn, obj$gr)

#Calculating uncertainties
sdr <- sdreport(obj)
sdr

#Simulating for checking the model:
set.seed(1)
chk <- checkConsistency(obj)
chk