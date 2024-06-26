#' Simulates accelerometer data based on a hidden Markov model
#' 
#' Simulates accelerometer data. The simulation function is based on a hidden
#' Markov model, as described in the example for function acc. This function is
#' provided for convenience to generate data from a pre-specified transition
#' probabilities to mimic activity levels of low, moderate and high. To
#' generate data from a specific transition probabilities and distributions,
#' please refer to the example for function acc.
#' 
#' 
#' @param timelength Number of observations to be generated.
#' @param paLevel Pre-specified levels of physical activity for convenience.
#' User can specify all parameter as preferred, by stating the option as
#' paLevel=NULL. Default is `moderate'. Options: 1) `low', `moderate', or
#' `high'. Low specifies a hidden markov model with transition probabilities
#' 0.95, 0.04, 0.01, 0.09, 0.9, 0.01, 0.1, 0.2, 0.7, respectively for P11, P12,
#' P13, P21, P22, P23, P31, P32, P33, respectively. Moderate specifies a hidden
#' markov model with transition probabilities 0.95, 0.04, 0.01, 0.09, 0.8,
#' 0.11, 0.1, 0.1, 0.8 respectively for P11, P12, P13, P21, P22, P23, P31, P32,
#' P33, respectively. High specifies a hidden markov model with transition
#' probabilities 0.95, 0.04, 0.01, 0.09, 0.7, 0.21, 0.1, 0.1, 0.8, respectively
#' for P11, P12, P13, P21, P22, P23, P31, P32, P33, respectively. For all
#' levels, it is assumed that the activity intensities are realized from a
#' mixture of two normal distributions (for sedentary activity and mvpa) and a
#' constant at zero (for non-wear time), with means mu = c(0, 30, 2500) and
#' variance sigma = c(0, 30, 1000).
#' @param epoch Epoch size. User can specify desired epoch size in units of
#' time larger than seconds. Defaults to 1 minute epoch.
#' @param startDate Start date in ISOdate format. For example
#' ISOdate(2017,1,1,hour=0,min=0,sec=0,tz="GMT").
#' @param endDate End date in ISOdate format. For example
#' ISOdate(2017,1,1,hour=0,min=0,sec=0,tz="GMT").
#' @param mu Mean levels for each activity type.
#' @param sigma Standard deviations for each activity type.
#' @param seedset Sets seed for random data generation. Defaults to 1234.
#' @param tpm Transition probability matrix that specify probability of change
#' from one activity state to another.
#' @return A simulated dataset is returned with two columns: [TimeStamp,
#' counts]
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @keywords accelerometer
#' @examples
#' 
#' ##
#' ## Example: Simulate a dataset for two days, for an individual with low activity level.
#' ##
#' mvpaLowData <- simAcc(timelength=(60*24*2),paLevel='low')
#' summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
#'                      spuriousDef=20, nonwearDef=60, minWear=600, 
#'                      patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
#'                      boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
#' summary
#' 
#' ##
#' ## Example: Simulate a dataset for two days, for an individual with moderate activity level.
#' ##
#' mvpaLowData <- simAcc(timelength=(60*24*2),paLevel='moderate')
#' summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
#'                      spuriousDef=20, nonwearDef=60, minWear=600, 
#'                      patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
#'                      boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
#' summary
#' 
#' ##
#' ## Example: Simulate a dataset for two days, for an individual with high activity level.
#' ##
#' mvpaLowData <- simAcc(timelength=(60*24*2),paLevel='high')
#' summary <- acc(data=mvpaLowData, tri='FALSE', axis=NULL,
#'                      spuriousDef=20, nonwearDef=60, minWear=600, 
#'                      patype=c('Sedentary','MVPA'),pacut=c(c(0,99),c(1952,Inf)), 
#'                      boutsize=c(10,10), tolerance=c('FALSE','TRUE'))
#' summary
#' 
#' 
#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom PhysicalActivity dataCollapser
simAcc <- function(timelength,paLevel='moderate',epoch="1 min",
                   startDate=ISOdate(2017,1,1,hour=0,min=0,sec=0,tz="GMT"),
                   endDate=ISOdate(2017,1,10,hour=24,min=0,sec=0,tz="GMT"),
                   mu = c(0, 30, 2500),
                   sigma = c(0, 30, 1000),
                   seedset=1234,tpm=NULL){
  
  
  randomTime <- seq(startDate,endDate,epoch)
  
  # User specified 
  if( is.null(paLevel)==TRUE ){
    
    b <- list(mu = mu, sigma = sigma)
    J <- length(mu); initial <- rep(1/J, J)
    model <- hmmspec(init = initial, trans = tpm, parms.emission = b,dens.emission = dnorm.hsmm)
    train <- simulate.hmmspec(model, nsim = (timelength), seed = seedset, rand.emission = rnorm.hsmm)
    simdata <- data.frame(TimeStamp = randomTime[1:timelength], counts = round(train$x,0))
    
  }
  
  # Using pre set parameters 
  if( is.null(paLevel)==FALSE ){
    
    mu = c(0, 30, 2500)
    sigma = c(0, 30, 1000)
    
    J <- 3; initial <- rep(1/J, J)
    P <- matrix(rep(NA,9),byrow='TRUE',nrow=J)
    
    if(paLevel=='low'){
      P <- matrix(c(0.95, 0.04, 0.01, 
                    0.09, 0.9, 0.01, 
                    0.1, 0.2, 0.7), byrow='TRUE',nrow = J)
    }
    
    if(paLevel=='moderate'){
      P <- matrix(c(0.95, 0.04, 0.01, 
                    0.09, 0.8, 0.11, 
                    0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
    }
    
    if(paLevel=='high'){
      P <- matrix(c(0.95, 0.04, 0.01, 
                    0.09, 0.7, 0.21, 
                    0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
    }
    
    b <- list(mu = mu, sigma = sigma)
    model <- hmmspec(init = initial, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
    train <- simulate.hmmspec(model, nsim = (timelength), seed = seedset, rand.emission = rnorm.hsmm)
    simdata <- data.frame(TimeStamp = randomTime[1:timelength], counts = round(train$x,0))
  }
  
  simdata
}
