#' Summarizes accelerometer data for a single type of physical activity
#' 
#' Summarizes accelerometer data for a single type of physical activity.
#' Functionality is same as function acc, except that this function provides a
#' summary for a single type of physical activity, with more detailed
#' information.
#' 
#' 
#' @param data Data which consists of two columns [TimeStamp,counts] (i.e. raw
#' accelerometer file read in by function readRaw)
#' @param tri Whether the data is from a tri-axial accelerometer. Default is
#' tri=`FALSE'. If tri=`TRUE' then option `axis' should be specified.
#' @param axis This option is only used for the tri-axial accelerometer.
#' Options are `x',`y',`z',`sum', or `vm'. Options `x', `y', or `z' can be
#' spefied to summarize data using only data from a single axis. If the option
#' 'vm' is used, the square root of the squared sum of counts from three axes
#' (i.e. \eqn{\sqrt{{x}^{2}+{y}^{2}+{z}^{2}}} are used for the summary. If the
#' option 'sum' is used, sum of the counts from three axes are used.
#' @param spuriousDef Definition of spurious observation. Defined as minutes of
#' consecutive zeros. For example, if spuriousDef = 20, this means that an
#' observation point will be determined as a spurious observation if there are
#' consequtive counts of at least 20 zeros before and after the single non-zero
#' observation. Default is spuriousDef = 20.
#' @param nonwearDef Definition of non-wear time. Defined as minutes of
#' consecutive zeros. For example, if nonwearDef=60, this means that a period
#' will be defined as non-wear time if there are at least 60 consecutive zeros.
#' Default is nonwearDef=60. To consider all observations as wear time specify
#' nonwearDef=`Inf'
#' @param minWear Minimum wear time definition. Defined as minutes of wear
#' time. or example, if minWear = 600, this means that a day will be considered
#' valid only if the wear time is at least 600 minutes. Default is minWear =
#' 600. To return summary for all dates in the data, set minWear = 0.
#' @param patype Type of physical activity for summary. For example, to
#' summarize sedentary activity, use option patype=c(`Sedentary'). To summarize
#' moderate-vigorous physical activities, user specifies patype=c(`MVPA'). This
#' labels the summary accordingly.
#' @param pacut Cut points to be used for the physical activity type. For
#' example, if the user specified patype=c(`Sedentary'), pacut can be specified
#' as pacut=c(c(0,99)). The options requires to have a lower and a upper limit
#' for each activity type (i.e. c(0,99) for sedentary activity). The specified
#' interval includes its lower and upper endpoints (it is a closed inerval).
#' @param boutsize Boutsize to summarize a physical activity. If multiple
#' patype is specified, boutsize should be for each one (e.g., if
#' patype=c(`Sedentary') then one can use boutsize=c(10)).
#' @param epoch Epoch size. Default is '1 min'. Other epoch size can be
#' specified using this option (e.g., '1 sec')
#' @param tolerance Whether two observations outside the physical activity cut
#' point should be permitted in summarizing a physical activity (e.g. if
#' patype=c(`Sedentary') then one can use tolerance=c(`FALSE')).
#' @param returnbout Whether to return data with bout indicators. If
#' returnbout='FALSE' then only a summary of daily physical activity is
#' returned.
#' @return If returnbout='FALSE', then a summary for each specified physical
#' activity types (number of bouts and minutes of the activity), for valid
#' dates. Defaults to 'TRUE'.
#' 
#' If returnbout='TRUE', then a list of summary object is returned with:
#' \item{totalDates}{Number of unique days available in data.}
#' \item{validDates}{A summary for each specified physical activity types
#' (number of bouts and minutes of the activity), for valid dates (as defined
#' by minWear).} \item{PA}{Data supplied by the user is returned with three
#' additional columns [inPA, nonwear, inboutPA], indicating whether the
#' observation was considered to be in the defined cutpoint of the physical
#' activity (inPA), in the defined cutpoint of nonwear time (nonwear), or in
#' bout (inboutPA), respectively.}
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @references Choi, L., Liu, Z., Matthews, C.E. and Buchowski, M.S.  (2011).
#' Validation of Accelerometer Wear and Nonwear Time Classification Algorithm.
#' Med Sci Sports Exerc, 43(2):357-64.
#' 
#' Hall, K. S., Howe, C. A., Rana, S. R., Martin, C. L., and Morey, M. C.
#' (2013). METs and Accelerometry of Walking in Older Adults: Standard versus
#' Measured Energy Cost. Medicine and Science in Sports and Medicine, 45(3).
#' 574-82.
#' 
#' Freedson, P., Melanson, E., and Sirard, J. (1998). Calibration of the
#' Computer Sciences and Applications, Inc. accelerometer. Medicine and Science
#' in Sports and Exercercise, 30(5):777-81.
#' 
#' Swartz, A. M., Strath, S. J., Bassett, D. R. Jr., O'Brien, W. L., King, G.
#' A., and Ainsworth, B. E. (2000). Estimation of energy expenditure using CSA
#' accelerometers at hip and wrist sites. Medicine and Science in Sports and
#' Exercercise, 32: S450-456.
#' 
#' Copeland, J. L., and Esliger, D. W. (2009). Accelerometer assessment of
#' physical activity in active, healthy older adults. J Aging Phys Act, 17:
#' 17-30.
#' @keywords accelerometer
#' @examples
#' 
#' ##
#' ## Example 1: Loading the activity counts data using readCounts function
#' ##      
#' \dontrun{
#' library(acc)
#' infile <- "DataName.dat"
#' counts <- readCounts(infile)
#' 
#' 
#' ##
#' ## Example 2: Summarizing accelerometer data for a sedentary individual"  
#' ##        
#' 
#' # For this example, data is generated using a Hidden Markov model
#' # First, a sequence of time is generated
#' randomTime <- seq(ISOdate(2015,4,1),ISOdate(2015,4,3),"min")
#' # Load the mhsmm package to generate data using a Hidden Makov model
#' library(mhsmm)
#' # It is assumed that the counts are generated from a Hidden Markov model 
#' # with three states, being non-wear, sedentary, and moderate-vigorous activity
#' J <- 3; initial <- rep(1/J, J)
#' # Set up a transition matrix for the Hidden Markov model.
#' P <- matrix(c(0.95, 0.04, 0.01, 
#'              0.09, 0.9, 0.01, 
#'              0.1, 0.2, 0.7), byrow='TRUE',nrow = J)
#' # It is assumed that the counts are realized from a mixture of
#' # two normal distributions (for sedentary activity and mvpa) 
#' # and a constant at zero (for non-wear time).
#' b <- list(mu = c(0, 30, 2500), sigma = c(0, 30, 1000))
#' model <- hmmspec(init = initial, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
#' # Generate data!
#' train <- simulate.hmmspec(model, nsim = (60*24*2), seed = 1234, rand.emis = rnorm.hsmm)
#' # Now set up a dataset that mimicks the accelerometry data
#' counts <- data.frame(TimeStamp = randomTime[1:length(train$x)], counts = train$x)
#' library(acc)
#' # summarize the data using the acc function.
#' # Sedentary and moderate-vigorous activity is summarized, using Freedson's cut points by default.
#' # Option returnbout='TRUE' returns a more detailed information on how the summary was calculated.
#' summary1 <- accSummary(data=counts, tri='FALSE', axis=NULL,
#'                      spuriousDef=20, nonwearDef=60, minWear=600, 
#'                      patype='MVPA',pacut=c(1952,Inf), 
#'                      boutsize=10, tolerance='TRUE',returnbout='TRUE')
#' summary1$validDates # This returns the same summary as when returnbout='FALSE'
#' # summary1$PA # This returns the activity classification and bout information
#' 
#' ##
#' ## Example 3: Summarizing accelerometer data for an active individual.
#' ##
#' 
#' randomTime <- seq(ISOdate(2015,4,1),ISOdate(2015,4,3),"min")
#' library(mhsmm)
#' J <- 3; initial <- rep(1/J, J)
#' P <- matrix(c(0.95, 0.04, 0.01, 
#'              0.09, 0.7, 0.21, 
#'              0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
#' b <- list(mu = c(0, 30, 2500), sigma = c(0, 30, 1000))
#' model <- hmmspec(init = initial, trans = P, parms.emission = b,dens.emission = dnorm.hsmm)
#' train <- simulate.hmmspec(model, nsim = (60*24*2), seed = 1234, rand.emission = rnorm.hsmm)
#' 
#' counts <- data.frame(TimeStamp = randomTime[1:length(train$x)], counts = train$x)
#' library(acc)
#' # Option returnbout='TRUE' returns a more detailed information on how the summary was calculated.
#' summary2 <- accSummary(data=counts, tri='FALSE', axis=NULL,
#'                      spuriousDef=20, nonwearDef=60, minWear=600, 
#'                      patype='MVPA',pacut=c(1952,Inf), 
#'                      boutsize=10, tolerance='TRUE',returnbout='TRUE')
#' summary2$validDates # This returns the same summary as when returnbout='FALSE'
#' # summary2$PA # This returns the activity classification and bout information
#' 
#' }
#' 
#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom PhysicalActivity dataCollapser
accSummary <- function (data, tri = "FALSE", axis = "NULL", spuriousDef = 20, 
                         nonwearDef = 60, minWear = 600, patype = c("MVPA"), pacut = c(1952), 
                         epoch = "1 min", boutsize = 10, tolerance = "TRUE", returnbout = "TRUE") 
{
  
  
  #data=testData; tri='TRUE'; axis='vm';spuriousDef=20; nonwearDef=60; minWear=600;patype='MVPA';pacut=c(1952,Inf);epoch='10 sec';boutsize=10; tolerance='TRUE';returnbout='FALSE'
  
  
  isSec <- grepl("sec", epoch) | grepl("sec.", epoch) | grepl("seconds", 
                                                              epoch) | grepl("Seconds", epoch) | grepl("Sec", epoch) | 
    grepl("Sec.", epoch)
  isMin <- grepl("min", epoch) | grepl("Min", epoch) | grepl("minutes", 
                                                             epoch) | grepl("Min", epoch) | grepl("Min.", epoch) | 
    grepl("Minutes", epoch)
  epLength <- as.numeric(unlist(strsplit(epoch, "[^[:digit:]]")))[1]
  if (isSec == "TRUE" & epLength > 60) {
    stop("Please specify epoch size less than or equal to 60 seconds.")
  }
  if (isSec == "TRUE" & epLength == 0) {
    stop("Please specify epoch size greater than 0.")
  }
  if (isMin == "TRUE" & epLength != 1) {
    stop("Epoch size cannot be greater than 1 minute.")
  }
  if (tri == "TRUE" & is.null(axis)) {
    stop("Please choose an option for 'axis'. Choose from options 'vm', 'sum', 'x', 'y', or 'z'.")
  }
  if (tri == "TRUE" & !is.null(axis)) {
    if (axis == "vm") {
      data$counts <- sqrt(data[, 2]^2 + data[, 3]^2 + data[, 
                                                           4]^2)
    }
    if (axis == "sum") {
      data$counts <- sqrt(data[, 2] + data[, 3] + data[, 
                                                       4])
    }
    if (axis == "x") {
      data$counts <- data[, 2]
    }
    if (axis == "y") {
      data$counts <- data[, 3]
    }
    if (axis == "z") {
      data$counts <- data[, 4]
    }
  }
  if (length(patype) > 1) {
    stop("Please specify one patype. Please use function acc to obtain summary for multiple physical activities")
  }
  if (boutsize == 0) {
    stop("Please specify bout size of at least 1")
  }
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
                                                                     round(x)) < tol
  if (is.wholenumber(boutsize) == "FALSE") {
    stop("Please specify bout size as an integer value")
  }
  epochData <- mean(c(as.numeric(difftime(strptime(data$TimeStamp[2], 
                                                   format = "%Y-%m-%d %H:%M:%S"), strptime(data$TimeStamp[1], 
                                                                                           format = "%Y-%m-%d %H:%M:%S"), units = c("secs"))), as.numeric(difftime(strptime(data$TimeStamp[3], 
                                                                                                                                                                            format = "%Y-%m-%d %H:%M:%S"), strptime(data$TimeStamp[2], 
                                                                                                                                                                                                                    format = "%Y-%m-%d %H:%M:%S"), units = c("secs"))), as.numeric(difftime(strptime(data$TimeStamp[4], 
                                                                                                                                                                                                                                                                                                     format = "%Y-%m-%d %H:%M:%S"), strptime(data$TimeStamp[3], 
                                                                                                                                                                                                                                                                                                                                             format = "%Y-%m-%d %H:%M:%S"), units = c("secs")))), 
                    na.rm = TRUE)
  uncollapsed <- data
  if (isSec == "TRUE" & epLength < epochData) {
    stop(paste("You have specified ", epoch, " to summarize the data, but the sampling frequency of your data is ", 
               epochData, " seconds.", sep = ""))
  }
  if (isMin == "TRUE" & (epLength * 60) < epochData) {
    stop(paste("You have specified ", epoch, " to summarize the data, but the sampling frequency of your data is ", 
               epochData, " seconds.", sep = ""))
  }
  if (epochData < 60) {
    data <- dataCollapse(data, TS = "TimeStamp", col = "counts", by = 60)
  } # View(data)
  if (epochData > 60) {
    stop("Epoch is larger than 60 seconds. Please provide a dataset with epoch of 1 minutes or less")
  }
  value <- rep(rle(as.numeric(data$counts))$values, rle(as.numeric(data$counts))$lengths)
  length <- rep(rle(as.numeric(data$counts))$lengths, rle(as.numeric(data$counts))$lengths)
  d1.lag <- cbind(data, value, length, head = head(c(0, length), 
                                                   -1), tail = tail(c(length, 0), -1), dif1 = head(c(0, 
                                                                                                     length), -1) - length, dif2 = tail(c(length, 0), -1) - 
                    length, actb = head(c(0, data$counts), -1), acta = tail(c(data$counts, 
                                                                              0), -1))
  spDef <- spuriousDef - 1
  d1s <- cbind(d1.lag, spurious = ifelse(d1.lag$dif1 >= spDef & 
                                           d1.lag$dif2 >= spDef & d1.lag$actb == 0 & d1.lag$acta == 
                                           0, 1, 0))
  d1s$counts2 <- ifelse(d1s$spurious == 1, 0, d1s$counts)
  d2 <- data.frame(TimeStamp = d1s$TimeStamp, counts = d1s$counts2)
  value2 <- rep(rle(as.numeric(d2$counts))$values, rle(as.numeric(d2$counts))$lengths)
  length2 <- rep(rle(as.numeric(d2$counts))$lengths, rle(as.numeric(d2$counts))$lengths)
  d2w <- cbind(d2, value2, length2)
  nonwear <- ifelse(d2w$value2 == 0 & d2w$length > nonwearDef, 
                    1, 0)
  d2nw <- cbind(d2w, nonwear)
  d3 <- data.frame(TimeStamp = d2nw$TimeStamp, counts = d2nw$counts, 
                   nonwear = d2nw$nonwear)
  d3$countsWear <- ifelse(d3$nonwear == 1, NA, d3$counts)
  d3$mydates <- as.factor(as.numeric(strptime(d3$TimeStamp, 
                                              format = "%Y-%m-%d")))
  uniqueDates <- unique(strptime(d3$TimeStamp, format = "%Y-%m-%d"))
  d3$wear <- ifelse(d3$nonwear == 1, 0, 1)
  dts <- strptime(d3$TimeStamp, format = "%Y-%m-%d %H:%M:%S")
  wearSum <- tapply(d3$wear, format(dts, format = "%Y-%m-%d"), 
                    sum)
  wearTime <- data.frame(Date = names(wearSum), wearTime = wearSum)
  if (tolerance == "TRUE" & boutsize > 2) {
    tolerance <- 2
  }
  if (tolerance == "FALSE") {
    tolerance <- 0
  }
  if (tolerance == "TRUE" & boutsize <= 2) {
    tolerance <- 0
  }
  myRollSum<- function(x, k) { 
    rs <- RcppRoll::roll_sum(x, k)
    rsp <- c(rs,rep(NA,k-1))
    return(rsp)
  }
  myLag <- function(x, k) {
    c(rep(NA, k), x)[1:length(x)]
  }
  myLagUp <- function(x, k) {
    c(x[(k + 1):(length(x))], rep(NA, k))
  }
  if (isSec == TRUE & epLength < 60) {
    if (epLength != 60) {
      data <- dataCollapse(uncollapsed, TS = "TimeStamp", 
                           col = "counts", by = epLength)
    }
    data$Time <- substring(data$TimeStamp, 1, 16)
    d3$Time <- substring(d3$TimeStamp, 1, 16)
    myvars <- names(d3) %in% c("TimeStamp", "counts", "countsWear")
    d3 <- d3[!myvars]
    d3 <- merge(data, d3, by = "Time")
    d3$countsWear <- ifelse(d3$nonwear == 1, NA, d3$counts)
    d3$inPA <- ifelse(d3$countsWear >= as.numeric(pacut[1]) & 
                        d3$countsWear <= as.numeric(pacut[2]), 1, 0)
    d3$inPA2 <- ifelse(is.na(d3$inPA), 0, d3$inPA)
    d3$mydates <- as.factor(as.numeric(strptime(d3$TimeStamp, 
                                                format = "%Y-%m-%d")))
    boutsize <- boutsize * (60/epLength)
    mybPA <- boutsize - tolerance * (60/epLength)
    mylistPA <- list()
    dSplitPA <- split(d3, d3$mydates)
    for (k in 1:length(dSplitPA)) {
      dsiPA <- data.frame(dSplitPA[k])
      dsidPA <- data.frame(TimeStamp = dsiPA[, 2], counts = dsiPA[, 
                                                                  3], nonwear = dsiPA[, 4], inPA2 = dsiPA[, 9])
      dsidPA$mvpaB <- myRollSum(dsidPA$inPA2, boutsize)
      dsidPA$mvB <- ifelse(dsidPA$mvpaB >= mybPA, 1, 0)
      suppressWarnings(rm(bm))
      bm <- matrix(NA, nrow = nrow(dsidPA), ncol = (boutsize + 
                                                      1))
      bm[, 1] <- dsidPA$mvB
      for (i in 1:(boutsize - 1)) {
        bm[, (i + 1)] <- myLag(dsidPA$mvB, i)
      }
      if (boutsize > 1) {
        bm[, ncol(bm)] <- rowSums(bm[, 1:(ncol(bm) - 
                                            1)], na.rm = TRUE)
        dsidPA$inbout <- ifelse(bm[, ncol(bm)] >= 1, 
                                1, 0)
        dsidPA$inboutLagb1 <- myLag(dsidPA$inbout, 1)
        dsidPA$inboutLagb2 <- myLag(dsidPA$inbout, 2)
        dsidPA$PALagb1 <- myLag(dsidPA$inPA2, 1)
        dsidPA$inboutUpLagb1 <- myLagUp(dsidPA$inbout, 
                                        1)
        dsidPA$inboutUpLagb2 <- myLagUp(dsidPA$inbout, 
                                        2)
        dsidPA$PALagUpb1 <- myLagUp(dsidPA$inPA2, 1)
        dsidPA$inbout[dsidPA$inbout == 1 & dsidPA$inboutLagb1 == 
                        0 & dsidPA$inPA2 == 0] <- 0
        dsidPA$inbout[dsidPA$inbout == 1 & dsidPA$inboutLagb2 == 
                        0 & dsidPA$PALagb1 == 0 & dsidPA$inPA2 == 0] <- 0
        dsidPA$inbout[dsidPA$inbout == 1 & dsidPA$inboutUpLagb1 == 
                        0 & dsidPA$inPA2 == 0] <- 0
        dsidPA$inbout[dsidPA$inbout == 1 & dsidPA$inboutUpLagb2 == 
                        0 & dsidPA$PALagUpb1 == 0 & dsidPA$inPA2 == 
                        0] <- 0
        dsidPA$value <- rep(rle(as.numeric(dsidPA$inbout))$values, 
                            rle(as.numeric(dsidPA$inbout))$lengths)
        dsidPA$length <- rep(rle(as.numeric(dsidPA$inbout))$lengths, 
                             rle(as.numeric(dsidPA$inbout))$lengths)
        dsidPA$valueLag <- myLag(dsidPA$value)
        dsidPA$lengthLag <- myLag(dsidPA$length)
        dsidPA$first <- ifelse(dsidPA$value == dsidPA$valueLag & 
                                 dsidPA$length == dsidPA$lengthLag, 0, 1)
        dsidPA$first[1] <- ifelse(dsidPA$inbout[1] == 
                                    1, 1, 0)
        dsidPA$wear <- ifelse(dsidPA$nonwear == 0, 1, 
                              0)
        dsidPA$firstBout <- ifelse(dsidPA$first == 1 & 
                                     dsidPA$inbout == 1 & dsidPA$wear == 1 & dsidPA$length >= 
                                     boutsize, 1, 0)
      }
      if (boutsize == 1) {
        dsidPA$inbout <- ifelse(bm[, 1] >= 1, 1, 0)
        dsidPA$inboutLagb1 <- NA
        dsidPA$inboutLagb2 <- NA
        dsidPA$PALagb1 <- NA
        dsidPA$inboutUpLagb1 <- NA
        dsidPA$inboutUpLagb2 <- NA
        dsidPA$PALagUpb1 <- NA
        dsidPA$value <- rep(rle(as.numeric(dsidPA$inbout))$values, 
                            rle(as.numeric(dsidPA$inbout))$lengths)
        dsidPA$length <- rep(rle(as.numeric(dsidPA$inbout))$lengths, 
                             rle(as.numeric(dsidPA$inbout))$lengths)
        dsidPA$valueLag <- myLag(dsidPA$value)
        dsidPA$lengthLag <- myLag(dsidPA$length)
        dsidPA$first <- ifelse(dsidPA$value == dsidPA$valueLag & 
                                 dsidPA$length == dsidPA$lengthLag, 0, 1)
        dsidPA$first[1] <- ifelse(dsidPA$inbout[1] == 
                                    1, 1, 0)
        dsidPA$wear <- ifelse(dsidPA$nonwear == 0, 1, 
                              0)
        dsidPA$firstBout <- ifelse(dsidPA$first == 1 & 
                                     dsidPA$inbout == 1 & dsidPA$wear == 1 & dsidPA$length >= 
                                     boutsize, 1, 0)
      }
      mylistPA[[k]] <- dsidPA
      rm(dsidPA)
    }
    dfPA <- do.call("rbind", mylistPA)
    boutsPA <- data.frame(TimeStamp = dfPA$TimeStamp, counts = dfPA$counts, 
                          inPA = dfPA$inPA2, nonwear = dfPA$nonwear, inboutPA = dfPA$inbout)
    dfPA$inPABout <- ifelse(dfPA$inbout == 1 & dfPA$wear == 
                              1, 1, 0)
    d4.tempPA <- dfPA[which(dfPA$firstBout == 1 & dfPA$wear == 
                              1), ]
    boutSumPA <- tapply(d4.tempPA$firstBout, format(strptime(d4.tempPA$TimeStamp, 
                                                             format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d"), 
                        sum)
    numBoutsPA <- data.frame(Date = names(boutSumPA), numberOfBoutsPA = boutSumPA)
    d4PA <- data.frame(TimeStamp = d4.tempPA$TimeStamp, paMinutes = d4.tempPA$length)
    dts2PA <- strptime(d4PA$TimeStamp, format = "%Y-%m-%d %H:%M:%S")
    d4PA$Date <- format(dts2PA, format = "%Y-%m-%d")
    d4PA$TimeStamp <- NULL
    daySumPA <- round(tapply(d4PA$paMinutes, format(dts2PA, 
                                                    format = "%Y-%m-%d"), sum)/(60/epLength), 0)
    if (boutsize > 1) {
      pasummary <- data.frame(Date = rownames(daySumPA), 
                              paMinutes = daySumPA, numberOfBoutsPA = boutSumPA)
    }
    if (boutsize == 1) {
      pasummary <- data.frame(Date = rownames(daySumPA), 
                              paMinutes = daySumPA, numberOfBoutsPA = daySumPA)
    }
    rownames(pasummary) <- NULL
    if (nrow(pasummary) == 0) {
      pasummary <- data.frame(format(dfPA$TimeStamp[1], 
                                     format = "%Y-%m-%d"), matrix(rep(NA, 2), ncol = 2))
    }
    mycolname1 <- paste(patype, ".minutes", sep = "")
    mycolname2 <- paste(patype, ".num.bouts", sep = "")
    colnames(pasummary) <- c("Date", mycolname1, mycolname2)
    summarized <- list()
    summarized$validDates <- pasummary
    summarized$wearTime <- wearTime
    summary <- Reduce(function(...) merge(..., all = TRUE), 
                      summarized)
    summary[is.na(summary)] <- 0
    summary <- summary[which(summary$wearTime >= minWear), 
                       ]
    summary <- summary[order(as.Date(summary$Date)), ]
    if (returnbout == "FALSE") {
      return(summary)
    }
    if (returnbout == "TRUE") {
      summarized <- list()
      summarized$totalDates <- uniqueDates
      summarized$validDates <- summary
      summarized$PA <- boutsPA
      summarized$boutsize <- boutsize
      summarized$pacut <- pacut
      return(summarized)
    }
  }
  if ((isMin == TRUE & epLength == 1) | (isSec == TRUE & epLength == 
                                         60)) {
    mybPA <- boutsize - tolerance
    d3$countsWear <- ifelse(d3$nonwear == 1, NA, d3$counts)
    d3$inPA <- ifelse(d3$countsWear >= as.numeric(pacut[1]) & 
                        d3$countsWear <= as.numeric(pacut[2]), 1, 0)
    d3$inPA2 <- ifelse(is.na(d3$inPA), 0, d3$inPA)
    d3$mydates <- as.factor(as.numeric(strptime(d3$TimeStamp, 
                                                format = "%Y-%m-%d")))
    mylistPA <- list()
    dSplitPA <- split(d3, d3$mydates)
    for (k in 1:length(dSplitPA)) {
      dsiPA <- data.frame(dSplitPA[k])
      dsidPA <- data.frame(TimeStamp = dsiPA[, 1], counts = dsiPA[, 
                                                                  2], nonwear = dsiPA[, 3], inPA2 = dsiPA[, 8])
      dsidPA$mvpaB <- myRollSum(dsidPA$inPA2, boutsize)
      dsidPA$mvB <- ifelse(dsidPA$mvpaB >= mybPA, 1, 0)
      suppressWarnings(rm(bm))
      bm <- matrix(NA, nrow = nrow(dsidPA), ncol = (boutsize + 
                                                      1))
      bm[, 1] <- dsidPA$mvB
      for (i in 1:(boutsize - 1)) {
        bm[, (i + 1)] <- myLag(dsidPA$mvB, i)
      }
      if (boutsize > 1) {
        bm[, ncol(bm)] <- rowSums(bm[, 1:(ncol(bm) - 
                                            1)], na.rm = TRUE)
        dsidPA$inbout <- ifelse(bm[, ncol(bm)] >= 1, 
                                1, 0)
        dsidPA$inboutLagb1 <- myLag(dsidPA$inbout, 1)
        dsidPA$inboutLagb2 <- myLag(dsidPA$inbout, 2)
        dsidPA$PALagb1 <- myLag(dsidPA$inPA2, 1)
        dsidPA$inboutUpLagb1 <- myLagUp(dsidPA$inbout, 
                                        1)
        dsidPA$inboutUpLagb2 <- myLagUp(dsidPA$inbout, 
                                        2)
        dsidPA$PALagUpb1 <- myLagUp(dsidPA$inPA2, 1)
        dsidPA$inbout[dsidPA$inbout == 1 & dsidPA$inboutLagb1 == 
                        0 & dsidPA$inPA2 == 0] <- 0
        dsidPA$inbout[dsidPA$inbout == 1 & dsidPA$inboutLagb2 == 
                        0 & dsidPA$PALagb1 == 0 & dsidPA$inPA2 == 0] <- 0
        dsidPA$inbout[dsidPA$inbout == 1 & dsidPA$inboutUpLagb1 == 
                        0 & dsidPA$inPA2 == 0] <- 0
        dsidPA$inbout[dsidPA$inbout == 1 & dsidPA$inboutUpLagb2 == 
                        0 & dsidPA$PALagUpb1 == 0 & dsidPA$inPA2 == 
                        0] <- 0
        dsidPA$value <- rep(rle(as.numeric(dsidPA$inbout))$values, 
                            rle(as.numeric(dsidPA$inbout))$lengths)
        dsidPA$length <- rep(rle(as.numeric(dsidPA$inbout))$lengths, 
                             rle(as.numeric(dsidPA$inbout))$lengths)
        dsidPA$valueLag <- myLag(dsidPA$value)
        dsidPA$lengthLag <- myLag(dsidPA$length)
        dsidPA$first <- ifelse(dsidPA$value == dsidPA$valueLag & 
                                 dsidPA$length == dsidPA$lengthLag, 0, 1)
        dsidPA$first[1] <- ifelse(dsidPA$inbout[1] == 
                                    1, 1, 0)
        dsidPA$wear <- ifelse(dsidPA$nonwear == 0, 1, 
                              0)
        dsidPA$firstBout <- ifelse(dsidPA$first == 1 & 
                                     dsidPA$inbout == 1 & dsidPA$wear == 1 & dsidPA$length >= 
                                     boutsize, 1, 0)
      }
      if (boutsize == 1) {
        dsidPA$inbout <- ifelse(bm[, 1] >= 1, 1, 0)
        dsidPA$inboutLagb1 <- NA
        dsidPA$inboutLagb2 <- NA
        dsidPA$PALagb1 <- NA
        dsidPA$inboutUpLagb1 <- NA
        dsidPA$inboutUpLagb2 <- NA
        dsidPA$PALagUpb1 <- NA
        dsidPA$value <- rep(rle(as.numeric(dsidPA$inbout))$values, 
                            rle(as.numeric(dsidPA$inbout))$lengths)
        dsidPA$length <- rep(rle(as.numeric(dsidPA$inbout))$lengths, 
                             rle(as.numeric(dsidPA$inbout))$lengths)
        dsidPA$valueLag <- myLag(dsidPA$value)
        dsidPA$lengthLag <- myLag(dsidPA$length)
        dsidPA$first <- ifelse(dsidPA$value == dsidPA$valueLag & 
                                 dsidPA$length == dsidPA$lengthLag, 0, 1)
        dsidPA$first[1] <- ifelse(dsidPA$inbout[1] == 
                                    1, 1, 0)
        dsidPA$wear <- ifelse(dsidPA$nonwear == 0, 1, 
                              0)
        dsidPA$firstBout <- ifelse(dsidPA$first == 1 & 
                                     dsidPA$inbout == 1 & dsidPA$wear == 1 & dsidPA$length >= 
                                     boutsize, 1, 0)
      }
      mylistPA[[k]] <- dsidPA
      rm(dsidPA)
    }
    dfPA <- do.call("rbind", mylistPA)
    boutsPA <- data.frame(TimeStamp = dfPA$TimeStamp, counts = dfPA$counts, 
                          inPA = dfPA$inPA2, nonwear = dfPA$nonwear, inboutPA = dfPA$inbout)
    dfPA$inPABout <- ifelse(dfPA$inbout == 1 & dfPA$wear == 
                              1, 1, 0)
    d4.tempPA <- dfPA[which(dfPA$firstBout == 1 & dfPA$wear == 
                              1), ]
    boutSumPA <- tapply(d4.tempPA$firstBout, format(strptime(d4.tempPA$TimeStamp, 
                                                             format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d"), 
                        sum)
    numBoutsPA <- data.frame(Date = names(boutSumPA), numberOfBoutsPA = boutSumPA)
    d4PA <- data.frame(TimeStamp = d4.tempPA$TimeStamp, paMinutes = d4.tempPA$length)
    dts2PA <- strptime(d4PA$TimeStamp, format = "%Y-%m-%d %H:%M:%S")
    d4PA$Date <- format(dts2PA, format = "%Y-%m-%d")
    d4PA$TimeStamp <- NULL
    daySumPA <- tapply(d4PA$paMinutes, format(dts2PA, format = "%Y-%m-%d"), 
                       sum)
    if (boutsize > 1) {
      pasummary <- data.frame(Date = rownames(daySumPA), 
                              paMinutes = daySumPA, numberOfBoutsPA = boutSumPA)
    }
    if (boutsize == 1) {
      pasummary <- data.frame(Date = rownames(daySumPA), 
                              paMinutes = daySumPA, numberOfBoutsPA = daySumPA)
    }
    rownames(pasummary) <- NULL
    if (nrow(pasummary) == 0) {
      pasummary <- data.frame(format(dfPA$TimeStamp[1], 
                                     format = "%Y-%m-%d"), matrix(rep(NA, 2), ncol = 2))
    }
    mycolname1 <- paste(patype, ".minutes", sep = "")
    mycolname2 <- paste(patype, ".num.bouts", sep = "")
    colnames(pasummary) <- c("Date", mycolname1, mycolname2)
    summarized <- list()
    summarized$validDates <- pasummary
    summarized$wearTime <- wearTime
    summary <- Reduce(function(...) merge(..., all = TRUE), 
                      summarized)
    summary[is.na(summary)] <- 0
    summary <- summary[which(summary$wearTime >= minWear), 
                       ]
    summary <- summary[order(as.Date(summary$Date)), ]
    if (returnbout == "FALSE") {
      return(summary)
    }
    if (returnbout == "TRUE") {
      summarized <- list()
      summarized$totalDates <- uniqueDates
      summarized$validDates <- summary
      summarized$PA <- boutsPA
      summarized$boutsize <- boutsize
      summarized$pacut <- pacut
      return(summarized)
    }
  }
}
