#' Plots accelerometer data
#' 
#' Plots accelerometer data. This function receives summary object from
#' function accsummary.
#' 
#' 
#' @param object An object returned from either the function accsummary.
#' @param markbouts Whether to mark bouts. If markbout='TRUE' a bar along the
#' time axis will indicate whether the epoch was counted as in bout or not.
#' Default is false.
#' @return A plot is returned.
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @keywords accelerometer
#' @examples
#' 
#' \dontrun{
#' ##
#' ## Example: Simulate a dataset for two days, for an individual with low MVPA level.
#' ##
#' mvpaLowData <- simAcc(timelength=(60*24*2),paLevel='low')
#' summary <- accSummary(data=mvpaLowData)
#' summary$validDates
#' plotAcc(summary,markbouts='FALSE')
#' 
#' ##
#' ## Example: Simulate a dataset for two days, for an individual with moderate MVPA level.
#' ##
#' mvpaModData <- simAcc(timelength=(60*24*2),paLevel='moderate')
#' summary <- accSummary(data=mvpaModData, tri='FALSE', axis=NULL,
#'              spuriousDef=20, nonwearDef=60, minWear=600, 
#'              patype='MVPA',pacut=c(1952,Inf), boutsize=10, 
#'              tolerance='TRUE', returnbout='TRUE')
#' summary$validDates
#' plotAcc(summary,markbouts='FALSE')
#' 
#' ##
#' ## Example: Simulate a dataset for two days, for an individual with high MVPA level.
#' ##
#' mvpaHighData <- simAcc(timelength=(60*24*2),paLevel='high')
#' summary <- accSummary(data=mvpaHighData, tri='FALSE', axis=NULL,
#'              spuriousDef=20, nonwearDef=60, minWear=600, 
#'              patype='MVPA',pacut=c(1952,Inf), boutsize=10, 
#'              tolerance='TRUE', returnbout='TRUE')
#' summary$validDates
#' plotAcc(summary,markbouts='FALSE')
#' 
#' 
#' ##
#' ## Example: Simulate a tri-axial dataset for five days.
#' ##
#'   library(acc)
#'   library(mhsmm)
#'   seedset=1234
#'   minutes=(60*24*5)
#'   randomTime <- seq(ISOdate(2015,1,1),ISOdate(2020,1,1),"min")
#'   J <- 3; initial <- rep(1/J, J)
#'   P <- matrix(rep(NA,9),byrow='TRUE',nrow=J)
#' 
#'   P1 <- matrix(c(0.95, 0.04, 0.01, 
#'                   0.09, 0.9, 0.01, 
#'                   0.1, 0.2, 0.7), byrow='TRUE',nrow = J)
#' 
#'   b <- list(mu = c(0, 30, 2500), sigma = c(0, 30, 1000))
#'   model1 <- hmmspec(init = initial, trans = P1, parms.emis = b,dens.emis = dnorm.hsmm)
#'   x <- simulate.hmmspec(model1, nsim = (minutes), seed = seedset, rand.emis = rnorm.hsmm)
#' 
#'   seedset=12345
#'   P2 <- matrix(c(0.95, 0.04, 0.01, 
#'                   0.09, 0.8, 0.11, 
#'                   0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
#'   model2 <- hmmspec(init = initial, trans = P2, parms.emis = b,dens.emis = dnorm.hsmm)
#'   y <- simulate.hmmspec(model2, nsim = (minutes), seed = seedset, rand.emis = rnorm.hsmm)
#' 
#'   seedset=123456
#'   P3 <- matrix(c(0.95, 0.04, 0.01, 
#'                   0.09, 0.8, 0.11, 
#'                   0.1, 0.1, 0.8), byrow='TRUE',nrow = J)
#'   model3 <- hmmspec(init = initial, trans = P3, parms.emis = b,dens.emis = dnorm.hsmm)
#'   z <- simulate.hmmspec(model3, nsim = (minutes), seed = seedset, rand.emis = rnorm.hsmm)
#' 
#'   counts <- data.frame(TimeStamp = randomTime[1:minutes], x=x$x, y=y$x, z=z$x)
#'   summary <- accSummary(data=counts, tri='TRUE', axis='vm',
#'                         spuriousDef=20, nonwearDef=60, minWear=600, 
#'                         patype='MVPA',pacut=c(1952,Inf), boutsize=10, tolerance='TRUE',
#'                         returnbout='TRUE')
#' summary$validDates
#' 
#' plotAcc(summary,markbouts='FALSE')
#' }
#' 
#' 
#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend lines
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom zoo rollmean rollsum rollmedian
#' @importFrom PhysicalActivity dataCollapser
#' @importFrom grDevices dev.new
plotAcc <- function(object,markbouts='FALSE'){
  
  
  ##
  ## Need code to reset graphical parameters here!!
  ##
  dev.new()
  
  if(names(object[1])=="totalDates"){
    counts <- object[[3]]$counts  # View(counts)
    time <- object[[3]]$TimeStamp
    PAState <- object[[3]]$inPA
    nonwearState <- object[[3]]$nonwear
    inbout <- object[[3]]$inboutPA
    
    par(mar=c(6, 4, 3, 2)) # bottom, left, top and right
    plot(counts,type="l",ylim=c((min(counts,na.rm=TRUE)-30),(max(counts,na.rm=TRUE)+300)),axes=FALSE, ann=FALSE)
    mytime <- strptime(time,format='%Y-%m-%d %H:%M:%S')
    timeaxis <- format(mytime, format="%Y-%m-%d")
    min <- format(mytime, format="%M")
    hour <- format(mytime, format="%H")
    
    if(length(time)<=120){
      hourmark <- ifelse(( min=="00" | min=="15" | min=="30" | min=="45"  | min=="60"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(120<length(time) & length(time)<=600){
      hourmark <- ifelse(( min=="00" | min=="30" | min=="60"),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(600<length(time) & length(time)<=1440){
      hourmark <- ifelse(min=="00",1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>1440 & length(time)<=2880){
      hourmark <- ifelse(min=="00"  & ((hour=="00")|(hour=="06")|(hour=="12")|(hour=="18")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>2880 & length(time)<=4320){
      hourmark <- ifelse(min=="00"  & ((hour=="00")|(hour=="12")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>4320 & length(time)<=7200){
      hourmark <- ifelse(min=="00"  & ((hour=="00")),1,0)
      myrownum <- rep(1:length(mytime))
      timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.6, xlab="Dates available in data")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    if(length(time)>7200){
      hourmark <- ifelse(min=="00"  & (hour=="00"),1,0)
      myrownum <- rep(1:length(mytime))
      #timeaxis <- format(mytime, format="%Y-%m-%d %H:%M")
      timeaxis <- format(mytime, format="%Y-%m-%d")
      timebind <- data.frame(cbind(timeaxis,hourmark))
      labels <- timebind[ which(timebind[,2]==1), ] 
      axis(1, at=as.numeric(rownames(labels)), labels=labels$timeaxis, las=2, cex.axis=0.9, xlab="Date")
      axis(1,labels=FALSE,  at=c(0,length(time)))
      axis(2)
    }
    
    title(main="Plot of accelerometer data",cex.main=1.5)
    title(ylab="Activity counts")
    #title(xlab="Time",line=5)
    
    if(markbouts=='TRUE'){
      cols <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
      t <- rep(1:length(counts))
      
      Statebind <- data.frame(cbind(t,PAState))  
      Statebind$col[Statebind$PAState == 1] <- "thistle3"
      rect(xleft =Statebind$t-1, xright = Statebind$t, 
           ybottom=min(counts,na.rm=TRUE), ytop=max(counts,na.rm=TRUE), 
           col=Statebind$col, border=NA )
      legend(3*length(counts)/6, (max(counts,na.rm=TRUE))*1.12, 
             #c(expression(italic("Activity in ten minute bouts"))), 
             paste(object$pacut[1]," <= Activity counts <= ",object$pacut[2],sep=""), 
             text.col = "blue", cex=.8, horiz = TRUE, 
             bty = "n", fill=c("thistle3"), xjust = 0.01)
      
      lines(counts,type="l",ylim=c((min(counts)-30),(max(counts)+30)))
      
      barcols <- c("white","sienna1")
      for(ii in 0:length(inbout)){
        rect(xleft   = ii,
             ybottom = (min(counts,na.rm=TRUE)-max(counts,na.rm=TRUE)*.01),
             xright  = ii+1, 
             ytop    = (min(counts,na.rm=TRUE)-max(counts,na.rm=TRUE)*.04),
             col = barcols[inbout[ii]+1], border = 0)
        # Create a legend 
        #legend(1*length(counts)/6, (max(counts,na.rm=TRUE))*1.08, 
        legend(1*length(counts)/6, (max(counts,na.rm=TRUE))*1.12, 
               #"In bout", 
               paste("Activity in ",object$boutsize," minute bouts",sep=""), 
               text.col = "blue", cex=.8, horiz = TRUE, #xpd=TRUE, 
               bty = "n", fill=c("sienna1"), xjust = 0.01)
        
      }              
    }
    
    
  }
  
}
