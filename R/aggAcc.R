#' Aggregates multiple accelerometer summary files
#' 
#' Aggregates multiple accelerometer summary files (e.g. summary files for
#' multiple individuals) by day. This function can be used in sequence with
#' readRawBatch and accBatch.
#' 
#' 
#' @param path Path to accelerometer summary files created by function acc or
#' accBatch
#' @return A folder `aggregate' is created within the specified path. In the
#' folder, aggregate.Rdata file (object `aggregate') consists of columns [ID,
#' Date, SedentaryMinutes, wearTime, numberOfBoutsSed, mvpaMinutes,
#' numberOfBoutsMVPA], where ID is the filename.
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @keywords accelerometer
#' @examples
#' 
#' ##
#' ## Example
#' ##
#' \dontrun{
#' mypath <- "C:/Accelerometry files/readfiles/summaryfiles"
#' aggAcc(mypath)
#' }
#' 
#' 
#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom zoo rollmean rollsum rollmedian
#' @importFrom PhysicalActivity dataCollapser
aggAcc <- function(path){
  
  myfilenames <- list.files(path = path)
  newpath <- paste(path,"/aggregate",sep="")
  dir.create(newpath,showWarnings='FALSE')
  
  mylist <- list() #create an empty list
  
  for(i in 1:length(myfilenames)){
    mynchar <- nchar(myfilenames[i])
    mystr <- substr(myfilenames[i],mynchar-5,mynchar)
    
    if(mystr == ".Rdata"){
      file.out <- paste(path,"/",myfilenames[i],sep="")
      load(file.out)
      if(nrow(summary)>=1){
        mylist[[i]] <- as.matrix(summary)
      }
      rm(summary)
    }
  }
  
  aggregate <- do.call("rbind",mylist) #View(databind)
  indicator <- c(3:ncol(aggregate))
  aggregate <- data.frame(aggregate)
  aggregate[,indicator] <- as.numeric(as.character(unlist(aggregate[,indicator])))
  file.out <- paste(newpath,"/","aggregate.Rdata",sep="")
  save(aggregate, file = file.out) 
}
