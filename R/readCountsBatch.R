#' Reads multiple accelerometer counts data in a folder
#' 
#' Reads multiple accelerometer counts data in a folder. This is a batch mode
#' of the readCounts function.
#' 
#' 
#' @param path Path to a folder which contains accelerometer counts data in
#' .dat or .csv format.
#' @param filetype Specify whether the data to read is in dat or csv format.
#' Options are either 'dat' or 'csv'. For example if filetype = 'csv' is
#' specified, all csv data will be read and all other types in the same folder
#' will be ignored. By defalt, it is assumed that all files in the specified
#' path are either csv or dat files and are intended to be read.
#' @return A folder `readfiles' is created inside the specified 'path'. In the
#' folder, files are saved by the same filenames as in the raw data.
#' 
#' For uni-axial accelerometer (GT1M), two columns are returned, consisting of:
#' [TimeStamp,Counts] For tri-axial accelerometer (GT3X), four columns are
#' returned, consisting of: [TimeStamp,x,y,z]
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @keywords accelerometer
#' @examples
#' 
#' ##
#' ## Example
#' ##
#' # filepath to locate the activity counts data files
#' \dontrun{
#' mypath <- "C:/Accelerometry files"
#' readCountsBatch(mypath,filetype='csv')
#' }
#' 
#' @export
#' @importFrom utils head tail 
#' @importFrom graphics par axis title plot rect legend
#' @importFrom mhsmm simulate.hmmspec hmmspec dnorm.hsmm rnorm.hsmm
#' @importFrom PhysicalActivity dataCollapser
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
readCountsBatch <- function(path,filetype=NULL){
  
  if(is.null(filetype)=='TRUE'){
    myfilenames <- list.files(path = path)
  }
  if(is.null(filetype)=='FALSE'){
    allfilenames <- list.files(path = path)
    myfilenames = allfilenames[ grep(filetype, allfilenames) ]
  }
  newpath <- paste(path,"/readfiles",sep="")
  dir.create(newpath,showWarnings='FALSE')
  
  for(i in 1:length(myfilenames)){
    mynchar <- nchar(myfilenames[i])
    mystr <- substr(myfilenames[i],mynchar-2,mynchar)
    
    if(mystr == "dat" | mystr == "csv" | mystr == "agd"){
      infilename <- paste(path,"/",myfilenames[i],sep="")
      myID <- substr(myfilenames[i], 1, mynchar-4)
      counts <- readCounts(infilename)
      file.out <- paste(newpath,"/",myID,".Rdata",sep="")
      save(counts, file = file.out) 
    }
  }
}
