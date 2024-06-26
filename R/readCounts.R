#' Reads counts data in .dat, .agd, or .csv format for Actigraph GT1M and GT3X
#' devices
#' 
#' Reads counts data in .dat, .agd, or .csv format for Actigraph GT1M and GT3X
#' devices. Device type and epoch is automatically detected and reported in the
#' console.
#' 
#' 
#' @param filename Speficy full file path and file name. e.g. C:/mydata.dat or
#' @param dateformat format for dates
#' @param timeformat format for times
#' @param timezone time zone of the data captured
#' C:/mydata.csv
#' @return
#' 
#' For uni-axial accelerometer (GT1M), two columns are returned, consisting of:
#' [TimeStamp,Counts] For tri-axial accelerometer (GT3X), four columns are
#' returned, consisting of: [TimeStamp,x,y,z]
#' @author Jaejoon Song <jjsong2@@mdanderson.org>
#' @keywords accelerometer
#' @examples
#' 
#' 
#' ##
#' ## A example to read counts data
#' ##
#' \dontrun{
#' accData1 <- readCounts("C:/mydata.agd")
#' accData1 <- readCounts("C:/mydata.dat")
#' accData2 <- readCounts("C:/mydata.csv")
#' }
#' 
#' 
#' 
#' @export
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @importFrom stats complete.cases
readCounts <- function (filename, dateformat = "%y-%b-%d", timeformat = "%H:%M:%S", 
                        timezone = "GMT") 
{
  
  myfiletype <- substring(filename, nchar(filename) - 3, nchar(filename))
  if (myfiletype == ".dat" | myfiletype == ".csv") {
    Tfile <- file(filename, "r")
    if (isOpen(Tfile, "r")) {
      seek(Tfile, 0, rw = "r")
      lines = suppressWarnings(readLines(Tfile))
      close(Tfile)
    }
    skipPos = grep("-----", lines)[2]
    startTPos = grep("Start Time", lines)
    startEPos = grep("Epoch Period", lines)
    startDPos = grep("------------ Data File Created By ActiGraph ", 
                     lines)
    if (length(startDPos) == 0) {
      startDPos = grep("------------ Data Table File Created By ActiGraph ", 
                       lines)
    }
    deviceName = gsub("------------ Data File Created By ActiGraph ", 
                      "", lines[startDPos])
    deviceName = head(strsplit(deviceName, split = " ")[[1]], 
                      1)
    if (substring(deviceName, 1, 1) != "G" & substring(deviceName, 
                                                       1, 1) != "w") {
      deviceName = gsub("------------ Data Table File Created By ActiGraph ", 
                        "", lines[startDPos])
      deviceName = head(strsplit(deviceName, split = " ")[[1]], 
                        1)
    }
    startSPos = grep("Serial Number: ", lines)
    startTime = gsub("Start Time ", "", lines[startTPos])
    startTime = gsub("\\,", "", startTime)
    startTime = gsub("[[:blank:]]", "", startTime)
    startDatePos = grep("Start Date ", lines)
    startDateChr = gsub("Start Date ", "", lines[startDatePos])
    startDateChr = gsub("\\,", "", startDateChr)
    startDateChr = gsub("[[:blank:]]", "", startDateChr)        
    startDateOriginal = startDateChr
    startDateChr = strptime(as.character(startDateChr), dateformat)
    startDate <- substring(startDateChr, 1, 10)
    
    rawTimeStamp1 = paste(startDate, startTime, sep = " ")
    epochTime = gsub("Epoch Period (hh:mm:ss) ", "", lines[startEPos])
    epochTime = gsub("\\,", "", epochTime)
    epochTime = substr(epochTime, nchar(epochTime) - 7, nchar(epochTime))
    ep = as.numeric(as.difftime(c(epochTime), units = "secs"))
    serialNumber = gsub("Serial Number: ", "", lines[startSPos])
    serialNumber = gsub("\\,", "", serialNumber)
  }
  if (myfiletype == ".agd") {
    con = DBI::dbConnect(SQLite(), dbname = filename)
    settings <- DBI::dbReadTable(con, "settings")
    deviceName <- settings$settingValue[settings$settingName == 
                                          "devicename"]
    serialNumber <- settings$settingValue[settings$settingName == 
                                            "deviceserial"]
    ep <- as.numeric(as.character(settings$settingValue[settings$settingName == 
                                                          "epochlength"]))
    startdatetime <- settings$settingValue[settings$settingName == 
                                             "startdatetime"]
    startdatetime2 <- as.POSIXlt((as.numeric(startdatetime)/1e+07), 
                                 origin = "0001-01-01 00:00:00", tz = "GMT")
    startDate <- substring(startdatetime2, 1, 10)
  }
  type <- NA
  if (deviceName == "GT1M") {
    type <- "uni-axial"
  }
  if (deviceName == "GT3X") {
    type <- "tri-axial"
  }
  if (deviceName == "GT3XPlus") {
    type <- "tri-axial"
  }
  if (deviceName == "GT3X+") {
    type <- "tri-axial"
  }
  if (deviceName == "wGT3XBT") {
    type <- "tri-axial"
  }
  cat(noquote(paste("Raw data read for ", deviceName, " device. This is a ", 
                    type, " device.", sep = "")))
  cat("\n")
  cat(noquote(paste("Serial number: ", serialNumber, ".", sep = "")))
  cat("\n")
  cat(noquote(paste("Start date is ", startDate, " and epoch is ", 
                    ep, " seconds.", sep = "")))
  cat("\n")
  if (myfiletype == ".dat" | myfiletype == ".csv") {
    startline = skipPos + 1
    endline = length(lines)
    col0 = gsub("[[:blank:]]+", " ", lines[startline])
    col = strsplit(col0, c("\\, |\\,| "))[[1]]
    col = length(col[col != ""])
    timeline = c()
    if (substring(gsub("[[:blank:]]+", " ", lines[startline])[1], 
                  1, 4) != "Date" & substring(gsub("[[:blank:]]+", 
                                                   " ", lines[startline])[1], 1, 4) != "Axis") {
      mymatrix <- matrix(NA, (endline - startline + 1), 
                         col)
      for (i in startline:endline) {
        temp0 = gsub("[[:blank:]]+", " ", lines[i])
        temp = strsplit(temp0, c("\\, |\\,| "))[[1]]
        temp = temp[temp != ""]
        if (length(temp) > 0) {
          mymatrix[(i - startline + 1), 1:length(temp)] <- temp
        }
      }
      counts = as.numeric(as.vector(t(mymatrix)))
      counts <- counts[!is.na(counts)]
      if (type == "uni-axial") {
        timeline = (0:as.integer((length(counts)) - 1) * 
                      ep)
        rawTimeStamp = rep(rawTimeStamp1, (length(counts)))
        rst = gsub(" GMT", "", as.POSIXlt(rawTimeStamp, 
                                          tz = "GMT") + timeline)
        data = data.frame(TimeStamp = as.vector(rst), 
                          counts = counts)
      }
      if (type == "tri-axial") {
        n <- length(counts)
        x = counts[seq(1, n, 3)]
        y = counts[seq(2, n, 3)]
        z = counts[seq(3, n, 3)]
        maxlength <- max(length(x), length(y), length(z))
        if (length(x) < maxlength) {
          x <- c(x, NA)
        }
        if (length(y) < maxlength) {
          y <- c(y, NA)
        }
        if (length(z) < maxlength) {
          z <- c(z, NA)
        }
        timeline = (0:(maxlength - 1) * ep)
        rawTimeStamp = rep(rawTimeStamp1, maxlength)
        rst = gsub(" GMT", "", as.POSIXlt(rawTimeStamp, 
                                          tz = "GMT") + timeline)
        data = data.frame(TimeStamp = as.vector(rst), 
                          x = x, y = y, z = z)
      }
    }
    if (substring(gsub("[[:blank:]]+", " ", lines[startline])[1], 
                  1, 4) == "Date" & substring(gsub("[[:blank:]]+", 
                                                   " ", lines[startline])[1], 1, 4) != "Axis") {
      mymatrix <- matrix(NA, (endline - startline), col)
      for (i in startline:endline) {
        temp0 = gsub("[[:blank:]]+", " ", lines[i])
        temp = strsplit(temp0, c("\\, |\\,| "))[[1]]
        temp = temp[temp != ""]
        if (length(temp) > 0 & temp[1] != "Date") {
          mymatrix[(i - startline), 1:length(temp)] <- temp
        }
      }
      if (type == "uni-axial") {
        data = data.frame(TimeStamp = paste(mymatrix[, 
                                                     1], mymatrix[, 2]), counts = mymatrix[, 3])
      }
      if (type == "tri-axial") {
        data = data.frame(TimeStamp = paste(mymatrix[, 
                                                     1], mymatrix[, 2]), x = mymatrix[, 3], y = mymatrix[, 
                                                                                                         4], z = mymatrix[, 5])
      }
    }
    if (substring(gsub("[[:blank:]]+", " ", lines[startline])[1], 
                  1, 4) == "Axis" & type == "tri-axial") {
      mymatrix <- matrix(NA, (endline - startline), col)
      for (i in startline:endline) {
        temp0 = gsub("[[:blank:]]+", " ", lines[i])
        temp = strsplit(temp0, c("\\, |\\,| "))[[1]]
        temp = temp[temp != ""]
        if (length(temp) > 0 & temp[1] != "Date") {
          mymatrix[(i - startline), 1:length(temp)] <- temp
        }
      }
      startingTime <- strptime(paste(startDate, startTime), 
                               format = paste('%Y-%m-%d', timeformat), tz = timezone)
      timesequence <- seq(from = startingTime, by = paste(ep, 
                                                          "sec"), length = (endline - startline))
      if (type == "tri-axial") {
        data = data.frame(TimeStamp = timesequence, x = as.numeric(mymatrix[, 
                                                                            1]), y = as.numeric(mymatrix[, 2]), z = as.numeric(mymatrix[, 
                                                                                                                                        3]))
      }
    }
  }
  if (myfiletype == ".agd") {
    counts <- DBI::dbReadTable(con, "data")
    counts <- counts[complete.cases(counts), ]
    timeline = (0:as.integer((dim(counts)[1]) - 1) * ep)
    rawTimeStamp = rep(startdatetime2, dim(counts)[1])
    rst = gsub(" GMT", "", as.POSIXlt(rawTimeStamp, tz = "GMT") + 
                 timeline)
    if (type == "uni-axial") {
      data = data.frame(TimeStamp = as.vector(rst), counts = counts[, 
                                                                    2])
    }
    if (type == "tri-axial") {
      data = data.frame(TimeStamp = as.vector(rst), x = counts[, 
                                                               2], y = counts[, 3], z = counts[, 4])
    }
  }
  data
}
