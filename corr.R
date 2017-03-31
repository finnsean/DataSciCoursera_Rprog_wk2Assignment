corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'threshold' is the number of completely observed
  ## observations required to calculate the correlation between
  ## sulfate and nitrate 
  
  ## NB this function assumes the data file for monitor 123 is
  ## can be found in a CSV file with path:
  ##    "directory/123.csv"
  ##
  
  # determine the number of CSV files in this dir, following the filename convention above
  maxFiles <- 999;
  idsGood = vector(mode = "logical", length = maxFiles)
  for (ii in 1:maxFiles) {
    filename <- paste(formatC(ii, width=3, flag="0"), ".csv", sep = "")
    filepath <- file.path(directory, filename)
    if (file.exists(filepath)) {
      idsGood[ii] <- TRUE;
    }
  }
  id = which(idsGood)
  
  # create a vector to store the mean from each file
  corrVec = vector(mode="numeric", length = length(id)); # each is initialised to 0
    

  # process each input file
  for (ii in seq_along(id)) {
    filename <- paste(formatC(id[ii], width=3, flag="0"), ".csv", sep = "")
    filepath <- file.path(directory, filename)
    
    print(paste("Loading monitor data", ii, "of", length(id), ":", filepath))
    pollutionFrame <- read.csv(filepath, header=TRUE, colClasses = c("Date","numeric","numeric","integer"))
    
    # grab a subset of the rows to work with during development
    # pollutionFrame <- head(pollutionFrame, n=200)
    
    # information on the data frame
    # > colnames(pollutionFrame)
    # [1] "Date"    "sulfate" "nitrate" "ID"  
    
    # how to access elements
    # element-wise: pollutionFrame[1,3] or pollutionFrame["Date",3]
    # a column: 
    #    pollutionFrame[[2]] or pollutionFrame[["sulfate"]]
    #       can use this [ ] form pollutionFrame[,"sulfate"] (wildcard for row)
    #       but do not use this [ ] form: pollutionFrame["sulfate"] ( this contains the header)
    #    pollutionFrame$sulfate
    # 
    # using variable for name
    # pollutionFrame$pollutant # does not work
    # pollutionFrame[[pollutant]] # works OK
    
    # check if the ID of the monitor matches that in the CSV
    if (!all(pollutionFrame$ID == id[ii])) {
      print(paste("Error: ID of monitor in file was not consistent with filename"))
      return(NA)   # exit the function
    }
    
    # extract the column with values from the requested pollutants
    pollutData <- pollutionFrame[["sulfate"]]
    ## or 'nitrate"
    
    
    # find the number of available values in the specified column
    validDataIndecies <- !is.na(pollutionFrame[["sulfate"]]) & !is.na(pollutionFrame[["nitrate"]])
    numValidIndicies = sum(validDataIndecies)
    if (numValidIndicies <= threshold) {
      print(paste("  WARNING: there were less valid measurments", numValidIndicies, "in the specified file than the threshold", threshold))
      corrVec[ii] <- NA
      next # skip the rest of this file. The mean for this value already = 0
    } else {
      corrVec[ii] <- cor(pollutionFrame[validDataIndecies, "sulfate"], pollutionFrame[validDataIndecies, "nitrate"])
    }
    
    #nvalues =
    #names(pollutionFrame)
    #nrow(pollutionFrame)
    #print(table(pollutionFrame))
  }
  # remove empty values
  corrVec[!is.na(corrVec)] # last value computed is returned
}