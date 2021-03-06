pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of lenght 1, inidcating
  ## the name of the pollutant. Valid values are 'sulfate'
  ## or 'nitrate'
  
  ## 'id' is an integer vector indicating the monitor ID 
  ## numbers to be used
  
  ## NB this function assumes the data file for monitor 123 is
  ## can be found in a CSV file with path:
  ##    "directory/123.csv"
  ##
  
  # check the pollutant argumnet is correctly specified
  if ( (pollutant != "sulfate") && (pollutant != "nitrate")) {
    print(paste("Error: valid values of argument 'pollutant' are 'sulfate' or 'nitrate'"))
    return(NA)   # exit the function
  }
  
  # create a vector to store the mean from each file
  pollutantSumVec = vector(mode="numeric", length = length(id)); # each is initialised to 0
  pollutantCountVec = vector(mode="numeric", length = length(id)); # each is initialised to 0
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
    pollutData <- pollutionFrame[[pollutant]]
    
    
    # find the number of available values in the specified column
    validDataIndecies <- !is.na(pollutData)
    pollutantCountVec[ii] <- sum(validDataIndecies)
    if (0 == pollutantCountVec[ii]) {
      print(paste("  WARNING: there were no valid values for ", pollutant, "in the specified file"))
      next # skip the rest of this file. The mean for this value already = 0
    } else {
      pollutantSumVec[ii] <- sum(pollutData[validDataIndecies])
      print(paste("  Found", pollutantCountVec[ii], "valid values for ", pollutant, ", mean = ", pollutantSumVec[ii]))
    }
    
    #nvalues = 
    #names(pollutionFrame)
    #nrow(pollutionFrame)
    #print(table(pollutionFrame))
  }
  
  # finally, get the mean of the vector of means
  sum(pollutantSumVec) / sum(pollutantCountVec) # last value computed is returned
}