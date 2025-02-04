library(tools)  # to get file name without extension


# convert files -----

summarizeCSV <- function(filename) {
  
  skiplines <- getLinesToSkip(filename)
  
  df <- read.csv(filename, skip=skiplines)
  
  # select relevant rows
  # we want to go with all events for now:
  events <- c("STAY_CENTRE",
              "TARGET_ON",
              "HOLD_AT_TARGET",
              "WAIT_FOR_BUTTON",
              "TASK_BUTTON_10_CLICKED")
  
  # select only samples that have any of the events:
  df <- df[which(df$Event.name %in% events),]
  
  # make sure these variables are numeric:
  df <- fillInNumericColumns(df, c(1,2))

  # select relevant columns:
  
  # let's say we want these columns
  # (might add some more later on)
  columns <- c( "Trial..",
                "TP.Row",
                "Event.name",
                "Frame.time..s.",
                "Right..Hand.position.X",
                "Right..Hand.position.Y",
                "Left..Hand.position.X",
                "Left..Hand.position.Y")
  
  # so now we only take those columns from the data frame:
  df <- df[, columns]
  
  # we give these columns easier names to work with:
  names(df) <- c('trial_no', 'trial_protocol', 'event_id', 'frametime_s', 'rightX_m', 'rightY_m', 'leftX_m', 'leftY_m')
  
  # and we convert the remaining character columns to numeric type:
  for (colname in c('frametime_s', 'rightX_m', 'rightY_m', 'leftX_m', 'leftY_m')) {
    colidx <- which(names(df) == colname)
    df[,colidx] <- as.numeric(df[,colidx])
  }
  
  # we return the much smaller data frame to the caller:
  return(df)  
  
}

getLinesToSkip <- function(filename) {
  
  # open a file connection with only reading rights
  # (we don't want to change the file accidentally)
  con <- file(filename,open="r")
  
  # we'll set this to FALSE when we find the start of the data
  ongoing <- TRUE
  
  # we need to keep track of which line we've previously read:
  lineno <- 0
  
  skip <- NA
  
  # as long as the start of the actual data hasn't been found,
  # we keep running this block of code:
  while (ongoing) { 
    
    # we read a single line:
    line <- readLines(con,           # from the file connection
                      n = 1)         # read 1 line
    
    # the length of the line will be 0 at the end of the file
    # so in that case, we should stop looking as well
    if (length(line) == 0) { ongoing <- FALSE }
    
    # the data part begins at a line that starts with "Trial #"
    if (substr(line,1,7) == "Trial #") {
      
      # we now know where to start reading the file:
      skip <- lineno
      
      # so if we see that we have found what we need
      # and can stop looking:
      ongoing <- FALSE
      
    }
    
    # we need to increment the line count:
    lineno <- lineno + 1
    
  }
  
  # close up the connection to the file:
  close(con)
  
  # return the relevant number:
  return(skip)
  
  # note that this will be NA if we didn't find a correct start
  
}

fillInNumericColumns <- function(df, column_nos) {
  
  # # things should start right away...
  # # but just in case we set it to NA (NOT APPLICABLE or something... an unknown value)
  # current_values <- rep(NA, length(column_nos))
  # 
  # # we take just the relevant columns to work with:
  # trialcols <- df[,column_nos]
  
  # # we create a new data frame to store continuous numeric values:
  # newtrialcols <- data.frame(  matrix(NA, nrow=dim(df)[1], ncol=length(column_nos) ) )
  # 
  # # old (potentially very slow) way:
  # 
  # # we loop through the rows of the data frame:
  # for (entry in c(1:dim(df)[1])) {
  #   # if the entry has more than 0 characters, we check it out:
  #   for (cn in c(1:length(column_nos))) {
  #     if (nchar(trialcols[entry,cn]) > 0) {
  #       # converting to numeric type should give NA (and a warning) if the string
  #       # contains something that ca not be converted to a numeric type:
  #       if (suppressWarnings(!is.na(as.numeric(trialcols[entry,cn])))) {
  #         # if we do not get NA, we use this as the current value:
  #         current_values[cn] <- as.numeric(trialcols[entry,cn])
  #       }
  #     }
  #   }
  #   # we store the current numeric values in the new columns:
  #   newtrialcols[entry,] <- current_values
  # }
  # 
  # # and replace the trial# column with the new vector:
  # for (cn in c(1:length(column_nos))) {
  #   df[,column_nos[cn]] <- newtrialcols[cn]
  # }
  
  # trying out something much simpler:
  for (cn in c(1:length(column_nos))) {
    df[,column_nos[cn]] <- as.numeric(df[,column_nos[cn]])
  }
  # (doesn't seem to be slower, so keeping this)
  
  
  # and return the full data frame:
  return(df)
  
}


# find / convert / organize participants -----

# first we need to identify participants
# the data comes organized in 2 folders
# one for left-handers and one for right-handers
#
# we need to specify the folder for left-handers,
# and the folder for right-handers
#
# there are no subfolders
#
# the data for each participant comes in 12 files
# with way too much data in it
# we will find all files and convert them to simpler forms


# findParticipants(folders = c( 'left' = '../APM/Left-Handers', 'right' = '../APM/Right-Handers'))

findParticipants <- function(folders) {
  
  participants <- list()
  
  for (group in c('left','right')) {
    
    folder <- folders[group]
    group_participants <- c()
    
    # list files in the folder:
    files <- list.files( path = folder,
                         pattern = '*.csv' )
    
    # loop through all the files:
    for (filename in files) {
      
      # unnecessary check that the filename starts with 'APM_'
      if (substr(filename, 1, 4) == 'APM_') {
        
        # extract the ID
        thisID <- substr(filename, 5, 10)
        # add the ID to a list of IDs in this group:
        group_participants <- c(group_participants, thisID)
      }

    }
    
    # sort IDs by group and only keep unique IDs
    participants[[group]] <- unique(group_participants)
    
  }
  
  return(participants)
  
}


convertAllParticipantsData <- function(folders, participants) {
  
  for (group in c('left','right')) {
    
    src_folder = folders[group]
    IDs = participants[[group]]
    
    dir.create(file.path('data', group))
    
    for (ID in IDs) {
      
      dir.create(file.path('data', group, ID))
      
      participantFiles <- list.files( path = file.path( folders[group] ),
                                      pattern = sprintf('APM_%s*', ID))
      
      if (length(participantFiles) != 12) {
        cat(sprintf('WARNING: not 12 files for participant: %s in %s-handers\n', ID, group))
      }
      
      for (pfile in participantFiles) {
        
        # strip the last 8 chars of the filename (without extention)
        outfile <- file.path( 'data',
                              group,
                              ID,
                              sprintf( '%s.csv',  c( substr(pfile, 1, 15)))
        )
        
        if (file.exists(outfile)) {
          
          cat(sprintf('skip existing: %s\n',outfile))
          
        } else {
          
          cat(sprintf('working on: %s\n',outfile))
          
          df <- summarizeCSV( filename = file.path( folders[group], pfile ) )
          
          write.csv( df, 
                     file = outfile,
                     row.names = FALSE )
        }
        
      }
      
    }
    
    
  }
  
  # no output returned:
  # files should be in their respective folders
  
}

convertData <- function(folders) {
  
  
  participants <- findParticipants(folders)
  
  convertAllParticipantsData(folders, participants)
  
}


# demographics - handedness -----

addHandedness <- function() {
  
  df <- read.csv('data/demographics.csv', stringsAsFactors = F)
  
  df$handedness_score <- 0
  
  for (column in c('writing','drawing','throwing','scissors','toothbrush','knife','spoon','broom','match','box')) {
    df$handedness_score <- df$handedness_score + c('left'=-10, 'right'=10)[df[,column]]
  }
  
  write.csv(df, 'data/demographics.csv', quote=F, row.names=F)  
}