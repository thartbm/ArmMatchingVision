
# pre-processing data -----

getParticipantData <- function(ID, 
                               group, 
                               matched_hands=c('left','right'),
                               matching_hands=c('seen','unseen')) {
  
  participant_data <- NA
  
  path <- sprintf('data/%s/%s/', group, ID)
  
  for (matched_hand in matched_hands) {
    
    for (matching_hand in matching_hands) {
      
      for (block in c(1,2,3)) {
        
        filename <- sprintf('%sAPM_%s_%s%s_%d.csv',
                            path,
                            ID,
                            c('left'='R', 'right'='L')[matched_hand],
                            substr(matching_hand,1,1),
                            block
                            )
        
        if (!file.exists(filename)) {
          # skip processing this file...
          cat(sprintf('missing file: %s\n',filename))
          next
        } else {
          
          # add the file
          df <- read.csv(filename, stringsAsFactors = FALSE)
          
          df <- df[which(df$event_id == 'TASK_BUTTON_10_CLICKED'),]
          df <- df[,!(names(df) %in% c('event_id'))]
          
          df$participant <- ID
          df$group <- group
          df$matched_hand <- matched_hand
          df$matching_hand_seen <- (matching_hand == 'seen')
          df$block <- block
          
          df$dominant <- (matched_hand == group)
          
          if (matched_hand == 'left') {
            df$devX_cm <- df$rightX_m + df$leftX_m
            df$devY_cm <- df$rightY_m - df$leftY_m
          }
          if (matched_hand == 'right') {
            df$devX_cm <- df$leftX_m + df$rightX_m
            df$devY_cm <- df$leftY_m - df$rightY_m
          }
          
          df$devX_cm <- round( (100 * df$devX_cm), 3)
          df$devY_cm <- round( (100 * df$devY_cm), 3)
          
          if (is.data.frame(participant_data)) {
            participant_data <- rbind(participant_data, df)
          } else {
            participant_data <- df
          }
          
        }
        
      }
      
    }
    
  }
  
  return(participant_data)
  
}


getParticipants <- function() {
  
  left  <- basename(list.dirs(path='data/left/'))
  left  <- left[which(left != 'left')]
  right <- basename(list.dirs(path='data/right/'))
  right <- right[which(right != 'right')]
  
  demographics <- read.csv('data/demographics.csv', stringsAsFactors = F)
  left  <- left[ which(left  %in% demographics$ID[which(demographics$handedness_score < 0)])]
  right <- right[which(right %in% demographics$ID[which(demographics$handedness_score > 0)])]
  
  df <- rbind( data.frame('ID'=left,
                          'group'=rep('left',length(left))),
               data.frame('ID'=right,
                          'group'=rep('right',length(right)))
               )
  
  
  
  # demo <- read.csv('data/demographics.csv', stringsAsFactors = F)
  # demo <- demo[which(demo$handedness_score != 0),]
  # demo$handedness_group <- NA
  # demo$handedness_group[which(demo$handedness_score > 0)] <- 'right'
  # demo$handedness_group[which(demo$handedness_score < 0)] <- 'left'
  # 
  
  return(df)
               
}

getAllData <- function(outfile=NULL) {
  
  # this is now hard-coded... but will come from the demographics file later on:
  # IDs     <- c('0bd1d0', '4ab788', '6d91a6', 'a93fee', 'bbcf73', '92ad8e', '437f47', '840d07', 'ac4a66', 'c03555')
  # groups  <- c('left',   'left',   'left',   'left',   'left',   'right',  'right',  'right',  'right',  'right' )
  
  df <- getParticipants()
  IDs <- df$ID
  groups <- df$group
  
  allData <- NA
  
  for (ppno in c(1:length(IDs))) {
    
    pdf <- getParticipantData( ID = IDs[ppno],
                               group = groups[ppno] )
    
    if (is.data.frame(allData)) {
      allData <- rbind(allData, pdf)
    } else {
      allData <- pdf
    }
    
  }
  if (is.null(outfile)) {
    return(allData)
  } else {
    write.csv(allData, file=outfile, row.names = FALSE, quote = FALSE)
  }
  
}


# precision / accuracy -----

getMatchingDescriptor  <- function(descriptor='precision', grid.variables=c('dominant')) {
  
  # get all the data we need:
  allData <- getAllData()
  
  
  # create combinations of values on interesting variables:
  grid.factors <- list()
  grid.variables <- c(grid.variables, 'participant')
  for (gv in grid.variables) {
    values <- sort(unique(allData[,gv]))
    grid.factors[[gv]] <- values
  }
  combinations <- expand.grid(grid.factors)
  
  outdata <- combinations
  outdata[,descriptor] <- NA
  outdata[,'group']      <- NA
  
  # loop through combinations:
  for (combno in c(1:dim(combinations)[1])) {
    
    # extract variables making up this combination:
    combi <- combinations[combno,]
    
    # select data :
    subdf <- allData
    for (key in names(combi)) {
      value = unlist(combi[key])
      subdf <- subdf[which(subdf[,key] == value),]
    }
    
    outdata[combno,'group'] <- subdf$group[1]
    # get descriptors for the data on this combination:
    if (descriptor == 'precision') {
      outdata[combno,descriptor] <- get95CIellipse(subdf, vars=c('devX_cm','devY_cm'))
    }
    if (descriptor == 'accuracy') {
      outdata[combno,descriptor] <- mean(sqrt((subdf$devX_cm^2)+(subdf$devY_cm^2)))
    }
    
  }
  
  return(outdata)

}


get95CIellipse <- function(df, vars=NULL) {
  
  if (!is.null(vars)) {
    df <- df[, vars]
  }
  
  return(qnorm(0.975) * prod(princomp( df )$sdev) * pi)

}

saveFullDataFrame <- function(indvars=c('dominant', 'matching_hand_seen')) {
  
  
  pr_df <- getMatchingDescriptor(grid.variables = indvars, descriptor = 'precision')
  ac_df <- getMatchingDescriptor(grid.variables = indvars, descriptor = 'accuracy')
  
  df <- merge( x = pr_df,
               y = ac_df,
               by = c('group', 'participant', indvars))
  
  demo <- read.csv('data/demographics.csv', stringsAsFactors = F)
  demo <- demo[which(demo$handedness_score != 0),]
  demo$handedness_group <- NA
  demo$handedness_group[which(demo$handedness_score > 0)] <- 'right'
  demo$handedness_group[which(demo$handedness_score < 0)] <- 'left'
  
  for (ID in df$participant) {
    df$group[which(df$participant == ID)] <- demo$handedness_group[demo$ID == ID]
  }
  
  write.csv( x         = df,
             file      = 'data/AOVdata.csv',
             row.names = F,
             quote     = F )
  
}