
participantBlockOrder <- function(N=100) {
  
  # we get this pre-generated list of random IDs
  ID_table <- read.csv('randomIDs.csv', stringsAsFactors = FALSE)
  
  # these are the 4 main conditions:
  conditions <- c('Rs', 'Ru', 'Ls', 'Lu')
  
  # this is a vector for all participant IDs:
  participant <- c()
  
  # we store conditions in a list of vectors:
  # - each vector represents one run (or block)
  # - in those vectors, the first entry is for the first participant,
  #   the second entry for the second participant, and so on: a table of sorts
  runs <- list()
  for (runno in c(1:12)) {
    runs[[sprintf('run%d',runno)]] <- c()
  }
  
  # now loop through the participants:
  for (ppno in c(1:N)) {
    
    # we need track of what to do with the latin square
    latsq_idx <- (ppno-1) %% 4
    
    # get new latin square:
    if (latsq_idx == 0) {
      latsq <- Reach::latinSquare(4)
    } else {
      # or shuffle the columns:
      latsq <- latsq[,c(2,3,4,1)]
    }
    
    # get the first 3 columns:
    order <- latsq[,1:3]
    # put them in one long vector:
    order <- as.vector(order)
    
    # we start out with doing no motion blocks at all:
    motion <- rep(0,12)
    # for each condition, we pick 1 block at random to make a motion block:
    for (mc in c(1:4)) {
      idx <- which(order == mc)
      motion[sample(idx,1)] <- 1
    }
    
    # prepend 'APM' to the participant, and store in new vector
    participant <- c(participant, sprintf('APM_%s', ID_table$randomIDs[ppno]))
    
    # add this participants run/conditions to the list of vectors:
    for (runno in c(1:12)) {
      entry <- conditions[order[runno]]
      if (motion[runno] == 1) {
        # add M if people are supposed to move/follow the robot instead of wait
        # entry <- sprintf('%sM',entry)
      }
      runs[[sprintf('run%d',runno)]] <- c(runs[[sprintf('run%d',runno)]], entry)
    }
    
  }
  
  # convert to a data frame:
  df <- data.frame(participant)
  for (runno in c(1:12)) {
    df[sprintf('run%d',runno)] <- runs[[sprintf('run%d',runno)]]
  }
  
  # write to a file:
  # write.csv(df,'participant_block_order.csv', quote=TRUE, row.names=FALSE)
  return(df)
}