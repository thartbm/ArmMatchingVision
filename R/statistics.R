library(afex)

customANOVA <- function(depvar='precision', indvars=c('dominant', 'matching_hand_seen')) {
  
  aov_data <- getMatchingDescriptor(grid.variables = indvars, descriptor = depvar)
  
  aov_data$group <- as.factor(aov_data$group)
  
  print( afex::aov_ez( id = 'participant',
                       dv = depvar,
                       data = aov_data,
                       between = 'group',
                       within = indvars)
  )
  
}

noVisionANOVAs <- function() {
  
  # customANOVA(depvar = 'precision', indvars=c("dominant"))
  # customANOVA(depvar = 'accuracy', indvars=c("dominant"))
  customANOVA(depvar = 'precision')
  customANOVA(depvar = 'accuracy')
  
}

# AOV data based ANOVAs ------

dataANOVA <- function(depvar='precision', indvars=c('dominant', 'matching_hand_seen')) {
  
  aov_data <- read.csv('data/AOVdata.csv', stringsAsFactors = F)
  
  aov_data$group <- as.factor(aov_data$group)
  
  print( afex::aov_ez( id = 'participant',
                       dv = depvar,
                       data = aov_data,
                       between = 'group',
                       within = indvars)
  )
  
}

bothDataANOVAs <- function() {
  
  for (depvar in c('accuracy', 'precision')) {
    
    cat(sprintf('*** ANOVA on %s:\n\n',toupper(depvar)))
    
    dataANOVA(depvar=depvar)
    
  }
  
}