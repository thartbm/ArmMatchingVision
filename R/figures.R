plotHandednessDistribution <- function() {
  
  demographics <- read.csv('data/demographics.csv', stringsAsFactors = F)
  hist(demographics$handedness_score, breaks=seq(-110,110,by=20),xlab='handedness score',ylab='frequency',main='',ax=F)
  axis(side=1,at=c(-100,-80,-60,-40,-20,0,20,40,60,80,100))
  axis(side=2,at=seq(0,15,by=5))
  
}

plotDepvar <- function(depvar) {
  
  df <- read.csv('data/AOVdata.csv', stringsAsFactors = F)
  df$depvar <- df[,depvar]
  
  if (depvar == 'accuracy') {
    ylim <- c(0,25)
    yticks <- seq(0,25,5)
    
    ylab = 'average error [cm]'
    
    ylim <- c(0,10)
    yticks <- seq(0,10,2)
  }
  if (depvar == 'precision') {
    ylim <- c(0,450)
    yticks <- seq(0,450,150)
    
    ylab = 'average 95% CI ellipse [cm^2]'
    
    ylim <- c(0,150)
    yticks <- seq(0,150,30)
  }

  plot(-1000,-1000,
       main=depvar,xlab='',ylab='',
       xlim=c(0,5),ylim=ylim,
       bty='n',ax=F)
  
  for (group in c('right','left')) {
    
    for (dominant in c(TRUE, FALSE)) {
      
      subdf <- df[which(df$group == group & df$dominant == dominant),]
      
      avg <- aggregate(depvar ~ matching_hand_seen, data=subdf, FUN=mean)
      CI  <- aggregate(depvar ~ matching_hand_seen, data=subdf, FUN=Reach::getConfidenceInterval)
      
      if (dominant) {
        scol <- '#0066FFFF'
        tcol <- '#0066FF33'
      } else {
        scol <- '#FF6600FF'
        tcol <- '#FF660033'
      }
      
      X=list('right'=c(3,4),'left'=c(1,2))[[group]]
      polygon(x=c(X,rev(X)),
              y=c(CI$depvar[,1],rev(CI$depvar[,2])),
              border=NA,
              col=tcol)
      # print(X)
      lines(x=X,
            y=avg$depvar,
            col=scol)
      
      # print(avg)
      # print(CI)
      
    }
    
  }
  
  legend(x=0.5,y=max(ylim),
         title='hand being matched:',
         legend=c('dominant','non-dominant'),
         lty=c(1,1),
         col=c('#0066FFFF', '#FF6600FF'),
         bty='n')
  
  axis(side=1, at=c(1:2), labels=c('seen','unseen'))
  axis(side=1, at=c(3:4), labels=c('seen','unseen'))
  text(x=1.5,y=max(ylim)/20,labels='left-handed')
  text(x=3.5,y=max(ylim)/20,labels='right-handed')
  axis(side=2, at=yticks)
  
  title(ylab=ylab)
  
}



# inspect file -----


plotOneFile <- function(filename) {
  
  colors <- c('#000000',
              '#FF0000',
              '#00FF00',
              '#0000FF',
              '#FFFF00',
              '#00FFFF',
              '#FF00FF',
              '#FF7700',
              '#7700FF')
  
  
  # load data from file:
  df <- expandTrialInfo(filename)
  # select reelevant rows:
  df <- df[which(df$event_id == 'TASK_BUTTON_10_CLICKED'),]
  
  # determine scale of plot:
  xrange <- range(c(df$rightX_m, df$leftX_m))
  yrange <- range(c(df$rightY_m, df$leftY_m))
  
  xrange[1] <- min(xrange[1],-0.30)
  xrange[2] <- max(xrange[2],0.30)
  yrange[1] <- min(0, yrange[1])
  yrange[2] <- max(0.30, yrange[2])
  
  
  plot(-1000,-1000,
       main='', xlab='x coordinate [m]', ylab='y coordinate [m]',
       xlim=xrange, ylim=yrange, asp=1,
       bty='n',ax=F)
  
  
  for (condition in unique(df$trial_protocol)) {
    
    cdf <- df[which(df$trial_protocol == condition),]
    col <- colors[condition]
    
    points( x = cdf$leftX_m,
            y = cdf$leftY_m,
            pch = 0,
            col = col)
    points( x = cdf$rightX_m,
            y = cdf$rightY_m,
            pch = 1,
            col = col)
    
  }
  
  
  
  axis(side=1,at=c(-.3,-.2,-.1,0,.1,.2,.3))
  axis(side=2,at=c(0,.1,.2,.3))
  
}