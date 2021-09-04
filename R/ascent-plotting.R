##########################################################################################
# A function to produce ascent summary from the logbook ascent data
##########################################################################################
ascent.summary <- function(lb.routes, gradeColName="grade", successColName="success") {
  
  tab <- table(unlist(lb.routes[,gradeColName]), unlist(lb.routes[,successColName]))
  
  if (ncol(tab) == 1) {
    print(tab)
    print(nrow(lb.routes))
    print(paste0("success.count=",sum(lb.routes[,successColName])))
    print(table(lb.routes[,successColName]))
    print(table(lb.routes[,gradeColName]))
    
    return -1;
  }
  
  summary <- data.frame(
    grade=as.numeric(rownames(tab)), 
    attempts = tab[,1]/tab[,2], 
    total.attempts = tab[,1] + tab[,2], 
    counts = tab[,2])
  
  summary <- summary[is.finite(summary$attempts) & summary$attempts>0.0,]
  
  return (summary)
}

##########################################################################################
# plot attempts of all climbers
##########################################################################################
plot.all.attempts <- function(lb) {
  
  climbers <- unique(lb$account.id)
  
  legend = c();
  
  plot.text = length(climbers) <= 8;
  
  if (plot.text) {
    cols <- rainbow(length(climbers))
  } else {
    cols <- rep(rgb(0.0, 0.0, 1.0, 0.5), length(climbers))
  }
  m <- c()
  t <- c()
  
  make.plot = T
  
  summary <- list()
  for (c in 1:length(climbers)) {
    summary[[c]] <- ascent.summary(lb[lb$account.id == climbers[c],])
    
    if (c == 1) {
      totalsummary = summary[[1]]
    } else {
      totalsummary = rbind(totalsummary, summary[[c]])
    }
  }  

  for (c in 1:length(climbers)) {
    
    if (is.data.frame(summary[[c]])) {
      if (nrow(summary[[c]])>1) {
        if (make.plot) {
          x <- totalsummary$grade
          y <- log(totalsummary$attempts)
        
          plot(x, y, type="n", xlab="Grade", ylab="log failures per success", pch=19)
          make.plot = F
        }
      
        res = plot.attempts(summary[[c]], col=cols[c], gradeName=paste0("C", c), plot.text=plot.text)
        m <- c(m, res$m)
        t <- c(t, res$t)
      }
    }
  }
  
  if (plot.text) legend("topleft", t, col=cols, pch=19)
  
  return (m)
}

##########################################################################################
# plot attempts of a single climber from the provided climber summary table
##########################################################################################
plot.attempts <- function(summary, col=col, gradeName="C", plot.text=F) {
  x <- summary$grade
  y <- log(summary$attempts)
  
  #se <- summary$attempts.se
  
  counts <- summary$count
  
  lm <- lm(y~x)
  abline(lm, col=col)
  #segments(x,y-se,x,y+se, col=col)
  points(x, y, pch=19, col=col)
  
  if (plot.text) text(x,y,counts,pos=4, col=col)
  
  m = lm$coefficients[2]
  c = lm$coefficients[1]
  
  g <- -c/m;
  
  t <- paste0("m = ",round(lm$coefficients[2],2), "; ", gradeName," = ", round(g,1))
  
  return (list(m=m, t=t));
}