# Load necessary packages
library(lubridate)
library(rstan)  # Stan interface for R
options(mc.cores = parallel::detectCores())

##########################################################################################
### Function for Logistic Model with RStan 
##########################################################################################
run.stan.climbing.model <- function(d, chains=4, iter=4000) {
      
  # Model code
  mod_string <- "
  data {                          
    int<lower=1> C;            // number of climbers
    int<lower=1> N;            // number of ascents
    int<lower=1> P;            // number of pages
    int<lower=1> minPage[C];   // min page for each climber
    int<lower=1> maxPage[C];   // max page for each climber
    int<lower=0,upper=1> y[N]; // ascent success/failure
    int<lower=1> page[N];      // the time block (page) of each ascent
    int<lower=1> c[N];         // the climber of each ascent
    vector[N] x;               // route grade of each ascent
    int meanGradePrior;        // the mean of the grade prior
  }
  parameters {
    // mid-point intercept
    real climberGrade[C, P]; 
    
    // slope of increase in difficulty per grade increment
    real<lower=0.0> m;       
  }
  model {
    m ~ lognormal(-0.5,0.6); // prior on slope
    for(j in 1:C) {
      // prior on grades up to and including first month with data
      for(i in 1:minPage[j]) {
        climberGrade[j, i]  ~ normal(meanGradePrior,5);    
      }
      // prior on grade in months after first that have data
      for(i in (minPage[j]+1):maxPage[j]) {
        climberGrade[j, i] ~ normal(climberGrade[j, i-1], 0.5);     
      }
      // prior on grades on months after data
      for(i in (maxPage[j]+1):P) {
        climberGrade[j, i]  ~ normal(meanGradePrior,5);    
      }
    }
    // likelihood
    for (i in 1:N) {
      y[i] ~ bernoulli_logit(m*(climberGrade[c[i], page[i]]-x[i])); 
    }    
  }
  
  "
  
  # Run the model
  time = system.time(fit1 <- stan(model_code = mod_string, data=d, chains=chains, iter=iter))
  print(paste0("Stan analysis took: ", time["elapsed"]))
  
  return (list(fit=fit1, time=time))
}

##########################################################################################
# Function to construct data for Stan analysis
#   Takes a list of climbers and a data frame of ascents
##########################################################################################

source("ascents.R")

construct.data.for.stan.climbing.model <- function(startDate, climbers, df, mean.grade.prior=18) {

  for (climber in 1:length(climbers)) {

    lb.routes <- df[df$account.id == climbers[climber],]
    
    lb.routes$Ascent.Type <- lb.routes$ascent.type
    lb.routes$year <- year(lb.routes$date)
    lb.routes$Ascent.Date <- lb.routes$date
    lb.routes$Route.Name <- lb.routes$route.name

    lb.routes$climber = climber
    
    lb.routes$page <- (lb.routes$year - year(startDate)) * 12 + month(lb.routes$date) - month(startDate) + 1

    if (climber == 1) {
      d <- list(N = nrow(lb.routes), P=max(lb.routes$page), page=lb.routes$page, y = lb.routes$success, 
           x = as.integer(lb.routes$grade), c = lb.routes$climber, minPage = min(lb.routes$page), maxPage=max(lb.routes$page))
    } else {
        d$c = c(d$c, lb.routes$climber)
        d$page = c(d$page, lb.routes$page)
        d$P = max(d$P, max(lb.routes$page))
        d$N = d$N + nrow(lb.routes)
        d$y = c(d$y, lb.routes$success)
        d$x = c(d$x, as.integer(lb.routes$grade))
        d$minPage = c(d$minPage, min(lb.routes$page))
        d$maxPage = c(d$maxPage, max(lb.routes$page))
    }
  }
  d$C = length(climbers)
  d$meanGradePrior = mean.grade.prior
  
  return (d)
}

##########################################################################################
# Plots a figure from the results of a Bayesian analysis of climbing grades
##########################################################################################

plot.stan.climbing.results <- function(startDate, endDate, fit1, d, filename, ylab="Grade", lb, to.png=T, cex=0.9) { 

  startDate = as.Date(startDate)
  endDate = as.Date(endDate)
  
  df <- as.data.frame(fit1)

  climber = d$C;
  miny=min(d$x);
  maxy=max(d$x)-1;

  quants <- c(0.025, 0.5, 0.975)
  cpg <- as.data.frame(t(apply( df[,1:(ncol(df)-2)], 2 , quantile , probs = quants , na.rm = TRUE )))
  mean <- apply( df[,1:(ncol(df)-2)], 2 , mean , na.rm = TRUE )
  
  outline.col <- rainbow(climber, alpha=0.67)
  fill.col <- rainbow(climber, alpha=0.33)

  if (to.png) {
    pngFile <- paste0(filename)
    png(pngFile, width=1800, height=900, pointsize=24)
  }
  par(mfrow=c(1, 2))

  x <- seq.Date(startDate, endDate, by = "month")
  xlab <- seq.Date(startDate, endDate, by = "quarter")

  if (climber <= 8) {
    plot.credible.interval=T;
  } else {
    plot.credible.interval=F;
  } 
  made.plot=F

  for (i in 1:climber) {

    stepx <- c(x[d$minPage[i]], rep(x[(d$minPage[i]+1):(d$maxPage[i])], each=2), x[d$maxPage[i]+1])

    climberName = paste0("climberGrade\\[", i,",")
    
    cpgc <- cpg[grepl(climberName, rownames(cpg)),]
  
    cpgc <- cpgc[d$minPage[i]:d$maxPage[i],]
        
    y <- cpgc[,"50%"]

    yu <- cpgc[,"97.5%"]
    yl <- cpgc[,"2.5%"]
  
    stepy <- rep(y, each=2)
    stepyu <- rep(yu, each=2)
    stepyl <- rep(yl, each=2)
    
    if (length(stepx) == length(stepy)) {
      if (!made.plot) {
        
        if (is.factor(lb$grade)) {
          plot(stepx,stepy, type="n", col="red", xlab="Date", ylab=ylab, xlim=c(startDate, endDate), ylim=c(miny,maxy), xaxt="n", yaxt="n")
          axis(2, at=1:length(levels(lb$grade)), labels=levels(lb$grade), las=2, cex.axis=cex)
        } else {
          plot(stepx,stepy, type="n", col="red", xlab="Date", ylab=ylab, xlim=c(startDate, endDate), ylim=c(miny,maxy), xaxt="n")
        }
        axis(1, xlab, format(xlab, "%d %b %y"), cex.axis = cex)
        made.plot <- T
      }
      lines(stepx,stepy, col=outline.col[i], lwd=2)

      if (plot.credible.interval) {
        polygon(c(stepx, rev(stepx)), c(stepyl ,rev(stepyu)), col = fill.col[i], border = NA )

        lines(stepx,stepyl, col=outline.col[i])
        lines(stepx,stepyu, col=outline.col[i])
      }
    }
  }

  #legend("topleft", legend=climbers, col=outline.col,pch=16)

  par(lwd=3)
  
  hist(exp(df$m), xlab="grade increment difficulty increase", main="", freq=F, col="gray", border=F, breaks=30)
  
  if (to.png) {
    dev.off();
  }
}


compute.mean.grades <- function(df, climber.count, data) {
  
  quants <- c(0.025, 0.5, 0.975)
  cpg <- as.data.frame(t(apply( df[,1:(ncol(df)-2)], 2 , quantile , probs = quants , na.rm = TRUE )))
  mean <- apply( df[,1:(ncol(df)-2)], 2 , mean , na.rm = TRUE )
  
  mean.grade <- list()
  
  for (i in 1:climber.count) {
    
    climberName = paste0("climberGrade\\[", i,",")
    
    cpgc <- cpg[grepl(climberName, rownames(cpg)),]
    
    cpgc <- cpgc[data$d$minPage[i]:data$d$maxPage[i],]
    
    mean.grade[[i]] <- mean(cpgc[,2])
  }
  mean.grade <- unlist(mean.grade)
  
  return (mean.grade)
}
