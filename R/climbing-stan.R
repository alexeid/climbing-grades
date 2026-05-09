# Load necessary packages
library(lubridate)
library(rstan)  # Stan interface for R
options(mc.cores = parallel::detectCores())

##########################################################################################
### Function for Logistic Model with RStan
###   estimate.w = FALSE (default): the Wiener-process step SD is fixed at 0.5/month
###   estimate.w = TRUE:  w is a parameter with Gamma(2, 4) prior, jointly estimated
###     with m and the climber-grade trajectories.
###   m.prior.code: a Stan statement giving the prior on m. Defaults to the headline
###     "m ~ lognormal(-0.5, 0.6);". Pass any valid Stan sampling statement (or
###     "target += ...;") to swap the prior, e.g. "m ~ lognormal(-0.5, 1.0);" or
###     "m ~ gamma(2, 4);". Used by the prior-sensitivity script.
###   sample_file: prefix for per-chain CSV outputs. Stan appends "_<chain>.csv".
###     Defaults to a tempfile prefix; pass a stable path (e.g. under logs/)
###     to inspect partial draws while the run is in flight.
##########################################################################################
run.stan.climbing.model <- function(d, chains=4, iter=4000, estimate.w=FALSE,
                                    m.prior.code="m ~ lognormal(-0.5, 0.6);",
                                    sample_file=tempfile("climbing-stan-chain_")) {

  message("Per-chain Stan draws will be written to: ", sample_file, "_<chain>.csv")
  message("Prior on m: ", m.prior.code)

  mod_string_fixed_w <- "
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
    real climberGrade[C, P];   // mid-point intercept
    real<lower=0.0> m;         // slope of increase in difficulty per grade increment
  }
  model {
    __M_PRIOR_PLACEHOLDER__  // prior on slope
    for (j in 1:C) {
      for (i in 1:minPage[j]) {
        climberGrade[j, i] ~ normal(meanGradePrior, 5);
      }
      for (i in (minPage[j]+1):maxPage[j]) {
        climberGrade[j, i] ~ normal(climberGrade[j, i-1], 0.5);
      }
      for (i in (maxPage[j]+1):P) {
        climberGrade[j, i] ~ normal(meanGradePrior, 5);
      }
    }
    for (i in 1:N) {
      y[i] ~ bernoulli_logit(m * (climberGrade[c[i], page[i]] - x[i]));
    }
  }
  "

  mod_string_estimate_w <- "
  data {
    int<lower=1> C;
    int<lower=1> N;
    int<lower=1> P;
    int<lower=1> minPage[C];
    int<lower=1> maxPage[C];
    int<lower=0,upper=1> y[N];
    int<lower=1> page[N];
    int<lower=1> c[N];
    vector[N] x;
    int meanGradePrior;
  }
  parameters {
    real climberGrade[C, P];
    real<lower=0.0> m;
    real<lower=0.0> w;         // Wiener-process step SD, jointly estimated
  }
  model {
    __M_PRIOR_PLACEHOLDER__
    w ~ gamma(2, 4);           // mean 0.5, mode 0.25, sd ~0.35; density linear at zero
                                // (so we are not a priori favouring a degenerate w=0 process)
    for (j in 1:C) {
      for (i in 1:minPage[j]) {
        climberGrade[j, i] ~ normal(meanGradePrior, 5);
      }
      for (i in (minPage[j]+1):maxPage[j]) {
        climberGrade[j, i] ~ normal(climberGrade[j, i-1], w);
      }
      for (i in (maxPage[j]+1):P) {
        climberGrade[j, i] ~ normal(meanGradePrior, 5);
      }
    }
    for (i in 1:N) {
      y[i] ~ bernoulli_logit(m * (climberGrade[c[i], page[i]] - x[i]));
    }
  }
  "

  mod_string <- if (estimate.w) mod_string_estimate_w else mod_string_fixed_w
  mod_string <- gsub("__M_PRIOR_PLACEHOLDER__", m.prior.code, mod_string, fixed=TRUE)

  time = system.time(fit1 <- stan(model_code = mod_string, data=d, chains=chains, iter=iter,
                                   sample_file=sample_file))
  print(paste0("Stan analysis took: ", time["elapsed"]))

  diagnostics <- extract.convergence.diagnostics(fit1, estimate.w=estimate.w)

  return (list(fit=fit1, time=time, diagnostics=diagnostics,
               mcmc.settings=list(chains=chains, iter=iter, warmup=floor(iter/2),
                                  estimate.w=estimate.w)))
}

##########################################################################################
### Extract convergence diagnostics (R-hat, bulk/tail ESS) from a stanfit object.
### Optionally also extracts diagnostics for w when the joint-estimation model was used.
##########################################################################################
extract.convergence.diagnostics <- function(fit, sample.climber.params=20, estimate.w=FALSE) {

  s <- summary(fit)$summary
  param.names <- rownames(s)

  cols <- c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")
  m.row <- s["m", cols, drop=FALSE]
  w.row <- if (estimate.w && "w" %in% param.names) s["w", cols, drop=FALSE] else NULL

  climber.idx <- grep("^climberGrade\\[", param.names)
  if (length(climber.idx) > sample.climber.params) {
    set.seed(1)
    climber.idx <- sort(sample(climber.idx, sample.climber.params))
  }
  climber.rows <- s[climber.idx, c("Rhat", "n_eff"), drop=FALSE]

  rhats <- s[, "Rhat"]
  neffs <- s[, "n_eff"]

  summary.stats <- list(
    m = m.row,
    w = w.row,
    climber.subset = climber.rows,
    rhat.range = range(rhats, na.rm=TRUE),
    rhat.max = max(rhats, na.rm=TRUE),
    neff.min = min(neffs, na.rm=TRUE),
    neff.median = median(neffs, na.rm=TRUE),
    n.params = length(param.names),
    n.divergent = sum(sapply(rstan::get_sampler_params(fit, inc_warmup=FALSE),
                              function(x) sum(x[, "divergent__"]))),
    n.maxtreedepth = sum(sapply(rstan::get_sampler_params(fit, inc_warmup=FALSE),
                                 function(x) sum(x[, "treedepth__"] >= 10)))
  )

  return(summary.stats)
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
