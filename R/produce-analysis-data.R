library(stringr)
library(xtable)
library(knitr)

source("thecrag-json.R")
source("climbing-stan.R")
source("ascent-plotting.R")
source("ascent-filtering.R")
source("ascents.R")

produce.analysis.data <- function(params, res, routes) {

  #########################################
  # Analysis settings
  #########################################

  startDate = as.Date(params$startDate)
  endDate = as.Date(params$endDate)
  file.stem = paste0(params$out.path,params$data.set.name.short,"/ascents-from-", startDate,"-to-", endDate, "-minAscents", params$min.ascents, "-minFails", params$min.failures,"-", paste0(str_replace(params$gear, " ", "_"), collapse="+"),"-",params$grade.type)

  suffix <- ""

  if (params$filter.by.tickprop.lrp) suffix = "-tl"
  if (params$per.session) suffix = paste0(suffix, "-session")

  file.stem <- paste0(file.stem, suffix)
  
  print("file stem created")

  #########################################
  # initial processing
  #########################################

  res2 <- process.file(res, startDate, endDate, matching.gear=params$gear, grade.type=params$grade.type, min.grade=params$min.grade, route.df=routes)
  print("initial processing completed")
  
  res2$df$success <- as.integer(res2$df$success)
  
  

  #########################################
  # Checking logging patterns
  #########################################

  # make sure there is min failures
  res3 <- filter.climbers.by.ascent.counts(res2, min.ascents=params$min.ascents, min.failures=params$min.failures)
  print("filter.climbers.by.ascent.counts completed")
  
  print(paste0("params$filter.by.tickprop.lrp=",params$filter.by.tickprop.lrp))
  
  if (params$filter.by.tickprop.lrp) {
    res3 = filter.by.tick.proportion.and.lonely.redpoints(res3, res, max.tip.prop=0.05, max.lonely.redpoint.prop=0.05)
    print("filter.by.tick.proportion.and.lonely.redpoints completed")
  } 

  df.top <- res3$df
  df.top$ascent.type <- as.character(df.top$ascent.type)
  

  climbers <- sort(unique(df.top$account.id));
  fr <- res3$filter.results
  
  if (params$max.climbers < length(climbers)) {
    
    start.rows <- nrow(df.top)
    
    climbers <- sort(table(df.top$account.id), decreasing=T)
    df.top <- df.top[df.top$account.id %in% names(climbers[1:params$max.climbers]),]
    print("filter by max climbers completed")
    climbers <- sort(unique(df.top$account.id));
    
    fr <- add.filter.summary(start.rows, nrow(df.top), "Kept top 100 climbers by number of ascents", fr)
  }
  
  df.final = df.top
  final.filter.results = fr
  
  ##########################################################################################
  # If per session analysis then keep best ascent in each session
  ##########################################################################################

  print(paste0("params$per.session=",params$per.session))
  if (params$per.session) {
  
    in.rows <- nrow(df.final)
  
    lb.routes <- df.final[df.final$account.id == climbers[1],]
    lb.routes <- keep.best.ascent.in.session(lb.routes)
    print("keep.best.ascent.in.session completed")
    
    for (climber in 2:length(climbers)) {
    
      lb.next <- df.final[df.final$account.id == climbers[climber],]
      lb.next <- keep.best.ascent.in.session(lb.next)
      
      lb.routes <- rbind(lb.routes, lb.next)
    }
    df.final <- lb.routes
    final.filter.results <- add.filter.summary(in.rows, nrow(df.final), "Keep only the best ascent on each route-climber-day", res3$filter.results)
  }

  pngname <- paste0(file.stem, "-regression.png")

  png(pngname, width=900, height=900, pointsize=24)
  print(paste0("plot.all.attempts about to start using ", nrow(df.final), " ascents."))
  res.plot <- plot.all.attempts(df.final)
  print("plot.all.attempts completed")
  dev.off();

  ##########################################################################################
  # construct data for Stan analysis
  ##########################################################################################

  if (params$grade.type == "AU") {
    mean.grade.prior = as.numeric(params$mean.grade.prior)
  } else {
    mean.grade.prior = match(params$mean.grade.prior, levels(df.final$grade))
  }
  
  d <- construct.data.for.stan.climbing.model(startDate, climbers, df.final, mean.grade.prior)
  
  data = list(d=d, df=df.final, filter=final.filter.results, m=res.plot$m, grade.range=res.plot$grade.range, file.stem=file.stem, suffix=suffix, regressionpng=pngname, climbers=climbers)
  
  return (data)
}
