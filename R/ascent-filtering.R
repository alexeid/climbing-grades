source("ascents.R")

##########################################################################################
# A function to process an ascent data frame.
# This function will
#   * keep only sport routes
#   * remove artificial ascents
#   * remove ambiguous ascents
#   * remove greenpoints
#   * remove boulder ascent types
#   * remove non-ascents (e.g. target, hit)
#   * remove ascents that are not graded by Ewbank grade ("AU")
#   * remove ascents lower than min.grade (default <16)   
##########################################################################################
process.file <- function(res, startDate, endDate, min.grade=16, route.df) {
  
  source("thecrag-json.R")
  
  lb <- res$df
  lb$success <- lb$ascent.type %in% clean.ascent.types()
  
  in.rows <- nrow(lb)
  lb <- lb[lb$artificial == 0,]
  out.rows <- nrow(lb)
  filter <- "Exclude artificial ascents"
  
  # keep only sport and top rope gear styles
  in.rows <- c(in.rows, nrow(lb))
  res.gear <- keep.matching.gear(lb, route.df, matching.gear=c("Sport", "Top rope", "Trad", NA, "Unknown"))
  lb <- res.gear$df
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, paste("Exclude gear styles:", paste(res.gear$excluded.gear, collapse=", "), sep=" "))
  
  # remove trad ascents
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[!(lb$ascent.type %in% trad.ascent.types()),]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, paste("Exclude trad ascent types:", paste(trad.ascent.types(), collapse=", "), sep=" "))
  
  # remove boulder ascents
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[!(lb$ascent.type %in% bouldering.ascent.types()),]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, paste("Exclude boulder ascent types:", paste(bouldering.ascent.types(), collapse=", "), sep=" "))
  
  # remove non ascents
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[!(lb$ascent.type %in% non.ascent.types()),]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, paste("Exclude non-ascent types:", paste(non.ascent.types(), collapse=", "), sep=" "))
  
  # Keep only Ewbank graded ascents
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[lb$grade.type == "AU",]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, "Keep only Ewbank grades")
  
  # Remove '--' grades
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[lb$grade != "--",]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, "Remove grades with value '--'")
  
  # remove old ascents
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[lb$date >= startDate,]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, paste("Remove ascents before", startDate, sep=" "))
  
  # remove too recent ascents
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[lb$date < endDate,]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, paste("Remove ascents on or after", endDate, sep=" "))
  
  # remove dates with no month information
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[!is.na(lb$month),]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, "Remove ascents with no month information.")
  
  lb$account.id <- as.character(lb$account.id)
  lb$grade <- as.numeric(as.character(lb$grade))
  
  # remove ascents below minimum grade
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[lb$grade >= min.grade,]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, paste("Remove ascents with grade less than", min.grade, sep=" "))
  
  # remove ambiguous ascents
  in.rows <- c(in.rows, nrow(lb))
  lb <- lb[!(lb$ascent.type %in% ambiguous.ascent.types()),]
  out.rows <- c(out.rows, nrow(lb))
  filter <- c(filter, paste("Exclude ambiguous ascent types:", paste(ambiguous.ascent.types(), collapse=", "), sep=" "))
  
  filter.results <- data.frame(rows.in=in.rows, rows.out=out.rows, filter=filter)
  
  filter.results <- rbind(res$filter.results, filter.results)
  
  return (list(filter.results=filter.results, df=lb))
}

##########################################################################################
# A function to add a row to the filter results table
##########################################################################################
add.filter.summary <- function(in.rows, out.rows, description, current.filter.summary) {
  filter.results <- data.frame(rows.in=in.rows, rows.out=out.rows, filter=description)  
  filter.results <- rbind(current.filter.summary, filter.results)
  filter.results$rows.in <- as.integer(filter.results$rows.in)
  return (filter.results)
}


keep.matching.gear <- function(df.ascents, df.routes, matching.gear=c("Sport")) {
  
  gear.of.ascents <- df.routes[match(df.ascents$node.id, df.routes$node.id),]$gear	
  
  excluded.gear <- setdiff(unique(gear.of.ascents),matching.gear)
  
  df.filtered <- df.ascents[gear.of.ascents %in% matching.gear,]
  
  return (list(excluded.gear=excluded.gear, df=df.filtered))
}


##########################################################################################
# A function to filter a list of ascents to include only climbers with a minimum specified number of ascents and failures
##########################################################################################
filter.climbers.by.ascent.counts <- function(res, min.ascents=200, min.failures=50) {
  
  lb <- res$df
  
  tab <- t(table(lb$success,lb$account.id))
  fail.counts <- tab[,1]
  success.counts <- tab[,2]
  log.style <- data.frame(climber=rownames(tab), fail=fail.counts, success=success.counts)
  log.style$total = log.style$fail + log.style$success
  log.style$failprob = log.style$fail / log.style$total
  log.style <- log.style[order(log.style$failprob, decreasing=T),]
  log.style <- log.style[log.style$total>=min.ascents & log.style$fail>=min.failures,]
  
  top.accounts <- as.character(log.style$climber)
  
  keep.top <- length(top.accounts)
  
  df.top <- lb[lb$account.id %in% top.accounts,]
  
  filter <- paste0("Keep climbers with at least ", min.ascents," ascents, and at least ", min.failures, " failed ascents.")
  
  # add filter results
  filter.results <- add.filter.summary(nrow(lb), nrow(df.top), filter, res$filter.results)
  
  return (list(df=df.top, filter.results=filter.results))
}

##########################################################################################
# A function to filter by tick proportion and lonely red points
##########################################################################################

filter.by.tick.proportion.and.lonely.redpoints <- function(res.small, res.full, max.tip.prop=0.05, max.lonely.redpoint.prop=0.05) {
  
  climbers <- unique(res.small$df$account.id)
  
  lrp <- tabulate.lonely.redpoints(climbers, res.full$df)
  amb <- tabulate.ambiguous.ascents(climbers, res.full$df)
  
  ascent.info <- merge(amb,lrp, by="climbers")
  
  filter <- paste0("Keep climbers with <= ", max.tip.prop," tick ascents, and <= ", max.lonely.redpoint.prop, " lonely redpoints.")
  
  
  climbers <- ascent.info$climbers[ascent.info$tickprop<=0.05 & ascent.info$lrf <= 0.05]
  df.final <- res.small$df[res.small$df$account.id %in% climbers,]
  filter.results <- add.filter.summary(nrow(res.small$df), nrow(df.final), filter, res.small$filter.results)
  
  return (list(df=df.final, filter.results=filter.results))
}

##########################################################################################
# A function to return a table of lonely red point ascents
#   A lonely redpoint is a redpoint ascent for which there are no other ascents of that
#   route by the climber in the ascent data. It indicates incomplete data
##########################################################################################
tabulate.lonely.redpoints <- function(climbers, lb) {
  
  lonely.redpoints <- c()
  route.count <- c()
  
  for (c in 1:length(climbers)) {
    
    lb.climber <- lb[lb$account.id == climbers[c],]
    
    lb.climber$redpoint <- lb.climber$ascent.type %in% red.or.pink.point.types()
    
    lb.climber$node.id <- as.character(lb.climber$node.id)
    
    tc <- table(lb.climber$redpoint, lb.climber$node.id)
    
    if (nrow(tc)>1) {
      # a lonely redpoint is a single redpoint of a route with no other ascents of that route ever logged
      lonely.redpoints = c(lonely.redpoints, sum(tc[2,]==1 & tc[1,]==0))
    } else {
      lonely.redpoints = c(lonely.redpoints, 0)
    } 
    route.count = c(route.count, ncol(tc))
  }
  
  df <- data.frame(climbers=climbers, lonely.redpoints=lonely.redpoints, route.count=route.count, lrf=lonely.redpoints/route.count)
  
  return (df)
  
}

##########################################################################################
# A function to return a table of lonely red point ascents
#   A lonely redpoint is a redpoint ascent for which there are no other ascents of that
#   route by the climber in the ascent data. It indicates incomplete data
##########################################################################################
tabulate.ambiguous.ascents <- function(climbers, lb) {
  
  ambiguous.ascents <- c()
  tick.ascents <- c()
  ascent.count <- c()
  
  for (c in 1:length(climbers)) {
    
    lb.climber <- lb[lb$account.id == climbers[c],]
    
    lb.climber$ambiguous <- lb.climber$ascent.type %in% ambiguous.ascent.types()
    lb.climber$tick <- lb.climber$ascent.type == "tick"
    
    lb.climber$node.id <- as.character(lb.climber$node.id)
    
    tc <- table(lb.climber$ambiguous, lb.climber$node.id)
    
    ambiguous.ascents = c(ambiguous.ascents, sum(lb.climber$ascent.type %in% ambiguous.ascent.types()))
    tick.ascents = c(tick.ascents, sum(lb.climber$ascent.type == "tick"))
    ascent.count = c(ascent.count, nrow(lb.climber))
  }
  
  df <- data.frame(climbers=climbers, ambiguous.ascents=ambiguous.ascents, tick.ascents=tick.ascents, ascent.count=ascent.count, ambigprop=ambiguous.ascents/ascent.count, tickprop=tick.ascents/ascent.count)
  
  return (df)
  
}
