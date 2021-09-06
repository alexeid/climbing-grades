library(dplyr)
source("thecrag-json.R")

clean_ascents <- c("onsightsolo", "greenpointonsight", "onsight", "topropeonsight", "flash", "topropeflash", "firstfreeascent", "firstascent", "greenpoint", "groundupredpoint", "redpoint", "pinkpoint", "solo", "topropeclean", "secondclean", "clean", "repeat", "send")
ambiguous_ascents <- c("leadsolo", "ropedsolo", "lead", "tick")
failed_ascents <- c("allfreewithrest","dog", "toproperest", "secondrest", "aid", "attempt", "retreat", "ghost", "working")

ascent.type.levels <- c(clean_ascents, ambiguous_ascents, failed_ascents)

##########################################################################################
# this function only keeps the best ascent in each day for each climb
# can be used to downsample a complete log book to mimic a best-in-day logging style
##########################################################################################
keep.best.ascent.in.session <- function(lb) {
	
  ascent.type <- lb$ascent.type
  lb$ascent_factor <- factor(ascent.type, ordered = TRUE, levels = ascent.type.levels)
  lb_grouped <- lb %>% group_by(date, route.name) %>% slice_min(ascent_factor, n=1)

  return (lb_grouped);
}

##########################################################################################
# returns the number of ascents for this log book once it has been reduced to day grade ascents
##########################################################################################
best.ascent.in.session.count <- function(lb) {
  return (nrow(keep.best.ascent.in.session(lb)))
}

##########################################################################################
##########################################################################################
red.or.pink.point.count <- function(lb) {
	return (nrow(lb[lb$ascent.type %in% red.or.pink.point.types(),]))
}

##########################################################################################
##########################################################################################
failed.attempts.count <- function(lb) {
	return (nrow(lb[lb$ascent.type %in% failed.ascent.types(),]))
}

##########################################################################################
##########################################################################################
explicit.failed.attempts.count <- function(lb) {
  return (nrow(lb[lb$ascent.type %in% explicit.failed.ascent.types(),]))
}

