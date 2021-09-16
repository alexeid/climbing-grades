library(tidyjson)
library(tidyr)
library(stringr)

##########################################################################################
# Useful constants for ascent types
##########################################################################################

# Red or pink point types
red.or.pink.point.types <- function() {
   c("firstfreeascent", "firstascent",  "groundupredpoint", "redpoint", "pinkpoint")
}

# Clean ascent types
clean.ascent.types <- function() {
   c("greenpointonsight", "onsight", "flash", "greenpoint", red.or.pink.point.types(), "onsightsolo", "solo", "deepwatersolo", "secondclean", "topropeclean", "greenpoint", "topropeonsight", "topropeflash", "clean", "repeat", "send")
}

# Trad ascent types
trad.ascent.types <- function() { c("greenpoint", "greenpointonsight") }

# ambiguous ascent types
ambiguous.ascent.types <- function() { c("tick", "lead", "leadsolo", "second", "toprope", "aidsolo", "ropedsolo") }
  
# bouldering ascent types
bouldering.ascent.types <- function() { c("send", "dab", "repeat") }

# non ascent types
non.ascent.types <- function() { c("hit", "target", "mark") }

# solo ascent types
solo.ascent.types <- function() { c("onsightsolo", "solo", "deepwatersolo", "ropedsolo", "leadsolo") }

failed.ascent.types <- function() { c("allfreewithrest", "dog", "toproperest", "secondrest", "aid", "attempt", "retreat", "ghost", "working") }

explicit.failed.ascent.types <- function() { c("dog", "attempt", "retreat", "working", "dab", "ghost") }

##########################################################################################
# Germany: 11747131

# Function to read a json file of ascents that have been retrieved through thecrag API
# e.g. by
#   curl -v -k  "https://www.thecrag.com/api/facet/ascents/at/7478254/?key=02cr0b89l9rkllgz&pretty=1&thin=1&withdata=AscentID,CreateDate,Date,LastUpdated,AccountID,NodeID,Tick,Label,Artificial,Grade&markupType=text&perPage=50000&page=1" > aus-ascents-01.json
##########################################################################################
read.json.ascents <- function(jsonfile, filterByHasGrade=T, filterByHasDate=T) {
  json <- read_json(path=jsonfile)

  ascents <- json %>% enter_object(data) %>% enter_object(ascents) %>% gather_array(column.name="row.index") %>% gather_array(column.name="column.index") %>% append_values_string 

  rows.in.json <- max(ascents$row.index)
  
  row.count <- rows.in.json

  #print(paste0("Read ", row.count, " ascents from file ", jsonfile));

  # filter by has date has to go before filter by has grade because it relies on all row.indices still being in place
  if (filterByHasDate) {
    has.date <- !is.na(ascents[ascents$column.index==3,]$string)
    
    ascents.filtered <- ascents[ascents$row.index %in% which(has.date),]
    ascents <- ascents.filtered

    unique.rows <- unique(ascents$row.index)
    new.row.count <- length(unique.rows)

    #print(paste0("  Missing date in ", (row.count-new.row.count), " ascents"));
    row.count <- new.row.count
  }    

  if (filterByHasGrade) {
    has.grade <- ascents[ascents$column.index==10,]$row.index
    ascents.filtered <- ascents[ascents$row.index %in% has.grade,]
    ascents <- ascents.filtered

    unique.rows <- unique(ascents$row.index)
    new.row.count <- length(unique.rows)

    #print(paste0("  Missing grades in a further ", (row.count-new.row.count), " ascents"));
    
    row.count <- new.row.count
  }

  ascents$document.id <- NULL


  ascent.id <- ascents[ascents$column.index==1,]$string
  create.date <- ascents[ascents$column.index==2,]$string
  date <- ascents[ascents$column.index==3,]$string
  last.updated <- ascents[ascents$column.index==4,]$string
  account.id <- ascents[ascents$column.index==5,]$string
  node.id <- ascents[ascents$column.index==6,]$string
  ascent.type <- ascents[ascents$column.index==7,]$string
  label <- ascents[ascents$column.index==8,]$string
  artificial <- ascents[ascents$column.index==9,]$string

  grades <- ascents[ascents$column.index==10,0] %>% gather_array %>% append_values_string
  grade.id <- grades[grades$array.index==1,]$string
  grade <- grades[grades$array.index==2,]$string
  grade.type <- grades[grades$array.index==3,]$string

  ymd <- str_match(date, "^([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9])")

  lb <- data.frame(ascent.id=ascent.id, node.id=node.id, account.id=account.id, ascent.type=ascent.type, artificial=artificial, route.name=label, grade.id=grade.id, grade=grade, grade.type=grade.type)

  # avoid trying to parse dates with zero for month
  lb$date <- as.Date(sapply(date, FUN=function(x) { if (grepl("^([0-9][0-9][0-9][0-9])-00", x)) { return (NA) } else { return(x)}}))
  lb$year <- as.integer(ymd[,2])
  lb$month <-  as.integer(ymd[,3])
  lb$day <-  as.integer(ymd[,4])
  
  lb$account.id <- as.character(lb$account.id)
  lb$grade <- as.character(lb$grade)
  
  return (list(rows.in.json=rows.in.json, df=lb))
}

##########################################################################################
# A function to process all the files for a given path and save as csv to provided file
##########################################################################################

process.all.json.ascent.files <- function(path, file) {

  files <- list.files(path)

  total.rows <- 0;  

  for (i in 1:length(files)) {
    res <- read.json.ascents(paste0(path,files[i]))
    if (i == 1) {
      lb <- res$df
    } else {
      lb <- rbind(lb, res$df)
    }
    total.rows = total.rows + res$rows.in.json
    print(paste0(res$rows.in.json, " rows in json filter to ", nrow(res$df)))
  }

  filter.results <- data.frame(rows.in=total.rows, rows.out=nrow(lb), filter="Exclude ascents with no date or no grade information.")
  
  res <- list(filter.results=filter.results, df=lb);	
	  
  saveRDS(res,file)
  
  return (res)
}

##########################################################################################
# Function to read a json file of area/route information that have been retrieved through thecrag API
# e.g. by
#    curl "https://www.thecrag.com/api/index/detail/11737723?key=<key>&pretty=1&withdata=NodeID,ParentID,NodeType,Name,AreaType,NumberAscents,NumberRoutes,Popularity,GearStyle,Grade,RegisteredGrades,Stars,Height&abbr=0&to=leaf" > nz.json

##########################################################################################

read.json.nodes <- function(jsonfile, filterByHasGear=T) {
  json.nodes <- read_json(path=jsonfile)

  print("Read nodes from json")
  nodes <- json.nodes %>% enter_object(data) %>% gather_array(column.name="row.index") %>% gather_array(column.name="column.index") %>% append_values_string 

  # filter by has date has to go before filter by has grade because it relies on all row.indices still being in place
  if (filterByHasGear) {
    print("Filter by has gear")
    
    has.gear <- !is.na(nodes[nodes$column.index==9,]$string)
    
    nodes.filtered <- nodes[nodes$row.index %in% which(has.gear),]
    nodes <- nodes.filtered
  }    


  node.id <- nodes[nodes$column.index==1,]$string
  parent.id <- nodes[nodes$column.index==2,]$string
  node.type <- nodes[nodes$column.index==3,]$string
  name <- nodes[nodes$column.index==4,]$string
  area.type <- nodes[nodes$column.index==5,]$string
  ascent.count <- as.numeric(nodes[nodes$column.index==6,]$string)
  gear <- nodes[nodes$column.index==9,]
  print(colnames(gear))
  
  grades <- nodes[nodes$column.index==10,]
  print(colnames(grades))

  print("Construct data frame")
  nodes.df <- data.frame(node.id=node.id, parent.id=parent.id, node.type=node.type, name=name, area.type=area.type, ascent.count=ascent.count)

  nodes.df$parent.name <- nodes.df$name[match(nodes.df$parent.id, nodes.df$node.id)]
  nodes.df$parent.type <- nodes.df$node.type[match(nodes.df$parent.id, nodes.df$node.id)]
  
  nodes.df$grade <- rep(NA, nrow(nodes.df))
  class(nodes.df$grade) <- "character"

  nodes.df$gear <- rep(NA, nrow(nodes.df))
  class(nodes.df$gear) <- "character"

    
  print("Set grades")
  for (i in 1:nrow(grades)) {
    nodes.df$grade[grades[i,]$row.index] <- grades[i,0]
  }
  print("Set gear")
  for (i in 1:nrow(gear)) {
    nodes.df$gear[gear[i,]$row.index] <- gear[i,]$string
  }    
  nodes.df <- nodes.df[order(nodes.df$ascent.count, decreasing=T),]
  

  return (nodes.df)
}

##########################################################################################
# A function to process all the routes from a json node file
##########################################################################################

process.json.routes <- function(jsonfile, outfile) {

  df <- read.json.nodes(jsonfile, filterByHasGear=T)
	  
  saveRDS(df,outfile)
  
  return (df)
}
