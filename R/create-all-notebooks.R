setwd("~/Git/climbing-grades/R")
library(stringr)

config <- read.table("config.csv", sep=",", header=T)

for (i in 7:nrow(config)) {
  
  min.grade <- as.character(config$min.grade[i]);
  mean.grade.prior <- as.character(config$mean.grade.prior[i]);
  
  if (config$grade.type[i] == "AU") {
    min.grade <- as.numeric(min.grade)
    mean.grade.prior <- as.numeric(mean.grade.prior)
  } 
  
  cparams = list(
    min.grade = min.grade,
    mean.grade.prior = mean.grade.prior,
    min.ascents = as.integer(config$min.ascents[i]),
    min.failures = as.integer(config$min.failures[i]),
    gear = unlist(str_split(config$gear[i], "\\+")),
    grade.type = as.character(config$grade.type[i]),
    per.session = as.logical(config$per.session[i]),
    data.set.name = as.character(config$data.set.name[i]),
    data.set.name.short = as.character(config$data.set.name.short[i]))
  
  print(cparams)
  
  rmarkdown::render("bayesian-climbing-analysis.Rmd", output_dir="~/Git/climbing-grades/R", output_file=paste0("bayesian-climbing-analysis-",i,".html"), output_format = "html_notebook", params=cparams)
}

