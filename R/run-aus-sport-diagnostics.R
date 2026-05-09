#!/usr/bin/env Rscript
# Launch the AUS Sport per-session analysis (config.csv row 18) and save
# convergence diagnostics for use in the JQAS revision.

suppressPackageStartupMessages({
  library(stringr)
  library(rstan)
})

repo <- "/Users/adru001/Git/climbing-grades"
setwd(file.path(repo, "R"))

source("produce-analysis-data.R")
source("climbing-stan.R")
source("ascent-plotting.R")

config <- read.table("config.csv", sep=",", header=TRUE)
i <- 18  # Australia, Sport, AU grade type, per.session=TRUE

params <- list(
  data.set.name        = as.character(config$data.set.name[i]),
  data.set.name.short  = as.character(config$data.set.name.short[i]),
  startDate            = "2016-08-01",
  endDate              = "2021-08-01",
  filter.by.tickprop.lrp = FALSE,  # match the existing published-results runs
  gear                 = unlist(str_split(config$gear[i], "\\+")),
  grade.type           = as.character(config$grade.type[i]),
  min.ascents          = as.integer(config$min.ascents[i]),
  min.failures         = as.integer(config$min.failures[i]),
  min.grade            = as.numeric(as.character(config$min.grade[i])),
  mean.grade.prior     = as.numeric(as.character(config$mean.grade.prior[i])),
  max.climbers         = 100L,
  per.session          = as.logical(config$per.session[i]),
  in.path              = file.path(repo, "data", "processed/"),
  out.path             = file.path(repo, "results/")
)

cat("=== Configuration row", i, "===\n")
print(params)

cat("\n=== Loading data ===\n")
res <- readRDS(paste0(params$in.path, "ascents-", params$data.set.name.short, ".rds"))
routes <- readRDS(paste0(params$in.path, "routes-", params$data.set.name.short, ".rds"))

cat("\n=== Running data preparation ===\n")
data <- produce.analysis.data(params, res, routes)
data$params <- params
cat("Climbers:", data$d$C, "  Ascents:", data$d$N, "  Months:", data$d$P, "\n")

cat("\n=== Running Stan inference ===\n")
options(mc.cores = parallel::detectCores())
stan.result <- run.stan.climbing.model(data$d,
  sample_file=file.path(repo, "logs", "stan-aus-sport-diagnostics-chain"))

fit1 <- stan.result$fit
data$time <- stan.result$time
data$diagnostics <- stan.result$diagnostics
data$mcmc.settings <- stan.result$mcmc.settings

cat("\n=== Convergence diagnostics ===\n")
cat("MCMC settings: chains=", data$mcmc.settings$chains,
    ", iter=", data$mcmc.settings$iter,
    ", warmup=", data$mcmc.settings$warmup, "\n", sep="")
cat("Total parameters monitored:", data$diagnostics$n.params, "\n")
cat("R-hat range across all parameters: [",
    signif(data$diagnostics$rhat.range[1], 4), ", ",
    signif(data$diagnostics$rhat.range[2], 4), "]\n", sep="")
cat("Min n_eff: ", signif(data$diagnostics$neff.min, 4),
    "; median n_eff: ", signif(data$diagnostics$neff.median, 4), "\n", sep="")
cat("Divergent transitions: ", data$diagnostics$n.divergent,
    "; max-treedepth hits: ", data$diagnostics$n.maxtreedepth, "\n", sep="")
cat("\nSummary for m:\n")
print(data$diagnostics$m)
cat("\nSample of climber-grade rows (R-hat, n_eff):\n")
print(data$diagnostics$climber.subset)

cat("\n=== Saving outputs ===\n")
fit.file <- paste0(data$file.stem, ".rds")
diag.file <- paste0(data$file.stem, "-diagnostics.rds")
data.file <- paste0(data$file.stem, "-data.rds")

saveRDS(as.data.frame(fit1), file=fit.file)
saveRDS(list(diagnostics=data$diagnostics, mcmc.settings=data$mcmc.settings,
             elapsed=data$time["elapsed"]),
        file=diag.file)
saveRDS(data, file=data.file)

cat("Wrote:\n  ", fit.file, "\n  ", diag.file, "\n  ", data.file, "\n")

cat("\n=== Generating posterior figure ===\n")
pngname <- paste0(data$file.stem, "-posterior.png")
ylab <- if (params$per.session) "Session Grade" else "Flash Grade"
plot.stan.climbing.results(params$startDate, params$endDate, fit1, data$d,
                            pngname, ylab=ylab, lb=data$df)
cat("Wrote: ", pngname, "\n")

cat("\n=== DONE ===\n")
cat("Elapsed: ", round(data$time["elapsed"], 1), " seconds (",
    round(data$time["elapsed"]/60, 1), " minutes)\n", sep="")
