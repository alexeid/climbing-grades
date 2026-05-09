#!/usr/bin/env Rscript
# Threshold-sensitivity runs for the AUS Sport per-session analysis.
# Runs the headline configuration at min.ascents=50 and min.ascents=100, with
# the n=100 cap held fixed (matching the headline run at min.ascents=30).
# Addresses reviewer comment #4.

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

base.params <- list(
  data.set.name        = as.character(config$data.set.name[i]),
  data.set.name.short  = as.character(config$data.set.name.short[i]),
  startDate            = "2016-08-01",
  endDate              = "2021-08-01",
  filter.by.tickprop.lrp = FALSE,
  gear                 = unlist(str_split(config$gear[i], "\\+")),
  grade.type           = as.character(config$grade.type[i]),
  min.failures         = as.integer(config$min.failures[i]),
  min.grade            = as.numeric(as.character(config$min.grade[i])),
  mean.grade.prior     = as.numeric(as.character(config$mean.grade.prior[i])),
  max.climbers         = 100L,
  per.session          = as.logical(config$per.session[i]),
  in.path              = file.path(repo, "data", "processed/"),
  out.path             = file.path(repo, "results/")
)

cat("=== Loading data once ===\n")
res <- readRDS(paste0(base.params$in.path, "ascents-",
                      base.params$data.set.name.short, ".rds"))
routes <- readRDS(paste0(base.params$in.path, "routes-",
                          base.params$data.set.name.short, ".rds"))

run_one <- function(min.ascents) {
  params <- base.params
  params$min.ascents <- as.integer(min.ascents)

  cat("\n\n========================================\n")
  cat("=== min.ascents =", min.ascents, "===\n")
  cat("========================================\n")

  cat("\n=== Running data preparation ===\n")
  data <- produce.analysis.data(params, res, routes)
  data$params <- params
  cat("Climbers:", data$d$C, "  Ascents:", data$d$N, "  Months:", data$d$P, "\n")

  cat("\n=== Running Stan inference ===\n")
  options(mc.cores = parallel::detectCores())
  stan.result <- run.stan.climbing.model(data$d,
    sample_file=file.path(repo, "logs",
      paste0("stan-aus-sport-threshold", min.ascents, "-chain")))

  fit1 <- stan.result$fit
  data$time <- stan.result$time
  data$diagnostics <- stan.result$diagnostics
  data$mcmc.settings <- stan.result$mcmc.settings

  cat("\n=== Diagnostics ===\n")
  cat("R-hat range across all parameters: [",
      signif(data$diagnostics$rhat.range[1], 4), ", ",
      signif(data$diagnostics$rhat.range[2], 4), "]\n", sep="")
  cat("Min n_eff: ", signif(data$diagnostics$neff.min, 4),
      "; median n_eff: ", signif(data$diagnostics$neff.median, 4), "\n", sep="")
  cat("Divergent transitions: ", data$diagnostics$n.divergent,
      "; max-treedepth hits: ", data$diagnostics$n.maxtreedepth, "\n", sep="")
  cat("Summary for m:\n")
  print(data$diagnostics$m)

  fit.df <- as.data.frame(fit1)
  m.draws <- fit.df$m
  d.draws <- exp(m.draws)
  cat("\nm: mean=", signif(mean(m.draws), 4),
      ", median=", signif(median(m.draws), 4),
      ", 95% CrI=[", signif(quantile(m.draws, 0.025), 4),
      ", ", signif(quantile(m.draws, 0.975), 4), "]\n", sep="")
  cat("d=exp(m): mean=", signif(mean(d.draws), 4),
      ", median=", signif(median(d.draws), 4),
      ", 95% CrI=[", signif(quantile(d.draws, 0.025), 4),
      ", ", signif(quantile(d.draws, 0.975), 4), "]\n", sep="")

  cat("\n=== Saving outputs ===\n")
  fit.file <- paste0(data$file.stem, ".rds")
  diag.file <- paste0(data$file.stem, "-diagnostics.rds")
  data.file <- paste0(data$file.stem, "-data.rds")

  saveRDS(fit.df, file=fit.file)
  saveRDS(list(diagnostics=data$diagnostics, mcmc.settings=data$mcmc.settings,
               elapsed=data$time["elapsed"]),
          file=diag.file)
  saveRDS(data, file=data.file)
  cat("Wrote:\n  ", fit.file, "\n  ", diag.file, "\n  ", data.file, "\n")

  pngname <- paste0(data$file.stem, "-posterior.png")
  ylab <- if (params$per.session) "Session Grade" else "Flash Grade"
  plot.stan.climbing.results(params$startDate, params$endDate, fit1, data$d,
                              pngname, ylab=ylab, lb=data$df)
  cat("Wrote: ", pngname, "\n")

  cat("\nElapsed: ", round(data$time["elapsed"], 1), " seconds (",
      round(data$time["elapsed"]/60, 1), " minutes)\n", sep="")

  invisible(NULL)
}

# Note: min.ascents=30 already done in run-aus-sport-diagnostics.R
run_one(50)
run_one(100)

cat("\n\n=== ALL THRESHOLD RUNS COMPLETE ===\n")
