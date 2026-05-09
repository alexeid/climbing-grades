#!/usr/bin/env Rscript
#
# run-aus-sport-prior-sensitivity.R
#
# Prior-sensitivity analysis for the slope parameter m on the Australia-Sport
# per-session dataset (config row 18). Re-fits the main model (fixed
# w = 0.5) under three alternative priors on m, with all other settings
# identical to the main analysis. Addresses reviewer comment #2 (prior
# justification / sensitivity analysis).
#
# Priors compared:
#   main   m ~ lognormal(-0.5, 0.6)   median 0.61, 95% [0.19, 1.97]
#   tight      m ~ lognormal(-0.5, 0.3)   median 0.61, 95% [0.34, 1.10]
#   wide       m ~ lognormal(-0.5, 1.0)   median 0.61, 95% [0.085, 4.36]
#   gamma      m ~ gamma(2, 4)            mean 0.5, mode 0.25, density vanishes at 0
#
# The main result is already saved by run-aus-sport-diagnostics.R; this
# script runs the three alternatives and saves their posteriors plus a small
# comparison summary file.
#
# Reproducibility: invoke from the repository root with
#   Rscript R/run-aus-sport-prior-sensitivity.R
# Outputs: results/aus/<file-stem>-priorM-<label>{.rds,-data.rds,-diagnostics.rds,-posterior.png}
# and results/aus/<file-stem>-prior-sensitivity-summary.rds

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
  min.ascents          = as.integer(config$min.ascents[i]),
  min.failures         = as.integer(config$min.failures[i]),
  min.grade            = as.numeric(as.character(config$min.grade[i])),
  mean.grade.prior     = as.numeric(as.character(config$mean.grade.prior[i])),
  max.climbers         = 100L,
  per.session          = as.logical(config$per.session[i]),
  in.path              = file.path(repo, "data", "processed/"),
  out.path             = file.path(repo, "results/")
)

priors <- list(
  list(label="tight", code="m ~ lognormal(-0.5, 0.3);"),
  list(label="wide",  code="m ~ lognormal(-0.5, 1.0);"),
  list(label="gamma", code="m ~ gamma(2, 4);")
)

cat("=== Loading data once ===\n")
res <- readRDS(paste0(base.params$in.path, "ascents-",
                      base.params$data.set.name.short, ".rds"))
routes <- readRDS(paste0(base.params$in.path, "routes-",
                          base.params$data.set.name.short, ".rds"))

cat("=== Preparing analysis data once ===\n")
data <- produce.analysis.data(base.params, res, routes)
data$params <- base.params
cat("Climbers:", data$d$C, "  Ascents:", data$d$N, "  Months:", data$d$P, "\n")

base.file.stem <- data$file.stem
summary.file <- paste0(base.file.stem, "-prior-sensitivity-summary.rds")
results <- if (file.exists(summary.file)) readRDS(summary.file) else list()

summarise.fit <- function(label, code, fit.df, diagnostics, elapsed.s) {
  m.draws <- fit.df$m
  d.draws <- exp(m.draws)
  list(
    label = label, code = code,
    m.median = median(m.draws),
    m.lower  = quantile(m.draws, 0.025, names=FALSE),
    m.upper  = quantile(m.draws, 0.975, names=FALSE),
    d.median = median(d.draws),
    d.lower  = quantile(d.draws, 0.025, names=FALSE),
    d.upper  = quantile(d.draws, 0.975, names=FALSE),
    rhat.m   = diagnostics$m[, "Rhat"],
    neff.m   = diagnostics$m[, "n_eff"],
    rhat.max = diagnostics$rhat.max,
    neff.min = diagnostics$neff.min,
    n.divergent = diagnostics$n.divergent,
    n.maxtreedepth = diagnostics$n.maxtreedepth,
    elapsed.s = unname(elapsed.s)
  )
}

print.summary.row <- function(r) {
  cat("\n--- summary for prior ", r$label, " ---\n", sep="")
  cat("m: median=", signif(r$m.median, 4),
      ", 95% CrI=[", signif(r$m.lower, 4),
      ",", signif(r$m.upper, 4), "]\n", sep="")
  cat("d: median=", signif(r$d.median, 4),
      ", 95% CrI=[", signif(r$d.lower, 4),
      ",", signif(r$d.upper, 4), "]\n", sep="")
  cat("R-hat (m)=", signif(r$rhat.m, 4),
      ", n_eff (m)=", signif(r$neff.m, 4), "\n", sep="")
  cat("Divergences: ", r$n.divergent,
      "; max-treedepth hits: ", r$n.maxtreedepth, "\n", sep="")
  cat("Elapsed: ", round(r$elapsed.s/60, 1), " min\n", sep="")
}

for (p in priors) {
  cat("\n\n========================================\n")
  cat("=== prior on m: ", p$code, " (label=", p$label, ") ===\n", sep="")
  cat("========================================\n")

  file.stem <- paste0(base.file.stem, "-priorM-", p$label)
  fit.file  <- paste0(file.stem, ".rds")
  diag.file <- paste0(file.stem, "-diagnostics.rds")
  data.file <- paste0(file.stem, "-data.rds")

  if (file.exists(fit.file) && file.exists(diag.file)) {
    cat("Outputs already exist; loading from disk and skipping inference.\n")
    fit.df <- readRDS(fit.file)
    diag.bundle <- readRDS(diag.file)
    summary.row <- summarise.fit(p$label, p$code, fit.df,
                                  diag.bundle$diagnostics, diag.bundle$elapsed)
  } else {
    options(mc.cores = parallel::detectCores())
    stan.result <- run.stan.climbing.model(
      data$d,
      m.prior.code = p$code,
      sample_file = file.path(repo, "logs",
                              paste0("stan-aus-sport-priorM-", p$label, "-chain"))
    )

    fit1 <- stan.result$fit
    fit.df <- as.data.frame(fit1)

    saveRDS(fit.df, file=fit.file)
    saveRDS(list(diagnostics=stan.result$diagnostics,
                 mcmc.settings=stan.result$mcmc.settings,
                 m.prior.code=p$code,
                 elapsed=stan.result$time["elapsed"]),
            file=diag.file)
    saveRDS(data, file=data.file)

    summary.row <- summarise.fit(p$label, p$code, fit.df,
                                  stan.result$diagnostics, stan.result$time["elapsed"])
    rm(fit1)
  }

  results[[p$label]] <- summary.row
  print.summary.row(summary.row)
  saveRDS(results, file=summary.file)
  rm(fit.df); gc(verbose=FALSE)
}

cat("\n\n=== ALL PRIOR-SENSITIVITY RUNS COMPLETE ===\n")
cat("Comparison table:\n")
cmp <- do.call(rbind, lapply(results, function(r) {
  data.frame(prior=r$label,
             m.median=signif(r$m.median, 4),
             m.95=sprintf("[%.3f, %.3f]", r$m.lower, r$m.upper),
             d.median=signif(r$d.median, 4),
             d.95=sprintf("[%.3f, %.3f]", r$d.lower, r$d.upper),
             rhat.m=signif(r$rhat.m, 4),
             neff.m=signif(r$neff.m, 4),
             elapsed.min=signif(r$elapsed.s/60, 3),
             stringsAsFactors=FALSE)
}))
print(cmp)
