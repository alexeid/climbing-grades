#!/usr/bin/env Rscript
# Verify the central-95% intervals quoted in the manuscript for the prior
# m ~ LogNormal(-0.5, 0.6^2), and the implied interval for d = e^m.
#
# Quoted in ms-jqas-r1.tex / response-to-reviewers.tex:
#   m in [0.19, 1.97]   (central 95% mass of the LogNormal)
#   d = e^m in [1.21, 7.14]   (push-forward through d = e^m)

mu    <- -0.5
sigma <-  0.6
alpha <-  0.05  # central 95% mass: 2.5% / 97.5% quantiles

m_lo <- qlnorm(alpha / 2,     meanlog = mu, sdlog = sigma)
m_hi <- qlnorm(1 - alpha / 2, meanlog = mu, sdlog = sigma)
d_lo <- exp(m_lo)
d_hi <- exp(m_hi)

cat(sprintf("m ~ LogNormal(%.1f, %.1f^2)\n", mu, sigma))
cat(sprintf("  central 95%% mass:  m in [%.4f, %.4f]  (rounded: [%.2f, %.2f])\n",
            m_lo, m_hi, m_lo, m_hi))
cat(sprintf("  push-forward:      d = e^m in [%.4f, %.4f]  (rounded: [%.2f, %.2f])\n",
            d_lo, d_hi, d_lo, d_hi))

check <- function(label, got, expected, tol = 0.005) {
  ok <- abs(got - expected) < tol
  cat(sprintf("  %s  expected %.2f, got %.4f -> %s\n",
              label, expected, got, if (ok) "OK" else "MISMATCH"))
  ok
}

cat("\nManuscript values:\n")
ok <- all(c(
  check("m_lo:", m_lo, 0.19),
  check("m_hi:", m_hi, 1.97),
  check("d_lo:", d_lo, 1.21),
  check("d_hi:", d_hi, 7.14)
))
if (!ok) quit(status = 1)
