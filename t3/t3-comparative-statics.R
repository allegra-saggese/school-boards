# =============================================================================
# T3 — comparative statics, and heterogeneity in the norm parameter
#
# A. COMPARATIVE STATICS. Elasticities of behaviour with respect to each
#    primitive, evaluated at the fitted parameters.
#
# B. WHO BEARS THE NORM. Because the model is DETERMINISTIC given observables,
#    solving the SAME household at two values of alpha is an exact matched pair
#    -- no matching error, no comparison group needed. h_f(alpha=0) - h_f(alpha)
#    is that couple's norm-induced hours loss. This lets us characterise which
#    households bear it, rather than only the aggregate.
#
# C. HETEROGENEOUS alpha_eff. The solver accepts a per-household alpha, so
#    alpha_j = alpha_bar * exp(sigma_a * e_j - sigma_a^2/2) assigns dispersion
#    while holding the mean fixed. The question this answers is an
#    IDENTIFICATION one: with homogeneous alpha every constrained couple bunches
#    at exactly parity, producing a sharp cliff. Dispersion in alpha smears that
#    bunching across the threshold. So the OBSERVED sharpness of the cliff
#    places an upper bound on how much alpha can vary across couples -- the data
#    tell us how heterogeneous the norm can be.
# =============================================================================
source(here::here("_setup.R"))

# Inputs : data/processed/results/*_t3_estimates_v2_by_year.csv
#          data/processed/panel/model_input_households.csv
# Outputs: data/processed/results/ (elasticities, norm-incidence tables)
suppressMessages({library(data.table); source(here::here("t3", "t3-model-solver.R"))})
set.seed(20260831)
results_dir <- data_path("processed","results")

YR <- 2019
d <- fread(data_path("processed","panel","model_input_households.csv"), showProgress=FALSE)
d <- d[YEAR==YR & is.finite(f_w)&is.finite(m_w)&f_w>0&m_w>0&is.finite(y0)&is.finite(f_h)&is.finite(m_h)]
d <- d[sample(.N, min(120000,.N))]
est <- read_newest(results_dir, "t3_estimates_v2_by_year.csv$")
A <- est[YEAR==YR]$alpha; Fd <- est[YEAR==YR]$F_dollars
int  <- d[f_h>0&m_h>0]
Cb   <- weighted.mean(int$m_w*int$m_h+int$f_w*int$f_h+int$y0, int$HHWT)
kap  <- weighted.mean(int$m_w,int$HHWT)/(Cb*weighted.mean(int$m_h,int$HHWT))
k    <- rep(kap,nrow(d)); w <- d$HHWT

agg <- function(s) {
  em <- d$m_w*s$h_m; ef <- d$f_w*s$h_f
  z  <- fifelse(em+ef>0, ef/(em+ef), NA_real_)
  b  <- sum(w[z>=0.40 & z<0.48],na.rm=TRUE); a <- sum(w[z>0.52 & z<=0.60],na.rm=TRUE)
  c(h_f=weighted.mean(s$h_f,w), h_m=weighted.mean(s$h_m,w),
    z=weighted.mean(z,w,na.rm=TRUE), cliff=b/a,
    corner=sum(w[s$h_f<=0])/sum(w), bunch=weighted.mean(s$regime==3,w))
}
base <- agg(solve_household(d$m_w,d$f_w,d$y0,Fd,A,k,k))

cat("=== A. COMPARATIVE STATICS (elasticities at the fitted 2019 parameters) ===\n")
cat("   % change in each outcome for a +10% change in each primitive\n\n")
cat(sprintf("%-14s %9s %9s %9s %9s %9s\n","primitive","wife hrs","husb hrs","z share","cliff","corner"))
shock <- function(lab, f) {
  s <- f(); n <- agg(s)
  r <- 100 * (n[c("h_f","h_m","z","cliff","corner")] /
              base[c("h_f","h_m","z","cliff","corner")] - 1)
  cat(sprintf("%-14s %+8.2f%% %+8.2f%% %+8.2f%% %+8.2f%% %+8.2f%%\n", lab,
              r[1], r[2], r[3], r[4], r[5]))
  data.table(primitive = lab, wife_hours = r[1], husband_hours = r[2],
             wife_share = r[3], cliff = r[4], corner = r[5])
}
tabA <- rbind(
  shock("alpha +10%",  function() solve_household(d$m_w,d$f_w,d$y0,Fd,A*1.1,k,k)),
  shock("w_m   +10%",  function() solve_household(d$m_w*1.1,d$f_w,d$y0,Fd,A,k,k)),
  shock("w_f   +10%",  function() solve_household(d$m_w,d$f_w*1.1,d$y0,Fd,A,k,k)),
  shock("y0    +10%",  function() solve_household(d$m_w,d$f_w,d$y0*1.1,Fd,A,k,k)),
  shock("F     +10%",  function() solve_household(d$m_w,d$f_w,d$y0,Fd*1.1,A,k,k)),
  shock("kappa +10%",  function() solve_household(d$m_w,d$f_w,d$y0,Fd,A,k*1.1,k*1.1)))

cat("\n=== B. WHO BEARS THE NORM (exact within-household counterfactual) ===\n")
s1 <- solve_household(d$m_w,d$f_w,d$y0,Fd,A,k,k)
s0 <- solve_household(d$m_w,d$f_w,d$y0,Fd,0,k,k)
d[, `:=`(lost = s0$h_f - s1$h_f, binds = s1$regime %in% c(2L,3L),
         wr = f_w/m_w)]
d[, wrq := cut(wr, c(0,.5,.75,1,1.25,Inf),
               labels=c("<0.50","0.50-0.75","0.75-1.00","1.00-1.25",">1.25"))]
cat("\nBy the couple's wage ratio w_f / w_m:\n")
cat(sprintf("%-12s %9s %9s %12s %14s\n","w_f/w_m","% couples","% bound","hrs lost","hrs lost|bound"))
tabB_ratio <- rbindlist(lapply(levels(d$wrq), function(g) {
  i <- d$wrq==g & !is.na(d$wrq)
  bi <- i & d$binds
  r <- data.table(group = g, pct_couples = 100*sum(w[i])/sum(w),
                  pct_bound = 100*weighted.mean(d$binds[i],w[i]),
                  hrs_lost = weighted.mean(d$lost[i],w[i]),
                  hrs_lost_bound = if (any(bi)) weighted.mean(d$lost[bi],w[bi]) else NA_real_)
  cat(sprintf("%-12s %8.1f%% %8.1f%% %12.0f %14s\n", g, r$pct_couples, r$pct_bound,
      r$hrs_lost, ifelse(is.na(r$hrs_lost_bound), "-", sprintf("%.0f", r$hrs_lost_bound))))
  r
}))
d[, hq := cut(m_w, quantile(m_w,seq(0,1,.2)), labels=paste0("Q",1:5), include.lowest=TRUE)]
cat("\nBy the husband's wage quintile:\n")
cat(sprintf("%-12s %9s %12s %14s\n","quintile","% bound","hrs lost","hrs lost|bound"))
tabB_q <- rbindlist(lapply(levels(d$hq), function(g) {
  i <- d$hq==g & !is.na(d$hq); bi <- i & d$binds
  r <- data.table(group = g, pct_bound = 100*weighted.mean(d$binds[i],w[i]),
                  hrs_lost = weighted.mean(d$lost[i],w[i]),
                  hrs_lost_bound = if (any(bi)) weighted.mean(d$lost[bi],w[bi]) else NA_real_)
  cat(sprintf("%-12s %8.1f%% %12.0f %14s\n", g, r$pct_bound, r$hrs_lost,
      ifelse(is.na(r$hrs_lost_bound), "-", sprintf("%.0f", r$hrs_lost_bound))))
  r
}))

cat("\n=== C. HOW HETEROGENEOUS CAN alpha BE? ===\n")
cat("alpha_j lognormal around the fitted mean. Dispersion smears the bunching,\n")
cat("so the OBSERVED cliff bounds how much alpha can vary across couples.\n\n")
em <- d$m_w*d$f_h*0 + rnorm(nrow(d))    # fixed draw, reused at every sigma
dz <- {ef<-d$f_w*d$f_h; emm<-d$m_w*d$m_h; z<-fifelse(emm+ef>0,ef/(emm+ef),NA_real_)
       b<-sum(w[z>=0.40&z<0.48],na.rm=TRUE); a<-sum(w[z>0.52&z<=0.60],na.rm=TRUE); b/a}
cat(sprintf("%-10s %10s %10s %10s %12s\n","sigma_a","cliff","% bunched","corner","vs data cliff"))
tabC <- rbindlist(lapply(c(0, 0.25, 0.5, 0.75, 1.0, 1.5), function(sg) {
  aj <- A*exp(sg*em - sg^2/2)
  s  <- solve_household(d$m_w,d$f_w,d$y0,Fd,aj,k,k); n <- agg(s)
  cat(sprintf("%-10.2f %10.3f %9.1f%% %9.3f %12.3f\n",
      sg, n["cliff"], 100*n["bunch"], n["corner"], n["cliff"]-dz))
  data.table(sigma_alpha = sg, cliff = n["cliff"], pct_bunched = 100*n["bunch"],
             corner = n["corner"], cliff_minus_data = n["cliff"]-dz)
}))
cat(sprintf("\n  data cliff (%d) = %.3f\n", YR, dz))

# ── Outputs: CSV + LaTeX (booktabs tabulars, for \input{} into slides/paper) ─
f3 <- function(x, dg = 3) formatC(x, format = "f", digits = dg, big.mark = ",")
fwrite(tabA,       dated_path(results_dir, "t3_comparative_statics.csv"))
fwrite(tabB_ratio, dated_path(results_dir, "t3_norm_incidence_by_wage_ratio.csv"))
fwrite(tabB_q,     dated_path(results_dir, "t3_norm_incidence_by_quintile.csv"))
fwrite(tabC,       dated_path(results_dir, "t3_alpha_heterogeneity.csv"))
write_tex_table(tabA[, .(`+10\\% in` = sub(" +\\+10%", "", primitive),
                         `Wife hours` = f3(wife_hours, 2), `Husband hours` = f3(husband_hours, 2),
                         `Wife earnings share` = f3(wife_share, 2), Cliff = f3(cliff, 2),
                         `Wife not working` = f3(corner, 2))],
                dated_path(results_dir, "t3_comparative_statics_table.tex"))
write_tex_table(tabB_ratio[, .(`$w_f / w_m$` = group, `\\% couples` = f3(pct_couples, 1),
                               `\\% norm binds` = f3(pct_bound, 1), `Hours lost` = f3(hrs_lost, 0),
                               `Hours lost, bound` = f3(hrs_lost_bound, 0))],
                dated_path(results_dir, "t3_norm_incidence_by_wage_ratio_table.tex"))
write_tex_table(tabB_q[, .(`Husband wage quintile` = group, `\\% norm binds` = f3(pct_bound, 1),
                           `Hours lost` = f3(hrs_lost, 0), `Hours lost, bound` = f3(hrs_lost_bound, 0))],
                dated_path(results_dir, "t3_norm_incidence_by_quintile_table.tex"))
write_tex_table(tabC[, .(`$\\sigma_\\alpha$` = f3(sigma_alpha, 2), Cliff = f3(cliff),
                         `\\% at kink` = f3(pct_bunched, 1), `Wife not working` = f3(corner),
                         `Cliff $-$ data` = f3(cliff_minus_data))],
                dated_path(results_dir, "t3_alpha_heterogeneity_table.tex"))
