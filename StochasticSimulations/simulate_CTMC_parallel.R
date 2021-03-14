# Example CTMC simulation of a simple SIS model, parallel version
library(GillespieSSA2)
library(parallel)

# Need a function that runs one simulation and returns a result. While we're at it,
# we also return an interpolated solution
run_one_sim = function(params) {
  IC <- c(S = (params$Pop-params$I_0), I = params$I_0)
  params_local <- c(gamma = params$gamma, beta = params$beta)
  reactions <- list(
    # propensity function effects name for reaction
    reaction("beta*S*I", c(S=-1,I=+1), "new_infection"),
    reaction("gamma*I", c(S=+1,I=-1), "recovery")
  )
  set.seed(NULL)
  sol <- ssa(
    initial_state = IC,
    reactions = reactions,
    params = params_local,
    method = ssa_exact(),
    final_time = params$t_f,
    log_firings = TRUE    # This way we keep track of events as well
  )
  # Interpolate result (just I will do)
  wanted_t = seq(from = 0, to = params$t_f, by = 0.01)
  sol$interp_I = approx(x = sol$time, y = sol$state[,"I"], xout = wanted_t)
  names(sol$interp_I) = c("time", "I")
  # Return result
  return(sol)
}

# To run in parallel, it useful to put parameters in a list
params = list()
params$Pop = 100
params$gamma = 1/5
params$R_0 = 1.5
params$t_f = 180
params$I_0 = 2
# R0 would be (beta/gamma)*S0, so beta=R0*gamma/S0
params$beta = params$R_0*params$gamma/(params$Pop-params$I_0)
# Number of simulations. We may want to save all simulation parameters later,
# so we add it here
params$number_sims = 100


# Detect number of cores, use all but 1
no_cores <- detectCores()-1
# Initiate cluster
cl <- makeCluster(no_cores)
# Export needed library to cluster
clusterEvalQ(cl,{
  library(GillespieSSA2)
})
# Export needed variable and function to cluster
clusterExport(cl,
              c("params",
                "run_one_sim"),
              envir = .GlobalEnv)
# Run computation
SIMS = parLapply(cl = cl, 
                 X = 1:params$number_sims, 
                 fun =  function(x) run_one_sim(params))
stopCluster(cl)

# The following is if running iteratively rather than in parallel
# SIMS = parLapply(cl = cl, 
#                  X = 1:params$number_sims, 
#                  fun =  function(x) sim_single_introduction(X,sim_constants))

mean_I = list(time = SIMS[[1]]$interp_I$time,
              I_all = rep(0, length(SIMS[[1]]$interp_I$time)),
              I_no_extinction = rep(0, length(SIMS[[1]]$interp_I$time)))
nb_sims_with_NA = 0
for (i in 1:params$number_sims) {
  if (any(is.na(SIMS[[i]]$interp_I$I))) {
    tmp = SIMS[[i]]$interp_I$I
    tmp[which(is.na(tmp))] = 0
    mean_I$I_all = mean_I$I_all+tmp
    nb_sims_with_NA = nb_sims_with_NA+1
  } else {
    mean_I$I_all = mean_I$I_all + SIMS[[i]]$interp_I$I
    mean_I$I_no_extinction = mean_I$I_no_extinction + SIMS[[i]]$interp_I$I
  }
}
mean_I$I_all = mean_I$I_all/params$number_sims
mean_I$I_no_extinction = mean_I$I_no_extinction/nb_sims_with_NA
# For plot, find max of I
max_I = max(unlist(lapply(SIMS, function(x) max(x$interp_I$I, na.rm = TRUE))))
# Plot
png(file = sprintf("%s/many_CTMC_sims_with_means.png", here::here()),
    width = 1200, height = 800, res = 200)
plot(mean_I$time, SIMS[[1]]$interp_I$I,
     type = "l", ylim = c(0, max_I), lwd = 0.2,
     xlab = "Time (days)", ylab = "Number infectious")
for (i in 2:params$number_sims) {
  lines(mean_I$time, SIMS[[i]]$interp_I$I,
        type = "l", lwd = 0.2)
}
lines(mean_I$time, mean_I$I_all,
      type = "l", lty = 2,
      lwd = 5, col = "red")
lines(mean_I$time, mean_I$I_no_extinction,
      type = "l",
      lwd = 5, col = "red")
dev.off()
crop_figure(file = sprintf("%s/many_CTMC_sims_with_means.png", here::here()))
