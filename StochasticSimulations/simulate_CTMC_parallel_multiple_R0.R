# Example CTMC simulation of a simple SIS model, parallel version, with multiple R_0 values
# Here, we just want to know how many simulations see extinction, so we do minimal post-processing

# BEWARE !!!
#
# With 100 sims and the number of values used, this can either be very lengthy on a machine with low thread
# count or very expensive in RAM on a machine with high thread count..

library(GillespieSSA2)
library(parallel)
library(dplyr)
library(latex2exp)

# Source a file with a few helpful functions for plotting (nice axes labels, crop figure)
source(sprintf("%s/../functions_useful.R", here::here()))

# Make data frame from list of results
# Assumes all list entries are similar
make_df_from_list = function(in_list) {
  names_list = names(in_list[[1]])
  tmp = list()
  for (n in names_list) {
    tmp[[n]] = unlist(lapply(in_list, function(x) x[[n]]))
  }
  OUT = do.call(cbind.data.frame, tmp)
  return(OUT)
}

# Need a function that runs one simulation and returns a result. While we're at it,
# we also return an interpolated solution
run_one_sim = function(params_vary, params) {
  IC <- c(S = (params$Pop-params$I_0), I = params$I_0)
  R_0 = params_vary$R_0
  # R0=(beta/gamma)*S0, so beta=R0*gamma/S0
  beta = R_0*params$gamma/(params$Pop-params$I_0)
  params_local <- c(gamma = params$gamma, beta = beta)
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
    final_time = params$t_f
  )
  # If the final time is less than t_f, we've hit extinction
  if (sol$state[dim(sol$state)[1],"I"] == 0) {
    sol$extinct = TRUE
  } else {
    sol$extinct = FALSE
  }
  # To lighten the load, select only a few items to return
  OUT = list()
  OUT$R_0 = R_0
  OUT$final_time = sol$time[length(sol$time)]
  OUT$final_I = sol$state[length(sol$time),"I"]
  OUT$extinct = sol$extinct
  # Return result
  return(OUT)
}

# To run in parallel, it useful to put parameters in a list
params = list()
params$Pop = 100000
params$gamma = 1/5
params$R_0 = 2.5
params$t_f = 150
params$I_0 = 2
# R0=(beta/gamma)*S0, so beta=R0*gamma/S0
params$beta = params$R_0*params$gamma/(params$Pop-params$I_0)
# Number of simulations. We may want to save all simulation parameters later,
# so we add it here
params$number_sims = 100

# To process efficiently in parallel, we make a list with the different parameter values
# we want to change, which will fed by parLapply to run_one_sim
params_vary = list()
# For now, we just vary R_0
i = 1
for (R_0 in seq(0.5, 3.5, by = 0.05)) {
  for (j in 1:params$number_sims) {
    params_vary[[i]] = list()
    params_vary[[i]]$R_0 = R_0
    i = i+1
  }
}


# Detect number of cores (often good to use all but 1, i.e. detectCores()-1)
no_cores <- detectCores()
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
                 X = params_vary, 
                 fun =  function(x) run_one_sim(x, params))
stopCluster(cl)

# The following is if running iteratively rather than in parallel
if (FALSE) {
  SIMS = lapply(X = params_vary, 
                FUN =  function(x) run_one_sim(x, params))
}

#saveRDS(SIMS, file = sprintf("%s/SIMS.Rds", here::here()))
#SIMS = readRDS(file = sprintf("%s/SIMS.Rds", here::here()))

# Use dplyr syntax: count the number of extinctions (TRUE and FALSE)
# after grouping by R_0 value
results = make_df_from_list(SIMS) %>%
  count(R_0, extinct) %>%
  filter(extinct == TRUE)

# Plot
png(file = sprintf("%s/FIGURES/extinctions_fct_R0.png", here::here()),
    width = 1200, height = 800, res = 200)
plot(results$R_0, results$n,
     type = "b", lwd = 2,
     xlab = TeX("R_0"), ylab = "Percentage extinctions")
dev.off()
crop_figure(file = sprintf("%s/FIGURES/extinctions_fct_R0.png", here::here()))

