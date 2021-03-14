# Example simulation of a simple SIS model
library(GillespieSSA2)

# Total population
Pop = 100
# Initial number of infectious
I_0 = 2
# Parameters
gamma = 1/5
# R0 would be (beta/gamma)*S0, so beta=R0*gamma/S0
beta = 1.5*gamma/(Pop-I_0)
# Final time
t_f = 100



IC <- c(S = (Pop-I_0), I = I_0)
params <- c(gamma = gamma, beta = beta)
reactions <- list(
  # propensity function effects name for reaction
  reaction("beta*S*I", c(S=-1,I=+1), "new_infection"),
  reaction("gamma*I", c(S=+1,I=-1), "recovery")
)
set.seed(NULL)
sol <-
  ssa(
    initial_state = IC,
    reactions = reactions,
    params = params,
    method = ssa_exact(),
    final_time = t_f,
    #census_interval = .001,
    #verbose = TRUE
  )

#plot_ssa(out)
png(file = sprintf("%s/one_CTMC_sim.png", 
                   here::here()),
    width = 800, height = 600)
plot(sol$time, sol$state[,"I"],
     type = "l",
     xlab = "Time (days)", ylab = "Number infectious")
dev.off()


nb_sims = 50
sol = list()
for (i in 1:nb_sims) {
  writeLines(paste("Start simulation", i))
  set.seed(NULL)
  sol[[i]] <-
    ssa(
      initial_state = IC,
      reactions = reactions,
      params = params,
      method = ssa_exact(),
      final_time = t_f,
      #census_interval = .001,
      #verbose = TRUE
    )
}

# Determine maximum value of I for plot
I_max = max(unlist(lapply(sol, function(x) max(x$state[,"I"], na.rm = TRUE))))

# We want to show trajectories that go to zero differently from those that go endemic,
# so we do a bit of preprocessing, adding a colour field each solution
for (i in 1:nb_sims) {
  idx_last_I = dim(sol[[i]]$state)[1]
  val_last_I = as.numeric(sol[[i]]$state[idx_last_I,"I"])
  if (val_last_I == 0) {
    sol[[i]]$colour = "dodgerblue4"
  } else {
    sol[[i]]$colour = "black"
  }
}

# Now do the plot
plot(sol[[1]]$time, sol[[1]]$state[,"I"],
     xlab = "Time (days)", ylab = "Number infectious",
     ylim = c(0, I_max), type = "l",
     col = sol[[1]]$colour)
for (i in 2:nb_sims) {
  lines(sol[[i]]$time, sol[[i]]$state[,"I"],
        type = "l",
        col = sol[[i]]$colour)
}
