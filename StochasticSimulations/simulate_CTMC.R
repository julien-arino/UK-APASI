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



IC <- c(S = Pop, I = (Pop-I_0))
params <- c(gamma = gamma, beta = beta)
reactions <- list(
  # propensity function effects name for reaction
  reaction("beta*S*I", c(S=-1,I=+1), "new_infection"),
  reaction("gamma*I", c(S=+1,I=-1), "recovery")
)
out <-
  ssa(
    initial_state = IC,
    reactions = reactions,
    params = params,
    method = ssa_exact(),
    final_time = 5,
    census_interval = .001,
    verbose = TRUE
  )
plot_ssa(out)
