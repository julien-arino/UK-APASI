# Example simulation of a simple SIS model. In this version, we use a small time step and
# thus need to scale time accordingly.
library(DTMCPack)

# Source a file with a few helpful functions for plotting (nice axes labels, crop figure)
source(sprintf("%s/../functions_useful.R", here::here()))

# Recreate state from solution
number_I = function(sol) {
  time = 1:dim(sol)[2]
  state = rep(0, length(time))
  for (t in time) {
    state[t] = which(sol[,t] == 1)-1
  }
  OUT = data.frame(time = time, state = state)
}


# Total population
Pop = 100
# Initial number of infectious
I_0 = 2
# Parameters
gamma = 1/5
R_0 = 1.5
# R0 would be (beta/gamma)*S0, so beta=R0*gamma/S0
beta = R_0*gamma/(Pop-I_0)
# Final time
t_f = 100
# Determine the maximum time step (nondiagonal entries)
# On each row, the offdiagonal entries (without Delta_t) have weight
# beta*(Pop-I)*I+gamma*I
# For I in [0,Pop], the maximum is at I=(Pop*beta+gamma)/(2*beta) and equals
# (gamma^2+2*Pop*beta*gamma+Pop^2*beta^2)/(4*beta)
max_weight = (gamma^2+2*Pop*beta*gamma+Pop^2*beta^2)/(4*beta)
# To make the time step a little smaller, take for instance
Delta_t = 1/(0.05+max_weight)

# Prepare I vector
IC = mat.or.vec(nr = (Pop+1), nc = 1)
# Make the transition matrix
T = mat.or.vec(nr = (Pop+1), nc = (Pop+1))
for (row in 2:(dim(T)[1]-1)) {
  I = row-1
  # Recoveries
  mv_right = gamma*I*Delta_t
  # Infections
  mv_left = beta*I*(Pop-I)*Delta_t
  # Fill up the matrix
  T[row,(row-1)] = mv_right
  T[row,(row+1)] = mv_left
}
# Last row only has move left
T[(Pop+1),Pop] = gamma*(Pop-1)*Delta_t
# Check that we don't have too large values (should not happen with Delta_t chosen)
if (max(rowSums(T))>1) {
  writeLines("max(rowSums(T))>1, scaling")
  max_T = max(rowSums(T))
  T = T/max_T
}
diag(T) = 1-rowSums(T)

# Initial condition
# By putting all weight in one compartment, we fix an initial condition
IC[(I_0+1)] = 1

# Need to change final time, since it is in number of steps
nb_steps = ceiling(t_f/Delta_t)
# While we are at it, make vector of times
t = (1:nb_steps)*Delta_t

sol = DTMC(tmat = T, io = IC, N = nb_steps, trace = FALSE)
I = number_I(sol)
I$time = t

plot(I$time, I$state,
     type = "l",
     xlab = "Time (days)", ylab = "Number infectious")

sol = MultDTMC(nchains = 20, tmat = T, io = IC, n = nb_steps)

# Process solutions
# We want to show trajectories that go to zero differently from those that go endemic,
# so we do a bit of preprocessing, adding a colour field each solution
I = list()
I_max = 0
for (i in 1:length(sol)) {
  I[[i]] = number_I(sol[[i]])
  I[[i]]$time = t
  if (max(I[[i]]$state)>I_max) {
    I_max = max(I[[i]]$state)
  }
  if (I[[i]]$state[length(I[[i]]$state)] == 0) {
    I[[i]]$colour = "dodgerblue4"
    I[[i]]$lwd = 2
  } else {
    I[[i]]$colour = "black"
    I[[i]]$lwd = 0.5
  }
}

png(file = sprintf("%s/FIGURES/several_DTMC_sims.png", here::here()),
    width = 1200, height = 800, res = 200)
plot(I[[1]]$time, I[[1]]$state,
     type = "l",
     xlab = "Time (days)", ylab = "Number infectious",
     ylim = c(0,I_max),
     col = I[[1]]$colour, lwd = I[[1]]$lwd)
for (i in 2:length(sol)) {
  lines(I[[i]]$time, I[[i]]$state,
        type = "l",
        col = I[[i]]$colour, lwd = I[[i]]$lwd)
}
dev.off()
crop_figure(file = sprintf("%s/FIGURES/several_DTMC_sims.png", here::here()))


### An example of using the markovchain package
library(markovchain)

# The transition matrix is already ready, so we just need to incorporate it.
# We also need to name the states
mcSIS <- new("markovchain", 
             states = sprintf("I_%d", 0:Pop),
             transitionMatrix = T,
             name = "SIS")

# For fun, plot the chain digraph
png(file = sprintf("%s/FIGURES/DTMC_digraph.png", here::here()),
    width = 1200, height = 800, res = 200)
plot(mcSIS)
dev.off()
crop_figure(sprintf("%s/FIGURES/DTMC_digraph.png", here::here()))

meanAbsorptionTime(mcSIS)
