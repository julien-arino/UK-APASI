# Example simulation of a simple SIS model
library(DTMCPack)

# Recreate state from solution
number_I = function(sol) {
  time = 1:dim(sol)[2]
  state = rep(0, length(time))
  for (t in time) {
    state[t] = which(sol[,t] == 1)-1
  }
  OUT = data.frame(time = time,
                   state = state)
}


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

# Prepare I vector
IC = mat.or.vec(nr = (Pop+1), nc = 1)
# Make the transition matrix
T = mat.or.vec(nr = length(IC), nc = length(IC))
for (row in 2:(dim(T)[1]-1)) {
  # Recoveries
  mv_right = gamma*row
  # Infections
  mv_left = beta*row*(Pop-row)
  # Need to regularise
  T[row,(row-1)] = mv_right/(mv_right+mv_left)
  T[row,(row+1)] = mv_left/(mv_right+mv_left)
}
# When we get to zero, we stay there
T[1,1] = 1
# When we get to the total population, we bounce back
T[(Pop+1),Pop] = 1

# Initial condition
# By putting all weight in one compartment, we fix an initial condition
IC[(I_0+1)] = 1

sol = DTMC(tmat = T, io = IC, N = t_f, trace = FALSE)
I = number_I(sol)

plot(I$time, I$state,
     type = "l",
     xlab = "Time (days)", ylab = "Number infectious")

sol = MultDTMC(nchains = 20, tmat = T, io = IC, n = t_f)
I = list()
I_max = 0
for (i in 1:length(sol)) {
  I[[i]] = number_I(sol[[i]])
  if (max(I[[i]]$state)>I_max) {
    I_max = max(I[[i]]$state)
  }
}

png(file = sprintf("%s/several_DTMC_sims.png", 
                   here::here()),
    width = 800, height = 600)
plot(I[[1]]$time, I[[1]]$state,
     type = "l",
     xlab = "Time (days)", ylab = "Number infectious",
     ylim = c(0,I_max))
for (i in 2:length(sol)) {
  lines(I[[i]]$time, I[[i]]$state,
        type = "l")
}
dev.off()