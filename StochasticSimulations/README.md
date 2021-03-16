# Code for stochastic simulations

Note: for the function `here::here()` used to say where to save the files to work, you have to use the code in this directory as an RStudio project, i.e., the Rproj file must be there (even if you do not open the project in Rstudio).

# `simulate_ODE.R`
Simple illustration of the use of `deSolve` to plot several solutions to the SIS epidemic model.

# `simulate_DTMC.R`
Simulation of the SIS as a discrete-time Markov chain using a random walk. Demonstrates the use of the libraries `DTMCPack` and `markovchain`.