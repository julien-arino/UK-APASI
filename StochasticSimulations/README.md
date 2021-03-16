# Code for stochastic simulations

Note: for the function `here::here()` used to say where to save the files to work, you have to use the code in this directory as an RStudio project, i.e., the Rproj file must be there (even if you do not open the project in Rstudio).

## `simulate_ODE.R`
Simple illustration of the use of `deSolve` to plot several solutions to the SIS epidemic model.

## `simulate_DTMC.R`
Simulation of the SIS as a discrete-time Markov chain using a random walk. Demonstrates the use of the libraries `DTMCPack` and `markovchain`.

## `simulate_CTMC.R`
Simulation of the SIS as a continuous-time Markov chain, using `GillespieSSA2`.

## `simulate_CTMC_parallel.R`
Same as above but parallelised.

## `simulate_CTMC_parallel_multiple_R0.R`
Implements what would normally be a double loop as a list parsed in parallel. Also uses `adatptivetau` instead of `GillespieSSA2` for illustration. Gives the following figure:
![width=600px](https://github.com/julien-arino/UK-APASI/blob/298796a1e3c675aa7d0f03e08b482ff2c68a412c/StochasticSimulations/FIGURES/extinctions_fct_R0.png)