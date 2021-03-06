# Code for stochastic simulations

Note: for the function `here::here()` used to say where to save the files to work, you have to use the code in this directory as an RStudio project, i.e., the Rproj file must be there (even if you do not open the project in Rstudio). Output (figures) are saved in the FIGURES subfolder.

## `simulate_ODE.R`
Simple illustration of the use of `deSolve` to plot several solutions to the SIS epidemic model. Produces the following figure.

![](FIGURES/ODE_SIS.png)

## `simulate_DTMC.R` and `simulate_DTMC_small_time_step.R`
Simulation of the SIS as a discrete-time Markov chain using a random walk. Demonstrates the use of the libraries `DTMCPack` and `markovchain`. Among other things, produces the following figure (using the version with small time step).

![](FIGURES/several_DTMC_sims.png)

Note that if you want to consider many different paths for the version with small time step, it could be useful to parallelise calls to `DTMC` instead of using `DTMCPack`, which is not parallelised.


## `simulate_CTMC.R`
Simulation of the SIS as a continuous-time Markov chain, using `GillespieSSA2`. Produces a figure such as the following.

![](FIGURES/several_CTMC_sims.png)

## `simulate_CTMC_parallel.R`
Same as above but parallelised. Also plot the mean, mean conditioned on non-extinction as well as solutions of the ODE, as in the following figure.

![](FIGURES/many_CTMC_sims_with_means.png)

## `simulate_CTMC_parallel_multiple_R0.R`
Implements what would normally be a double loop as a list parsed in parallel. Also uses `adatptivetau` instead of `GillespieSSA2` for illustration. Gives the following figure

![](FIGURES/extinctions_fct_R0.png)

This figure illustrates how a CTMC SIS model will see a sizeable percentage of realisations with disease extinction despite values of $R_0$ larger than 1 and how this percentage further depends on the initial number I_0 of infectious individuals in the population.
