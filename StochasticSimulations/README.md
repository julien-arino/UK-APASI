# Code for stochastic simulations

Note: for the function `here::here()` used to say where to save the files to work, you have to use the code in this directory as an RStudio project, i.e., the Rproj file must be there (even if you do not open the project in Rstudio). Output (figures) are saved in the FIGURES subfolder.

## `simulate_ODE.R`
Simple illustration of the use of `deSolve` to plot several solutions to the SIS epidemic model. Produces the following figure.

![width:400px](https://github.com/julien-arino/UK-APASI/blob/7db02e07e5af2259d578317b89c0db9080f945e8/StochasticSimulations/FIGURES/ODE_SIS.png)

## `simulate_DTMC.R`
Simulation of the SIS as a discrete-time Markov chain using a random walk. Demonstrates the use of the libraries `DTMCPack` and `markovchain`. Among other things, produces the following figure.

![width:400px](https://github.com/julien-arino/UK-APASI/blob/24d5ecb3a64c94da18d2f0b48e2b0ee36b2bfcb4/StochasticSimulations/FIGURES/several_DTMC_sims.png)


## `simulate_CTMC.R`
Simulation of the SIS as a continuous-time Markov chain, using `GillespieSSA2`. Produces a figure such as the following.

![width:300px](https://github.com/julien-arino/UK-APASI/blob/7db02e07e5af2259d578317b89c0db9080f945e8/StochasticSimulations/FIGURES/several_CTMC_sims.png)

## `simulate_CTMC_parallel.R`
Same as above but parallelised. Also plot the mean, mean conditioned on non-extinction as well as solutions of the ODE, as in the following figure.

![width:300px](https://github.com/julien-arino/UK-APASI/blob/24d5ecb3a64c94da18d2f0b48e2b0ee36b2bfcb4/StochasticSimulations/FIGURES/many_CTMC_sims_with_means.png)

## `simulate_CTMC_parallel_multiple_R0.R`
Implements what would normally be a double loop as a list parsed in parallel. Also uses `adatptivetau` instead of `GillespieSSA2` for illustration. Gives the following figure

![width:400px](https://github.com/julien-arino/UK-APASI/blob/298796a1e3c675aa7d0f03e08b482ff2c68a412c/StochasticSimulations/FIGURES/extinctions_fct_R0.png)

This figure illustrates how a CTMC SIS model will see a sizeable percentage of realisations with disease extinction despite values of $R_0$ larger than 1 and how this percentage further depends on the initial number I_0 of infectious individuals in the population.
