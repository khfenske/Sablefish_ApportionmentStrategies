# Sablefish_ApportionmentStrategies
sablefish apportionment strategy evaluation and general Alaska MSE framework.


***
## Simulation Objects Descriptions and Dimensionality

### Values

Name              | Description                       | Value
------------------|-----------------------------------|-------------------
A                 | Max age (plus age)                | 31
age.rec           | Age at recruitment                | 2
Bstart            | Starting biomass                  | 1600 kt *completely imaginary*
n.age             | Number of model ages              | 30
n.area            | Number of simulated model areas   | 6
n.fish            | Number of fisheries (fleets)      | 4
n.surv            | Number of surveys                 | 2
n.sex             | Number of sexes                   | 2
n.sims            | Number of simulations (realizations) | 100 *subject to change*
n.year            | Number of years in each simulation | 50 *subject to change*    
sigma_rec         | Log recruitment standard deviation ($/sigma$) | 1.2



### Vectors, Matrices, and Arrays

Name              | Description                       | Dimensions
------------------|-----------------------------------|----------------------------------
ages              | Vector - Age classes              | `r [1:n.age] [1:30]`
years             | Vector - Year identifiers         | `r [1:n.years] [1:50]`


### Lists



