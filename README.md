# Description
## Sablefish_ApportionmentStrategies
sablefish apportionment strategy evaluation and general Alaska MSE framework.

***
# Simulation Objects Descriptions and Dimensionality

## Values

### Input data to OM
Values are **fixed**.

Name              | Description                       | Value
------------------|-----------------------------------|-------------------
A                 | Max age (plus age)                | 31
age.rec           | Age at recruitment                | 2
Bstart            | Starting biomass                  | 1600 kt - *completely imaginary*
n.age             | Number of model ages              | 30
n.area            | Number of simulated model areas   | 6
n.fish            | Number of fisheries (fleets)      | 4
n.surv            | Number of surveys                 | 2
n.sex             | Number of sexes                   | 2
n.sims            | Number of simulations (realizations) | 100 - *subject to change*
n.year            | Number of years in each simulation | 50 - *subject to change*    
mu_rec            | Log mean recruitment              | 16.5
sigma_rec         | Log recruitment standard deviation ($/sigma$) | 1.2



## Vectors, Matrices, and Arrays

### Input data to OM
Values are **fixed**. 

Name              | Description                       | Dimensions
------------------|-----------------------------------|----------------------------------
ages              | Vector - Age classes              | `[1:n.age]`
len               | Vector - Length bins              | `[1:n.length]`
years             | Vector - Year identifiers         | `[1:n.years]`
surv              | Vector of factors - Survey Names  | `[1:n.surv]`
fish              | Vector of factors - Fishery Names | `[1:n.fish]`
la                | Matrix - Length @ Age             | `[1:n.sex, 1:n.age]`
ln.wa             | Matrix - ln(Weight @ Age)         | `[1:n.sex, 1:n.age]`
wa                | Matrix - Weight @ Age             | `[1:n.sex, 1:n.age]`
ma                | Matrix - Maturity @ Age           | `[1:n.sex, 1:n.age]`
mx                | Matrix - Natural Mortality @ Age  | `[1:n.sex, 1:n.age]`
*lx* **not implemented** | Matrix - Survivorship to Age      | `[1:n.sex, 1:n.age]`
ml *error!*       | Matrix - Maturity @ Length        | `[1:n.sex, 1:n.age]` - Should be length bins.

### OM Objects
Values are **variable**.

Name      | Description                       | Dimensions
----------|-----------------------------------|----------------------------------
B         | Biomass                           | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
N         | Numbers                           | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
C.b       | Catch in biomass                  | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
C.n       | Catch in numbers                  | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
harvest.b | Harvest in biomass by **fleet**   | `[1:n.sex, 1:n.year, 1:n.age, 1:n.fish, 1:n.area, 1:n.sims]`
harvest.n | Harvest in numbers by **fleet**   | `[1:n.sex, 1:n.year, 1:n.age, 1:n.fish, 1:n.area, 1:n.sims]`
Z.a       | Total instantaneout mortality     | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
F.a       | Total instantaneous fishing mortality | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
Fmort     | Fishing mortality by **fleet**    | `[1:n.fish, 1:n.year, 1:n.area, 1:n.sims]`
surv      | Survival Rate                     | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
mort      | Mortality Rate                    | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
**ssb**   | Spawning stock biomass            | `[1:n.sex, 1:n.age, 1:n.year, 1:n.area, 1:n.sims]`
**rec**   | Recruitment                       | `[1:n.sex, 1:n.year, 1:n.area, 1:n.sims]`
va        | **Fishery** vulnerability @ age   | `[1:n.fish, 1:n.area, 1:n.sex, 1:n.age]`

### EM Data Streams
Values are **variable**.

Name      | Description                       | Dimensions
----------|-----------------------------------|----------------------------------
surv.RPN  | longline survey RPN, 'current' year, for 6 areas then combine to 3 | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
Fish.RPW  | longline/fixed gear fishery CPUE/RPW, lagged 1 year, for 6 areas then combine to 3 | `[1:n.sex, 1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
Fish.AC   | longline/fixed gear fishery age comps, lagged 1 year, for 6 areas then combine to 3, single sex | `[1:n.year, 1:n.age, 1:n.area, 1:n.sims]`
Surv.AC   | longline survey age comps, lagged 1 year, for 6 areas then combine to 3, single sex | `[1:n.year, 1:n.age, 1:n.area, 1:n.sims]`


## Lists

***
# Functions

Name                                             | Description
-------------------------------------------------|-----------------------------------

