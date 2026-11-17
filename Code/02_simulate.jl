## 02 Simulate species persistence with the Bayesian network
## Reads the files written by 01_prepare_network.R and writes
##   Results/simulations/targeted.csv  persistence of all species, control and each targeted scenario
##   Results/simulations/random.csv    persistence of consumers when the same number of
##                                     plants is disturbed at random (one row per replicate)
##
## Run from Code/ with several threads, e.g.:  julia -t auto 02_simulate.jl
## Needs the Julia package Distributions:      julia -e 'using Pkg; Pkg.add("Distributions")'

## Settings
const ALPHA    = 3.0      # shape of the functional response (beta cdf)
const BETA     = 3.0
const PI_BASE  = 0.1      # baseline extinction probability, all species
const PI_DIST  = 0.5      # baseline extinction probability, disturbed plants
const N_MC     = 100_000  # Monte Carlo iterations per network evaluation
const N_RAND   = 5_000    # replicates of random disturbance per scenario
const SEED     = 2021
const RUN_TARGETED = true
const RUN_RANDOM   = true
const IN_PATH  = "Data/processed/"
const OUT_PATH = "../Results/simulations/"

using Distributions
using DelimitedFiles
using Random


## ---------------------------------------------------------------
## Functions
## ---------------------------------------------------------------

# Reads the network matrix.
# Input: file with a header of species codes; M[i,j] = 1 if i eats j,
#        rows in topological order (basal species first)
# Output: species codes, and for each species the indices of its prey
function read_network(file)
    data, header = readdlm(file, ',', header = true)
    codes = vec(String.(header))
    M = Int.(data)
    prey = [findall(M[i, :] .== 1) for i in 1:size(M, 1)]
    return codes, prey
end

# Reads which plants belong to which scenario.
# Input: memberships.csv (scenario, grouping, code)
# Output: ordered list of scenarios, Dict scenario => grouping,
#         Dict scenario => codes of its plants
function read_memberships(file)
    data, _ = readdlm(file, ',', String, header = true)
    scenarios = unique(data[:, 1])
    grouping = Dict(s => data[findfirst(data[:, 1] .== s), 2] for s in scenarios)
    plants = Dict(s => data[data[:, 1] .== s, 3] for s in scenarios)
    return scenarios, grouping, plants
end

# Monte Carlo evaluation of the Bayesian network.
# Input: prey lists, baseline extinction probabilities pb, functional response
#        parameters a and b, number of iterations, random number generator
# Output: probability of persistence for each species
function persistence(prey, pb, a, b, reps, rng)
    S = length(prey)
    n_prey = length.(prey)
    # probability of extinction for each species and number of lost prey:
    # p_ext[i][k+1] = pb + (1 - pb) * cdf(Beta(a,b), k / n_prey)
    p_ext = [n_prey[i] == 0 ? [pb[i]] :
             [pb[i] + (1 - pb[i]) * cdf(Beta(a, b), k / n_prey[i]) for k in 0:n_prey[i]]
             for i in 1:S]
    extant = falses(S)
    survived = zeros(Int, S)
    for rep in 1:reps
        for i in 1:S
            lost = 0
            for j in prey[i]
                lost += !extant[j]
            end
            extant[i] = rand(rng) >= p_ext[i][lost + 1]
        end
        survived .+= extant
    end
    return survived ./ reps
end

# Baseline extinction probabilities where the given species are disturbed.
# Input: species codes, codes of disturbed species
# Output: vector of baseline extinction probabilities
function baseline(codes, disturbed)
    pb = fill(PI_BASE, length(codes))
    pb[in(disturbed).(codes)] .= PI_DIST
    return pb
end


## ---------------------------------------------------------------
## Simulations
## ---------------------------------------------------------------

codes, prey = read_network(IN_PATH * "network.csv")
scenarios, grouping, plants = read_memberships(IN_PATH * "memberships.csv")
plant_codes = codes[isempty.(prey)]
consumers = findall(.!isempty.(prey))
mkpath(OUT_PATH)

## Targeted disturbance: all plants in a scenario are disturbed
if RUN_TARGETED
    open(OUT_PATH * "targeted.csv", "w") do io
        println(io, "scenario,grouping,code,persistence")
        all_scen = vcat("control", scenarios)
        for (k, s) in enumerate(all_scen)
            println("targeted: ", s)
            disturbed = s == "control" ? String[] : plants[s]
            g = s == "control" ? "control" : grouping[s]
            rng = Xoshiro(SEED + k)
            per = persistence(prey, baseline(codes, disturbed), ALPHA, BETA, N_MC, rng)
            for i in eachindex(codes)
                println(io, s, ",", g, ",", codes[i], ",", per[i])
            end
        end
    end
end

## Random disturbance: the same number of plants as in each scenario,
## drawn at random among all plants, N_RAND times
if RUN_RANDOM
    open(OUT_PATH * "random.csv", "w") do io
        println(io, "scenario,grouping,rep,", join(codes[consumers], ","))
        for (k, s) in enumerate(scenarios)
            n = length(plants[s])
            println("random: ", s, " (", n, " plants)")
            result = zeros(N_RAND, length(consumers))
            Threads.@threads for rep in 1:N_RAND
                # own random number generator per replicate, so that results
                # do not depend on the number of threads
                rng = Xoshiro(SEED + 1_000_000 * k + rep)
                disturbed = shuffle(rng, plant_codes)[1:n]
                per = persistence(prey, baseline(codes, disturbed), ALPHA, BETA, N_MC, rng)
                result[rep, :] = per[consumers]
            end
            for rep in 1:N_RAND
                println(io, s, ",", grouping[s], ",", rep, ",", join(result[rep, :], ","))
            end
        end
    end
end
