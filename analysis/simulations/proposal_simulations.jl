using DataFrames, DataFramesMeta, Query, Statistics, CSV, CSVFiles, Distributions, Random, PrettyTables

time = 20
function initial_value()
    x = rand(Normal(0,1), 1)
    return(x)
end




# base simulation

# a heuristic:
# employee 1 is the focal employee, set a random initial condition and run a random walk
# do the same for another random walk (walk2)
# out of 20 time steps, is the employee ever the moment citizen? yes or no...
# count the number of "yes" (e.g., 7)
# save 7, or the number of times the employee was the moment citizen, and run it again
# continue to run across however many simulations I generate
# now I have data that tells me the following
# out of 900 simulations, 20% of the time the focal employee has a moment citizen score of 3



# store my simulation runs
num_sims = 100
store_moments = zeros(num_sims)

for simulation in 1:num_sims

    # focal employee and other random walk
    # initial condition is random
    employee = zeros(time)
    employee[[1]] = initial_value()
    walk2 = zeros(time)
    walk2[[1]] = initial_value()

    # run the random walks across time, no drift
    for step in 2:time
    employee[step] = employee[step - 1] + rand(Normal(0,1), 1)[1]
    walk2[step] = walk2[step - 1] + rand(Normal(0,1), 1)[1]
    end

    # store the random walks
    df_base = DataFrame(
        employee = employee,
        walk2 = walk2
        )

        # identify at which time points the employee is the moment citizen
        function convert_moment_citizen(col1, col2)
            if col1 < col2
                result = "no"
            elseif col1 > col2
                result = "yes"
            elseif col1 == col2
                result = "no"
                return(result)
            end
        end

        df_base = @linq df_base |>
        transform(moment_citizen = convert_moment_citizen.(:employee, :walk2))

        # how many times is there a yes in the moment citizen column?

        moments = count(i == "yes" for i in df_base.moment_citizen)

        # place the number of times into a store
        store_moments[[simulation]] .= moments
end


# now I have a data set that tells me, for each simulation, how many time steps was the employee the moment citizen?
# so, I need to convert those to probabilities
# what's the probability that he spends 0 time steps?
# count the 0's, then divide by the number of simulations


# a vector from 0 to 20 increasing by 1 each step
prob_steps = collect(0:1:time)
# an empty vector to store the probabilities
probability_list = zeros(length(prob_steps))

# for each probability (k), 0, 1, 2, 3...
for prob in prob_steps
    # how many times does 0 appear in store_moments? how many times does 1 appear? etc...
    result = count(
                k == prob for k in store_moments) / length(store_moments)
    probability_list[[prob + 1]] .= result
end

# now I have a vector of probabilities.
# the first probability is the number of times 0 appeared
# the second is the number of times 1 appeared
# the third is the number of times 2 appeared...

base_results = DataFrame(
    k = [0,1,2,3,4,5,6,7,8,9,10,
         11,12,13,14,15,16,17,18,19,20],
    probability = probability_list
)









# rq1: drift
# only change is adding another for loop to adjust the drift parameter each time

drift_params = [0.2,0.4,0.6,0.8,1.0]

df_rq1 = DataFrame(
    k = [],
    probability = [],
    drift = []
)

for drift in drift_params


    num_sims = 100
    store_moments = zeros(num_sims)

    for simulation in 1:num_sims


        employee = zeros(time)
        employee[[1]] = initial_value()
        walk2 = zeros(time)
        walk2[[1]] = initial_value()

        for step in 2:time
        employee[step] = employee[step - 1] + drift + rand(Normal(0,1), 1)[1]
        walk2[step] = walk2[step - 1] + drift + rand(Normal(0,1), 1)[1]
        end

        df_base = DataFrame(
            employee = employee,
            walk2 = walk2
            )

            function convert_moment_citizen(col1, col2)
                if col1 < col2
                    result = "no"
                elseif col1 > col2
                    result = "yes"
                elseif col1 == col2
                    result = "no"
                    return(result)
                end
            end

            df_base = @linq df_base |>
            transform(moment_citizen = convert_moment_citizen.(:employee, :walk2))


            moments = count(i == "yes" for i in df_base.moment_citizen)

                store_moments[[simulation]] .= moments
            end

            prob_steps = collect(0:1:time)
            probability_list = zeros(length(prob_steps))

            for prob in prob_steps
                result = count(
                    k == prob for k in store_moments) / length(store_moments)
                        probability_list[[prob + 1]] .= result
            end

        single_run_results = DataFrame(
            k = [0,1,2,3,4,5,6,7,8,9,10,
                 11,12,13,14,15,16,17,18,19,20],
            probability = probability_list,
            drift = vcat(
                        repeat([drift], length(probability_list))
            )
            )

    append!(df_rq1, single_run_results)
end














# rq2: autoregressive
# only change is to run a for loop that changes the autoregrssive parameter


autor_params = [0.0,0.2,0.4,0.6,0.8]

df_rq2 = DataFrame(
    k = [],
    probability = [],
    autor = []
)

for autor in autor_params


    num_sims = 100
    store_moments = zeros(num_sims)

    for simulation in 1:num_sims


        employee = zeros(time)
        employee[[1]] = initial_value()
        walk2 = zeros(time)
        walk2[[1]] = initial_value()

        for step in 2:time
        employee[step] = autor*employee[step - 1] + rand(Normal(0,1), 1)[1]
        walk2[step] = autor*walk2[step - 1] + rand(Normal(0,1), 1)[1]
        end

        df_base = DataFrame(
            employee = employee,
            walk2 = walk2
            )

            function convert_moment_citizen(col1, col2)
                if col1 < col2
                    result = "no"
                elseif col1 > col2
                    result = "yes"
                elseif col1 == col2
                    result = "no"
                    return(result)
                end
            end

            df_base = @linq df_base |>
            transform(moment_citizen = convert_moment_citizen.(:employee, :walk2))


            moments = count(i == "yes" for i in df_base.moment_citizen)

                store_moments[[simulation]] .= moments
            end

            prob_steps = collect(0:1:time)
            probability_list = zeros(length(prob_steps))

            for prob in prob_steps
                result = count(
                    k == prob for k in store_moments) / length(store_moments)
                        probability_list[[prob + 1]] .= result
            end

        single_run_results = DataFrame(
            k = [0,1,2,3,4,5,6,7,8,9,10,
                 11,12,13,14,15,16,17,18,19,20],
            probability = probability_list,
            autor = vcat(
                        repeat([autor], length(probability_list))
            )
            )

    append!(df_rq2, single_run_results)
end













# rq3: more employees
