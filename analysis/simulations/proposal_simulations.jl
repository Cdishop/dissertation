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






using DataFrames, DataFramesMeta, Statistics, Distributions, Random, RCall


# what is the probability that the employee is in the top 20%

# common parameters

total_walks = 800
steps = 20
function initial_value()
    x = rand(Normal(0,1), 1)[1]
    return(x)
end

rq3_df = DataFrame(
    simulation = [],
    employee_condition = [],
    moment_citizen_counts = []
)

total_sims = 100

for sim in 1:total_sims

    # simulate all random walks

    df = DataFrame()

    for walk in 1:total_walks

        y = zeros(steps)
        y[1] = initial_value()

        for step in 2:steps
            y[step] = y[step - 1] + rand(Normal(0,1), 1)[1]
        end

        insert!(df, walk, y, Symbol(walk))
    end


    # iterate across employee amounts, select that many columns

    employee_conditions = [2, 200, 400, 500, 800]
    store_it = DataFrame(
        simulation = zeros(5),
        employee_condition = zeros(5),
        moment_citizen_counts = zeros(5)
    )

    counter = 0


    for employee_count in employee_conditions
         counter = counter + 1

        # pull employee_count (e.g., 2) random columns from a vector
        use_cols = collect(1:1:employee_count)

        # now select those columns to work with
        condition_df = @linq df |>
            select(use_cols)

            @rput condition_df
            @rput employee_count
            R"""
            library(tidyverse)
            moment_counts <- 0

            for (row_i in 1:20){

                row <- condition_df %>%
                    slice(row_i)

                percent_use <- round(ceiling(0.2*employee_count))

                max_percent_df <- row %>%
                    rownames_to_column() %>%
                    gather(column, value, -rowname) %>%
                    group_by(rowname) %>%
                    mutate(rk = rank(-value)) %>%
                    filter(rk <= percent_use) %>%
                    arrange(rowname, rk)

                column_contained <- sum(max_percent_df$column == "1")

                if(column_contained >= 1){moment_counts <- moment_counts + 1}

            }
            """
            @rget moment_counts

            store_it[counter, :simulation] = sim
            store_it[counter, :employee_condition] = employee_count
            store_it[counter, :moment_citizen_counts] = moment_counts

        end

        append!(rq3_df, store_it)
end



# condition 1: 2 employees

cond1_df = @linq rq3_df |>
    where(:employee_condition .== 2)

prob_steps = collect(0:1:steps)
probability_list_2 = zeros(length(prob_steps))

for prob in prob_steps
    result = count(
        k == prob for k in cond1_df.moment_citizen_counts) / length(cond1_df.moment_citizen_counts)
            probability_list_2[[prob + 1]] .= result
end

cond1_results = DataFrame(
    k = [0,1,2,3,4,5,6,7,8,9,10,
         11,12,13,14,15,16,17,18,19,20],
    probability = probability_list_2,
    num_employees = vcat(
                repeat([2], length(probability_list_2))
    )
    )



# create function

function create_results(employee_number)
    cond_df = @linq rq3_df |>
        where(:employee_condition .== employee_number)

    prob_steps = collect(0:1:steps)
    probability_list_z = zeros(length(prob_steps))

    for prob in prob_steps
        result = count(
            k == prob for k in cond_df.moment_citizen_counts) / length(cond_df.moment_citizen_counts)
                probability_list_z[[prob + 1]] .= result
    end

    cond_results = DataFrame(
        k = [0,1,2,3,4,5,6,7,8,9,10,
                 11,12,13,14,15,16,17,18,19,20],
        probability = probability_list_z,
        num_employees = vcat(
                        repeat([employee_number], length(probability_list_200))
        )
        )

        return(cond_results)
    end

# condition 2: 200 employees
cond2_results = create_results(200)
# 400 employees
cond3_results = create_results(400)
# 600 employees
cond4_results = create_results(600)
# 800 employees
cond5_results = create_results(800)




r3_output = append!(cond1_results, cond2_results)
r3_output = append!(r3_output, cond3_results)
r3_output = append!(r3_output, cond4_results)
r3_output = append!(r3_output, cond5_results)
