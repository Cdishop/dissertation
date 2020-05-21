using DataFrames, DataFramesMeta, Query, Statistics, CSV, CSVFiles, Distributions, Random, PrettyTables, RCall


# common parameters

total_walks = 600
steps = 20
function initial_value()
    x = rand(Normal(0,20), 1)[1]
    return(x)
end


# the simulation column will contain simulation 1, 2, 3, 4, 5, etc. through total_sims
# the employee condition column will contain 2, 200, 400, 600, 800
# the moment citizen counts will contain the number of times employee x_i was the moment citizen

rq3_df = DataFrame(
    simulation = [],
    employee_condition = [],
    moment_citizen_counts = []
)

total_sims = 10

for sim in 1:total_sims

    # simulate all 800 random walks

    df = DataFrame()

    for walk in 1:total_walks

        y = zeros(steps)
        y[1] = initial_value()

        for step in 2:steps
            y[step] = y[step - 1] + rand(Normal(0,1), 1)[1]
        end

    # place the walks into a data frame where each walk is a column
    # column 1 is the first walk
    # column 2 is the second walk
    # column 3 is the third walk
    # the column names are just called 1,2,3,4, etc.

        insert!(df, walk, y, Symbol(walk))
    end

    # now I have all 800 random walks, the total population possible

    # select 2 columns for condition 1, select 200 for condition 2, 400 for condition 3, etc.

    employee_conditions = [2, 200, 400, 600, 800]

    # within a single simulation, I evaluate the 2 person condition, the 200 person condition, etc.
    # there are 5 total conditions (2, 200, 400, 600, 800)

    store_it = DataFrame(
        simulation = zeros(5),
        employee_condition = zeros(5),
        moment_citizen_counts = zeros(5)
    )

    counter = 0

    # start with the 2 employee condition, then do the 200 person condition ...
    for employee_count in employee_conditions
         counter = counter + 1

        # randomly pull employee_count (2, then 200, then 400) random walks from the total 800
        use_cols = sample(1:employee_count, employee_count, replace = false)


        # narrow my data frame so I'm only looking at the pulled walks
        condition_df = @linq df |>
            select(use_cols)

        # I randomly selected columns, so change their names to 1,2,3,4, etc through last column
        # the first column will be employee x_i
        change_cols = collect(1:1:employee_count)
        rename!(condition_df, [Symbol("$i") for i in change_cols])

        # now I need to identify if employee x_i is in the top 20% of the randomly selected walks
        # this is hard to do in julia so I did it in R.
        # remember that I'm looking at whether x_i is in the top 20% of the randomly selected walks for EVERY TIME POINT


        # the actual structure of the syntax is as follows:
        # for each row (time point), identify the top (e.g., 20%) of values
        # is one of those values from the first column? (employee x_i)

        # in other words, is employee x_i in the top (e.g., 20%) set of values for a given time point
        # repeat across all time points

            @rput condition_df
            @rput employee_count
            R"""
            library(tidyverse)
            moment_counts <- 0

            for (row_i in 1:20){

                # select a single time point
                row <- condition_df %>%
                    slice(row_i)

                # within that time point, I want to identify the max 20% of values
                percent_use <- round(ceiling(0.2*employee_count))

                # each column contains a random walk
                # I want to identify the top 20% of values within a single row across all of my columns
                max_percent_df <- row %>%
                    rownames_to_column() %>%
                    gather(column, value, -rowname) %>%
                    group_by(rowname) %>%
                    mutate(rk = rank(-value)) %>%
                    filter(rk <= percent_use) %>%
                    arrange(rowname, rk)

                # above returns a df with a column called "column" that tells me which columns (1,3,9,etc.) were in the top 20%
                # is employee x_i in that list? i.e., is 1 listed in the column called "column"?
                column_contained <- sum(max_percent_df$column == "1")

                # if so
                # then employee x_i was in the top 20%
                # so count it and increase the counter
                if(column_contained >= 1){moment_counts <- moment_counts + 1}

            # repeat for every time point
            }
            """
            @rget moment_counts

        # now I have (for a single simulation run) the number of time points employee x_i was in the top 20% when there were 2 employees
            store_it[counter, :simulation] = sim
            store_it[counter, :employee_condition] = employee_count
            store_it[counter, :moment_citizen_counts] = moment_counts

        # repeat for every condition (200, 400, 600...)
        # this is still just a single simulation run
        end

        # do that for multiple simulation runs and append the results
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
                        repeat([employee_number], length(probability_list_z))
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
