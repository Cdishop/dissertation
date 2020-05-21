using DataFrames, DataFramesMeta, Query, Statistics, CSV, CSVFiles, Distributions, Random, PrettyTables, RCall

# common parameters
sims = 100
num_employees = 100
steps = 20
#function initial_value()
#    x = rand(0:100,1)[1]
#    return(x)
#end
function initial_value()
    x = (rand()*30)[1]
    return(x)
end


# sim1: respond to many: inertia

store_it = DataFrame(
    simulation = zeros(sims),
    moment_citizen_counts = zeros(sims)
)

counter = 0

for sim in 1:sims
     global counter = counter + 1


     df_requests = DataFrame()
     df_help = DataFrame()

     for walk in 1:num_employees

         requests = zeros(steps)
         requests[1] = initial_value()

         help = zeros(steps)
         help[1] = 1*requests[1]

         for step in 2:steps

             requests[step] = requests[step - 1] + rand(Normal(0,1), 1)[1]
             if requests[step] < 0
                 requests[step] = 0
             end

             help[step] = 1*requests[step]

         end

         insert!(df_requests, walk, requests, Symbol(walk))
         insert!(df_help, walk, help, Symbol(walk))
     end

# now take the help df and act like it is the request df

# condition df is just the df
# employee count is just total_employees
    @rput df_help
    @rput num_employees
    R"""
    library(tidyverse)
    moment_counts <- 0

    for (row_i in 1:20){

        # select a single time point
        row <- df_help %>%
            slice(row_i)

            # within that time point, I want to identify the max 20% of values
            percent_use <- round(ceiling(0.2*num_employees))

            # each column contains a string of help for a given individual
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


            # now do the same for the bottom 20%

            # within that time point, I want to identify the max 20% of values
            percent_use <- round(ceiling(0.2*num_employees))

            # each column contains a string of help for a given individual
            # I want to identify the top 20% of values within a single row across all of my columns
            max_percent_df_bottom <- row %>%
            rownames_to_column() %>%
            gather(column, value, -rowname) %>%
            group_by(rowname) %>%
            mutate(rk = rank(value)) %>%
            filter(rk <= percent_use) %>%
            arrange(rowname, rk)

            # above returns a df with a column called "column" that tells me which columns (1,3,9,etc.) were in the top 20%
            # is employee x_i in that list? i.e., is 1 listed in the column called "column"?
            column_contained_bottom <- sum(max_percent_df_bottom$column == "1")

            # if so
            # then employee x_i was in the top 20%
            # so count it and increase the counter
            if(column_contained_bottom >= 1){moment_counts <- moment_counts + 1}

            # repeat for every time point
        }
        """
        @rget moment_counts



# now store that in the simulation runs


# now I have (for a single simulation run) the number of time points employee x_i was in the top 20% when there were 2 employees
    store_it[counter, :simulation] = sim
    store_it[counter, :moment_citizen_counts] = moment_counts

end


prob_steps = collect(0:1:steps)
probability_list_2 = zeros(length(prob_steps))

for prob in prob_steps
    result = count(
        k == prob for k in store_it.moment_citizen_counts) / length(store_it.moment_citizen_counts)
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
