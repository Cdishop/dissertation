using DataFrames, DataFramesMeta, Query, Statistics, CSV, CSVFiles, Distributions, Random, PrettyTables, RCall

time_amounts = [3, 40, 200]
save_frame = DataFrame(
    k = Float64[],
    probability = Float64[],
    timeamount = Float64[]
)

for time_amount in time_amounts

    # common parameters
    sims = 200000
    num_employees = 300
    steps = time_amount
    #function initial_value()
    #    x = rand(0:100,1)[1]
    #    return(x)
    #end
    function initial_value()
        x = (rand()*20)[1]
        return(x)
    end









    ##########################################################################################################################
    # sim1b: respond to many: no inertia
    # change: autoregression on requests

    store_it_sim1b = DataFrame(
        simulation = zeros(sims),
        moment_citizen_counts = zeros(sims)
    )

    global counter = 0

    for sim in 1:sims
             global counter = counter + 1


             df_requests = DataFrame()
             df_help = DataFrame()

             for walk in 1:num_employees

                 requests = zeros(steps)
                 requests[1] = initial_value()

                 help = zeros(steps)
                 help[1] = 0.5*requests[1]

                 for step in 2:steps

                     requests[step] = 0*requests[step - 1] + rand(Normal(0,1), 1)[1]
                     if requests[step] < 0
                         requests[step] = 0
                     end

                     help[step] = 0.5*requests[step]

                 end

                 insert!(df_requests, walk, requests, Symbol(walk))
                 insert!(df_help, walk, help, Symbol(walk))
             end

        # now take the help df and act like it is the request df

        # condition df is just the df
        # employee count is just total_employees
            @rput df_help
            @rput num_employees
            @rput time_amount
            R"""
            library(tidyverse)
            df_help <- data.frame(df_help)
            moment_counts <- 0
            ecdf_fun <- function(x, perc) ecdf(x)(perc)
            location <- ecdf_fun(as.numeric(df_help[1,]), df_help[1,1])


              for(row_i in 2:time_amount){

                row <- df_help %>%
                  slice(row_i)

                  location_later <- ecdf_fun(as.numeric(row[1,]), row[1,1])
                  location_window <- seq(from = (location_later - 0.10), to = (location_later + 0.10), by = 0.01)

                  yes_it_is_contained <- location > min(location_window) && location < max(location_window)
                  if(yes_it_is_contained == TRUE){
                    moment_counts <- moment_counts + 1}

              }
                """
                @rget moment_counts



        # now store that in the simulation runs


        # now I have (for a single simulation run) the number of time points employee x_i was in the top 20% when there were 2 employees
            store_it_sim1b[counter, :simulation] = sim
            store_it_sim1b[counter, :moment_citizen_counts] = moment_counts

    end


    prob_steps = collect(0:1:steps)
    probability_list_2 = zeros(length(prob_steps))

    for prob in prob_steps
        result = count(
            k == prob for k in store_it_sim1b.moment_citizen_counts) / length(store_it_sim1b.moment_citizen_counts)
                probability_list_2[[prob + 1]] .= result
    end



    sim1b_results = DataFrame(
        k = collect(0:1:time_amount),
        probability = probability_list_2)


    sim1b_results = @linq sim1b_results |>
                    transform(timeamount = repeat([time_amount],  time_amount + 1))

    append!(save_frame, sim1b_results)

end

cd(dirname(@__FILE__))


CSV.write("sim-results/sim1b.csv", save_frame)