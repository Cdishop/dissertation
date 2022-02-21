using DataFrames, DataFramesMeta, Query, Statistics, CSV, CSVFiles, Distributions, Random, PrettyTables, RCall

employee_amounts = [100, 200, 300, 400, 500]
save_frame = DataFrame(
    k = Float64[],
    probability = Float64[],
    numemployees = Float64[]
)

for employee_amount in employee_amounts
    # common parameters
    sims = 10000
    num_employees = employee_amount
    steps = 20
    #function initial_value()
    #    x = rand(0:100,1)[1]
    #    return(x)
    #end
    function initial_value()
        x = (rand()*20)[1]
        return(x)
    end









    ##########################################################################################################################
    # sim4a: respond to outflux: inertia


    store_it_sim4a = DataFrame(
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
                 help[1] = requests[1]

                 for step in 2:steps

                     draw = rand(Normal(0,1), 1)[1]

                     requests[step] = requests[step - 1] + draw
                     if requests[step] < 0
                         requests[step] = 0
                     end

                     if draw < 0
                         help[step] = 5
                     else
                         help[step] = 0
                     end


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
            df_help <- data.frame(df_help)
            moment_counts <- 0
            ecdf_fun <- function(x, perc) ecdf(x)(perc)
            location <- ecdf_fun(as.numeric(df_help[1,]), df_help[1,1])


              for(row_i in 2:20){

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
            store_it_sim4a[counter, :simulation] = sim
            store_it_sim4a[counter, :moment_citizen_counts] = moment_counts

    end


    prob_steps = collect(0:1:steps)
    probability_list_2 = zeros(length(prob_steps))

    for prob in prob_steps
        result = count(
            k == prob for k in store_it_sim4a.moment_citizen_counts) / length(store_it_sim4a.moment_citizen_counts)
                probability_list_2[[prob + 1]] .= result
    end


    sim4a_results = DataFrame(
                k = [0,1,2,3,4,5,6,7,8,9,10,
                     11,12,13,14,15,16,17,18,19,20],
                probability = probability_list_2)


    sim4a_results = @linq sim4a_results |>
                        transform(numemployees = repeat([employee_amount], 21))

    append!(save_frame, sim4a_results)

end

cd(dirname(@__FILE__))


CSV.write("sim-results/sim4a.csv", save_frame)
