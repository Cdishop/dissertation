using DataFrames, DataFramesMeta, Statistics, Distributions, Random



# common parameters

total_walks = 800
steps = 20
function initial_value()
    x = rand(Normal(0,1), 1)[1]
    return(x)
end

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
    employee_condition = zeros(5),
    moment_citizen_counts = zeros(5)
)

counter = 0


for employee_count in employee_conditions
    global counter = counter + 1

    # pull employee_count (e.g., 2) random columns from a vector
    use_cols = sample(1:employee_count, employee_count, replace = false)

    # now select those columns to work with
    condition_df = @linq df |>
        select(use_cols)

    # first column is always the focal employee
    # set moment counts to zero

    moment_counts = 0

    for row_i in 1:steps

        # pull row
        row = @linq condition_df |>
            where(row_i)
        # identify positions of max values
        maxval = maximum(row)
        positions = [i for (i, x) in enumerate(row) if x == maxval]
        # is employee the moment citizen?
        moment_citizen = false
        if length(positions) < 2 && positions[1] == 1
            moment_citizen = true
        end
        # increase counter
        if moment_citizen == true
            moment_counts = moment_counts + 1
        end
    end

    store_it[counter, :employee_condition] = employee_count
    store_it[counter, :moment_citizen_counts] = moment_counts


end
