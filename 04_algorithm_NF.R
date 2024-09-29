

convert_to_population <- function(sprint, vodafone, num_adults, num_kids) {

  # set c3 as mean of the other two:
  third_provider = mean(sprint,vodafone)
  population = (sprint + vodafone + third_provider) * (1.2 * num_adults + 0.1 * num_kids )

    return(population)
}