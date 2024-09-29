
# if data were individual numbers ...
convert_to_population <- function(sprint, vodafone, num_adults, num_kids) {

  # set c3 as mean of the other two:
  third_provider = mean(sprint,vodafone)
  population = (sprint + vodafone + third_provider) * (1.2 * num_adults + 0.1 * num_kids )

    return(population)
}

#------------------------------------------------------------------------------
# now write in dyplr for df manipulation

# set c3 as mean of the other two:

convert_to_population <- function(df_midnight) {

  midnight_population %>% df_midnight
    mutate(third_provider = (spark_devices + vodafone_devices) / 2) %>%
    mutate(adult_count = (total_count - child_count)) %>%
    mutatue(population = (sprint_devices + vodafone_devices + third_provider * (1.2 * adult_count + 0.1 * child_count)))

  return(midnight_population)
}
