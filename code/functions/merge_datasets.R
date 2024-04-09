# This function merges all the datasets and saves it out as a csv

# Sarah Gao
# January 19, 2024
# hellosarahgao@gmail.com

merge_datasets <- function(ea_data, smartchem_data, qpcr_data, inorg_n_data,
                           co2_data, id_assignments, output_file) {

  # Merge non-CO2 data first
  no_co2 <- ea_data %>%
    rbind(qpcr_data, inorg_n_data) %>%
    # Add a 'licor' column for merging
    mutate(licor = case_when(
      measurement_type == "licor" ~ "yes",
      .default = "no")) %>%
    left_join(id_assignments, c("sample_id", "sample_type", "licor")) %>%
    select(-measurement_type.y) %>%
    rename("measurement_type" = measurement_type.x) %>%
    # Add CO2 columns for merging
    mutate(std_co2_amount = NA, std_co2_units = NA)

  # Merge CO2 only data
  if(!missing(co2_data)){co2_only <- co2_data %>%
    # Convert sampled_date to double for merging
    mutate(sampled_date = as.double(sampled_date)) %>%
    # Add a licor column for merging
    mutate(licor = case_when(
      measurement_type == "licor" ~ "yes",
      .default = "no")) %>%
    # Join with ID assignments
    left_join(filter(id_assignments, licor == "yes"), c("sample_id",
                                                        "sampled_date", "licor",
                                                        "measurement_type")) %>%
    select(-sample_type.x) %>%
    rename("sample_type" = sample_type.y)

  # The final merging! Add CO2 only and non-CO2 datasets together
  all_data <- co2_only %>%
    rbind(no_co2) %>%
    select(-licor)
  }

  if(missing(co2_data)) {
    all_data <- no_co2
  }

  # Save to csv
  if(!missing(output_file)){
    write_csv(all_data, file = output_file)
  }

  return(all_data)
}
