# This function randomly assigns jars to treatment levels, saves
# them out, and creates schedules for what to sample and when.

# This function accepts the following parameters:
# A group_name from which to draw from, group_num for how many groups to create,
# num_choose for how many in each group should be drawn, and already_chosen
# which is a dataframe that contains numbers that have already been used/drawn.

# Sarah Gao
# February 8, 2022
# hellosarahgao@gmail.com

# Load libraries
library("dplyr")

assign_jars <- function(group_name, group_num, num_choose, already_chosen) {
  already_chosen <- already_chosen %>%
    select(sample_no)
  assignments <- data.frame(sample_no = NA, group_no = NA)
  assignment_group <- 0
  for (group in 1:group_num) {
    assignment_group <- assignment_group + 1
    chosen_jars <- sample_n(subset(group_name, !(group_name$sample_no %in% already_chosen$sample_no)),
                            num_choose, replace = FALSE)
    group_assign <- cbind(chosen_jars, group_no = assignment_group)
    assignments <- rbind(group_assign, assignments)
    already_chosen <- rbind(already_chosen, chosen_jars)
  }
  return(assignments)
}
