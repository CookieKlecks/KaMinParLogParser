# Using chdir=T is important to temporary switch working directory, such that
# relative locations in the sourced script are correct.
source("../external_tools/experimental_plot_scripts/functions.R", chdir=T)
source("utility.R")




create_performance_profile_plot <- function(experiment_dir, 
                                            plot_file_name, 
                                            timelimit = 7200) {
  # read data
  dataframes <- read_and_aggregate_csv(experiment_dir, timelimit)
  
  # Specify Colors of Algorithms in Plots
  if (length(dataframes) <= 9) {
    algo_color_mapping <<- brewer.pal(n = length(dataframes), name = "Set1")
  } else {
    algo_color_mapping <<- brewer.pal(n = length(dataframes), name = "Set3")
  }
  
  # draw performance profile plot (print is necessary to actual output to pdf file)
  performace_plot(dataframes, 
                  objective = "avg_km1", 
                  hide_y_axis_title = F,
                  show_infeasible_tick = T,
                  show_timeout_tick = T,
                  widths = c(3,2,1,1),
                  latex_export = F,
                  small_size = F)
  
  ggsave(plot_file_name, path=experiment_dir, width = 22, height = 15, unit = "cm")
}


#create_performance_profile_plot("C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/Cluster_2022-01-31_0", "performance_plot.pdf")
