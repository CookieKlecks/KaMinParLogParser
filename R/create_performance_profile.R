base_wd <- getwd()

# We need to change the working directory to the base of the plot module, because
# in functions.R are source calls to relative locations inside the module.
setwd("../external_tools/experimental_plot_scripts")
lib_wd <- getwd()
source("functions.R")

setwd(base_wd)


create_performance_profile_plot <- function(experiment_dir, 
                                            plot_file_name, 
                                            timelimit = 7200, 
                                            epsilon = 0.03) {
  # List all .csv files in experiment_dir
  result_files <- list.files(path=experiment_dir, pattern="*.csv", full.names = T)
  
  if (length(result_files) == 0) {
    stop(paste("No .csv result files found in ", experiment_dir))
  }
  
  # import these .csv files into one dataframe each
  dataframes <- vector("list", length(result_files))
  i <- 1
  for (result in result_files) {
    dataframes[[i]] <- aggreg_data(read.csv(result, header = TRUE), 
                                   timelimit = timelimit, 
                                   epsilon = epsilon,
                                   seeds = .Machine$integer.max)
    i <- i + 1
  }
  
  # Specify Colors of Algorithms in Plots
  algo_color_mapping <<- brewer.pal(n = 9, name = "Set1")
  
  # draw performance profile plot (print is necessary to actual output to pdf file)
  performace_plot(dataframes, 
                  objective = "avg_km1", 
                  hide_y_axis_title = F,
                  show_infeasible_tick = F,
                  show_timeout_tick = F,
                  widths = c(3,2,1,1),
                  latex_export = F,
                  small_size = F)
  
  ggsave(plot_file_name, path=experiment_dir, width = 22, height = 15, unit = "cm")
}


#create_performance_profile_plot("C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/results/2022-01-16_2", "performance_plot.pdf")
