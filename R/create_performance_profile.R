# Using chdir=T is important to temporary switch working directory, such that
# relative locations in the sourced script are correct.
source("../external_tools/experimental_plot_scripts/functions.R", chdir=T)
source("utility.R")



#' Creates the performance profile plot for a single experiment
#' 
#' This function reads all .csv files in one directory and uses this data to
#' create a performance profile plot. It uses the experimental_plot_scripts
#' external library and its function performance_plot.
#' 
#' The result is saved in the directory of the experiment with the given name.
#'
#' @param experiment_dir path to the directory where all .csv-files of the
#' experiment are located.
#' @param output_dir directory where the resulting files should be saved.
#' @param plot_file_name file name of the resulting plot.
#' @param timelimit the time limit that should be used to identify time outs.
#' @param pdf_export whether the plot should be exported to pdf
#' @param latex_export whether the plot should be exported to latex 
#'
#' @return nothing. The plot is saved in the experiment_dir with the given
#' plot_file_name.
#'
#' @examples
#' Let 'experiment' be a directory in the current working directory with
#' following content:
#' ./experiment
#'    |___ method_1.csv
#'    |___ method_2.csv
#'    |___ method_3.csv
#'     
#' To create the performance profile to compare method 1 to 3:
#' 
#' create_performance_profile_plot("./experiment", "performance_profile.pdf")
#' 
#' The resulting plot is saved in the file:
#' ./experiment/performance_profile.pdf
#' 
create_performance_profile_plot <- function(experiment_dir, 
                                            output_dir,
                                            plot_file_name, 
                                            title = NULL,
                                            width = 22,
                                            height = 15,
                                            timelimit = 7200,
                                            pdf_export = T,
                                            latex_export = F,
                                            show_infeasible_tick = T,
                                            show_timeout_tick = T,
                                            widths = c(3,2,1,1),
                                            small_size = F,
                                            custom_color_mapping = NULL,
                                            filter_data = identity) {
  # read data
  dataframes <- read_and_aggregate_csv(experiment_dir, timelimit)
  
  # custom manipulation/filter of data:
  filtered_list <- vector("list", 0)
  for (df in dataframes) {
    filtered_df <- filter_data(df)
    if(nrow(filtered_df) > 0) { # only add data frame, if non empty
      tmp_list <- vector("list", 1)
      tmp_list[[1]] <- filtered_df
      filtered_list <- c(filtered_list, tmp_list)
    }
  }
  dataframes <- filtered_list
  
  # Specify Colors of Algorithms in Plots
  if (is.null(custom_color_mapping)) {
    if (length(dataframes) <= 9) {
      algo_color_mapping <<- brewer.pal(n = length(dataframes), name = "Set1")
    } else {
      algo_color_mapping <<- brewer.pal(n = length(dataframes), name = "Set3")
    }
  } else {
    algo_color_mapping <<- custom_color_mapping
  }
  
  # draw performance profile plot (print is necessary to actual output to pdf file)
  plot <- performace_plot(dataframes, 
                  objective = "avg_km1", 
                  title = title,
                  hide_y_axis_title = F,
                  show_infeasible_tick = show_infeasible_tick,
                  show_timeout_tick = show_timeout_tick,
                  widths = widths,
                  latex_export = latex_export,
                  small_size = small_size)
  
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = plot_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    add_default_theme = F
  )
  
  return(plot)
}


#create_performance_profile_plot("C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/Cluster_2022-01-31_0", "performance_plot.pdf")
