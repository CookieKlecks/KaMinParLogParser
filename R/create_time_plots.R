# Using chdir=T is important to temporary switch working directory, such that
# relative locations in the sourced script are correct.
source("../external_tools/experimental_plot_scripts/functions.R", chdir=T)
source("utility.R")



#' Creates a box plot of the running times.
#' 
#' This function reads and aggregates all .csv-files in the passed experiment_dir.
#' The total running times in the resulting data frames are visualized next to
#' each other in the form of a box plot.
#' 
#' The resulting plot is saved in the experiment_dir with the given
#' plot_file_name.
#' 
#' IMPORTANT:
#' It is assumed, that each .csv-file only contains data for exactly one
#' algorithm.
#'
#' @param experiment_dir path to the directory where the .csv-files are located.
#' @param plot_file_name name of the saved plot file.
#' @param timelimit limit for identifying time outs.
#'
#' @examples
#' Let 'experiment' be a directory in the current working directory with
#' following content:
#' ./experiment
#'    |___ method_1.csv
#'    |___ method_2.csv
#'    |___ method_3.csv
#'     
#' To create the running time plots to compare method 1 to 3:
#' 
#' create_running_time_box_plot("./experiment", "running_time.pdf")
#' 
#' The resulting plot is saved in the file:
#' ./experiment/running_time.pdf
#' 
create_running_time_box_plot <- function(experiment_dir,
                                         plot_file_name,
                                         output_dir = NULL,
                                         timelimit = 7200,
                                         width = 22,
                                         height = 15,
                                         pdf_export = T,
                                         latex_export = F,
                                         small_size = F,
                                         order = NULL,
                                         custom_color_mapping = NULL,
                                         filter_data = identity) {
  if(is.null(output_dir)) {
    output_dir = experiment_dir
  }
  
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
  
  if (is.null(order)) {
    # get first algorithm name of each data frame as ordering of the algorithms
    # it is assumed, that in each data frame, i.e. each .csv-file, is only data
    # of one algorithm.
    order <- sapply(sapply(dataframes, '[', 'algorithm'), '[', 1)
  }
  
  # draw time plot (print or ggsave is necessary to actual output to pdf file)
  plot <- running_time_box_plot(dataframes, 
                        show_infeasible_tick = T,
                        show_timeout_tick = T,
                        order = order,
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


#create_running_time_box_plot("C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/LP_vs_Random-pre-LP", "running_time_box.pdf")
