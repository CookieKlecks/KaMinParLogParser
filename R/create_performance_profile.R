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
#' @param only_first_panel special flag to only show the first panel of the plot.
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
                                            small_font = F,
                                            only_first_panel = F,
                                            only_first_and_second_panel = F,
                                            custom_color_mapping = NULL,
                                            filter_data = identity) {
  # read data
  dataframes <- read_and_aggregate_csv(experiment_dir, timelimit, filter_data)
  
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
  
  if(only_first_panel) {
    x_breaks = c(1, 1.02, 1.04, 1.06, 1.08, 1.1)
    x_labels = c(1, 1.02, 1.04, 1.06, 1.08, 1.1)
    x_limit = widths[[1]] # the first panel goes until x = widths[[1]]
    
    # taken from performance_plot function:
    plot <- plot + scale_x_continuous(
      breaks = as.numeric(lapply(x_breaks, FUN = plotted_tau, 100, widths)), # worst ratio is irrelevant as max x-break = 1.1
      labels = x_labels,
      expand = c(0, 0), limits=c(-0.02,x_limit)) +
      geom_vline(aes(xintercept=plotted_tau(1.05, 100, widths)), colour="grey", linetype="11", size=.5)  # add this special line for the now missing break x = 1.05
  }
  
  if(only_first_and_second_panel) {
    x_breaks = c(1, 1.02, 1.04, 1.06, 1.08, 1.1, 1.5)
    x_labels = c(1, 1.02, 1.04, 1.06, 1.08, 1.1, 1.5)
    x_limit = widths[[1]] + widths[[2]] # the first panel goes until x = widths[[1]]
    
    # taken from performance_plot function:
    plot <- plot + scale_x_continuous(
      breaks = as.numeric(lapply(x_breaks, FUN = plotted_tau, 100, widths)), # worst ratio is irrelevant as max x-break <= 1.5
      labels = x_labels,
      expand = c(0, 0), limits=c(-0.02,x_limit)) +
      geom_vline(aes(xintercept=plotted_tau(1.05, 100, widths)), colour="grey", linetype="11", size=.5)  # add this special line for the now missing break x = 1.05
  }
  
  custom_theme <- NULL
  if(small_font) {
    custom_theme <- theme(legend.text = element_text(size = legend_text_size(T, T)))
  }
  
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = plot_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    add_default_theme = F,
    custom_theme = custom_theme
  )
  
  return(plot)
}


#create_performance_profile_plot("C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/Cluster_2022-01-31_0", "performance_plot.pdf")
