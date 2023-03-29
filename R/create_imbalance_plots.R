source("utility.R")



#' Creates a plot of the imbalances of a solution
#'
#' @param data a dataframe that contains the plotted data
#' @param output_dir directory where the resulting files should be saved.
#' @param plot_file_name file name of the resulting plot.
#' @param timelimit the time limit that should be used to identify time outs.
#' @param pdf_export whether the plot should be exported to pdf
#' @param latex_export whether the plot should be exported to latex 
#'
#' @return nothing. The plot is saved in the output_dir with the given
#' plot_file_name.
create_imbalance_plot <- function(data, 
                                  output_dir,
                                  plot_file_name, 
                                  title = NULL,
                                  width = 22,
                                  height = 15,
                                  timelimit = 7200,
                                  pdf_export = T,
                                  latex_export = F,
                                  custom_color_mapping = NULL) {
  
  # Specify Colors of Algorithms in Plots
  if (is.null(custom_color_mapping)) {
    #if (length(dataframes) <= 9) {
      algo_color_mapping <<- brewer.pal(n = 9, name = "Set1")
    #} else {
      algo_color_mapping <<- brewer.pal(n = 12, name = "Set3")
    #}
  } else {
    algo_color_mapping <<- custom_color_mapping
  }
  
  custom_theme <- create_theme(latex_export, legend_position = "right", x_axis_text_angle = 45, x_axis_text_hjust = 1) +
    theme(strip.placement = "inside", strip.text = element_text(), strip.background = element_rect())

  plot <- ggplot(data, aes(x = graph, y = avg_imbalance, fill = algorithm)) +
    geom_col() +
    facet_grid(cols = vars(k), rows = vars(algorithm)) +
    #scale_y_continuous(limits = c(-0.01, 1.0)) + # scale y from 0 to 1 (use -0.01 as lower bound to avoid the removal from values with 0.0 due to floating point inaccuracy)
    theme_bw(base_size = 10) # white background
  
  
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = plot_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    custom_theme = custom_theme
  )
  
  return(plot)
}


#test_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaMinPar\\Constrained-V-Cycle"
#test_csv <- "kaminpar_constr-v-cycle-[KaHIP]_lazy-ilp_cutIncrease0.01_pairwise-lp__eps0__contractionMaxWeight0.01__2023-03-14_01-50-48.csv"

#test_data <- read.csv(paste(test_dir, test_csv, sep = "\\"), header = T)
#test_data <- aggreg_data(test_data, 70000, 0)
#test_data$algorithm <- "KaMinPar"
#test_data$graph <- sapply(test_data$graph, function(graph) {return(sub('\\..*$', '', basename(graph)))})
#create_imbalance_plot(test_data, "", "")
