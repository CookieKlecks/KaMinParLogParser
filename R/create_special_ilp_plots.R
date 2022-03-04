library(purrr)
library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(RColorBrewer)
library(tidyr)

source("utility.R")


aggregate_timeout_data <- function(df) data.frame(
  avg_solver_timed_out = mean(df$solver_timed_out, na.rm = TRUE)
)


create_timed_out_ilp_plot <- function(experiment_dir, 
                                      plot_file_name) {
  # =========================== READ DATA =====================================
  
  # TODO: use utility function
  
  # List all .csv files in experiment dir
  result_files <- list.files(path=experiment_dir, pattern="*.csv", full.names = T)
  if (length(result_files) == 0) {
    stop(paste("No .csv result files found in", experiment_dir))
  }
  
  # import these .csv files into one single data frame
  combined_data <- data.frame()
  for (result in result_files) {
    raw_data <- read.csv(result, header = TRUE)
    if (!"solver_timed_out" %in% colnames(raw_data)) {
      # this avoids warnings when averaging the solver_timed_out column
      next
    }
    raw_data <- na.omit(raw_data, cols="solver_timed_out")
    aggregated_data <- ddply(raw_data, 
                             c("algorithm", "graph", "k", "epsilon",  "num_threads", "max_non_zeroes"), 
                             aggregate_timeout_data)
    combined_data <- rbind(combined_data, aggregated_data)
  }
  
  # remove rows with no value. This avoids visualizing algorithms that have not
  # logged the time outs of a solver (e.g. label propagation)
  combined_data <- na.omit(combined_data, cols="avg_solver_timed_out")
  
  if (length(row.names(combined_data)) == 0) {
    warning(paste("Skipped creating timed out ILP plot for", basename(experiment_dir), ",because no ILP timeout data exists."))
    return()
  }
  
  # =========================== CREATE PLOT ===================================
  
  # create color pallet
  algo_color_mapping <<- brewer.pal(n = 9, name = "Set1")
  
  # create a labeller, that beautifies the labels for each facet in the plot
  max_non_zeroes_labeller <- function(raw_labels) {
    prettify_label <- function(cur_label) paste("Max non-zeroes:", prettyNum(cur_label, big.mark = ","))
    parsed_labels <- lapply(raw_labels, prettify_label)
    return(parsed_labels)
  }
  
  # create plot
  ggplot(combined_data, aes(x=algorithm, y=avg_solver_timed_out)) + # load data
    facet_grid(cols = vars(max_non_zeroes), scales = "free_x", space = "free_x", labeller = as_labeller(max_non_zeroes_labeller)) + # facet the plot according to the max non-zeroes
    geom_jitter(aes(fill=algorithm), alpha = 0.33, shape = 21, width = 0.3) + # add scattered points
    stat_boxplot(aes(color = algorithm), geom ='errorbar', width = 0.6) + # add top/bottom bar
    geom_boxplot(aes(color=algorithm), outlier.shape = NA, alpha = 0.5) + # add average and box
    scale_y_continuous(limits = c(-0.01, 1.0)) + # scale y from 0 to 1 (use -0.01 as lower bound to avoid the removal from values with 0.0 due to floating point inaccuracy)
    theme_bw(base_size = 10) + # white background
    scale_color_manual(values=algo_color_mapping, drop = F) + # use own color mappings
    scale_fill_manual(values=algo_color_mapping, drop = F) + # use own color mappings for fill
    labs(x = "", y = "Percentage of ILP time limits") + # set text for x- and y-axis
    theme(legend.position = "none") # remove legend
  
  ggsave(plot_file_name, path=experiment_dir, width = 22, height = 15, unit = "cm")
}


create_gains_density_plot <- function(experiment_dir,
                                      plot_file_name) {
  complete_data <- read_csv_into_df(experiment_dir)
  
  # remove rows were no gains data is given (e.g. label propagation)
  complete_data <- na.omit(complete_data, cols="gains")
  
  if (length(row.names(complete_data)) == 0) {
    warning(paste("Skipped creating gains density plot for", basename(experiment_dir), ", because no gains data exists."))
    return()
  }
  
  # split ';' separated gains into several rows (using tidyr)
  split_data <- separate_rows(complete_data, gains, sep = ";", convert = T)
  
  # only use base name of the graph (strips the path)
  split_data$graph <- sapply(split_data$graph, basename)
  
  
  # =========================== CREATE PLOT ===================================
  
  ggplot(split_data, aes(x=gains)) +
    facet_wrap(facets=vars(graph), scales = "free") +
    geom_density(aes(color=algorithm))
  
  ggsave(plot_file_name, path=experiment_dir, width = 42, height = 15, unit = "cm")
}


#experiment_dir <- "C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/001eps-Cluster_no_min_gain_2022-02-12_0"

#create_timed_out_ilp_plot(experiment_dir, "test.pdf")
#create_gains_density_plot(experiment_dir, "test.pdf")
