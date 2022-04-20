library(purrr)
library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(RColorBrewer)
library(tidyr)

source("utility.R")


create_solver_time_distribution <- function(experiment_dir,
                                            output_dir,
                                            plot_file_name) {
  complete_df <- read_csv_into_df(experiment_dir = experiment_dir) %>%
    dplyr::select(algorithm, graph, k, epsilon, gains, solver_time_presolve, solver_time_simplex, solver_time_mip)
  
  
  solver_time_data <- complete_df %>%
    drop_na(solver_time_presolve, solver_time_mip) %>% # remove runs without ILP
    pivot_longer(cols = c(solver_time_presolve, solver_time_mip), 
                 names_to = "step", 
                 names_prefix = "solver_time_",
                 values_to = "time",
                 values_drop_na = T) # Merge the columns for the solver step times in one
  
  if (length(row.names(solver_time_data)) == 0) {
    return()
  }
  
  ggplot(solver_time_data, aes(x = step, y = time)) +
    facet_wrap(vars(algorithm), scales = "free_x") + # facet the plot according to the max non-zeroes
    geom_jitter(aes(fill=algorithm), alpha = 0.33, shape = 21, width = 0.3) + # add scattered points
    stat_boxplot(aes(color = algorithm), geom ='errorbar', width = 0.6) + # add top/bottom bar
    geom_boxplot(aes(color=algorithm), outlier.shape = NA, alpha = 0.5) + # add average and box
    theme(legend.position = "none") # remove legend
  
  algo_count <- length(unique(solver_time_data$algorithm))
  ggsave(plot_file_name, path=output_dir, width = 10 * algo_count + 5, height = 10, unit = "cm", limitsize = F)
}




create_node_count_per_gain <- function(experiment_dir,
                                            output_dir,
                                            plot_file_name) {
  complete_df <- read_csv_into_df(experiment_dir = experiment_dir) %>%
    dplyr::select(algorithm, graph, k, epsilon, gains, solver_node_count)
  
  solver_data <- complete_df %>%
    drop_na(solver_node_count) %>%
    separate_rows(solver_node_count, gains, sep = ";", convert = T)
  
  if (length(row.names(solver_data)) == 0) {
    return()
  }
  
  solver_gain_data <- ddply(solver_data %>% mutate(gains = round(gains / 5)),
                            c("gains", "graph", "algorithm"),
                            function(df) data.frame(
                              gains_count = length(df$gains),
                              avg_node_count = mean(df$solver_node_count)
                            )) %>% mutate(gains = gains * 5)
  
  
  ggplot(solver_gain_data, aes(x = gains, y = avg_node_count)) +
    facet_wrap(vars(algorithm), scales = "free_x") + # facet the plot according to the max non-zeroes
    geom_point(aes(color = graph), pch = 20) + 
    scale_alpha_continuous(trans = "pseudo_log", range = c(0.25, 1))
  
  
  algo_count <- length(unique(solver_data$algorithm))
  ggsave(plot_file_name, path=output_dir, width = 10 * algo_count + 5, height = 10, unit = "cm", limitsize = F)
}


create_ilp_solver_plots <- function(experiment_dir,
                                    output_dir,
                                    override = F) {
  file_name <- "solver_time_distribution.pdf"
  if(!file.exists(paste(output_dir, file_name, sep = "/")) || override) {
    create_solver_time_distribution(experiment_dir, output_dir, file_name)
    print(" |___ created SOLVER TIME DISTRIBUTION plot", quote = F)
  }
  
  
  file_name <- "solver_node_count_per_gain.pdf"
  if(!file.exists(paste(output_dir, file_name, sep = "/")) || override) {
    create_node_count_per_gain(experiment_dir, output_dir, file_name)
    print(" |___ created SOLVER NODE COUNT PER GAIN plot", quote = F)
  }
}