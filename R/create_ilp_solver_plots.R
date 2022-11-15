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
                                            plot_file_name,
                                            width = NULL,
                                            height = 10,
                                            pdf_export = T,
                                            latex_export = F,
                                            small_size = F,
                                            breaks = NULL,
                                            labels = NULL,
                                            custom_color_mapping = NULL,
                                            filter_data = identity) {
  complete_df <- read_csv_into_df(experiment_dir = experiment_dir, filter_data = filter_data) 
  complete_df <- complete_df %>%
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
  
  mip_step_name <- "Branch-and-Bound"
  presolve_step_name <- "Presolve"
  solver_time_data$step <- sapply(solver_time_data$step, function(step) {
    if(step == "mip") {
      return(mip_step_name)
    }
    if(step == "presolve") {
      return(presolve_step_name)
    }
    return(step)
  })
  
  palette <- brewer.pal(n = 9, name = "Set1")
  color_mapping_solver_time_distribution <- c(
    "Branch-and-Bound" = palette[[1]],
    "Presolve" = palette[[2]]
  )
  
  plot <- ggplot(solver_time_data, aes(x = step, y = time, color = step)) +
    facet_wrap(vars(algorithm), scales = "free_x") + # facet the plot according to the max non-zeroes
    geom_jitter(alpha = 0.33, shape = 21, width = 0.3) + # add scattered points
    stat_boxplot(geom ='errorbar', width = 0.6) + # add top/bottom bar
    geom_boxplot(outlier.shape = NA, alpha = 0.5) + # add average and box
    scale_y_continuous(trans = "pseudo_log") +
    labs(x = "", y = "Running Time [s]")
  
  
  if(!is.null(custom_color_mapping)) {
    plot <- plot + scale_color_discrete(custom_color_mapping)
  } else {
    plot <- plot + scale_color_discrete(color_mapping_solver_time_distribution)
  }
  
  # set custom breaks
  if(!is.null(breaks)) {
    plot <- plot + scale_y_continuous(breaks = breaks, trans = "pseudo_log", labels = labels)
  }
  
  # set default values for width
  algo_count <- length(unique(solver_time_data$algorithm))
  if(is.null(width)) {
    width <- 10 * algo_count + 5
  }
  
  save_ggplot(plot, output_dir, plot_file_name, width = width, height = height,
              pdf_export = pdf_export, latex_export = latex_export,
              small_size = small_size,
              custom_theme = theme(legend.position = "none"))
  
  return(plot)
}




create_node_count_per_gain <- function(experiment_dir,
                                       output_dir,
                                       plot_file_name,
                                       width = NULL,
                                       height = 10,
                                       pdf_export = T,
                                       latex_export = F,
                                       small_size = F,
                                       hide_legend = F,
                                       filter_data = identity) {
  complete_df <- read_csv_into_df(experiment_dir = experiment_dir, filter_data = filter_data) %>%
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


  plot <- ggplot(solver_gain_data, aes(x = gains, y = avg_node_count)) +
    facet_wrap(vars(algorithm), scales = "free_x") + # facet the plot according to the max non-zeroes
    geom_point(aes(color = graph), pch = 20) +
    scale_x_continuous(trans = "pseudo_log") +
    scale_alpha_continuous(trans = "pseudo_log", range = c(0.25, 1)) +
    labs(x = "Gain", y = "Explored Branch-and-Bound Nodes")
  
  # total_solver_runs = nrow(solver_data)
  # total_gain = sum(solver_data$gains)
  # 
  # solver_data_aggregated <- ddply(solver_data, .(algorithm, graph, solver_node_count), function(df) data.frame(
  #   density_solver_node_count = nrow(df) / total_solver_runs,
  #   density_gain = sum(df$gains) / total_gain
  # ))
  # 
  # palette <- brewer.pal(n = 9, name = "Set1")
  # color_node_count <- palette[[1]]
  # 
  # plot <- ggplot(solver_data_aggregated, aes(x = solver_node_count)) +
  #   #geom_point(aes(y=density_solver_node_count)) +
  #   geom_point(aes(y=density_gain)) +
  #   scale_x_continuous(trans = "log2")
  
  
  # set default values for width
  algo_count <- length(unique(solver_data$algorithm))
  if(is.null(width)) {
    width <- 10 * algo_count + 5
  }
  
  custom_theme = NULL
  if(hide_legend) {
    custom_theme <- theme(legend.position = "none")
    plot <- plot + theme(legend.position = "none")
  }
  
  save_ggplot(plot, output_dir, plot_file_name, width = width, height = height,
              pdf_export = pdf_export, latex_export = latex_export,
              small_size = small_size, custom_theme = custom_theme)
  
  return(plot)
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