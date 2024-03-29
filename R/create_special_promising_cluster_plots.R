library(purrr)
library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(RColorBrewer)
library(tidyr)

source("utility.R")

get_cluster_ids <- function(clusters) {
  num_cluster <- length(strsplit(clusters, ';')[[1]])
  return(toString(seq(1, num_cluster)))
}

aggregate_promising_cluster_data <- function(df) data.frame(
  solver_runtime = mean(df$solver_runtime),
  gains = mean(df$gains),
  cluster_sizes = round(mean(df$cluster_sizes))
)

transform_special_cluster_data <- function(complete_data,
                                           average_over_seeds = T) {
  # create special id for each cluster to later aggregate same cluster
  complete_data$cluster_id <- sapply(complete_data$cluster_sizes, get_cluster_ids)
  # split ';' separated gains into several rows (using tidyr)
  split_data <- separate_rows(complete_data, solver_runtime, gains, cluster_sizes, cluster_id, convert = T)
  if (average_over_seeds) {
    # aggregate same cluster from different seeds
    split_data <- ddply(split_data,
                        c("algorithm", "graph", "k", "epsilon", "cluster_id", "solver_runtime_limit"),
                        aggregate_promising_cluster_data)
  }
  # round every solver time down to the maximum time
  split_data <- split_data %>% mutate(solver_runtime = ifelse(solver_runtime > solver_runtime_limit, solver_runtime_limit, solver_runtime))
  # only use base name of the graph (strips the path)
  split_data$graph <- sapply(split_data$graph, basename)
  
  return(split_data)
} 


get_custom_theme <- function() {
  return(theme(
    legend.position = 'none',
    strip.text.y = element_text(size = 8, margin = margin(2,2,2,2)),
    strip.background.y = element_rect()
  )
  )
}



#' Title
#'
#' @param experiment_dir directory where the input data can be found.
#' @param output_dir directory where the plot should be saved
#' @param plot_file_name the name of the created plot (with or without suffix)
#' @param pdf_export whether the plot should be exported to pdf
#' @param latex_export whether the plot should be exported to latex 
#' @param filter_data allows manipulating the data directly before plotting.
#'            The function is called with the whole data frame and should return
#'            the data frame that will be used as input for ggplot.
#'
#' @return
#' @export
#'
#' @examples
create_cluster_size_histogram <- function(experiment_dir,
                                          output_dir,
                                          plot_file_name,
                                          pdf_export = T,
                                          latex_export = F,
                                          filter_data = identity,
                                          height = 3,
                                          width = 6.5) {
  complete_data <- read_csv_into_df(experiment_dir) %>%
    dplyr::select(algorithm, graph, k, epsilon, gains, cluster_sizes, solver_runtime, solver_runtime_limit)
  # remove rows were no gains data is given (e.g. label propagation)
  complete_data <- na.omit(complete_data, cols=c("gains", "cluster_sizes", "solver_runtime"))
  if (length(row.names(complete_data)) == 0 || !("cluster_sizes" %in% colnames(complete_data))) {
    warning(paste("Skipped creating cluster size histogram for", basename(experiment_dir), ", because no special promising cluster data exists."))
    return()
  }
  
  split_data <- transform_special_cluster_data(complete_data, average_over_seeds = F)
  
  # =========================== CREATE PLOT ===================================
  filtered_data <- filter_data(split_data)
  plot <- ggplot(filtered_data, aes(x=cluster_sizes, fill=graph)) +
    facet_grid(cols = vars(algorithm), rows = vars(graph)) +
    geom_histogram(aes(y=after_stat(density)), binwidth = 1) +
    scale_y_continuous(limits = c(0,1)) + 
    labs(x="Cluster Size", y = "Relative Frequency")
  
  algo_count <- length(unique(filtered_data$algorithm))
  graph_count <- length(unique(filtered_data$graph))
  h <- height * graph_count
  w <- width * algo_count
  
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = plot_file_name,
    width = w,
    height = h,
    pdf_export = pdf_export,
    latex_export = latex_export,
    custom_theme = get_custom_theme()
  )
  
  return(plot)
}



create_gains_per_cluster_scatter_plot <- function(experiment_dir,
                                                  output_dir,
                                                  plot_file_name,
                                                  override) {
  complete_data <- read_csv_into_df(experiment_dir) %>%
    dplyr::select(algorithm, graph, k, epsilon, gains, cluster_sizes, solver_runtime, solver_runtime_limit)
  # remove rows were no gains data is given (e.g. label propagation)
  complete_data <- na.omit(complete_data, cols=c("gains", "cluster_sizes", "solver_runtime"))
  if (length(row.names(complete_data)) == 0 || !("cluster_sizes" %in% colnames(complete_data))) {
    warning(paste("Skipped creating gains-per-cluster scatter plot for", basename(experiment_dir), ", because no special promising cluster data exists."))
    return()
  }
  
  # =========================== CREATE PLOT ===================================
  
  algorithms <- unique(complete_data$algorithm)
  graph_count <- length(unique(complete_data$graph))
  
  # create total plot for all algorithms
  if (override || !file.exists(paste(output_dir, plot_file_name, sep = "/"))) {
    cur_data <- transform_special_cluster_data(complete_data)
    
    ggplot(cur_data, aes(x=cluster_sizes, y=gains)) +
      facet_grid(cols = vars(algorithm), rows = vars(graph), scales="free", space = "free_x") +
      scale_color_gradient(low = "green", high = "red", limits = c(0, max(cur_data$solver_runtime_limit))) +
      geom_jitter(aes(color = solver_runtime), pch = 20) +
      theme_bw(base_size = 10) # white background
    
    algo_count <- length(unique(cur_data$algorithm))
    ggsave(plot_file_name, path=output_dir, width = 15 * algo_count + 5, height = 5 * graph_count, unit = "cm")
    print(paste("      |___ CREATED gains-per-cluster scatter plot for all algorithms"), quote = F)
  } else {
    print(paste("      |___ skip creating gains-per-cluster scatter plot for all algorithms because it ALREADY EXISTS."), quote = F)
  }
  
  
  # create single plot per algorithm
  for (cur_algo in algorithms) {
    cur_filename <- paste(cur_algo, plot_file_name, sep = "_")
    
    if (override || !file.exists(paste(output_dir, cur_filename, sep = "/"))) {
      cur_data <- complete_data %>% filter(algorithm == cur_algo)
      cur_data <- transform_special_cluster_data(cur_data)
      
      ggplot(cur_data, aes(x=cluster_sizes, y=gains)) +
        facet_grid(rows = vars(graph), scales="free", space = "free_x") +
        scale_color_gradient(low = "green", high = "red", limits = c(0, max(cur_data$solver_runtime_limit))) +
        geom_jitter(aes(color = solver_runtime), pch = 20) +
        labs(title = cur_algo) +
        theme_bw(base_size = 10) # white background
      
      ggsave(cur_filename, path=output_dir, width = 42, height = 5 * graph_count, unit = "cm")
      print(paste("      |___ CREATED gains-per-cluster scatter plot for", cur_algo), quote = F)
    } else {
      print(paste("      |___ skip creating gains-per-cluster scatter plot for", cur_algo, "because it ALREADY EXISTS."), quote = F)
    }
  }
}

create_avg_gain_per_cluster_plot <- function(experiment_dir,
                                             output_dir,
                                             plot_file_name,
                                             pdf_export = T,
                                             latex_export = F,
                                             filter_data = identity,
                                             height = 3,
                                             width = 6.5) {
  complete_data <- read_csv_into_df(experiment_dir) %>%
    # only select necessary columns, as too many resulted in no rows after na.omit
    dplyr::select(algorithm, graph, k, epsilon, gains, cluster_sizes, solver_runtime, solver_runtime_limit)
  # remove rows were no gains data is given (e.g. label propagation)
  complete_data <- na.omit(complete_data, cols=c("gains", "cluster_sizes", "solver_runtime"))
  if (length(row.names(complete_data)) == 0 || !("cluster_sizes" %in% colnames(complete_data))) {
    warning(paste("Skipped creating average gain per cluster plot for", basename(experiment_dir), ", because no special promising cluster data exists."))
    return()
  }
  
  split_data <- transform_special_cluster_data(complete_data, average_over_seeds = F)
  
  calc_avg_gain <- function(df) data.frame(avg_gain = mean(df$gains))
  #split_data <- ddply(split_data, c("algorithm", "graph", "cluster_sizes"), calc_avg_gain)
  
  
  aggregated_data <- ddply(split_data, c("algorithm", "graph", "cluster_sizes"), function(df) data.frame(
    sum_gain = sum(df$gain)
  ))
  total_gains <- ddply(split_data, .(algorithm, graph), function(df) data.frame(
    total_gain = sum(df$gain)
  ))
  
  joined_data <- join(aggregated_data, total_gains, by = c("algorithm", "graph"))
  
  # =========================== CREATE PLOT ===================================
  filtered_data <- filter_data(joined_data)
  
  plot <- ggplot(filtered_data, aes(x = cluster_sizes, y = sum_gain / total_gain)) +
    facet_grid(cols = vars(algorithm), rows = vars(graph)) +
    geom_col(aes(fill = graph)) +
    scale_y_continuous(limits = c(0,1)) + 
    labs(x="Cluster Size", y = "Relative Gain")
  
  algo_count <- length(unique(filtered_data$algorithm))
  graph_count <- length(unique(filtered_data$graph))
  h <- height * graph_count
  w <- width * algo_count
  
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = plot_file_name,
    width = w,
    height = h,
    pdf_export = pdf_export,
    latex_export = latex_export,
    custom_theme = get_custom_theme()
  )
  
  return(plot)
}



create_avg_runtime_per_cluster_plot <- function(experiment_dir,
                                                output_dir,
                                                plot_file_name) {
  complete_data <- read_csv_into_df(experiment_dir) %>%
    # only select necessary columns, as too many resulted in no rows after na.omit
    dplyr::select(algorithm, graph, k, epsilon, gains, cluster_sizes, solver_runtime, solver_runtime_limit)
  # remove rows were no gains data is given (e.g. label propagation)
  complete_data <- na.omit(complete_data, cols=c("gains", "cluster_sizes", "solver_runtime"))
  if (length(row.names(complete_data)) == 0 || !("cluster_sizes" %in% colnames(complete_data))) {
    warning(paste("Skipped creating average runtime per cluster plot for", basename(experiment_dir), ", because no special promising cluster data exists."))
    return()
  }
  
  split_data <- transform_special_cluster_data(complete_data, average_over_seeds = F)
  
  calc_avg_runtime <- function(df) data.frame(avg_runtime = mean(df$solver_runtime))
  split_data <- ddply(split_data, c("algorithm", "graph", "cluster_sizes"), calc_avg_runtime)
  
  # =========================== CREATE PLOT ===================================
  
  ggplot(split_data, aes(x = cluster_sizes, y = avg_runtime)) +
    facet_grid(cols = vars(algorithm), rows = vars(graph), scales="free", space = "free_x") +
    geom_col(aes(fill = graph))
  
  algo_count <- length(unique(split_data$algorithm))
  graph_count <- length(unique(split_data$graph))
  ggsave(plot_file_name, path=output_dir, width = 10 * algo_count + 5, height = 5 * graph_count, unit = "cm", limitsize = F)
}


# experiment_dir <- "C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/Cluster_Stats_2022-02-24_0"

#("C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/Cluster_Stats_2022-02-24_0", "test_scatter.pdf", T)


#create_gains_per_cluster_scatter_plot("C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/001eps-Cluster_best", "test_scatter.pdf", T)

#create_cluster_size_histogram("C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/Cluster_Stats_2022-02-24_0", "test_histo.pdf")

#create_avg_gain_per_cluster_plot(experiment_dir, "C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/Cluster_Stats_2022-02-24_0/plots", "test_avg_gain.pdf")
