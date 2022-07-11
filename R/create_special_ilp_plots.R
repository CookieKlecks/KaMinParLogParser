library(purrr)
library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(RColorBrewer)
library(tidyr)
library(ggrepel)

source("utility.R")


aggregate_timeout_data <- function(df) data.frame(
  avg_solver_timed_out = mean(df$solver_timed_out, na.rm = TRUE)
)


create_timed_out_ilp_plot <- function(experiment_dir, 
                                      plot_file_name,
                                      output_dir = NULL,
                                      width = 22,
                                      height = 15,
                                      facet_non_zeroes = T,
                                      pdf_export = T,
                                      latex_export = F,
                                      small_size = F,
                                      custom_color_mapping = NULL,
                                      order_algorithms = NULL,
                                      filter_data = identity) {
  if(is.null(output_dir)) {
    output_dir <- experiment_dir
  }
  
  # =========================== READ DATA =====================================
  
  # TODO: use utility function
  
  # List all .csv files in experiment dir
  result_files <- list.files(path=experiment_dir, pattern=".csv", full.names = T)
  if (length(result_files) == 0) {
    stop(paste("No .csv result files found in", experiment_dir))
  }
  
  # import these .csv files into one single data frame
  combined_data <- data.frame()
  for (result in result_files) {
    raw_data <- read.csv(result, header = TRUE) %>%
      dplyr::select(algorithm, graph, k, epsilon, num_threads, max_non_zeroes, solver_timed_out)
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
  
  combined_data <- filter_data(combined_data)
  
  if (!is.null(order_algorithms)) {
    combined_data$algorithm <- factor(combined_data$algorithm, levels = order_algorithms)
  }
  
  if (length(row.names(combined_data)) == 0) {
    warning(paste("Skipped creating timed out ILP plot for", basename(experiment_dir), ",because no ILP timeout data exists."))
    return()
  }
  
  means <- stats::aggregate(avg_solver_timed_out ~ algorithm, data = combined_data, mean)
  
  # =========================== CREATE PLOT ===================================
  
  # create color pallet
  if (is.null(custom_color_mapping)) {
    algo_color_mapping <<- brewer.pal(n = 9, name = "Set1")
  } else {
    algo_color_mapping <<- custom_color_mapping
  }
  
  # create a labeller, that beautifies the labels for each facet in the plot
  max_non_zeroes_labeller <- function(raw_labels) {
    prettify_label <- function(cur_label) paste("Max non-zeroes:", prettyNum(cur_label, big.mark = ","))
    parsed_labels <- lapply(raw_labels, prettify_label)
    return(parsed_labels)
  }
  
  number_as_percentage = function(number) {
    value = round(number, 3) * 100
    if (latex_export) {
      return(paste(value, "\\%"))
    } else {
      return(paste(value, "%", sep = ""))
    }
  }
  
  # create plot
  p <- ggplot(combined_data, aes(x=algorithm, y=avg_solver_timed_out)) + # load data
    geom_jitter(aes(fill=algorithm), alpha = 0.33, shape = 21, width = 0.3, show.legend = F) + # add scattered points
    stat_boxplot(aes(color = algorithm), geom ='errorbar', width = 0.6, show.legend = F) + # add top/bottom bar
    geom_boxplot(aes(color=algorithm), outlier.shape = NA, alpha = 0.5, show.legend = F) + # add average and box
    geom_text(aes(x=algorithm, label=number_as_percentage(avg_solver_timed_out)), y = 0, data = means, size = plot_text_size(latex_export)) + # add the mean as text
    scale_y_continuous(limits = c(-0.01, 1.0)) + # scale y from 0 to 1 (use -0.01 as lower bound to avoid the removal from values with 0.0 due to floating point inaccuracy)
    theme_bw(base_size = 10) + # white background
    scale_color_manual(values=algo_color_mapping, drop = F) + # use own color mappings
    scale_fill_manual(values=algo_color_mapping, drop = F) + # use own color mappings for fill
    labs(x = "", y = "Percentage of ILP time limits") + # set text for x- and y-axis
    create_theme(latex_export = latex_export, legend_position = "none")
  
  if(facet_non_zeroes) {
    p <- p + facet_grid(cols = vars(max_non_zeroes), labeller = as_labeller(max_non_zeroes_labeller)) # facet the plot according to the max non-zeroes
  }
  
  save_ggplot(
    plot = p,
    output_dir = output_dir,
    filename = plot_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    small_size = small_size
  )
  
  return(p)
}


create_gains_density_plot <- function(experiment_dir,
                                      plot_file_name) {
  complete_data <- read_csv_into_df(experiment_dir) %>%
    dplyr::select(algorithm, graph, gains)
  
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




#' This function creates a plot, that shows the evolution of the currently best
#' found solution depending on the running time of each individual ILP run.
#' 
#' It is faceted on the different algorithms and different graphs.
#' Therefore for each algorithm-graph-pair one plot is created, that contains
#' for each individual ILP run one line. The end of each line represents the 
#' finally found solution and is marked with a diamond.
#' 
#' NOTE: For each instance only the runs with the minimum seed and maximum k is
#' used, as it is difficult to aggregate different runs, because it is not 
#' really possible to map individual ILP runs to each other.
#'
#' @param experiment_dir 
#' @param plot_file_name 
#' @param scatter_plot_file_name 
#'
#' @return
#' @export
#'
#' @examples
create_partial_gains_evolution_plot <- function(experiment_dir,
                                                plot_file_name,
                                                scatter_plot_file_name,
                                                n_th_improvment_file_name,
                                                output_dir = NULL,
                                                pdf_export = T,
                                                latex_export = F,
                                                filter_data = identity) {
  if(is.null(output_dir)) {
    output_dir <- experiment_dir
  }
  complete_data <- read_csv_into_df(experiment_dir) %>%
    dplyr::select(algorithm, graph, seed, k, partial_gains, partial_runtime, partial_objective, partial_ilp_id, gains, solver_runtime, solver_runtime_limit, ilp_id)
  
  # remove rows were no partial gains data is given (e.g. label propagation)
  complete_data <- na.omit(complete_data, cols=c("gains", "partial_gains"))
  
  if (length(row.names(complete_data)) == 0) {
    warning(paste("Skipped creating partial gains evolution plot for", basename(experiment_dir), ", because no gains data exists."))
    return()
  }
  
  complete_data <- filter_data(complete_data)
   
  # restrict to one seed and one k. This is to avoid over plotting.
  complete_data <- filter(complete_data, seed == min(seed))
  complete_data <- filter(complete_data, k == max(k))
  
  # split ';' separated partial gains into several rows (using tidyr)
  # Each row represents one partial solution.
  partial_points <- separate_rows(complete_data, partial_gains, partial_runtime,
                                  partial_objective, partial_ilp_id, 
                                  sep = ";", convert = T)
  
  # ignore balance objective solutions
  partial_points <- filter(partial_points, partial_objective == 1)
  partial_points$final <- F
  
  
  # Get the final solutions for each ILP run to draw each line until the solver 
  # actually stopped.
  final_points <- separate_rows(complete_data, gains, solver_runtime, ilp_id, 
                                sep = ";", convert = T)
  # set final points as partial points to easily handle them together
  final_points[c("partial_gains", "partial_runtime", "partial_ilp_id")] <- 
    final_points[c("gains", "solver_runtime", "ilp_id")]
  final_points$partial_objective <- 1 # to stay compliant with partial points
  final_points$final <- T
  
  
  all_points <- rbind(partial_points, final_points) %>%
    ddply(.(algorithm, graph, partial_ilp_id), function(df) {
      # there are always at least 2 points (first found solution and final solution) 
      # => points - 2 = number of actual improvements
      df$num_improvements <- nrow(df) - 2 
      return(df)
    })
  
  # ================= Custom aesthetic function ===============================
  #' This function calculates for each ilp_id of graph g and algorithm a 
  #' following value:
  #'     r := ilp_id / max_g_a(ilp_id)
  #'     
  #' where max_g_a(ilp_id) is the maximum ILP id of the calculations of graph g
  #' and algorithm a.
  #' This ratio is used to color the lines of each graph-algorithm-panel 
  #' indiviually.
  #'     
  get_color <- function(ilp_ids, graphs, algorithms) {
    max_ilp_ids <- ddply(all_points, .(graph, algorithm), function(df) 
      data.frame(graph = min(df$graph), 
                 algorithm = min(df$algorithm), 
                 max_id = max(df$partial_ilp_id))
    )
    
    ids_with_max <- left_join(data.frame(ilp_id = ilp_ids, 
                                         graph = graphs, 
                                         algorithm = algorithms), 
                              max_ilp_ids,
                              by = c("graph", "algorithm"))
    id_colors <- ids_with_max$ilp_id / ids_with_max$max_id
    return(id_colors)
  }
  
  # cap negative gains at -1 to improve plot readability
  all_points$partial_gains[all_points$partial_gains < -1] <- -1
  
  
  # ========================= Custom theme ====================================
  custom_theme <- theme(
    strip.text.y = element_text(size = 8, margin = margin(2,2,2,2)),
    strip.background.y = element_rect()
  )
  
  
  # =========================== CREATE PLOT ===================================
  isLimitNegative <- function(limits) min(limits) <= -1
  
  second_improvement_points <- filter(all_points, num_improvements > 1)
  text_no_second_improvement <- ddply(all_points, .(algorithm, graph), function(df) {
    ratio_no_second = nrow(filter(df, num_improvements <= 1)) / nrow(df)
    
    if (latex_export) {
      # escape % for latex export with a single \ (which has to be escaped in the string)
      template_text <- "%.2f \\%% no second improvement"
    } else {
      template_text <- "%.2f %% no second improvement"
    }
    
    return(data.frame(
      ratio_no_second = sprintf(template_text, ratio_no_second * 100),
      x = max(df$partial_runtime),
      y = 0
    ))
  })
  
  plot <- ggplot(second_improvement_points, aes(x = partial_runtime, y = partial_gains, color = get_color(partial_ilp_id, graph, algorithm))) +
    facet_grid(cols = vars(algorithm), rows = vars(graph)) +
    scale_y_continuous(trans = "pseudo_log",  # logarithmic scale
                       breaks = add_conditional_break(-1, isLimitNegative),  # if negative points exists, force break point at -1
                       labels = replace_label(-1, "negative")  # set the label of the break point -1 to negative (to indicate, that at -1 was capped)
                       ) +
    geom_text(data = text_no_second_improvement, aes(label = ratio_no_second, x = x, y = y), color = "black", hjust = 1) +
    geom_point(data = filter(second_improvement_points, final), aes(group = partial_ilp_id), pch = 18, show.legend = F) +
    labs(x = "ILP Solver running time", y = "Gain") +
    geom_line(aes(group = partial_ilp_id), alpha = 0.6, lineend = "square", show.legend = F)
  
  # Save graph in file
  algo_count <- length(unique(complete_data$algorithm))
  graph_count <- length(unique(complete_data$graph))
  width <- 10 * algo_count + 5
  height <- 10 * graph_count

  
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
  
  
  # ======================= CREATE SCATTER PLOT ===============================
  
  get_diff <- function(df) {
    sorted <- df[order(df$partial_runtime, decreasing = F),]
    sorted$number_improvement <- 0:(nrow(df) - 1)
    gains <- sorted$partial_gains
    sorted$partial_diff <- c(gains[1], gains[2:length(gains)] - gains[1:length(gains) - 1])
    gains[gains == 0] <- 1
    sorted$partial_diff_rel <- c(gains[1], gains[2:length(gains)] / gains[1:length(gains) - 1])
    return(filter(sorted, partial_diff > 0, number_improvement > 0))
  }
  
  
  all_points_diff <- ddply(all_points, .(graph, algorithm, partial_ilp_id), get_diff)
  
  plot <- ggplot(all_points_diff, aes(x = partial_runtime, y = partial_diff_rel)) +
    facet_grid(cols = vars(algorithm), rows = vars(graph)) +
    scale_y_continuous(trans = "pseudo_log", breaks = add_conditional_break(1)) +
    labs(x = "ILP Solver running time", y = "Relative Gain Improvement") +
    geom_point(aes(color = (as.factor(number_improvement))), pch = 20)
  
  # Save graph in file
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = scatter_plot_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    custom_theme = custom_theme
  )


  # ================== CREATE RATIO n-th IMPROVEMENT ==========================

  # add to each partial solution its position in the current ILP optimization
  points_with_num_impr <- ddply(all_points, .(algorithm, graph, partial_ilp_id), function(df) {
    # sort data frame in ascending run time
    df <- df %>% arrange(partial_runtime)
    df$number_improvement <- 0:(length(df$partial_gains) - 1)

    return(df)
  })

  # only use partial solutions of a specific range
  filtered_points <- filter(points_with_num_impr, 1 <= number_improvement, number_improvement <= 4)

  # total number of ILP runs
  num_instances <- length(unique(points_with_num_impr$partial_ilp_id))

  # calculate the percentage of instances that had their n-th improvment after
  # x time
  n_th_improvement_after_x_time <- ddply(filtered_points,
                                         .(algorithm, graph, number_improvement) ,
                                         function(df) {
    runtimes <- c(0, sort(unique(df$partial_runtime)))
    fraction_of_instances <- sapply(runtimes, function(runtime) {
      return(nrow(filter(df, partial_runtime <= runtime)))
    })
    fraction_of_instances <- fraction_of_instances / last(fraction_of_instances)

    return(data.frame(
      runtime = runtimes,
      instance = fraction_of_instances))
  })

  # the number of improvements is used to generate the positioning of the labels
  # that state the percentage of ILPs that reach a specific number of improvements
  max_num_improvements = max(filtered_points$number_improvement) -
    min(filtered_points$number_improvement) + 1

  ratio_improvements_texts <- ddply(filtered_points, .(algorithm, graph, number_improvement), function(df) {
    fraction_ilps = round((nrow(df) / num_instances) * 100, 1)

    number_improvement = df$number_improvement[1]
    y = (max_num_improvements - number_improvement) / max_num_improvements
    y = 0.4 * y
    
    if(latex_export) {
      # escape % for latex export with a single \ (which has to be escaped in the string)
      template_string <- "%.1f \\%%"
    } else {
      template_string <- "%.1f %%"
    }

    return(data.frame(
      algorithm = df$algorithm[[1]],
      graph = df$graph[[1]],
      number_improvement = df$number_improvement[1],
      solver_runtime_limit = df$solver_runtime_limit[1],
      fraction_ilps = sprintf(template_string, fraction_ilps),
      y = y
    ))
  })

  plot <- ggplot(filter(n_th_improvement_after_x_time, 1 <= number_improvement, number_improvement <= 4), aes(x = runtime, y = instance, color = as.factor(number_improvement))) +
    facet_grid(cols = vars(algorithm), rows = vars(graph)) +
    geom_text(data = ratio_improvements_texts,
                    aes(label = paste(number_improvement, ":"),
                        x = solver_runtime_limit,
                        y = y, hjust = "left"),
                    #nudge_x = -2.3,
                    color = "black",
                    show.legend = F) +
    geom_text(data = ratio_improvements_texts,
                    aes(label = fraction_ilps,
                        x = solver_runtime_limit,
                        y = y, hjust = "right"),
                    #nudge_x = -0.5,
                    #nudge_y = 0.02,
                    show.legend = F) +
    labs(x = "ILP Solver running time", y = "Percentage of ILP runs") +
    geom_step()

  # Save graph in file
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = n_th_improvment_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    custom_theme = custom_theme
  )
}


# =============================================================================
# =============================================================================
# =============================================================================
# ================== RELATIVE IMPROVEMENT after x TIME ========================
# =============================================================================
# =============================================================================
create_relative_improvement_after_x_time <- function(experiment_dir,
                                                     output_dir,
                                                     plot_file_name,
                                                     width = 22,
                                                     height = 10,
                                                     small_size = F,
                                                     pdf_export = T,
                                                     latex_export = F,
                                                     custom_color_mapping = NULL,
                                                     filter_data = identity) {
  raw_data <- read_csv_into_df(experiment_dir, filter_data) %>%
    dplyr::select(algorithm, graph, seed, k, partial_gains, partial_runtime, partial_objective, partial_ilp_id, gains)
  
  # remove rows were no partial gains data is given (e.g. label propagation)
  raw_data <- na.omit(raw_data, cols=c("gains", "partial_gains", "partial_runtime", "partial_objective", "partial_ilp_id"))
  if (nrow(raw_data) == 0) {
    warning(paste("Skipped creating relative improvement after x time plot for", basename(experiment_dir), 
                  ", because no partial gains data exists. Check that the partial ilp ids were added!"))
    return()
  }
  
  # restrict to one seed and one k. This is to avoid over plotting.
  raw_data <- filter(raw_data, seed == min(seed))
  raw_data <- filter(raw_data, k == max(k))
  
  # split ';' separated partial gains into several rows (using tidyr)
  # Each row represents one partial solution.
  split_data <- separate_rows(raw_data, partial_gains, partial_runtime,
                              partial_objective, partial_ilp_id, 
                              sep = ";", convert = T)
  
  # ignore balance objective solutions
  split_data <- filter(split_data, partial_objective == 1)
  
  gain_after_x_time <- ddply(split_data, .(algorithm, graph), function(df) {
                               ordered_df <- df %>%
                                 ddply(.(partial_ilp_id), function(df) {
                                   # transform partial gains for each ILP run to be the increase since the last found solution
                                   df <- arrange(df, partial_runtime)
                                   gains <- df$partial_gains
                                   len <- length(gains)
                                   return(data.frame(
                                     partial_runtime = df$partial_runtime,
                                     gain_increase = c(gains[1], gains[2:len] - gains[1:len - 1])
                                   ))
                                 }) %>% arrange(partial_runtime)
                               
                               return(data.frame(
                                 runtime = ordered_df$partial_runtime,
                                 partial_gain = cumsum(ordered_df$gain_increase) / sum(ordered_df$gain_increase)
                               ))
                             })
  
  # cap negative values (some ILP runs start shortly with a negative solution)
  gain_after_x_time$partial_gain[gain_after_x_time$partial_gain < 0] <- -0.1
  
  negative_label <- "negative"
  if (small_size) {
    if (latex_export) {
      negative_label <- "neg."
    } else {
      negative_label <- "neg."
    }
  }
  
  plot <- ggplot(gain_after_x_time, aes(x = runtime, y = partial_gain)) +
    facet_wrap(vars(algorithm)) +
    geom_step(aes(colour = graph), show.legend = T) +
    scale_y_continuous(breaks = add_conditional_break(-0.1, function(limit) min(limit) < 0),  # if negative points exists, force break point at -1
                       labels = replace_label(-0.1, negative_label)  # set the label of the break point -1 to negative (to indicate, that at -1 was capped)
    ) + labs(x = "Running time", y = "Relative improvement found") +
    theme_bw(base_size = 10) + create_theme(latex_export = latex_export)
  
  if(!is.null(custom_color_mapping)) {
    plot <- plot + scale_color_discrete(custom_color_mapping)
  }
  
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = plot_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    small_size = small_size
  )
  
  return(plot)
}




# =============================================================================
# =============================================================================
# =============================================================================
# ======================= CONFLICT vs LOCAL GAIN ==============================
# =============================================================================
# =============================================================================
create_conflict_vs_local_gain_plot <- function(experiment_dir,
                                               plot_file_name,
                                               output_dir = NULL,
                                               width = NULL,
                                               height = NULL,
                                               pdf_export = T,
                                               latex_export = F,
                                               small_size = F,
                                               show_infeasible_tick = T,
                                               show_timeout_tick = T,
                                               custom_color_mapping = NULL,
                                               filter_data = identity,
                                               add_percentage_zeroes = T) {
  if(is.null(output_dir)) {
    output_dir = experiment_dir
  }
  
  # load complete data
  complete_data <- read_csv_into_df(experiment_dir = experiment_dir,
                                    filter_data = filter_data) 
  
  # modify/aggregate data
  complete_data <- complete_data %>%
    # select only necessary columns, to avoid too many columns which conflicts with na.omit
    dplyr::select(algorithm, graph, k, seed, gains, local_gain, conflict_value, ilp_id) %>%
    # remove rows with missing data
    na.omit(complete_data, cols=c("gains", "local_gain", "conflict_value", "ilp_id")) %>%
    # split the gains data
    separate_rows(gains, local_gain, conflict_value, ilp_id, sep = ";", convert = T)

  if (length(row.names(complete_data)) == 0) {
    warning(paste("Skipped creating conflict value plot for", basename(experiment_dir), ", because no conflict value data exists."))
    return()
  }

  complete_data$is_conflict_greater = complete_data$conflict_value >= complete_data$local_gain
  no_zero_gains <- filter(complete_data, gains != 0)
  num_zero_gains <- ddply(complete_data, .(algorithm, graph), function(df) data.frame(
    algorithm = df$algorithm[1],
    graph = df$graph[1],
    num_zeroes = length(df$gains[df$gains == 0]),
    percentage_zeroes = length(df$gains[df$gains == 0]) / nrow(df)
  ))

  plot <- ggplot(no_zero_gains, aes(x = graph, y = conflict_value - local_gain, color = graph)) +
    facet_grid(rows = vars(algorithm), scales="free") +
    geom_hline(yintercept = 0) +
    #geom_jitter(show.legend = F) +
    stat_boxplot(geom ='errorbar', width = 0.6, show.legend = F) +
    geom_boxplot(outlier.shape = NA, alpha = 0.75, show.legend = F) +
    scale_y_continuous(trans = "pseudo_log", breaks = c(-100, -10, 0, 10, 100, 1000, 10000, 40000)) +
    scale_color_discrete(custom_color_mapping) +
    labs(x = NULL, y = "Difference betw. Conflict Value and Local Gain")
  
  
  # add (possibly) text that states how many non-zeroes where removed
  if(add_percentage_zeroes) {
    plot <- plot +
      geom_text_repel(data = num_zero_gains,
                      aes(label = paste("removed: ", num_zeroes, " (", round(percentage_zeroes * 100, 2), "%)", sep = ""),
                          y = -Inf,
                          group = graph,
                          hjust = "center"),
                      direction = "y",
                      show.legend = F)
  }
  
  
  if(is.null(width)) {
    graph_count <- length(unique(no_zero_gains$graph))
    width <- 4 * graph_count
  }
  if(is.null(height)) {
    algo_count <- length(unique(no_zero_gains$algorithm))
    height <- 20 * algo_count + 5
  }


  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = plot_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    small_size = small_size,
    add_default_theme = T
  )
}


# =============================================================================
# =============================================================================
# =============================================================================
# =============== FRACTION GAIN per NUMBER IMPROVEMENT ========================
# =============================================================================
# =============================================================================
create_fraction_gain_per_improvement_plot <- function(experiment_dir,
                                                      plot_file_name,
                                                      output_dir = NULL,
                                                      width = NULL,
                                                      height = NULL,
                                                      pdf_export = T,
                                                      latex_export = F,
                                                      small_size = F,
                                                      filter_data = identity) {
  if(is.null(output_dir)) {
    output_dir <- experiment_dir
  }
  
  # load complete data
  complete_data <- read_csv_into_df(experiment_dir = experiment_dir) %>%
    dplyr::select(algorithm, graph, k, seed, partial_gains, partial_runtime, partial_objective, partial_ilp_id, gains, solver_runtime, ilp_id) %>%
    # remove rows with missing data
    na.omit(complete_data, cols=c("partial_gains", "partial_runtime", "partial_objective", "partial_ilp_id"))

  if (length(row.names(complete_data)) == 0) {
    warning(paste("Skipped creating fraction gain per number improvement plot for", basename(experiment_dir), ", because no partial gains data exists."))
    return()
  }
  
  complete_data <- filter_data(complete_data)
  
  if (nrow(complete_data) == 0) {
    warning(paste("After filtering data for", basename(experiment_dir), ", no data is left"))
    return()
  }

  # split ';' separated partial gains into several rows (using tidyr)
  # Each row represents one partial solution.
  partial_points <- separate_rows(complete_data, partial_gains, partial_runtime,
                                  partial_objective, partial_ilp_id,
                                  sep = ";", convert = T)

  # ignore balance objective solutions
  partial_points <- filter(partial_points, partial_objective == 1)

  # calculate fraction of gain, per improvement
  annotated_points <- ddply(partial_points, .(algorithm, graph, k, seed, partial_ilp_id), function(df) {
    # sort data frame in ascending run time
    df <- df %>% arrange(partial_runtime)
    df$number_improvement <- 0:(length(df$partial_gains) - 1)
    df$fraction_gain <- df$partial_gain / max(df$partial_gain)

    return(df)
  })

  # cap negative gains and remove 0 gain values (fraction is NA)
  capped_annotated_points <- na.omit(annotated_points, cols = c("fraction_gain"))
  capped_annotated_points$fraction_gain[capped_annotated_points$fraction_gain < 0] <- -0.1
  capped_annotated_points <- filter(capped_annotated_points, number_improvement > 0)
  
  gmean_relative_gain_data <- ddply(capped_annotated_points, .(algorithm, graph, number_improvement),
                                    function(df) data.frame(
                                      mean_rel_gain = mean(df$fraction_gain)
                                    ))

  plot <- ggplot(capped_annotated_points, aes(x = as.factor(number_improvement), y = fraction_gain, color = as.factor(number_improvement))) +
    facet_grid(cols = vars(algorithm), rows = vars(graph)) +
    scale_y_continuous(breaks = add_conditional_break(-0.1, function(limits) min(limits) <= -0.1, n = 8),
                       labels = replace_label(-0.1, "negative"),
                       limits = function(limits) c(limits[[1]], 1.1)) +
    #geom_jitter(aes(y = fraction_gain), alpha = 0.5, pch = 21) +
    labs(x = "Improvement", y = "Relative to Final Gain") +
    stat_boxplot(aes(group = number_improvement), geom ='errorbar', width = 0.6) +
    geom_boxplot(aes(group = number_improvement), outlier.shape = NA, alpha = 0.75) +
    geom_text(data = gmean_relative_gain_data, 
              aes(x = number_improvement, label=round(mean_rel_gain, 3), group = algorithm), 
              y = 1.1, size = plot_text_size(latex_export))

  algo_count <- length(unique(complete_data$algorithm))
  graph_count <- length(unique(complete_data$graph))
  if(is.null(width)) {
    width <- 15 * algo_count
  }
  if(is.null(height)) {
    height <- 12 * graph_count
  }
  
  save_ggplot(
    plot = plot,
    output_dir = output_dir,
    filename = plot_file_name,
    width = width,
    height = height,
    pdf_export = pdf_export,
    latex_export = latex_export,
    small_size = small_size,
    custom_theme = theme(
      legend.position = "none",
      strip.text.y = element_text(size = 8, margin = margin(2,2,2,2)),
      strip.background.y = element_rect()
    )
  )
}



# =============================================================================
# =============================================================================
# =============================================================================
# ============================ ZERO GAIN RATIO Plot ===========================
# =============================================================================
# =============================================================================
create_zero_gain_ratio_plot <- function(experiment_dir,
                                        plot_file_name) {
  # load complete data
  complete_data <- read_csv_into_df(experiment_dir = experiment_dir) %>%
    dplyr::select(algorithm, graph, k, seed, gains) %>%
    drop_na(gains) %>%
    separate_rows(gains, sep = ";")
  
  if (length(row.names(complete_data)) == 0) {
    warning(paste("Skipped creating zero gain ratio plot for", basename(experiment_dir), ", because no gains data exists."))
    return()
  }
  
  aggregated_data <- ddply(complete_data, .(algorithm, graph, k, seed), function(df) {
    zero_gainer <- df$gains[df$gains == 0]
    return(data.frame(
      zero_gain_ratio = length(zero_gainer) / length(df$gains)
    ))
  })
  
  ggplot(aggregated_data, aes(x = graph, y = zero_gain_ratio)) +
    facet_grid(rows = vars(algorithm), scales = "free_x") + # facet the plot according to the max non-zeroes
    geom_jitter(aes(color = graph), alpha = 0.7, shape = 21, width = 0.4) +
    stat_boxplot(aes(color = graph), geom ='errorbar', width = 0.6) + # add top/bottom bar
    geom_boxplot(aes(color = graph), outlier.shape = NA, alpha = 0.5) + # add average and box
    scale_y_continuous(limits = c(0, 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  algo_count <- length(unique(complete_data$algorithm))
  graph_count <- length(unique(complete_data$graph))
  ggsave(plot_file_name, path=experiment_dir,
         width = 4 * graph_count + 2,
         height = 15 * algo_count,
         unit = "cm", limitsize = F)
}



#experiment_dir <- "C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/test_results/001eps-Cluster_no_min_gain_2022-02-12_0"

#create_timed_out_ilp_plot(experiment_dir, "test.pdf")
#create_gains_density_plot(experiment_dir, "test.pdf")
