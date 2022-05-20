base_wd <- getwd()

# We need to change the working directory to the base of the plot module, because
# in functions.R are source calls to relative locations inside the module.
setwd("../external_tools/experimental_plot_scripts")
lib_wd <- getwd()
source("functions.R")

setwd(base_wd)

library(tikzDevice) # for output to latex (tikz command)
options(tikzLatexPackages = c("\\usepackage{pifont}", "\\usepackage{marvosym}", "\\usepackage{tikz}", "\\usepackage{siunitx}")) # specify the packages for latex output

read_csv_into_df <- function(experiment_dir) {
  # List all .csv files in experiment dir
  result_files <- list.files(path=experiment_dir, pattern=".csv", full.names = T)
  if (length(result_files) == 0) {
    stop(paste("No .csv result files found in", experiment_dir))
  }
  
  # import these .csv files into one single data frame
  combined_data <- data.frame()
  for (result in result_files) {
    raw_data <- read.csv(result, header = TRUE)
    # only use base name of the graph (strips the path)
    raw_data$graph <- sapply(raw_data$graph, basename)
    raw_data$graph <- sapply(raw_data$graph, fs::path_ext_remove)
    
    # exchange underscores from algorithm and graph names with spaces
    # This is necessary to avoid errors when exporting to latex
    raw_data$algorithm <- sapply(raw_data$algorithm, str_replace_all, "_", " ")
    raw_data$graph <- sapply(raw_data$graph, str_replace_all, "_", " ")
    
    combined_data <- rbind(combined_data, raw_data)
  }
  
  return(combined_data)
}


#' Read and aggregate data of an experiment.
#' 
#' This function reads all .csv-files in a directory, aggregates the data and
#' returns a resulting list of data frames.
#' Each single data frame corresponds to one .csv-file.
#' 
#' The used epsilon is taken from the first .csv-file found. It is set to the
#' minimum epsilon in the data set.
#' The minimum is used, because KaMinPar relaxes the epsilon if the graph
#' contains isolated nodes.
#' 
#' WARNING: if the first .csv-file only contains data of graphs with isolated
#' nodes, the epsilon will probably be wrong!
#' 
#' To aggregate the data, the aggreg_data function of the external library
#' experimental_plot_scripts is used.
#'
#' @param experiment_dir path to the directory where the .csv-files are located.
#' @param timelimit limit to identify time outs in the data.
#'
#' @return list of data frames. Each single data frame contains the aggregated
#' data of one .csv-file in experiment_dir.
#'
#' @examples
read_and_aggregate_csv <- function(experiment_dir,
                                      timelimit = 7200) {
  # List all .csv files in experiment_dir
  result_files <- list.files(path=experiment_dir, pattern=".csv", full.names = T)
  
  if (length(result_files) == 0) {
    stop(paste("No .csv result files found in", experiment_dir))
  }
  
  # import these .csv files into one data frame each
  dataframes <- vector("list", length(result_files))
  i <- 1
  epsilon <- 0.03
  for (result in result_files) {
    raw_data <- read.csv(result, header = TRUE)
    if (i == 1) {
      # use minimum epsilon, because KaMinPar increases the epsilon, if a graph
      # contains isolated nodes.
      epsilon <- min(raw_data$epsilon)
    }
    # only use base name of the graph (strips the path)
    raw_data$graph <- sapply(raw_data$graph, basename)
    
    dataframes[[i]] <- aggreg_data(raw_data, 
                                   timelimit = timelimit, 
                                   epsilon = epsilon,
                                   seeds = .Machine$integer.max)
    i <- i + 1
  }
  
  return(dataframes)
}



save_ggplot <- function(plot, 
                        output_dir,
                        filename,
                        width,
                        height,
                        pdf_export = T,
                        latex_export = F,
                        add_default_theme = T,
                        custom_theme = theme()) {
  
  # strip the file ending (if existing) from file name
  name <- strsplit(basename(filename), "\\.")[[1]][[1]]
  
  if (add_default_theme) {
    plot <- plot + theme_bw(base_size = 10) # add black/white theme with font size 10
  }
  
  if (latex_export) {
    latex_base_name <- paste(name, "tex", sep=".")
    
    latex_plot <- plot
    if (add_default_theme) {
      latex_plot <- latex_plot + create_theme(latex_export = T)
    }
    print(latex_plot)
    
    tikz(file.path(output_dir, latex_base_name), width = width / 2.54, height = height / 2.54)
    print(latex_plot + custom_theme)
    dev.off()
  }
  
  if (pdf_export) {
    pdf_base_name <- paste(name, "pdf", sep=".")
    
    pdf_plot <- plot
    if (add_default_theme) {
      pdf_plot <- plot + create_theme()
    }
    
    ggsave(pdf_base_name, plot=pdf_plot + custom_theme, path=output_dir, 
           width = width, height = height, unit = "cm", limitsize = F)
  }
}



# ==================== Manipulate Graph scales ================================

#' This function returns a function that can be used as 'breaks' argument in 
#' scale_*_continuous(). 
#' The returned function creates the breaks for a scale as follows: 
#' 
#' Firstly the default breaks are calculated via scales::extended_breaks.
#' Then the passed conditionFunc is called with the current limits. If this
#' function returns true, the passed newBreakPoint is added to the default 
#' breaks. Otherwise the default breaks are returned as they are.
#'
#' @param newBreakPoint The new break that should be added to the default breaks
#' @param conditionFunc function that accepts the limits and returns a boolean.
#'                      If this function evaluates true, the new break is added
#'                      to the default breaks
#' @param n directly passed to scales::extended_breaks
#' @param ... directly passed to scales::extended_breaks
#'
#' @return
#' @export
#'
#' @examples
#' 
#' # add a new break add 4.5 if lower limit is lower or equal to 4.5
#' 
#' checkLimit <- function(limits) limits[[1]] <= 4.5
#' customBreakFunction <- add_conditional_break(4.5, conditionFunc = checkLimit)
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   scale_y_continuous(breaks = customBreakFunction) +
#'   geom_point()
#'   
add_conditional_break <- function(newBreakPoint, conditionFunc = true, n = 5, ...) {
  return(function(limits) {
    default_breaks <- scales::extended_breaks(n, ...)(limits)
    
    # check if new break point should be included
    if (conditionFunc(limits)) {
      extended_breaks <- c(newBreakPoint, default_breaks)
      return(extended_breaks)
    }
    
    return(default_breaks)
  })
}


#' This function creates a function that takes break points of a scale as input,
#' replaces the given break point with the given label and returns this.
#' It can be used in scales_*_continuous() as custom labeller (labels=).
#' The resulting effect is then, that the given break point (breakPoint) has
#' the given label (replaceLabelWith). The other break points are labelled as
#' they are.
#' 
#' If the given break point does not exist in the break points, nothing happens.
#'
#' @param breakPoint the break point of which the label should be replaced.
#' @param newLabel the label that should be used for this break point.
#'
#' @return
#' @export
#'
#' @examples
replace_label <- function(breakPoint, newLabel) {
  return(function(breaks) {
    breaks[breaks == breakPoint] <- newLabel
    return(breaks)
  })
}
