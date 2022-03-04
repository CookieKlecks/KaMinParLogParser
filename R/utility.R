base_wd <- getwd()

# We need to change the working directory to the base of the plot module, because
# in functions.R are source calls to relative locations inside the module.
setwd("../external_tools/experimental_plot_scripts")
lib_wd <- getwd()
source("functions.R")

setwd(base_wd)

read_csv_into_df <- function(experiment_dir) {
  # List all .csv files in experiment dir
  result_files <- list.files(path=experiment_dir, pattern="*.csv", full.names = T)
  if (length(result_files) == 0) {
    stop(paste("No .csv result files found in", experiment_dir))
  }
  
  # import these .csv files into one single data frame
  combined_data <- data.frame()
  for (result in result_files) {
    raw_data <- read.csv(result, header = TRUE)
    # only use base name of the graph (strips the path)
    raw_data$graph <- sapply(raw_data$graph, basename)
    
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
  result_files <- list.files(path=experiment_dir, pattern="*.csv", full.names = T)
  
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
