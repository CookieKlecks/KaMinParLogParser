suppressPackageStartupMessages(source("create_performance_profile.R"))


# Parse command line parameters
args <- commandArgs(trailingOnly = T)

if (length(args) > 1) {
  experiments_base_dir <<- args[[2]]
} else {
  print("using default experiment base path.", quote = F)
  experiments_base_dir <<- "C:/Users/Cedrico.DESKTOP-3BCMGI6/KIT/BA/experiments/results"
}

if (length(args) > 2) {
  override <<- as.logical(args[[3]])
  if (is.na(override)) {
    stop(paste("Could not parse '", args[[3]], "' as logical expression.",
               "Try to use 'T', 'TRUE', 'True', 'true', 'F', 'FALSE', 'False' or 'false'"))
  }
} else {
  print("using default override option (True).", quote = F)
  override <<- T
}


list_experiments <- list.dirs(experiments_base_dir, 
                              full.names = T, 
                              recursive = F)

# create plots for every experiment
for (experiment_dir in list_experiments) {
  print(paste("Start creating plots for:", experiment_dir), quote = F)
  
  pp_plot_name <- "performance_profile.pdf"

  if(!file.exists(paste(experiment_dir, pp_plot_name, sep = "/")) 
     || override) {
    create_performance_profile_plot(experiment_dir = experiment_dir,
                                    plot_file_name = pp_plot_name,
                                    timelimit = 90000)
    print(" |___ created PERFORMANCE PROFILE plot", quote = F)
  } else {
    print(" |___ Performance plot ALREADY EXISTS", quote = F)
  }
}
