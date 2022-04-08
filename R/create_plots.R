suppressPackageStartupMessages(source("create_performance_profile.R"))
suppressPackageStartupMessages(source("create_time_plots.R"))
suppressPackageStartupMessages(source("create_special_ilp_plots.R"))
suppressPackageStartupMessages(source("create_special_promising_cluster_plots.R"))


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
  
  output_dir <- paste(experiment_dir, sep = "/")
  if(!dir.exists(output_dir)) {
    dir.create(path = output_dir)
  }
  
  # ================= PERFORMANCE PLOT =======================
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
  # ==========================================================
  
  # ================= RUNNING TIME ===========================
  run_time_plot_name <- "running_time.pdf"
  
  if(!file.exists(paste(experiment_dir, run_time_plot_name, sep = "/")) 
     || override) {
    create_running_time_box_plot(experiment_dir = experiment_dir,
                                 plot_file_name = run_time_plot_name,
                                 timelimit = 90000)
    print(" |___ created RUNNING TIME BOX plot", quote = F)
  } else {
    print(" |___ Runing time box plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
  # ============= ILP TIMEOUT PERCENTAGE =====================
  ilp_timeout_plot_name <- "ilp_timeout_percentage.pdf"
  
  if(!file.exists(paste(experiment_dir, ilp_timeout_plot_name, sep = "/")) 
     || override) {
    create_timed_out_ilp_plot(experiment_dir = experiment_dir,
                              plot_file_name = ilp_timeout_plot_name)
    print(" |___ created ILP TIMEOUT PERCENTAGE plot", quote = F)
  } else {
    print(" |___ ILP timeout percentage plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
  # ============= ILP GAINS DENSITY =====================
  file_name <- "ilp_gains_density.pdf"
  
  if(!file.exists(paste(experiment_dir, file_name, sep = "/")) 
     || override) {
    create_gains_density_plot(experiment_dir = experiment_dir,
                              plot_file_name = file_name)
    print(" |___ created ILP GAINS DENSITY plot", quote = F)
  } else {
    print(" |___ ILP gains density plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
  # ============= PARTIAL ILP GAINS EVOLUTION =====================
  file_name <- "partial_ilp_gains_evolution.pdf"
  scatter_file_name <- "partial_ilp_gains_scatter.pdf"
  n_th_improvment_name <- "n_th_improvement_plot.pdf"
  
  if(!file.exists(paste(experiment_dir, file_name, sep = "/")) 
     || override) {
    create_partial_gains_evolution_plot(experiment_dir = experiment_dir,
                                        plot_file_name = file_name,
                                        scatter_plot_file_name = scatter_file_name,
                                        n_th_improvment_file_name = n_th_improvment_name)
    print(" |___ created PARTIAL ILP GAINS EVOLUTION plot", quote = F)
  } else {
    print(" |___ Partial ILP gains evolution plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
  # ============= ILP LOCAL GAINS vs CONFLICT VALUE =====================
  file_name <- "conflict_vs_local_gains.pdf"
  
  if(!file.exists(paste(experiment_dir, file_name, sep = "/")) 
     || override) {
    create_conflict_vs_local_gain_plot(experiment_dir = experiment_dir,
                                       plot_file_name = file_name)
    print(" |___ created ILP CONFLICT vs LOCAL GAINS plot", quote = F)
  } else {
    print(" |___ ILP conflict vs local gains plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
  # ============= ILP FRACTION GAINS per IMPROVEMENT =====================
  file_name <- "fraction_gains_per_improvement.pdf"
  
  if(!file.exists(paste(experiment_dir, file_name, sep = "/")) 
     || override) {
    create_fraction_gain_per_improvement_plot(experiment_dir = experiment_dir,
                                              plot_file_name = file_name)
    print(" |___ created ILP FRACTION GAINS PER IMPROVEMENT plot", quote = F)
  } else {
    print(" |___ ILP fraction gain per improvement plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
  # ========== PROMISING CLUSTER SIZE HISTOGRAM ==============
  file_name <- "cluster_size_histogram.pdf"
  
  if(!file.exists(paste(output_dir, file_name, sep = "/")) 
     || override) {
    create_cluster_size_histogram(experiment_dir = experiment_dir,
                                  output_dir = output_dir,
                                  plot_file_name = file_name)
    print(" |___ created CLUSTER SIZE HISTOGRAM plot", quote = F)
  } else {
    print(" |___ Cluster size histogram plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
  # ===== PROMISING CLUSTER GAINS-PER-CLUSTER SCATTER ========
  file_name <- "gains_per_cluster.pdf"
  
  print(" |___ create promising cluster gains-per-cluster plot:", quote = F)
  create_gains_per_cluster_scatter_plot(experiment_dir = experiment_dir,
                                        output_dir = output_dir,
                                        plot_file_name = file_name,
                                        override = override)
  # ==========================================================
  
  # ======== PROMISING CLUSTER AVG GAIN PER CLUSTER ==========
  file_name <- "average_gain_per_cluster.pdf"
  
  if(!file.exists(paste(output_dir, file_name, sep = "/")) 
     || override) {
    create_avg_gain_per_cluster_plot(experiment_dir = experiment_dir,
                                     output_dir = output_dir,
                                     plot_file_name = file_name)
    print(" |___ created AVERAGE GAIN PER CLUSTER plot", quote = F)
  } else {
    print(" |___ Average gain per cluster plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
  # ====== PROMISING CLUSTER AVG RUNTIME PER CLUSTER =========
  file_name <- "average_runtime_per_cluster.pdf"
  
  if(!file.exists(paste(output_dir, file_name, sep = "/")) 
     || override) {
    create_avg_runtime_per_cluster_plot(experiment_dir = experiment_dir,
                                        output_dir = output_dir,
                                        plot_file_name = file_name)
    print(" |___ created AVERAGE RUNTIME PER CLUSTER plot", quote = F)
  } else {
    print(" |___ Average runtime per cluster plot ALREADY EXISTS", quote = F)
  }
  # ==========================================================
  
}
