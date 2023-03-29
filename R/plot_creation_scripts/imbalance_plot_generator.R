source("../create_imbalance_plots.R", chdir = T)
library(tidyr)


generate_imbalance_plot <- function(run_configs, file_name = "", dry_run = F) {
  
  dataframes <- data.frame()
  # use .GlobalEnv to access the global variables (see https://stackoverflow.com/a/63487249)
  .GlobalEnv$algo_color_mapping <- c()
  algo_names <- vector("list", 0)
  
  generated_file_name <- "imbalance_"
  i <- 1
  for (config in run_configs) {
    print(paste(config["base_dir"], config["name"], sep = "\\"))
    cur_data <- read.csv(paste(config["base_dir"], config["name"], sep = "\\"), header = T)
    cur_data <- aggreg_data(cur_data, 70000, 0) #%>% drop_na()
    #filter(!infeasible)
    cur_data$algorithm <- config["algo_name"]
    dataframes <- rbind(dataframes, cur_data)
    
    .GlobalEnv$algo_color_mapping <- append(algo_color_mapping, c(config[["color"]]))
    algo_names <- append(algo_names, c(config[["algo_name"]]))
    
    if (i != 1) {
      generated_file_name <- paste(generated_file_name, "-vs-", sep = "")
    }
    generated_file_name <- paste(generated_file_name, config["algo_name"], sep = "")
    
    i <- i + 1
  }
  dataframes$graph <- sapply(dataframes$graph, function(graph) {return(sub('\\..*$', '', basename(graph)))})
  
  names(.GlobalEnv$algo_color_mapping) <- algo_names
  
  if (file_name == "") {
    file_name <- generated_file_name
  }
  
  if (!dry_run) {
    output_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_KaMinPar"
    plot <- create_imbalance_plot(dataframes, output_dir, file_name, pdf_export = T, width = 66, height = 22)
  }
  
  return(dataframes)
}


# ============= Define runs =====================

palette <- brewer.pal(n = 9, name = "Set1")


# ================== KaMinPar-Constrained-V-Cycle ======================
source("./Constrained-V-Cycle.R")

KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best[["algo_name"]] <- "KaMinPar"

KaHIP_input <- c("base_dir" = "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaMinPar\\0_GENERAL",
                  "name" = "kaminpar_output_KaHIP_partition__eps0.csv",
                  "algo_name" = "KaHIP",
                  "color" = palette[[1]])

t <- generate_imbalance_plot(list(KaHIP_input, KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best))

