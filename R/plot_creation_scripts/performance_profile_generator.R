source("../create_performance_profile.R", chdir = T)
source("../create_special_ilp_plots.R", chdir = T)
source("../create_time_plots.R", chdir = T)
library(tidyr)

experiment_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_KaMinPar"
output_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_KaMinPar"
kaminpar_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaMinPar"
kahip_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP\\runs"


create_performance_profile <- function(run_configs, file_name = "", dry_run = F) {
  
  dataframes <- vector("list", length(run_configs))
  # use .GlobalEnv to access the global variables (see https://stackoverflow.com/a/63487249)
  .GlobalEnv$algo_color_mapping <- c()
  algo_names <- vector("list", 0)
  
  generated_file_name <- ""
  i <- 1
  for (config in run_configs) {
    print(paste(config["base_dir"], config["name"], sep = "\\"))
    dataframes[[i]] <- read.csv(paste(config["base_dir"], config["name"], sep = "\\"), header = T)
    dataframes[[i]] <- aggreg_data(dataframes[[i]], 70000, 0) #%>% drop_na()
    dataframes[[i]] <- dataframes[[i]] %>% filter(graph %in% dataframes[[1]]$graph) %>% 
      filter(graph != '/global_data/graphs/benchmark_sets/bbgb_paper/fe_body.graph') %>%
      #filter(graph != '/global_data/graphs/benchmark_sets/bbgb_paper/dolphins.graph') %>%
      filter(graph != '/global_data/graphs/benchmark_sets/bbgb_paper/finan512.graph') #%>%
      #filter(!infeasible)
    dataframes[[i]]$algorithm <- config["algo_name"]
    
    .GlobalEnv$algo_color_mapping <- append(algo_color_mapping, c(config[["color"]]))
    algo_names <- append(algo_names, c(config[["algo_name"]]))
    
    if (i != 1) {
      generated_file_name <- paste(generated_file_name, "-vs-", sep = "")
    }
    generated_file_name <- paste(generated_file_name, config["algo_name"], sep = "")
    
    i <- i + 1
  }
  
  #semi_join_filter = semi_join(dataframes[[1]], dataframes[[2]], by=c('graph','k')) 
  #dataframes[[1]] = semi_join(dataframes[[1]], semi_join_filter, by=c('graph','k'))
  #dataframes[[2]] = semi_join(dataframes[[2]], semi_join_filter, by=c('graph','k')) 
  
  names(.GlobalEnv$algo_color_mapping) <- algo_names
  
  if (file_name == "") {
    file_name <- generated_file_name
  }
  
  algo_color_mapping <- "bla"
  
  if (!dry_run) {
    plot <- performace_plot(dataframes, objective = "avg_cut")
    save_ggplot(plot, output_dir, file_name, width = 22, height = 15)
  }
  
  return(dataframes)
}


palette <- brewer.pal(n = 9, name = "Set1")


KaHIP_single <- c("base_dir" = kahip_dir,
                  "name" = "kahip_only-negative-cycle_kahip_no-neg-cycle_eps_0_2023-01-30_02-40-27.csv",
                  "algo_name" = "KaHIP-single",
                  "color" = palette[[1]])

KaHIP_evolutionary <- c("base_dir" = kahip_dir,
                  "name" = "kahip_full_default_2022-11-27_13-40-55.csv",
                  "algo_name" = "KaHIP-evolutionary",
                  "color" = palette[[2]])

KaMinPar <- c("base_dir" = experiment_dir,
              "name" = "kaminpar_full_default_eps_0_2022-12-14_13-50-13.csv",
              "algo_name" = "KaMinPar",
              "color" = palette[[3]])

KaMinPar_after_KaHIP <- c("base_dir" = kaminpar_dir,
                          "name" = "kaminpar_only-ref_ilp_pairwise-fm_kaminpar_eps_0_2023-02-04_00-42-20.csv",
                          "algo_name" = "KaMinPar-PairwiseFM",
                          "color" = palette[[9]])


KaMinPar_PairwiseLP <- c("base_dir" = kaminpar_dir,
                         "name" = "kaminpar_only-ref-[KaHIP]_pairwise-lp_eps_0_2023-02-20_03-25-02.csv",
                         "algo_name" = "KaMinPar-PairwiseLP",
                         "color" = palette[[4]])

KaMinPar_PairwiseLP_geom_shrinking <- c("base_dir" = kaminpar_dir,
                         "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_pairwise-lp_eps_0_2023-02-20_03-25-02.csv",
                         "algo_name" = "KaMinPar-PairwiseLP-LazyILP",
                         "color" = palette[[5]])

KaMinPar_PairwiseLP_rand2 <- c("base_dir" = kaminpar_dir,
                         "name" = "kaminpar_only-ref-[KaHIP]_pairwise-lp_rand2_eps_0_2023-02-20_03-25-02.csv",
                         "algo_name" = "KaMinPar-PairwiseLP-gain",
                         "color" = palette[[6]])


KaMinPar_PairwiseLP_manyILPs <- c("base_dir" = kaminpar_dir,
                         "name" = "kaminpar_only-ref-[KaHIP]_pairwise-lp_many-ilps_eps_0_2023-02-20_04-04-06.csv",
                         "algo_name" = "KaMinPar-PairwiseLP-ManyILPs",
                         "color" = palette[[7]])

palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("KaHIP-evolutionary" = palette[[1]],
                        "KaHIP-single" = palette[[2]],
                        "KaMinPar" = palette[[3]],
                        "KaMinPar-ILP 20s" = palette[[4]],
                        "KaMinPar-ILP 120s" = palette[[5]],
                        "KaMinPar-ILP 600s" = palette[[6]],
                        "KaHIP after Mt-KaHyPar 1%" = palette[[7]],
                        "KaHIP after Mt-KaHyPar 3%" = palette[[8]],
                        "KaMinPar-ILP after KaHIP fixed" = palette[[3]],
                        "KaMinPar-Move-Batches-ILP after KaHIP fixed" = palette[[4]],
                        "KaMinPar-geometric after KaHIP" = palette[[5]],
                        "KaMinPar-linear after KaHIP" = palette[[6]],
                        "KaHIP after KaMinPar" = palette[[7]],
                        "KaMinPar-ILP after KaMinPar" = palette[[8]],
                        "KaMinPar-Batched-ILP after KaMinPar" = palette[[9]],
                        "KaMinPar-ILP-lazy" = palette[[3]],
                        "KaMinPar-Batched-ILP-lazy" = palette[[4]],
                        "KaMinPar-PairwiseLP" = palette[[5]],
                        "KaMinPar-PairwiseLP-LazyILP" = palette[[6]],
                        "KaMinPar-PairwiseLP-gain" = palette[[7]],
                        "KaMinPar-PairwiseLP-ManyILPs" = palette[[8]])


create_performance_profile(list(KaHIP_single, KaMinPar))
test_3 <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP, KaMinPar_PairwiseLP_geom_shrinking))
t <- create_performance_profile(list(KaMinPar_PairwiseLP, KaMinPar_PairwiseLP_geom_shrinking, KaMinPar_PairwiseLP_manyILPs, KaMinPar_PairwiseLP_rand2))
t <- create_performance_profile(list(KaMinPar_after_KaHIP, KaMinPar_PairwiseLP, KaMinPar_PairwiseLP_geom_shrinking))
t <- create_performance_profile(list(KaHIP_evolutionary, KaHIP_single, KaMinPar_PairwiseLP_geom_shrinking))
