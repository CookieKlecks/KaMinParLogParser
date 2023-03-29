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


# ============= Define runs =====================

palette <- brewer.pal(n = 9, name = "Set1")

# ================== KaHIP ======================
KaHIP_single <- c("base_dir" = kahip_dir,
                  "name" = "kahip_only-negative-cycle_kahip_no-neg-cycle_eps_0_2023-01-30_02-40-27.csv",
                  "algo_name" = "KaHIP-single",
                  "color" = palette[[1]])

KaHIP_evolutionary <- c("base_dir" = kahip_dir,
                  "name" = "kahip_full_default_2022-11-27_13-40-55.csv",
                  "algo_name" = "KaHIP-evolutionary",
                  "color" = palette[[2]])

# ================== KaMinPar ======================
KaMinPar <- c("base_dir" = experiment_dir,
              "name" = "kaminpar_full_default_eps_0_2022-12-14_13-50-13.csv",
              "algo_name" = "KaMinPar",
              "color" = palette[[3]])

# ================== KaMinPar-refine-KaHIP ======================
KaMinPar_after_KaHIP <- c("base_dir" = paste(kaminpar_dir, "0_GENERAL", sep = "\\"),
                          "name" = "kaminpar_only-ref_ilp_pairwise-fm_kaminpar_eps_0_2023-02-04_00-42-20.csv",
                          "algo_name" = "KaMinPar-PairwiseFM",
                          "color" = palette[[9]])


KaMinPar_PairwiseLP <- c("base_dir" = paste(kaminpar_dir, "0_GENERAL", sep = "\\"),
                         "name" = "kaminpar_only-ref-[KaHIP]_pairwise-lp_eps_0_2023-02-20_03-25-02.csv",
                         "algo_name" = "KaMinPar-PairwiseLP",
                         "color" = palette[[4]])

KaMinPar_PairwiseLP_rand2 <- c("base_dir" = paste(kaminpar_dir, "0_GENERAL", sep = "\\"),
                         "name" = "kaminpar_only-ref-[KaHIP]_pairwise-lp_rand2_eps_0_2023-02-20_03-25-02.csv",
                         "algo_name" = "KaMinPar-PairwiseLP-gain",
                         "color" = palette[[6]])


KaMinPar_PairwiseLP_manyILPs <- c("base_dir" = paste(kaminpar_dir, "0_GENERAL", sep = "\\"),
                         "name" = "kaminpar_only-ref-[KaHIP]_pairwise-lp_many-ilps_eps_0_2023-02-20_04-04-06.csv",
                         "algo_name" = "KaMinPar-PairwiseLP-ManyILPs",
                         "color" = palette[[7]])

# ================== KaMinPar-PairwiseLP geom-shrinking ======================
KaMinPar_PairwiseLP_geom_shrinking_nz50 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking", sep = "\\"),
                                        "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_pairwise-lp__eps0__nz50k__2023-02-27_17-19-16.csv",
                                        "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps 50nz",
                                        "color" = palette[[4]])
KaMinPar_PairwiseLP_geom_shrinking_nz100 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking", sep = "\\"),
                                        "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_pairwise-lp__eps0__nz100k__2023-02-27_17-19-16.csv",
                                        "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps 100nz",
                                        "color" = palette[[5]])
KaMinPar_PairwiseLP_geom_shrinking_nz150 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking", sep = "\\"),
                                        "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_pairwise-lp__eps0__nz150k__2023-02-27_17-19-16.csv",
                                        "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps 150nz",
                                        "color" = palette[[6]])
KaMinPar_PairwiseLP_geom_shrinking_nz200 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking", sep = "\\"),
                                        "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_pairwise-lp__eps0__nz200k__2023-02-27_17-19-16.csv",
                                        "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps 200nz",
                                        "color" = palette[[7]])
KaMinPar_PairwiseLP_geom_shrinking_nz500 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking", sep = "\\"),
                                        "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_pairwise-lp__eps0__nz500k__2023-02-27_17-19-16.csv",
                                        "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps 500nz",
                                        "color" = palette[[8]])

# ================== KaMinPar-PairwiseLP geom-shrinking lazy-ILP ======================
KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz50 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking__lazy-ILP", sep = "\\"),
                                             "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz50k__2023-02-27_19-47-56.csv",
                                             "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 50nz",
                                             "color" = palette[[2]])
KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz100 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking__lazy-ILP", sep = "\\"),
                                              "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz100k__2023-02-27_19-47-56.csv",
                                              "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 100nz",
                                              "color" = palette[[3]])
KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz150 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking__lazy-ILP", sep = "\\"),
                                              "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz150k__2023-02-27_19-47-56.csv",
                                              "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 150nz",
                                              "color" = palette[[4]])
KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz200 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking__lazy-ILP", sep = "\\"),
                                              "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz200k__2023-02-27_19-47-56.csv",
                                              "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 200nz",
                                              "color" = palette[[5]])
KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz500 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__geometric-shrinking__lazy-ILP", sep = "\\"),
                                              "name" = "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz500k__2023-02-27_19-47-56.csv",
                                              "algo_name" = "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 500nz",
                                              "color" = palette[[6]])


# ================== KaMinPar-PairwiseLP no-shrinking lazy-ILP ======================
KaMinPar_PairwiseLP_no_shrinking_lazy_ilp_nz50 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                                      "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_pairwise-lp__eps0__nz50k__2023-02-27_19-47-56.csv",
                                                      "algo_name" = "KaMinPar-PairwiseLP-lazyILP 50nz",
                                                      "color" = palette[[6]])
KaMinPar_PairwiseLP_no_shrinking_lazy_ilp_nz100 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                                    "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_pairwise-lp__eps0__nz100k__2023-02-27_19-47-56.csv",
                                                    "algo_name" = "KaMinPar-PairwiseLP-lazyILP 100nz",
                                                    "color" = palette[[7]])
KaMinPar_PairwiseLP_no_shrinking_lazy_ilp_nz150 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                                    "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_pairwise-lp__eps0__nz150k__2023-02-27_19-47-56.csv",
                                                    "algo_name" = "KaMinPar-PairwiseLP-lazyILP 150nz",
                                                    "color" = palette[[8]])
KaMinPar_PairwiseLP_no_shrinking_lazy_ilp_nz200 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                                    "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_pairwise-lp__eps0__nz200k__2023-02-27_19-47-56.csv",
                                                    "algo_name" = "KaMinPar-PairwiseLP-lazyILP 200nz",
                                                    "color" = palette[[9]])
KaMinPar_PairwiseLP_no_shrinking_lazy_ilp_nz500 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                                    "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_pairwise-lp__eps0__nz500k__2023-02-27_19-47-56.csv",
                                                    "algo_name" = "KaMinPar-PairwiseLP-lazyILP 500nz",
                                                    "color" = palette[[5]])


# ================== KaMinPar-PairwiseLP no-shrinking lazy-ILP max cut increase compare ======================
KaMinPar_PairwiseLP_lazy_ilp_cut0_1 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                                      "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_cutIncrease0.01_pairwise-lp__eps0__nz150k__2023-03-06_19-57-53.csv",
                                                      "algo_name" = "KaMinPar-PairwiseLP-lazyILP cutIncrease 0-01",
                                                      "color" = palette[[2]])
KaMinPar_PairwiseLP_lazy_ilp_cut0_2 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                         "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_cutIncrease0.02_pairwise-lp__eps0__nz150k__2023-03-06_19-57-53.csv",
                                         "algo_name" = "KaMinPar-PairwiseLP-lazyILP cutIncrease 0-02",
                                         "color" = palette[[3]])
KaMinPar_PairwiseLP_lazy_ilp_cut0_4 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                         "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_cutIncrease0.04_pairwise-lp__eps0__nz150k__2023-03-06_19-57-53.csv",
                                         "algo_name" = "KaMinPar-PairwiseLP-lazyILP cutIncrease 0-04",
                                         "color" = palette[[4]])
KaMinPar_PairwiseLP_lazy_ilp_cut0_8 <- c("base_dir" = paste(kaminpar_dir, "PairwiseLP__no-shrinking__lazy-ILP", sep = "\\"),
                                         "name" = "kaminpar_only-ref-[KaHIP]_lazy-ilp_cutIncrease0.08_pairwise-lp__eps0__nz150k__2023-03-06_19-57-53.csv",
                                         "algo_name" = "KaMinPar-PairwiseLP-lazyILP cutIncrease 0-08",
                                         "color" = palette[[5]])


# ================== KaMinPar-Constrained-V-Cycle ======================
source("./Constrained-V-Cycle.R")


t <- create_performance_profile(list(KaHIP_single, KaMinPar))
t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP, KaMinPar_PairwiseLP_geom_shrinking))
t <- create_performance_profile(list(KaMinPar_PairwiseLP, KaMinPar_PairwiseLP_geom_shrinking, KaMinPar_PairwiseLP_manyILPs, KaMinPar_PairwiseLP_rand2))
t <- create_performance_profile(list(KaMinPar_after_KaHIP, KaMinPar_PairwiseLP, KaMinPar_PairwiseLP_geom_shrinking))
t <- create_performance_profile(list(KaHIP_evolutionary, KaHIP_single, KaMinPar_PairwiseLP_geom_shrinking))

# ======================== Compare NZs =====================================
t <- create_performance_profile(list(KaMinPar_PairwiseLP_geom_shrinking_nz50, KaMinPar_PairwiseLP_geom_shrinking_nz100, KaMinPar_PairwiseLP_geom_shrinking_nz150), dry_run = F)
t <- create_performance_profile(list(KaMinPar_PairwiseLP_geom_shrinking_nz50, KaMinPar_PairwiseLP_geom_shrinking_nz150, KaMinPar_PairwiseLP_geom_shrinking_nz500), dry_run = F)
t <- create_performance_profile(list(KaMinPar_PairwiseLP_geom_shrinking_nz150, KaMinPar_PairwiseLP_geom_shrinking_nz200, KaMinPar_PairwiseLP_geom_shrinking_nz500))
t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_geom_shrinking_nz50, KaMinPar_PairwiseLP_geom_shrinking_nz150))
t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_geom_shrinking_nz500, KaMinPar_PairwiseLP_geom_shrinking_nz150))
t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_geom_shrinking_nz500, KaMinPar_PairwiseLP_geom_shrinking_nz50))


t <- create_performance_profile(list(KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz50, KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz100, KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz150), dry_run = F)
t <- create_performance_profile(list(KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz150, KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz200, KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz500))


t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_geom_shrinking_nz500, KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz500))


t <- create_performance_profile(list(KaMinPar_PairwiseLP_lazy_ilp_cut0_1, KaMinPar_PairwiseLP_lazy_ilp_cut0_2, KaMinPar_PairwiseLP_lazy_ilp_cut0_4))
t <- create_performance_profile(list(KaMinPar_PairwiseLP_lazy_ilp_cut0_2, KaMinPar_PairwiseLP_lazy_ilp_cut0_4, KaMinPar_PairwiseLP_lazy_ilp_cut0_8))

t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_lazy_ilp_cut0_1, KaMinPar_PairwiseLP_lazy_ilp_cut0_2))
t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_lazy_ilp_cut0_4, KaMinPar_PairwiseLP_lazy_ilp_cut0_8))


t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_lazy_ilp_cut0_1, KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz150))
t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_no_shrinking_lazy_ilp_nz150, KaMinPar_PairwiseLP_geom_shrinking_lazy_ilp_nz150))

t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_no_shrinking_lazy_ilp_nz150, KaMinPar_PairwiseLP_geom_shrinking_nz150))
t <- create_performance_profile(list(KaHIP_single, KaMinPar_PairwiseLP_lazy_ilp_cut0_1, KaMinPar_PairwiseLP_geom_shrinking_nz150))

t <- create_performance_profile(list(KaMinPar_PairwiseLP_lazy_ilp_cut0_1, KaMinPar_PairwiseLP_geom_shrinking_nz150), dry_run = F)






t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_02_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_04_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_08_maxClusterWeight0_01), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_02_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_04_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_08_maxClusterWeight0_03), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_02_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_04_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_08_maxClusterWeight0_05), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_02_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_04_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_08_maxClusterWeight0_10), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_20, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_02_maxClusterWeight0_20, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_04_maxClusterWeight0_20, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_08_maxClusterWeight0_20), dry_run = F)

t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best, KaMinPar_Constr_V_Cycle_maxClusterWeight0_03_best, KaMinPar_Constr_V_Cycle_maxClusterWeight0_05_best), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_maxClusterWeight0_05_best, KaMinPar_Constr_V_Cycle_maxClusterWeight0_10_best, KaMinPar_Constr_V_Cycle_maxClusterWeight0_20_best), dry_run = F)

t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best, KaMinPar_PairwiseLP_lazy_ilp_cut0_1), dry_run = F)
t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_04_maxClusterWeight0_01, KaMinPar_PairwiseLP_lazy_ilp_cut0_1), dry_run = F)
t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_maxClusterWeight0_03_best, KaMinPar_PairwiseLP_lazy_ilp_cut0_1), dry_run = F)
t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_maxClusterWeight0_05_best, KaMinPar_PairwiseLP_lazy_ilp_cut0_1), dry_run = F)


t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best, KaMinPar_PairwiseLP_lazy_ilp_cut0_1), dry_run = F)


# No imbalance increase
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_02_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_04_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_08_maxClusterWeight0_01), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_02_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_04_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_08_maxClusterWeight0_03), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_02_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_04_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_08_maxClusterWeight0_05), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_02_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_04_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_08_maxClusterWeight0_10), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_20, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_02_maxClusterWeight0_20, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_04_maxClusterWeight0_20, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_08_maxClusterWeight0_20), dry_run = F)

t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_01), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_03), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_05), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_10), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_20, KaMinPar_Constr_V_Cycle_lazy_ilp_cut0_01_maxClusterWeight0_20), dry_run = F)

t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_05), dry_run = F)
t <- create_performance_profile(list(KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_20), dry_run = F)

t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_01, KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best), dry_run = F)
t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_03, KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best), dry_run = F)
t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_05, KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best), dry_run = F)
t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_10, KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best), dry_run = F)
t <- create_performance_profile(list(KaHIP_single, KaMinPar_Constr_V_Cycle_lazy_ilp_no_imbalance_increase_cut0_01_maxClusterWeight0_20, KaMinPar_Constr_V_Cycle_maxClusterWeight0_01_best), dry_run = F)

