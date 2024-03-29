source("../create_performance_profile.R", chdir = T)
source("../create_special_ilp_plots.R", chdir = T)
source("../create_time_plots.R", chdir = T)
library(tidyr)

experiment_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_KaMinPar"
output_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_KaMinPar"
kaminpar_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaMinPar\\runs"
kahip_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP\\runs"

###############################################################################
###############################################################################
############################# READ DATA #######################################
###############################################################################

kahip_evo <- read.csv(paste(experiment_dir, "kahip_full_default_2022-11-27_13-40-55.csv", sep = "\\"), header = T)
kahip_evo <- aggreg_data(kahip_evo, 70000, 0) %>% drop_na()
kahip_evo$algorithm <- "KaHIP-evolutionary"

kahip_single <- read.csv(paste(experiment_dir, "kahip_full_without_evolutionary_eps_0_2022-11-29_17-53-04.csv", sep = "\\"), header = T)
kahip_single <- aggreg_data(kahip_single, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kahip_single$algorithm <- "KaHIP-single"

kahip_after_kaminpar <- read.csv(paste(kahip_dir, "kahip_only-ref_kaminpar_eps_0_2023-02-04_00-42-20.csv", sep = "\\"), header = T)
kahip_after_kaminpar <- aggreg_data(kahip_after_kaminpar, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kahip_after_kaminpar$algorithm <- "KaHIP after KaMinPar"

kaminpar <- read.csv(paste(kaminpar_dir, "kaminpar_full_default_eps_0_2022-12-14_13-50-13.csv", sep = "\\"), header = T)
kaminpar <- aggreg_data(kaminpar, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar$algorithm <- "KaMinPar"

kaminpar_ilp <- read.csv(paste(kaminpar_dir, "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_13-23-45_60s.csv", sep = "\\"), header = T)
kaminpar_ilp <- aggreg_data(kaminpar_ilp, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_ilp$algorithm <- "KaMinPar-ILP 20s"

kaminpar_ilp_120s <- read.csv(paste(kaminpar_dir, "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_13-56-33_120s.csv", sep = "\\"), header = T)
kaminpar_ilp_120s <- aggreg_data(kaminpar_ilp_120s, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_ilp_120s$algorithm <- "KaMinPar-ILP 120s"

kaminpar_ilp_600s <- read.csv(paste(kaminpar_dir, "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_16-48-40_600s.csv", sep = "\\"), header = T)
kaminpar_ilp_600s <- aggreg_data(kaminpar_ilp_600s, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_ilp_600s$algorithm <- "KaMinPar-ILP 600s"

kaminpar_ilp_no_balance <- read.csv(paste(kaminpar_dir, "kaminpar_full_ilp_pairwise-fm_no-balance_eps_0_2022-12-20_05-17-54.csv", sep = "\\"), header = T)
kaminpar_ilp_no_balance <- aggreg_data(kaminpar_ilp_no_balance, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_ilp_no_balance$algorithm <- "KaMinPar-ILP no balance"

kaminpar_fixed_after_kahip <- read.csv(paste(kaminpar_dir, "kaminpar_only-refinement_ilp_pairwise-fm_no-balance_kahip_no-neg-cycle_eps_0_2022-12-20_05-18-05.csv", sep = "\\"), header = T)
kaminpar_fixed_after_kahip <- aggreg_data(kaminpar_fixed_after_kahip, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_fixed_after_kahip$algorithm <- "KaMinPar-ILP after KaHIP fixed"


fixed_kaminpar_batched_after_kahip <- read.csv(paste(kaminpar_dir, "kaminpar_only-ref_move-batches_2023-01-30_01-13-38.csv", sep = "\\"), header = T)
fixed_kaminpar_batched_after_kahip <- aggreg_data(fixed_kaminpar_batched_after_kahip, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
fixed_kaminpar_batched_after_kahip$algorithm <- "KaMinPar-Move-Batches-ILP after KaHIP fixed"




kaminpar_pariwiseLP <- read.csv(paste(kaminpar_dir, "kaminpar_only-ref-[KaHIP]_geometric-shrinking_pairwise-lp_eps_0_2023-02-20_03-25-02.csv", sep = "\\"), header = T)
kaminpar_pariwiseLP <- aggreg_data(kaminpar_pariwiseLP, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_pariwiseLP$algorithm <- "KaMinPar-PairwiseLP"


kaminpar_ilp_lazy <- read.csv(paste(kaminpar_dir, "kaminpar_only-ref_ilp-lazy_pairwise-fm_no-balance_kahip_no-neg-cycle_eps_0_2023-02-06_02-55-37.csv", sep = "\\"), header = T)
kaminpar_ilp_lazy <- aggreg_data(kaminpar_ilp_lazy, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_ilp_lazy$algorithm <- "KaMinPar-ILP-lazy"

kaminpar_batched_ilp_lazy <- read.csv(paste(kaminpar_dir, "kaminpar_only-ref_ilp_pairwise-fm_kaminpar_eps_0_2023-02-04_00-42-20.csv", sep = "\\"), header = T)
kaminpar_batched_ilp_lazy <- aggreg_data(kaminpar_batched_ilp_lazy, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_batched_ilp_lazy$algorithm <- "KaMinPar-Batched-ILP-lazy"



############################# After KaMinPar ###############################

kaminpar_ilp_after_kaminpar <- read.csv(paste(kaminpar_dir, "kaminpar_only-ref_ilp_pairwise-fm_kaminpar_eps_0_2023-02-04_00-42-20.csv", sep = "\\"), header = T)
kaminpar_ilp_after_kaminpar <- aggreg_data(kaminpar_ilp_after_kaminpar, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_ilp_after_kaminpar$algorithm <- "KaMinPar-ILP after KaMinPar"

kaminpar_batched_ilp_after_kaminpar <- read.csv(paste(kaminpar_dir, "kaminpar_only-ref_move-batches-ilp_pairwise-fm_kaminpar_eps_0_2023-02-04_00-42-20.csv", sep = "\\"), header = T)
kaminpar_batched_ilp_after_kaminpar <- aggreg_data(kaminpar_batched_ilp_after_kaminpar, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_batched_ilp_after_kaminpar$algorithm <- "KaMinPar-Batched-ILP after KaMinPar"

############################# Shrinking methods ###############################
kaminpar_geometric_after_kahip <- read.csv(paste(kaminpar_dir, "kaminpar_only-ref_geometric-shrinking_ilp_pairwise-fm_initial-kahip_eps_0_2023-01-23_06-22-54.csv", sep = "\\"), header = T)
kaminpar_geometric_after_kahip <- aggreg_data(kaminpar_geometric_after_kahip, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_geometric_after_kahip$algorithm <- "KaMinPar-geometric after KaHIP"

kaminpar_linear_after_kahip <- read.csv(paste(kaminpar_dir, "kaminpar_only-ref_linear-shrinking_ilp_pairwise-fm_initial-kahip_eps_0_2023-01-23_06-22-54.csv", sep = "\\"), header = T)
kaminpar_linear_after_kahip <- aggreg_data(kaminpar_linear_after_kahip, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_linear_after_kahip$algorithm <- "KaMinPar-linear after KaHIP"

###############################################################################
###############################################################################
########################## CREATE GRAPHS ######################################
###############################################################################

# set colors
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
                        "KaMinPar-Batched-ILP-lazy" = palette[[4]])


plot <- performace_plot(list(kahip_single, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP_single-vs-KaMinPar_ILP", width = 22, height = 15)


plot <- performace_plot(list(kahip_evo, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP_evolutionary-vs-KaMinPar_ILP", width = 22, height = 15)


plot <- performace_plot(list(kahip_evo, kahip_single), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP_evolutionary-vs-KaHIP_single", width = 22, height = 15)


plot <- performace_plot(list(kaminpar, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaMinPar-vs-KaMinPar_ILP", width = 22, height = 15)


plot <- performace_plot(list(kaminpar_ilp, kaminpar_ilp_120s, kaminpar_ilp_600s), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaMinPar_ILP-Time-limit-comparison", width = 22, height = 15)


plot <- performace_plot(list(kahip_single, kaminpar_ilp_120s), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP_single-vs-KaMinPar_ILP_120s", width = 22, height = 15)


plot <- performace_plot(list(kahip_single, kaminpar_ilp_600s), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP_single-vs-KaMinPar_ILP_600s", width = 22, height = 15)



plot <- performace_plot(list(kaminpar_ilp, kaminpar_ilp_no_balance), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaMinPar_ILP-vs-KaMinPar_ILP_no_balance", width = 22, height = 15)



plot <- performace_plot(list(kaminpar_fixed_after_kahip, kahip_single), objective = "avg_cut")
save_ggplot(plot, output_dir, "FIXED_KaHIP-vs-KaMinPar_ILP_after_KaHIP", width = 22, height = 15)


plot <- performace_plot(list(fixed_kaminpar_batched_after_kahip, kahip_single), objective = "avg_cut")
save_ggplot(plot, output_dir, "FIXED_KaHIP-vs-KaMinPar__Move_Batches_ILP_after_KaHIP", width = 22, height = 15)


plot <- performace_plot(list(kaminpar_ilp_lazy, kahip_single), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP-vs-KaMinPar_ILP_lazy", width = 22, height = 15)

plot <- performace_plot(list(kaminpar_batched_ilp_lazy, kahip_single), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP-vs-KaMinPar_Batched_ILP_lazy", width = 22, height = 15)


############################# After KaMinPar ###############################


plot <- performace_plot(list(kahip_after_kaminpar, kaminpar_ilp_after_kaminpar), objective = "avg_cut")
save_ggplot(plot, output_dir, "init_KaMinPar-KaHIP-vs-KaMinPar_ILP", width = 22, height = 15)

plot <- performace_plot(list(kahip_after_kaminpar, kaminpar_batched_ilp_after_kaminpar), objective = "avg_cut")
save_ggplot(plot, output_dir, "init_KaMinPar-KaHIP-vs-KaMinPar_Batched_ILP", width = 22, height = 15)

plot <- performace_plot(list(kaminpar_batched_ilp_after_kaminpar, kaminpar_ilp_after_kaminpar), objective = "avg_cut")
save_ggplot(plot, output_dir, "init_KaMinPar-KaMinPar_ILP-vs-KaMinPar_Batched_ILP", width = 22, height = 15)


############################# Shrinking methods ###############################
plot <- performace_plot(list(kaminpar_geometric_after_kahip, kahip_single), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP-vs-KaMinPar_geometric", width = 22, height = 15)

plot <- performace_plot(list(kaminpar_geometric_after_kahip, kaminpar_fixed_after_kahip), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaMinPar-no-shrinking-vs-geometric", width = 22, height = 15)

plot <- performace_plot(list(kaminpar_linear_after_kahip, kahip_single), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaHIP-vs-KaMinPar_linear", width = 22, height = 15)

plot <- performace_plot(list(kaminpar_linear_after_kahip, kaminpar_fixed_after_kahip), objective = "avg_cut")
save_ggplot(plot, output_dir, "KaMinPar-no-shrinking-vs-linear", width = 22, height = 15)



###############################################################################
###############################################################################
########################## CREATE ILP GRAPHS ##################################
###############################################################################
improve_algo_names <- function(df) {
  df$algorithm <- sapply(df$algorithm, function(algo) {
    algorithm <- algo
    if(algo == "kaminpar_full_default_eps_0_2022-12-14_13-50-13") {
      algorithm <- "KaMinPar"
    }
    if(algo == "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_13-23-45_60s") {
      algorithm <- "ILP 60s"
    }
    if(algo == "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_13-56-33_120s") {
      algorithm <- "ILP 120s"
    }
    if(algo == "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_16-48-40_600s") {
      algorithm <- "ILP 600s"
    }
    if(algo == "kaminpar_only-refinement_ilp_pairwise-fm_no-balance_kahip_no-neg-cycle_eps_0_2022-12-20_05-18-05") {
      algorithm <- "ILP-KaHIP"
    }
    if(algo == "kaminpar_full_ilp_pairwise-fm_no-balance_eps_0_2022-12-20_05-17-54") {
      algorithm <- "ILP no balance"
    }
    if(algo == "kaminpar_full_move-batches-ilp_pairwise-fm_no-balance_kahip_no-neg-cycle_eps_0_2023-01-02_17-44-24") {
      algorithm <- "MB-ILP"
    }
    if(algo == "kahip_full_default_2022-11-27_13-40-55") {
      algorithm <- "KaHIP-evolutionary"
    }
    if(algo == "kahip_full_without_evolutionary_eps_0_2022-11-29_17-53-04") {
      algorithm <- "KaHIP-single"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_pairwise-lp_eps_0_2023-02-20_03-25-02") {
      algorithm <- "KaMinPar-PairwiseLP"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_geometric-shrinking_pairwise-lp_eps_0_2023-02-20_03-25-02") {
      algorithm <- "KaMinPar-PairwiseLP-LazyILP"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz50k__2023-02-27_19-47-56") {
      algorithm <- "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 50nz"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz100k__2023-02-27_19-47-56") {
      algorithm <- "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 100nz"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz150k__2023-02-27_19-47-56") {
      algorithm <- "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 150nz"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz200k__2023-02-27_19-47-56") {
      algorithm <- "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 200nz"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_geometric-shrinking_lazy-ilp_pairwise-lp__eps0__nz500k__2023-02-27_19-47-56") {
      algorithm <- "KaMinPar-PairwiseLP-shrinking_eps-lazyILP 500nz"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_lazy-ilp_cutIncrease0.01_pairwise-lp__eps0__nz150k__2023-03-06_19-57-53") {
      algorithm <- "KaMinPar-PairwiseLP-lazyILP cutIncrease 0-01"
    }
    if(algo == "kaminpar_only-ref-[KaHIP]_lazy-ilp_pairwise-lp__eps0__nz150k__2023-02-27_19-47-56") {
      algorithm <- "KaMinPar-PairwiseLP-lazyILP 150nz"
    }
    return(algorithm)
  })
  
  if (df$algorithm[[1]] == "KaHIP") {
    df$num_threads = 1
  }
  return(df %>% filter(!is.na(graph)))
}

create_timed_out_ilp_plot(kaminpar_dir, "timed_out_ilps.pdf", output_dir, filter_data = improve_algo_names)


experiment_dir_running_times <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\TMP_PARSE_RUNNING_TIMES"
create_running_time_box_plot(experiment_dir_running_times, "running_times.pdf", experiment_dir_running_times, epsilon = 0, filter_data = improve_algo_names)



output_dir <- paste(output_dir, "transformed", sep = "\\")

plot <- performace_plot(list(kahip_transformed, kaminpar_ilp_after_kahip), objective = "avg_cut")
save_ggplot(plot, output_dir, "transformed_KaHIP_single-vs-KaMinPar_ILP_after_KaHIP", width = 22, height = 15)


plot <- performace_plot(list(kahip_transformed, kaminpar_move_batch_ilp_after_kahip), objective = "avg_cut")
save_ggplot(plot, output_dir, "transformed_KaHIP_single-vs-KaMinPar__Move-Batches-ILP_after_KaHIP", width = 22, height = 15)


plot <- performace_plot(list(kahip_transformed, kaminpar_move_batch_ilp_after_kahip_plus), objective = "avg_cut")
save_ggplot(plot, output_dir, "transformed_KaHIP_single-vs-KaMinPar__Move-Batches-ILP_plus_after_KaHIP", width = 22, height = 15)



