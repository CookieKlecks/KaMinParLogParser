source("../create_performance_profile.R", chdir = T)
source("../create_special_ilp_plots.R", chdir = T)
library(tidyr)

experiment_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_KaMinPar"
kaminpar_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaMinPar\\runs"

###############################################################################
###############################################################################
############################# READ DATA #######################################
###############################################################################

kahip_evo <- read.csv(paste(experiment_dir, "kahip_full_default_2022-11-27_13-40-55.csv", sep = "\\"), header = T)
kahip_evo <- aggreg_data(kahip_evo, 70000, 0) %>% drop_na()
kahip_evo$algorithm <- "KaHIP-evolutionary"

kahip_single <- read.csv(paste(experiment_dir, "kahip_full_without_evolutionary_eps_0_2022-11-29_17-53-04.csv", sep = "\\"), header = T)
kahip_single <- aggreg_data(kahip_single, 70000, 0) %>% drop_na()
kahip_single$algorithm <- "KaHIP-single"

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

kaminpar_ilp_after_kahip <- read.csv(paste(kaminpar_dir, "kaminpar_only-refinement_ilp_pairwise-fm_no-balance_kahip_no-neg-cycle_eps_0_2022-12-20_05-18-05.csv", sep = "\\"), header = T)
kaminpar_ilp_after_kahip <- aggreg_data(kaminpar_ilp_after_kahip, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_ilp_after_kahip$algorithm <- "KaMinPar-ILP after KaHIP"

kaminpar_ilp_no_balance <- read.csv(paste(kaminpar_dir, "kaminpar_full_ilp_pairwise-fm_no-balance_eps_0_2022-12-20_05-17-54.csv", sep = "\\"), header = T)
kaminpar_ilp_no_balance <- aggreg_data(kaminpar_ilp_no_balance, 70000, 0) %>% filter(graph %in% kahip_evo$graph)
kaminpar_ilp_no_balance$algorithm <- "KaMinPar-ILP no balance"


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
                        "KaHIP after Mt-KaHyPar 3%" = palette[[8]])


plot <- performace_plot(list(kahip_single, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_single-vs-KaMinPar_ILP", width = 22, height = 15)


plot <- performace_plot(list(kahip_evo, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_evolutionary-vs-KaMinPar_ILP", width = 22, height = 15)


plot <- performace_plot(list(kahip_evo, kahip_single), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_evolutionary-vs-KaHIP_single", width = 22, height = 15)


plot <- performace_plot(list(kaminpar, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaMinPar-vs-KaMinPar_ILP", width = 22, height = 15)


plot <- performace_plot(list(kaminpar_ilp, kaminpar_ilp_120s, kaminpar_ilp_600s), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaMinPar_ILP-Time-limit-comparison", width = 22, height = 15)


plot <- performace_plot(list(kahip_single, kaminpar_ilp_120s), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_single-vs-KaMinPar_ILP_120s", width = 22, height = 15)


plot <- performace_plot(list(kahip_single, kaminpar_ilp_600s), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_single-vs-KaMinPar_ILP_600s", width = 22, height = 15)



plot <- performace_plot(list(kahip_single, kaminpar_ilp_after_kahip), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_single-vs-KaMinPar_ILP_after_KaHIP", width = 22, height = 15)


plot <- performace_plot(list(kaminpar_ilp, kaminpar_ilp_no_balance), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaMinPar_ILP-vs-KaMinPar_ILP_no_balance", width = 22, height = 15)


###############################################################################
###############################################################################
########################## CREATE ILP GRAPHS ##################################
###############################################################################

improve_algo_names <- function(df) {
  df$algorithm <- sapply(df$algorithm, function(algo) {
    algorithm <- algo
    if(algo == "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_13-23-45_60s") {
      algorithm <- "KaMinPar ILP 60s"
    }
    if(algo == "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_13-56-33_120s") {
      algorithm <- "KaMinPar ILP 120s"
    }
    if(algo == "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_16-48-40_600s") {
      algorithm <- "KaMinPar ILP 600s"
    }
    if(algo == "kaminpar_only-refinement_ilp_pairwise-fm_no-balance_kahip_no-neg-cycle_eps_0_2022-12-20_05-18-05") {
      algorithm <- "KaMinPar ILP after KaHIP"
    }
    if(algo == "kaminpar_full_ilp_pairwise-fm_no-balance_eps_0_2022-12-20_05-17-54") {
      algorithm <- "KaMinPar ILP no balance"
    }
    return(algorithm)
  })
  return(df)
}

create_timed_out_ilp_plot(kaminpar_dir, "timed_out_ilps.pdf", experiment_dir, filter_data = improve_algo_names)

