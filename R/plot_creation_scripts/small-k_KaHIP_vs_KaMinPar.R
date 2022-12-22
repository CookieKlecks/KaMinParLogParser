source("../create_performance_profile.R", chdir = T)
library(tidyr)

experiment_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_KaMinPar"

###############################################################################
###############################################################################
############################# READ DATA #######################################
###############################################################################

kahip_evo <- read.csv(paste(experiment_dir, "kahip_full_default_2022-11-27_13-40-55.csv", sep = "\\"), header = T)
kahip_evo <- aggreg_data(kahip_evo, 70000, 0) %>% drop_na() %>% filter(k < 8)
kahip_evo$algorithm <- "KaHIP-evolutionary"

kahip_single <- read.csv(paste(experiment_dir, "kahip_full_without_evolutionary_eps_0_2022-11-29_17-53-04.csv", sep = "\\"), header = T)
kahip_single <- aggreg_data(kahip_single, 70000, 0) %>% drop_na() %>% filter(k < 8)
kahip_single$algorithm <- "KaHIP-single"

kaminpar <- read.csv(paste(experiment_dir, "kaminpar_full_default_eps_0_2022-12-14_13-50-13.csv", sep = "\\"), header = T)
kaminpar <- aggreg_data(kaminpar, 70000, 0) %>% filter(graph %in% kahip_evo$graph) %>% filter(k < 8)
kaminpar$algorithm <- "KaMinPar"

kaminpar_ilp <- read.csv(paste(experiment_dir, "kaminpar_full_ilp_pairwise-fm_eps_0_2022-12-14_13-23-45.csv", sep = "\\"), header = T)
kaminpar_ilp <- aggreg_data(kaminpar_ilp, 70000, 0) %>% filter(graph %in% kahip_evo$graph) %>% filter(k < 8)
kaminpar_ilp$algorithm <- "KaMinPar-ILP"


###############################################################################
###############################################################################
########################## CREATE GRAPHS ######################################
###############################################################################

# set colors
palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("KaHIP-evolutionary" = palette[[1]],
                        "KaHIP-single" = palette[[2]],
                        "KaMinPar" = palette[[3]],
                        "KaMinPar-ILP" = palette[[4]],
                        "Mt-KaHyPar 3%" = palette[[5]],
                        "Mt-KaHyPar 10%" = palette[[6]],
                        "KaHIP after Mt-KaHyPar 1%" = palette[[7]],
                        "KaHIP after Mt-KaHyPar 3%" = palette[[8]])

output_dir <- paste(experiment_dir, "0_small_k_results", sep = "\\")

plot <- performace_plot(list(kahip_single, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, output_dir, "small_k-KaHIP_single-vs-KaMinPar_ILP", width = 22, height = 15)


plot <- performace_plot(list(kahip_evo, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, output_dir, "small_k-KaHIP_evolutionary-vs-KaMinPar_ILP", width = 22, height = 15)


plot <- performace_plot(list(kahip_evo, kahip_single), objective = "avg_cut")
save_ggplot(plot, output_dir, "small_k-KaHIP_evolutionary-vs-KaHIP_single", width = 22, height = 15)


plot <- performace_plot(list(kaminpar, kaminpar_ilp), objective = "avg_cut")
save_ggplot(plot, output_dir, "small_k-KaMinPar-vs-KaMinPar_ILP", width = 22, height = 15)

