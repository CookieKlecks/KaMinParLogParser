source("../create_performance_profile.R", chdir = T)
library(tidyr)

experiment_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\parsed_results"


################################ READ DATA ####################################

kahip <- read.csv(paste(experiment_dir, "kahip_full_default_2022-11-27_13-40-55_eps0.csv", sep = "\\"), header = T)
kahip <- aggreg_data(kahip, 70000, 0) %>% drop_na()
kahip$algorithm <- "KaHIP 0%"

kahip_eps_0_1 <- read.csv(paste(experiment_dir, "kahip_full_default_2022-11-27_14-55-39_eps0.01.csv", sep = "\\"), header = T)
kahip_eps_0_1 <- aggreg_data(kahip_eps_0_1, 70000, 0) %>% drop_na()
kahip_eps_0_1$algorithm <- "KaHIP 1%"


kahip_no_evolutionary_eps_0 <- read.csv(paste(experiment_dir, "kahip_full_without_evolutionary_eps_0_2022-11-29_17-53-04.csv", sep = "\\"), header = T)
kahip_no_evolutionary_eps_0 <- aggreg_data(kahip_no_evolutionary_eps_0, 70000, 0) %>% drop_na()
kahip_no_evolutionary_eps_0$algorithm <- "KaHIP (no evolutionary) 0%"

kahip_no_evolutionary_eps_0_1 <- read.csv(paste(experiment_dir, "kahip_full_without_evolutionary_eps_0.01_2022-11-29_17-53-04.csv", sep = "\\"), header = T)
kahip_no_evolutionary_eps_0_1 <- aggreg_data(kahip_no_evolutionary_eps_0_1, 70000, 0) %>% drop_na()
kahip_no_evolutionary_eps_0_1$algorithm <- "KaHIP (no evolutionary) 1%"


kaminpar <- read.csv(paste(experiment_dir, "kaminpar_full_default_2022-11-19_11-25-49.csv", sep = "\\"), header = T)
kaminpar <- aggreg_data(kaminpar, 70000, 0) %>% filter(graph %in% kahip$graph)
kaminpar$algorithm <- "KaMinPar 0%"
kaminpar <- kaminpar %>% filter(k < 64)


mt_kahypar_eps_0_1 <- read.csv(paste(experiment_dir, "mt-kahypar_full_quality_flows_2022-11-27_14-55-39_eps0.01.csv", sep = "\\"), header = T)
mt_kahypar_eps_0_1 <- aggreg_data(mt_kahypar_eps_0_1, 70000, 0.01) %>% filter(graph %in% kahip$graph)
mt_kahypar_eps_0_1$algorithm <- "Mt-KaHyPar 1%"


mt_kahypar_eps_0_3 <- read.csv(paste(experiment_dir, "mt-kahypar_full_quality_flows_2022-11-28_19-56-11_eps0.03.csv", sep = "\\"), header = T)
mt_kahypar_eps_0_3 <- aggreg_data(mt_kahypar_eps_0_3, 70000, 0.03) %>% filter(graph %in% kahip$graph)
mt_kahypar_eps_0_3$algorithm <- "Mt-KaHyPar 3%"


mt_kahypar_eps_1_0 <- read.csv(paste(experiment_dir, "mt-kahypar_full_quality_flows_2022-11-28_19-56-11_eps0.10.csv", sep = "\\"), header = T)
mt_kahypar_eps_1_0 <- aggreg_data(mt_kahypar_eps_1_0, 70000, 0.10) %>% filter(graph %in% kahip$graph)
mt_kahypar_eps_1_0$algorithm <- "Mt-KaHyPar 10%"


kahip_mt_kahypar_eps_0_1 <- read.csv(paste(experiment_dir, "kahip_only-negative-cycle_mt-kahypar_0.01eps_2022-11-22_10-45-35.csv", sep = "\\"), header = T)
kahip_mt_kahypar_eps_0_1 <- aggreg_data(kahip_mt_kahypar_eps_0_1, 70000, 0) %>% filter(graph %in% kahip$graph)
kahip_mt_kahypar_eps_0_1$algorithm <- "KaHIP after Mt-KaHyPar 1%"


kahip_mt_kahypar_eps_0_3 <- read.csv(paste(experiment_dir, "kahip_only-negative-cycle_mt-kahypar_0.03eps_2022-11-22_11-35-43.csv", sep = "\\"), header = T)
kahip_mt_kahypar_eps_0_3 <- aggreg_data(kahip_mt_kahypar_eps_0_3, 70000, 0.01) %>% filter(graph %in% kahip$graph)
kahip_mt_kahypar_eps_0_3$algorithm <- "KaHIP after Mt-KaHyPar 3%"



############################# CREATE PLOTS ####################################

#algo_color_mapping <- brewer.pal(n = 9, name = "Set1")
palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("KaHIP 0%" = palette[[1]],
                        "KaHIP 1%" = palette[[2]],
                        "KaHIP (no evolutionary) 0%" = palette[[9]],
                        "KaHIP (no evolutionary) 1%" = palette[[9]],
                        "KaMinPar 0%" = palette[[3]],
                        "Mt-KaHyPar 1%" = palette[[4]],
                        "Mt-KaHyPar 3%" = palette[[5]],
                        "Mt-KaHyPar 10%" = palette[[6]],
                        "KaHIP after Mt-KaHyPar 1%" = palette[[7]],
                        "KaHIP after Mt-KaHyPar 3%" = palette[[8]])

plot <- performace_plot(list(kahip, kaminpar), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_KaMinPar", width = 22, height = 15)

plot <- performace_plot(list(kahip, mt_kahypar_eps_0_1), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_Mt-KaHyPar_0-01", width = 22, height = 15)

plot <- performace_plot(list(kahip, kaminpar, mt_kahypar_eps_0_1), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_KaMinPar_vs_Mt-KaHyPar", width = 22, height = 15)

plot <- performace_plot(list(kahip, mt_kahypar_eps_0_1, kahip_mt_kahypar_eps_0_1), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_Mt-KaHyPar_vs_KaHIP-after-Mt-KaHyPar", width = 22, height = 15)

plot <- performace_plot(list(kahip, mt_kahypar_eps_0_3, kahip_mt_kahypar_eps_0_3), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_Mt-KaHyPar_vs_KaHIP-after-Mt-KaHyPar_0-03", width = 22, height = 15)

plot <- performace_plot(list(kahip, mt_kahypar_eps_0_1, mt_kahypar_eps_0_3), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_Mt-KaHyPar_0-01_vs_0-03", width = 22, height = 15)

plot <- performace_plot(list(kahip, mt_kahypar_eps_0_1, mt_kahypar_eps_1_0), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_Mt-KaHyPar_0-01_vs_0-10", width = 22, height = 15)

plot <- performace_plot(list(kahip, kahip_eps_0_1), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP-eps0_vs_KaHIP-eps0-01", width = 22, height = 15)

plot <- performace_plot(list(kahip_mt_kahypar_eps_0_1, kahip_mt_kahypar_eps_0_3), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP-Vergleich-initial-solutions", width = 22, height = 15)

plot <- performace_plot(list(kahip_no_evolutionary_eps_0, kahip, kaminpar), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP-Vergleich-without-evolutionary", width = 22, height = 15)

plot <- performace_plot(list(kahip_no_evolutionary_eps_0, kahip_mt_kahypar_eps_0_1), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP-without-evolutionary_vs_KaHIP-after-MT-KaHyPar_0-01", width = 22, height = 15)


test <- inner_join(kahip, kahip_eps_0_1, by = c("graph", "k")) %>%
  mutate(KaHIP = avg_cut.x, KaHIP_eps_0_1 = avg_cut.y, graph = basename(graph), diff = KaHIP_eps_0_1 - KaHIP) %>%
  dplyr::select(graph, k, KaHIP, KaHIP_eps_0_1, diff) %>%
  filter(KaHIP < KaHIP_eps_0_1)


