source("../create_performance_profile.R", chdir = T)
library(tidyr)

experiment_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_KaMinPar"

#data <- read_and_aggregate_csv(experiment_dir)

full_data <- read_csv_into_df(experiment_dir)

kahip <- read.csv(paste(experiment_dir, "kahip_full_default_2022-11-15_21-24-34.csv", sep = "\\"), header = T)
kahip <- aggreg_data(kahip, 70000, 0) %>% drop_na()
kahip$algorithm <- "KaHIP"

kaminpar <- read.csv(paste(experiment_dir, "kaminpar_full_default_2022-11-19_11-25-49.csv", sep = "\\"), header = T)
kaminpar <- aggreg_data(kaminpar, 70000, 0) %>% filter(graph %in% kahip$graph)
kaminpar$algorithm <- "KaMinPar"

algo_color_mapping <- brewer.pal(n = 9, name = "Set1")
plot <- performace_plot(list(kahip, kaminpar), objective = "avg_cut")

save_ggplot(plot, experiment_dir, "KaHIP_vs_KaMinPar", width = 22, height = 15)


kahip_mtkahypar_01 <- read.csv(paste(experiment_dir, "kahip_only-negative-cycle_mt-kahypar_0.01eps_2022-11-22_10-45-35.csv", sep = "\\"), header = T)
kahip_mtkahypar_01 <- aggreg_data(kahip_mtkahypar_01, 70000, 0) %>% drop_na()
kahip_mtkahypar_01$algorithm <- "KaHIP-Mt-KaHyPar"

kahip_filtered <- kahip %>% filter(graph %in% kahip_mtkahypar_01$graph) %>% filter(k %in% kahip_mtkahypar_01$k)


plot <- performace_plot(list(kahip_filtered, kahip_mtkahypar_01), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_KaHIP-Mt-KaHyPar", width = 22, height = 15)



kaminpar_filtered <- kaminpar %>% filter(graph %in% kahip_mtkahypar_01$graph) %>% filter(k %in% kahip_mtkahypar_01$k)

plot <- performace_plot(list(kaminpar_filtered, kahip_mtkahypar_01), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaMinPar_vs_KaHIP-Mt-KaHyPar", width = 22, height = 15)
