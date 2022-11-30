source("../create_performance_profile.R", chdir = T)
library(tidyr)

experiment_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_vs_Mt-KaHyPar"
epsilon <- 0.01
time_limit <- 70000


kahip <- read.csv(paste(experiment_dir, "kahip_full_default_2022-11-27_14-55-39.csv", sep = "\\"), header = T)
kahip <- aggreg_data(kahip, time_limit, epsilon) %>% drop_na()
kahip$algorithm <- "KaHIP 1%"

mt_kahypar <- read.csv(paste(experiment_dir, "mt-kahypar_full_quality_flows_2022-11-27_14-55-39.csv", sep = "\\"), header = T)
mt_kahypar <- aggreg_data(mt_kahypar, time_limit, epsilon) %>% filter(graph %in% kahip$graph)
mt_kahypar$algorithm <- "Mt-KaHyPar 1%"

#algo_color_mapping <- brewer.pal(n = 9, name = "Set1")
palette <- brewer.pal(n = 9, name = "Set1")
algo_color_mapping <- c("KaHIP 0%" = palette[[1]],
                        "KaHIP 1%" = palette[[2]],
                        "KaMinPar 0%" = palette[[3]],
                        "Mt-KaHyPar 1%" = palette[[4]],
                        "Mt-KaHyPar 3%" = palette[[5]],
                        "Mt-KaHyPar 10%" = palette[[6]],
                        "KaHIP after Mt-KaHyPar 1%" = palette[[7]],
                        "KaHIP after Mt-KaHyPar 3%" = palette[[8]])

plot <- performace_plot(list(kahip, mt_kahypar), objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_Mt-KaHyPar", width = 22, height = 15)


