source("../create_performance_profile.R", chdir = T)
library(tidyr)


experiment_dir <- "E:\\Cedrico\\KIT\\HiWi\\parse_results\\KaHIP_eps0"

KaHIP <- read_csv_into_df(experiment_dir) %>% drop_na()

KaHIP$km1 <- KaHIP$cut
KaHIP$Epsilon <- 0.00
aggreg_KaHIP <- aggreg_data(KaHIP, 7200, 0, 3) %>% dplyr::select(graph, k, epsilon, min_cut, avg_cut)
aggreg_KaHIP$graph <- sapply(aggreg_KaHIP$graph, str_replace_all, " ", "_")


Optimal <- read.csv("E:\\Cedrico\\KIT\\HiWi\\bbgb_paper.csv", header=TRUE) %>% 
  mutate(graph = Graph, k = K) %>%
  dplyr::select(graph, k, Opt, Category, Epsilon)


joined <- inner_join(aggreg_KaHIP, Optimal, by = c("graph", "k")) %>%
  mutate(abs_diff = avg_cut - Opt, rel_diff = (avg_cut - Opt) / Opt) %>%
  dplyr::select(graph, k, Category, Epsilon, Opt, min_cut, avg_cut, abs_diff, rel_diff)

KaHIP_vs_bbgb_paper <- joined %>%
  mutate(Graph = graph, K = k, KaHIP = min_cut) %>%
  dplyr::select(Graph, Category, Epsilon, Opt, KaHIP, abs_diff, rel_diff, K)

write.csv(KaHIP_vs_bbgb_paper, file = paste(experiment_dir, "KaHIP_vs_bbgb_paper.csv", sep = "/"), row.names = F, quote = F)

KaHIP_k_2 <- KaHIP_vs_bbgb_paper %>%
  dplyr::select(Graph, Category, Epsilon, KaHIP, K)
write.csv(KaHIP_k_2, file = paste(experiment_dir, "KaHIP.csv", sep = "/"), row.names = F, quote = F)

################ Create performance profile ###################################
filtered_KaHIP <- joined %>% dplyr::select(graph, k, avg_cut) %>%
  mutate(timeout = F, infeasible = F, algorithm = "KaHIP")
filtered_Optimal <- joined %>% mutate(avg_cut = Opt) %>% dplyr::select(graph, k, avg_cut) %>%
  mutate(timeout = F, infeasible = F, algorithm = "Best")

algo_color_mapping <- brewer.pal(n = 9, name = "Set1")


plot <- performace_plot(list(filtered_KaHIP, filtered_Optimal),
                        objective = "avg_cut")
save_ggplot(plot, experiment_dir, "KaHIP_vs_bbgb_paper.pdf", height = 15, width = 22)
