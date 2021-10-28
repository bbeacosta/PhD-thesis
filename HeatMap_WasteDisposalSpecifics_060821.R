install.packages("ellipsis")
install.packages("pillar")
install.packages("broom")

### Load libraries
library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)

### Set workin directory and load files ###

# setwd("C:/Users/skgtbco/OneDrive - University College London/PhD folder/PPI Networks/WPPINA/December2019/GenePrioritisation/Targets/")
# getwd()
# 
# df <- read_excel(path = "Targets_HeatMap.xlsx", col_names = T)

setwd("C:/Users/skgtbco/OneDrive - University College London/PhD folder/PPI Networks/WPPINA/December2019/CoreIIHs/")
getwd()

df <- read_excel(path = "gProfiler_CoreIIHs_1stEnrichment_10-02-2020.xlsx", sheet = "HeatMap_input_R_06-02-2020", col_names = T)


# theme and style

theme_bc <-  theme_bw(base_family = "Helvetica") + 
  theme(panel.grid.major.x = element_blank(),
        legend.position = "right",
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 0.6),
        axis.title = element_text(size = 12),
        panel.spacing = unit(0.1, "lines"),
        legend.text=element_text(size=8))

# Data wrangling and plotting 
# ggplot <- df %>%
#   dplyr::group_by(Analysis.Module, Semantic.Classes, Adj.pvalue) %>% 
#   dplyr::summarise(n_GO_terms = n()) %>%
#   ggplot(aes(x = Analysis.Module, y = Semantic.Classes)) +
# geom_tile(aes(fill = n_GO_terms, size = Adj.pvalue), colour = "black") +    
#   # facet_grid(rows = vars(Semantic.classes), scales = "free_y", space = "free_y") +
#   scale_fill_viridis_c(na.value = "grey") +
#   labs(x = "Molecular pathways", y = "Modules/WPPINA") +
#   theme_bc + 
#   theme(panel.grid = element_blank(),
#         strip.text.y = element_text(size = 10, angle = 0))
  
dotplot <- df %>% 
  dplyr::group_by(Analysis.Module, Semantic.Classes) %>% 
  dplyr::summarise(min_adj_pvalue = min(Adj.pvalue),n_GO_terms = n()) %>% 
  mutate(min_adj_pvalue=-log10(as.numeric(min_adj_pvalue))) %>% 
  ggplot(mapping= aes(x = Analysis.Module, y = Semantic.Classes)) +
  geom_point(mapping = aes(size = n_GO_terms, color = min_adj_pvalue))+
  scale_size_continuous(name="area", range = c(1,20))+
  # theme_classic()+
  # scale_fill_viridis_c(na.value = "grey") +
  labs(x = "Input gene list", y = element_blank()) +
  theme_bc +
  theme(panel.grid = element_blank())+
        # strip.text.y = element_text(size = 10, angle = 0), 
        # axis.text.x = element_text(size = 12, vjust = 1, angle = 45))+ #vjust is the distance between the x-label and the plot
  ggtitle("Molecular function")+
  guides(color=guide_legend("Prop. enriched terms"), size = guide_legend("Adj. p-value (-log10)")) #to change legend titles

print(dotplot)


#save
ggsave(filename = "Heatmap_190821.png", 
       plot = dotplot, 
       width = 10,
       path = "C:/Users/skgtbco/OneDrive - University College London/PhD folder/PPI Networks/WPPINA/December2019/CoreIIHs/", 
       height = 7, 
       dpi = 200) 
