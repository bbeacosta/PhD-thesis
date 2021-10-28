# install.packages
# install.packages("radiant")
# install.packages("Rtools")
install.packages("wesanderson")

#library
library(ggplot2)
library (readxl)
library(tidyverse)
library(stringr)
library(RColorBrewer)
library(qdapTools)
library (dplyr)
library(forcats)
library(wesanderson)

# library(radiant)
# library(Rtools)

# load dataset

data <- read_excel("C:/Users/skgtbco/OneDrive - University College London/PhD folder/PPI Networks/WPPINA/December2019/WGCNA/StackPlot_WGCNAEnrichments.xlsx", sheet = "Sheet2")


# Data wrangling

Functional_block_summary <- data %>%
  dplyr:: filter (! Functional_Block %in% c("general","metabolism","physiology", "intracellular organisation")) %>% 
  unique() %>%
  group_by(Functional_Block) %>%
  summarise(number = n()) %>% 
  mutate(new_order=forcats::fct_reorder(.f = Functional_Block, .x = number))

Functional_block_summary$new_order

# Stacked plot
# geom_bar(position="fill") for percent on y-axis

ggplot <- 
  ggplot(data = Functional_block_summary, aes(y = number, x = new_order), colour = "black") + 
  geom_col(aes(fill = number, y = number), colour = "black") +
  coord_flip()+   #change orientation of the plot
  scale_x_discrete(name = "Cellular function", ) +
  scale_y_continuous(name = "Proportion of enriched modules", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme_classic() +   # with no grids in the background
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5), legend.position = "none")+
  theme(axis.text.y = element_text(size = 12, angle = 0), legend.position = "none")+
  ggtitle("Functional enrichment of WGCNA modules") +
  theme(plot.title = element_text(face = "bold", size = 20))+
  scale_fill_gradient(low = "blue", high = "red") # only for continous values

print(ggplot)

# save plot

ggsave(filename = "WGCNA_enrichments_190821.png", 
       plot = ggplot, width = 10, 
       path = "C:/Users/skgtbco/OneDrive - University College London/PhD folder/PPI Networks/WPPINA/December2019/WGCNA/", 
       height = 10, 
       dpi = 200)


  




