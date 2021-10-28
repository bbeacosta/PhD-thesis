install.packages("scales") 
install.packages("tidyverse") 
install.packages("viridis")

library(ggplot2)
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(viridis)

### Set working directory and load files ###

setwd("C:/Users/skgtbco/OneDrive - University College London/PhD folder/Gene Co-Exp Networks/WGCNA/")
getwd()

df <- read_excel(path = "CoExp_Output_041219_All.xlsx", sheet = "FCTX_Modules_forR", col_names = T)


# data wrangling

df$category <-  gsub("10", "", df$category) # to subtract a common pattern from your df

df2 <- df %>% 
   mutate(mod_db = paste(category, module)) %>%  #to combine to columns in a new one
  mutate(new_order_modules =  factor(x = mod_db, levels = unique(c("UKBEC darkgreen","UKBEC lightyellow",  # to reorder a character vector not by value but just for visuals
                                                            "gtexv6 brown","gtexv6 brown", "gtexv6 brown", 
                                                            "gtexv6 brown", "gtexv6 brown", 
                                                            "UKBEC red", "UKBEC red", "UKBEC red", "UKBEC red", 
                                                            "gtexv6 red", 
                                                            "gtexv6 greenyellow", "UKBEC royalblue", "UKBEC royalblue",
                                                            "gtexv6 lightcyan", "gtexv6 lightcyan", "gtexv6 lightcyan",
                                                            "UKBEC yellow", "UKBEC yellow", "gtexv6 turquoise",
                                                            "UKBEC black", "UKBEC black", "gtexv6 cyan", "UKBEC magenta", 
                                                            "gtexv6 midnightblue","UKBEC tan","gtexv6 magenta",
                                                            "UKBEC blue","gtexv6 darkred", "gtexv6 darkred","UKBEC grey60")))) %>% 
  mutate(new_order_gene =  factor(x = gene, levels = rev(c("DCTN1","TBK1","UBQLN2",   "OPTN",     "C9orf72","VCP","SQSTM1",
                                                                   "TARDBP","CHMP2B","TMEM106B","MAPT","GRN","IFT74","TIA1",
                                                                   "FUS","CHCHD10","HLA-DRA","RAB38" ))))
         

df2$new_order_modules
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


# plotting

ggplot <- ggplot(df2, aes(x = new_order_modules, y = new_order_gene, fill= mm)) + 
  geom_tile()+
  geom_text(aes(label = round(mm, 1))) +
  # scale_fill_viridis(discrete=FALSE)+
  theme_bc+
  scale_x_discrete(name = "Modules" ) +
  scale_y_discrete(name = "Seeds")+
  ggtitle("Seeds co-expression pattern") +
  theme(plot.title = element_text(face = "bold", size = 15))+
  theme(panel.grid = element_blank())+
  labs(fill = "Module membership")+
  scale_fill_gradient(low = "yellow", high = "red") # only for continous values
 
print(ggplot)


#save
ggsave(filename = "Heatmap_Modules_coexpr.png", 
       plot = ggplot, 
       width = 10,
       path = "C:/Users/skgtbco/OneDrive - University College London/PhD folder/Gene Co-Exp Networks/WGCNA/", 
       height = 7, 
       dpi = 200) 

