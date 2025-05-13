# Script para plotar arvore de anello
# colocar no diretorio
setwd("C:/Users/macos/OneDrive/Doutorado/Novel_Virus/CoV_Sinop/BLAST/")
dir.create("Rtree")

#Load libraries
library(ggtree) 
library(tidyverse)
library(tidytree)
library(ape)
library(treeio) 
library(phytools)
library("ggforce")
# install.packages("ggforce")

# palheta de cores
cores_biomas <- c(
  "Amazonia" = "#22a884",       
  "Atlantic RainForest" = "#414487", 
  "Caatinga" = "#FDE725",       
  "Pantanal" = "#7ad151",       
  "Cerrado" =  "#440154",
  "Tropical RainForest" = "#2a788e"  
)
  

# load tree file and root by midpoint
tree <- read.tree(file = "90_trim_Cosentino_CoV_nt_aln.treefile") %>%
  midpoint_root()

tree$tip.label

###Read in metadata
metadata_df <- read.table("Cosentino_CoV_Metadata.txt", sep = "\t", header = T, as.is = T, fill = T)

metadata_df$organism_name <- paste(metadata_df$Acc, "CoV",
                                   metadata_df$Host, metadata_df$Country,
                                   metadata_df$Brazilian.State, sep = "|")
metadata_df$organism_name

metadata_df$novel <- gsub(metadata_df$Biome, replacement = NA , pattern = "Other")

# plot initial tree
ggtree(tree)

#get node labels on tree and save it
ggtree(tree)  %<+% metadata_df +
  geom_tiplab() + #aes(label = Name_2), size = 3, parse=T) +
  geom_text(aes(label = node), size = 1, hjust = -0.3)

ggsave("Rtree/colpased_nodess.pdf", width = 30, height = 50, units= "cm", limitsize = FALSE )

#Tree with tips coloured by lineage for country of interest
p <- ggtree(tree, color='black', ladderize = T, right = T) %<+% metadata_df +
  geom_highlight(node = 246, fill = "#BF674B", alpha = 0.4, shape = 21) +
  geom_highlight(node = 282, fill = "#4AB6C0", alpha = 0.4, shape = 21) +
  geom_point2(aes(subset = as.numeric(sub("/.*", "", label))>75 & as.numeric(sub(".*/", "", label))<75 & !isTip),
              colour = "black", size=3, fill = "white", shape = 23, alpha = 5) +
  geom_point2(aes(subset = as.numeric(sub("/.*", "", label))<75 & as.numeric(sub(".*/", "", label))>75 & !isTip),
              colour = "black", size=3, fill = "gray", shape = 23, alpha = 5) +
  geom_point2(aes(subset = as.numeric(sub("/.*", "", label))>75 & as.numeric(sub(".*/", "", label))>75 & !isTip),
              colour = "black", size=3, fill = "black", shape = 23, alpha = 5) +
  geom_tippoint(aes(subset = !is.na(novel), fill = novel), size = 5, shape = 21, stroke = 0.3,na.rm = T)+ 
  scale_fill_manual(values = cores_biomas, name = "Bioma") +  # Substitui scale_fill_viridis_d()
  geom_treescale(width = 0.08, color = 'black', family = "arial", x = 2, y = 4) +
  theme(legend.position = "right")  # Ajusta a posição da legenda se necessário

p

ggsave("Rtree/phylo.svg", width = 24, height = 30, units= "cm", limitsize = FALSE )

# PUT TIP LABELS
p1 <- p + geom_tiplab(aes(label = organism_name), size = 6)
p1

# Save image as svg
# install.packages("svglite")
# library(svglite)
# ggsave("Rtree/phylo.svg", width = 24, height = 30, units= "cm", limitsize = FALSE )


# save version to see
zoomClade(p1 , node = 246, xexpand = 0.001)
ggsave("Rtree/zoom_phylo_246.svg", width = 24, height = 30, units= "cm", limitsize = FALSE )

zoomClade(p1 , node = 282, xexpand = 0.001)
ggsave("Rtree/zoom_phylo_282.svg", width = 24, height = 30, units= "cm", limitsize = FALSE )

