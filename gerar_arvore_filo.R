library("seqinr")
library("stringr")
library(ggtree)
library(treeio)
library("readxl")
library("stringi")
library(ggplot2)
library("plyr")
library(dplyr)

setwd('D:/Dropbox/RESULTS')

#Color Palette
c25 <- c(
  "dodgerblue2", "#E31A1C","green4","#6A3D9A","#FF7F00","black", "gold1","skyblue2", "#FB9A99", 
               "palegreen2","#CAB2D6","#FDBF6F","gray70", "khaki2","maroon", "orchid1", "deeppink1",
               "blue1", "steelblue4","darkturquoise", "green1", "yellow4", "yellow3","darkorange4", "brown"
)

manualcolors<-c('forestgreen', 'red2', 'orange', 'cornflowerblue', 
                       'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue', 
                       'mediumorchid1','firebrick4',  'yellowgreen', 'lightsalmon', 'tan3',
                       "tan1",'darkgray', 'wheat4', '#DDAD4B', 'chartreuse', 
                       'seagreen1', 'moccasin', 'mediumvioletred', 'seagreen','cadetblue1',
                       "darkolivegreen1" ,"tan2" ,   "tomato3" , "#7CE3D8","gainsboro")
                       
paleta1 <- c("#0000FF", "#FF0000", "#00FF00",  "#FF00B6", "#005300", "#FFD300", "#009FFF", "#9A4D42", 
                      "#00FFBE","#783FC1", "#1F9698", "#FFACFD", "#B1CC71", "#F1085C", "#FE8F42", "#DD00FF", "#720055", "#766C95",
   "#02AD24", "#C8FF00", "#886C00", "#FFB79F", "#858567", "#A10300", "#14F9FF", "#00479E", "#DC5E93", 
   "#93D4FF","#004CFF", "#F2F318")

cores <- c(c25, manualcolors, paleta1)

#Inserir o arquivo principal do sequenciamento e criar uma nova tabela com os codigos e clades
pango <- read.csv("lineage_report_arvore.csv")

fasta_arvore_ref <- read.fasta("fasta_referencia.fasta")
codes_arvore_ref <- names(fasta_arvore_ref)

#criação do df com os nomes das amostras, variante, clade e tipos (novas x antigas)
novas_amostras_df <- data.frame(pango$taxon, pango$lineage, pango$scorpio_call)
colnames(novas_amostras_df)<-c("Sample", "Lineage", "Variant")
novas_amostras_df$Tipo <- rep("Novas",nrow(pango))

novas_amostras_df$Variant <- sapply(novas_amostras_df$Variant, function(x) strsplit(x, " ")[[1]][1])
novas_amostras_df$Variant <- gsub("Problable", "Omicron", novas_amostras_df$Variant)
novas_amostras_df$Variant[1] <- "Wuhan"

novas_amostras_df$Tipo <- ifelse(
  grepl("^h|^N", novas_amostras_df$Sample),
  "Antigas",
  novas_amostras_df$Tipo
)
novas_amostras_df$Sample[1] <- "NC_045512.2"
novas_amostras_df$Sample <- gsub("/", "_", novas_amostras_df$Sample)
novas_amostras_df$Sample <- gsub("\\|", "_", novas_amostras_df$Sample)

#Criação da árvore 
tree <-read.newick("fasta_referencia_align.fasta.treefile")
arvore <-as.treedata(full_join(as_tibble(tree), novas_amostras_df, by = c('label'= 'Sample')))                                 
  
#Circular tree
plot_arvore <- ggtree(arvore, layout = "circular", size=0.2) + geom_tippoint(aes(color = Lineage, fill=Lineage, shape = Tipo), size=1) +
  theme(legend.position = c(0.0003, 0.6),legend.title = element_text(size=3),legend.text = element_text(size=3), legend.background = element_blank(),
         legend.title.align = 0.5, legend.key.height=unit(0.5,"line")) + guides(color = guide_legend(override.aes = list(size = 3)))  + scale_color_manual(values = sample(cores,length(table(novas_amostras_df$Lineage))))
plot(rotate_tree(plot_arvore,180))
ggsave('arvore_mar_23_nova.png')

#Vertical tree
plot_arvore <- ggtree(arvore, size = .2) + geom_tippoint(aes(color = Lineage, fill=Lineage, shape = Tipo), size=1.1) +
  theme(legend.position = c(0.1, 0.65),legend.title = element_text(size=3),legend.text = element_text(size=3), legend.background = element_blank(),
        legend.title.align = 0.5, legend.key.height=unit(0.5,"line")) + guides(color = guide_legend(override.aes = list(size = 3))) + scale_color_manual(values = sample(cores,length(table(novas_amostras_df$Lineage))))
plot(plot_arvore)                                                                                                                  
ggsave('arvore_jan_23_nova.png')
