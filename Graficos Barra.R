library("readxl")
library(ggplot2)

data <- read_excel("D:/OPAS/Seqs_mai.xlsx")
colnames(data) <- (c('IDs', 'Variante'))
p <- ggplot(data, aes(x=Variante, fill=Variante)) + 
  geom_bar(stat = "count") +
  ylab("Quantidade de Amostras") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Variantes do SARS-CoV-2 nas Amostras Selecionadas (Maio/2022)") +
  theme(legend.position="none")
  #theme(axis.text = element_text(size = 8))
  

ggsave(p, filename = "Maio.png")
