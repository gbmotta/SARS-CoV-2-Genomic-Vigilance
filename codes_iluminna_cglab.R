library("seqinr")
library("stringr")
library(ggtree)
library(treeio)
library("readxl")
library("stringi")
library(ggplot2)
library("plyr")
library(dplyr)

#Para criar arquivos com os nomes sugeridos pela CGLAB
#Pegar os fastas do arquivo multifasta (seqbatch)
fasta_jan23 <- read.fasta("D:/OPAS/JAN_23/RESULTS/Arvore/fasta_arvore.fasta")

#Retirar somente os codigos das amostras
codes <- names(fasta_jan23)

#Criar um dataframe com os codigos na primeira coluna
codes_jan23 <- data.frame(codes)

#Retirar os primeiros valores dos codigos
partircao_codes <- strsplit(codes, "_")

numeros_amostras <- sapply(partircao_codes, function(x) x[1])
#Conferir se est?o no mesmo formato da lista de codigos (Sem o 0 inicial)
for (i in 1:length(numeros_amostras)){
  if (substr(numeros_amostras[i],1,1) == 0) {
    numeros_amostras[i] <- substr(numeros_amostras[i],2,2)
  }
}

#Criar o match entre o codigo das amostras e codigo CGLAB e adicionar no dataframe codes_jan23
codes_jan23$codigos_amostras <- numeros_amostras
codes_jan23$codigo_CGLAB <- xlsx_jan_23_n$X.13[match(codes_jan23$codigos_amostras,xlsx_jan_23_n$X)]

#Criar uma coluna com os novos c?digos de nomea??o como pedido pela CGLAB e encontrar as variantes
codes_jan23$new_code <-paste0("hCoV-19/Brazil/RN-LACENRN-", codes_jan23$codigo_CGLAB, "/2023")
xlsx_jan_23_n[7,3] <- 'hCoV-19/Brazil/RN-LACENRN-240881174/2023'
codes_jan23$variantes <- xlsx_jan_23_n$X.20[match(codes_jan23$new_code,gsub("[\n]", "", xlsx_jan_23_n$X.14))]

#___________________________________________________________________________________________________#

#Para criar um multifasta com os nomes dos arquivos j? dados
#Inserir o arquivo principal do sequenciamento e criar uma nova tabela com os codigos e clades
xlsx_jan_23 <- read_excel("D:/OPAS/JAN_23/RESULTS/PLANILHA SEQUENCIAMENTO-JANEIRO-2023.xlsx")
xlsx_jan_23_n <-xlsx_jan_23[c(1,14,15,21)]
xlsx_jan_23_n <- xlsx_jan_23_n[-1,]


#___________________________________________________________________________________________________#

#Leitura do arquivo multifasta com os resultados do Viralflow
fasta_arvore <- read.fasta("D:/OPAS/JAN_23/RESULTS/Arvore/fasta_arvore_references.fasta")
codes_arvore <- names(fasta_arvore)
codes_arvore_n <- c()

#Associa??o dos nomes do arquivos como pedido pela CGLAB e e aqueles sa?dos do Viralflow
for (i in 1:length(codes_arvore)){
  xx <- (strsplit((codes_arvore), "_")[[i]][1])
  if (i == 69 || 70 || 71 || 72 || 73 || 74) {
    codes_arvore_n[i] <- codes_arvore[i]}
  else{
    if (substr(xx,1,1) == 0){
    xx <-substr(xx,2,2)}
    codes_arvore_n[i] <- (xlsx_jan_23_n[xlsx_jan_23_n[1] == xx, 3])
  } 
}

codes_arvore_n <- gsub("[\r\n]", "", codes_arvore_n)

#Escrita de um novo fasta com os nomes pedidos pela CGLAB
write.fasta(fasta_arvore, codes_arvore_n, "Multifasta_JAN_23_LACENRN_Novo_arvore.fasta", open = "w", as.string = FALSE)           

#criar uma ?rvore filogenetica com os resultados
#Entrada do arquivo treefile gerado pelo iqtree com anterior passagem pelo mafft
tree <-read.newick("D:/OPAS/JAN_23/RESULTS/Arvore/fasta_arvore_references_align.fasta.treefile")

#leitura do tsv com as annotations dos clades
clades <- data.frame('label' = c(xlsx_jan_23_n$X.14), "Variante" = c(xlsx_jan_23_n$X.20) )
clades$label <- gsub("[\r\n]", "", clades$label)

#Cria??o da ?vore 
arvore <-as.treedata(full_join(as_tibble(tree), clades, by = 'label' )) #(1)

arvore_mapping <- na.omit(data.frame(arvore[,1],arvore[,2]))

plot_arvore <- ggtree(arvore) + geom_tiplab(size =7, aes(color = Variante),show.legend=FALSE) + geom_tippoint(aes(color = Variante)) + expand_limits(x = 0.0021) + 
theme(legend.position = c(0.1, 0.85),legend.title = element_text(size=30),legend.text = element_text(size=30), legend.background = element_blank(),
legend.box.background = element_rect(colour = "black")) + guides(color = guide_legend(override.aes = list(size = 20)))
ggsave('arvore_jan_23.png',width=20, height=20)
