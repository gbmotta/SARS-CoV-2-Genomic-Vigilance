counts_clades <- data.frame(table(xlsx_jan_23_n$Pangolin_Clade))
colnames(counts_clades) <- c("Variante", "Quantidade_de_Genomas")

ggplot(counts_clades,
       aes(x = Quantidade_de_Genomas,
           xend = 0,
           y = Variante,
           yend = Variante,
           colour = Variante)) +
  geom_segment(linewidth = 1.4) +
  geom_point() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::pretty_breaks(n = 5)) +
  scale_y_discrete(limits = rev)  + labs(x = "Quantidade de Genomas Sequenciados",
                                        y = "Variante",
                                        title = "Variantes de SARS-CoV-2 encontradas e suas quantidades em Janeiro de 2023 no LACEN/RN",
                                        ) + theme_classic() +
  
  theme(legend.position = "off") + theme(plot.title = element_text(hjust = 0.5))