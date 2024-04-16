# From CRAN
install.packages("geobr")

library(geobr)
library(ggplot2)
library(sf)
library(dplyr)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# State of Rio Grande do Norte
state <- read_state(
  code_state="RN",
  year=2020,
  showProgress = TRUE
)

# Municipality of Natal
muni <- read_municipality(
  code_muni = 2408102, 
  year=2020, 
  showProgress = TRUE
)

plot(state['name_state'])

all_muni <- read_municipality(
  code_muni = "RN", 
  year= 2020,
  showProgress = TRUE
)

# plot
ggplot() +
  geom_sf(data=all_muni, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(title="Cidades do Rio Grande do Norte, 2020", size=8) +
  theme_void() + theme(plot.title = element_text(hjust = 0.5))

qnt_por_variante <- data.frame(table(all_muni$name_muni[match(xlsx_jan_23$`MUNICIPIO DE RESIDENCIA`, all_muni$name_muni)]))
colnames(qnt_por_variante) <- c("name_muni", "Quantidade")
qnt_por_variante <- st_as_sf(right_join(qnt_por_variante,all_muni, by="name_muni"))

ggplot() +
  geom_sf(data=qnt_por_variante, aes(fill=Quantidade), size=.15) +
  labs(title ="Cidades do RN com Genomas de SARS-CoV-2 Sequenciados", size=8) +
  scale_fill_distiller(palette = "Blues", name="Quantidade") +
  theme_void() + no_axis + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  guides((color = guide_colorbar(title.vjust = .8)))



