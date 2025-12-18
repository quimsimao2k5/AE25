library(tidyverse)
library(ggplot2)

Sys.setlocale("LC_CTYPE", "pt_PT.UTF-8")

dados <- read_csv("beers.csv")

theme(text = element_text(face = "bold"))

preco_medio <- dados |>
  group_by(brand) |>
  summarise(preco_medio = mean(price_eur, na.rm = TRUE))|>
  arrange(desc(preco_medio))

ggplot(preco_medio, aes(x = reorder(brand, preco_medio), y = preco_medio, fill = brand)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(preco_medio, 2)), 
            hjust = 1.2, color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "💶 Preço médio por marca de cerveja",
    x = "Marca",
    y = "Preço médio (€)"
  ) +
  theme_minimal(base_size = 14) + scale_fill_viridis_d()

