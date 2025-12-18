library(tidyverse)
library(dplyr)

mtcars |>
  as_tibble(rownames= "modelo") |>
  select(modelo,mpg,cyl,hp)


mtcars |>
  filter(mpg>25, cyl %in% c(4,6))

mtcars |>
  as_tibble(rownames = "modelo") |>
  mutate(kmpl = mpg * 0.4251,
         potencia_cat = case_when(
           hp < 100 ~ "baixa",
           hp < 150 ~ "média",
           TRUE     ~ "alta"
         )) |>
  select(modelo, mpg, kmpl, hp, potencia_cat)
