library(tidyverse)
library(ggplot2)

portugal <- dados_eurostat |> filter(country == "Portugal")


ggplot(portugal, aes(x=time,y=Total)) +
  geom_line()

ultimo_ano <- dados_eurostat |>
  filter(year == 2019) |>
  mutate(MvsH = abs(Males - Females)) |>
  group_by(country) |>
  summarise(MvsH_media = mean(MvsH, na.rm = TRUE)) |>
  arrange(desc(MvsH_media))

ultimo_ano

ggplot(ultimo_ano,aes(x=country,MvsH_media)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ex4 <- dados_eurostat |>
  filter(year >= 2015) |>
  mutate(JvsA = Under25/Over25, na.rm =TRUE) |>
  group_by(country) |>
  summarise(JvsA_media = mean(JvsA, na.rm = TRUE)) |>
  arrange(desc(JvsA_media)) |> filter(JvsA_media!="NaN")

head(ex4)

ggplot(ex4,aes(x=country,JvsA_media)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
