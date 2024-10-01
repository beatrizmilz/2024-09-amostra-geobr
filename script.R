# Link para slides: 
# https://beamilz.com/talks/pt/2024-09-amostra-ime-usp-geobr/
# https://beatrizmilz.github.io/2024-09-amostra-geobr/

# Instalação de pacotes necessários
# install.packages("remotes")
# remotes::install_github(repo = "r-spatial/sf")
# install.packages(c("geobr", "tidyverse", "sf", "abjData"))

# Carregar pacotes necessários
library(geobr)
library(tidyverse)

# Ver a lista de funções do geobr
View(list_geobr())

# Exemplo 1 ----------------------------------


brasil_ufs <- read_state()

class(brasil_ufs)

brasil_ufs

brasil_ufs |>
  ggplot() +
  geom_sf()


estado_sp <- brasil_ufs |>
  filter(abbrev_state == "SP")


muni_sp <- read_municipality(3550308)

muni_sp |>
  ggplot() +
  geom_sf()


escolas_brasil <- read_schools()

glimpse(escolas_brasil)

escolas_saopaulo <- escolas_brasil |>
  filter(abbrev_state == "SP", name_muni == "São Paulo")

rm(escolas_brasil)

count(escolas_saopaulo, education_level) |> View()


escolas_em_sp <- escolas_saopaulo |>
  filter(str_detect(education_level, "Ensino Médio"))

rm(escolas_saopaulo)


ggplot() + 
  geom_sf(data = muni_sp) +
  geom_sf(data = escolas_em_sp,
          aes(color = government_level),
          alpha = 0.3, 
          show.legend = FALSE) +
  facet_wrap(vars(government_level))



escolas_em_sp |>
  filter(government_level %in% c("Estadual", "Privada")) |>
  ggplot() +
  geom_sf(data = muni_sp) +
  geom_sf(aes(color = government_level, alpha = 0.1), show.legend = FALSE) +
  facet_wrap(~ government_level) +
  labs(x = "Longitude",
       y = "Latitude") +
  scale_color_viridis_d() +
  theme(legend.spacing.x = unit(1.0, 'cm')) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(
      color = gray(0.9),
      linetype = "dashed",
      size = 0.1
    ),
    panel.background = element_rect(fill = "white")
  ) +
  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      size = 8
    ),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_text(size = rel(0.8)),
    axis.title.x = element_text(size = rel(0.8)),
    legend.title = element_text(size = rel(0.8))
  )

# Exemplo 2 --------------------------------------

library(abjData)
pnud_min

pnud_min_sp <- pnud_min |>
  filter(uf_sigla == "SP")

municipios_sp <- read_municipality("SP")

municipios_sp_arrumado <- municipios_sp |>
  mutate(code_muni = as.character(code_muni))

# join:
# munipios_sp com pnud_min_sp

pnud_sp_geo <- left_join(municipios_sp_arrumado, pnud_min_sp, by = c("code_muni" = "muni_id"))

ggplot() +
  geom_sf(data = municipios_sp) +
  geom_sf(data = pnud_sp_geo, aes(fill = espvida)) +
  facet_wrap(vars(ano)) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Esperança de vida") +
  theme(legend.position = "bottom")


  
