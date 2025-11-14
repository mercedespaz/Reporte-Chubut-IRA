#-------------------------------------------------------------------
# 1.GRAFICO DETERMINACIONES POR SE (INTERNADOS Y AMBULATORIOS)
#-------------------------------------------------------------------

casos_completos_1 <- vr_final %>%
  group_by(SEMANA_MIN, VIRUS_GRUPO) %>%
  summarise(n_casos = n(), .groups = "drop") %>%
  complete(SEMANA_MIN, VIRUS_GRUPO, fill = list(n_casos = 0)) %>%
  filter(VIRUS_GRUPO != "Otros virus")

#Sumo totales para N del gráfico
total_casos_1<- sum(casos_completos_1$n_casos)

#Paleta de colores para cada virus
paleta <-c("Adenovirus" = "#cc66ff",
           "Influenza A"= "#f35bc0",
           "Influenza B" = "#00b050",
           "Metaneumovirus" =  "#a6a6a6",
           "Parainfluenza" = "#ed7d31",
           "Rinovirus" = "#ffff00",
           "SARS-CoV-2" = "#0070c0",
           "VSR" = "#00b0f0")


#-------------------------------------------------------------------
# CÓDIGO GRÁFICO DE BARRAS APILADAS
#-------------------------------------------------------------------

grafico_casos_total <- ggplot(casos_completos_1, aes(x = SEMANA_MIN, y = n_casos, fill = VIRUS_GRUPO)) +
  geom_col() +
  scale_fill_manual(values = paleta) +
  labs(
    x = "Semana epidemiológica",
    y = "Número de determinaciones",
    caption= "Fuente: elabroración propia a partir de datos del SNVS 2.0") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_x_continuous(
    breaks = seq(min(casos_completos_1$SEMANA_MIN), max(casos_completos_1$SEMANA_MIN), by = 1)
  )


grafico_casos_total


#-------------------------------------------------------------------
# 2.GRAFICO DETERMINACIONES POR SE EN AMBULATORIOS
#-------------------------------------------------------------------

eventos_ambulatorios <- c("COVID-19, Influenza y OVR en ambulatorios (No UMAs)", "Monitoreo de SARS COV-2, Influenza y VSR en ambulatorios")


casos_completos_ambulatorios <- vr_final %>%
  filter(EVENTO %in% eventos_ambulatorios) %>%
  group_by(SEMANA_MIN, VIRUS_GRUPO) %>%
  summarise(n_casos = n(), .groups = "drop") %>%
  complete(SEMANA_MIN, VIRUS_GRUPO, fill = list(n_casos = 0)) %>%
  filter(VIRUS_GRUPO != "Otros virus")


total_casos_ambulatorios <- sum(casos_completos_ambulatorios$n_casos)

#CODIGO PARA GRÁFICO DE BARRAS APILADAS

grafico_casos_ambulatorios <- ggplot(casos_completos_ambulatorios, aes(x = SEMANA_MIN, y = n_casos, fill = VIRUS_GRUPO)) +
  geom_col() +
  scale_fill_manual (values = paleta) +
  labs(
    x = "Semana Epidemiológica",
    y = "Número de determinaciones") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(
    breaks = seq(min(casos_completos_ambulatorios$SEMANA_MIN), max(casos_completos_ambulatorios$SEMANA_MIN), by = 1) 
  )


grafico_casos_ambulatorios