#-------------------------------------------------------------------
# ---------GRAFICO DE DETERMINACIONES EN INTERNADOS----------------
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# 3.GRAFICO DETERMINACIONES POR SE EN INTERNADOS
#-------------------------------------------------------------------

#Filtro eventos ambulatorios
eventos_internados <- c("Internado y/o fallecido por COVID o IRA", "Unidad Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)")

#Paleta de colores para cada virus
paleta <-c("Adenovirus" = "#cc66ff",
           "Influenza A"= "#f35bc0",
           "Influenza B" = "#00b050",
           "Metaneumovirus" =  "#a6a6a6",
           "Parainfluenza" = "#ed7d31",
           "Rinovirus" = "#ffff00",
           "SARS-CoV-2" = "#0070c0",
           "VSR" = "#00b0f0")


# Contar los casos por semana y por grupo de virus, con los filtros 
casos_completos_internados <- vr_final %>%
  filter(EVENTO %in% eventos_internados) %>%
  group_by(SEMANA_MIN, VIRUS_GRUPO) %>%
  summarise(n_casos = n(), .groups = "drop") %>%
  complete(SEMANA_MIN, VIRUS_GRUPO, fill = list(n_casos = 0)) %>%
  filter(VIRUS_GRUPO != "Otros virus")

#Total de ambulatorios para el gráfico
total_casos_internados_1 <- sum(casos_completos_internados$n_casos)


#-------------------------------------------------------------------
# CÓDIGO GRÁFICO DE BARRAS APILADAS
#-------------------------------------------------------------------

#Gráfico barras apiladas 

Graficos_casos_internados <- ggplot(casos_completos_internados, aes(x = SEMANA_MIN, y = n_casos, fill = VIRUS_GRUPO)) +
  geom_col() +
  scale_fill_manual(values = paleta) +
  labs(
    x = "Semana epidemiológica",
    y = "Número de determinaciones",
    caption = "Fuente: elabroración propia a partir de datos del SNVS 2.0"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_x_continuous(
    breaks = seq(min(casos_completos_internados$SEMANA_MIN), max(casos_completos_internados$SEMANA_MIN), by = 1)
  )


Graficos_casos_internados


#-------------------------------------------------------------------
# 4.GRAFICO DETERMINACIONES POR GRUPO ETARIO
#-------------------------------------------------------------------

orden_grupo_etario <- c("Sin Especificar",
                        "Mayores de 65 años",
                        "De 45 a 65 años",
                        "De 35 a 44 años",
                        "De 25 a 34 años",
                        "De 20 a 24 años",
                        "De 15 a 19 años",
                        "De 10 a 14 años",
                        "De 5 a 9 años",
                        "De 2 a 4 años",
                        "De 13 a 24 meses",
                        "Posneonato (29 hasta 365 días)",
                        "Neonato (hasta 28 días)")


#Contar los casos y aplicar el orden
casos_etarios_internados <- vr_final %>% mutate(
  GRUPO_ETARIO = stringr::str_replace(GRUPO_ETARIO, "dÍas", "días"), # Corregir el acento incorrecto en los datos 
  GRUPO_ETARIO = tidyr::replace_na(GRUPO_ETARIO, "Sin Especificar")
) %>%
  
  # Aplicar el orden manual a la columna GRUPO_ETARIO
  mutate(GRUPO_ETARIO = factor(GRUPO_ETARIO, levels = orden_grupo_etario)) %>%
  filter (EVENTO %in% eventos_internados) %>%
  group_by(VIRUS_GRUPO, GRUPO_ETARIO) %>%
  summarise(n_casos = n(), .groups = "drop") %>%
  complete(VIRUS_GRUPO, GRUPO_ETARIO, fill = list(n_casos = 0)) %>%
  filter(VIRUS_GRUPO != "Otros virus")

#CONTAR CASOS
total_casos_internados_etario_1 <- sum(casos_etarios_internados$n_casos)


# 3. Crear el gráfico de barras apiladas con el factor ordenado
Graficos_casos_etarios_internados <- ggplot(casos_etarios_internados, aes(x = GRUPO_ETARIO, y = n_casos, fill = VIRUS_GRUPO)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = paleta) +
  labs(
    x = "Grupo etario", 
    y = "Número de determinaciones") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) 


#-------------------------------------------------------------------
# 5. TEXTO ENTIQUECIDO PARA VIRUS POR GRUPO ETARIO
#-------------------------------------------------------------------

virus_predominantes <- casos_etarios_internados %>%
  group_by(VIRUS_GRUPO) %>%
  slice_max(order_by = n_casos, 
            n = 1, 
            with_ties = FALSE)

grupo_max_list <- setNames(str_to_lower(virus_predominantes$GRUPO_ETARIO), virus_predominantes$VIRUS_GRUPO)
casos_max_list <- setNames(virus_predominantes$n_casos, virus_predominantes$VIRUS_GRUPO)

#print(grupo_max_list)

#grupo_max_list[["Influenza A"]] 
#casos_max_list[["Influenza A"]]
# 
# 
# virus_predominantes <- casos_etarios_internados %>%
#   arrange(desc(n_casos)) %>%
#   slice_max(order_by = n_casos, n = 5)
#   
