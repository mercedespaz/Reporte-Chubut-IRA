#--------------------------------------------------
# 1.Carga de base nominal de vigilancia universal
#--------------------------------------------------

vr_nominal <- read_excel("Data/NOM_P26_VR_(2025_10_20).xlsx")

#-------------------------------------------------------------------
# 2.Parámetros para filtrar el analisis desde el comienzo del código
#-------------------------------------------------------------------

#Se analizan los registros del año en curso

anio_analisis <- 2025

positivos <- c("Positivo", "Detectable")


vr_nominal <- vr_nominal %>%
  # Paso 1: Convertir las columnas de fecha a formato de fecha
  mutate(across(starts_with("FTM"), as_date)) %>%
  mutate(across(starts_with("FIS"), as_date)) %>%
  mutate(across(starts_with("FECHA_"), as_date))


#Calculo fecha minima y año minimo

vr_nominal <- vr_nominal %>% mutate(
  FECHA_MIN = pmin(FTM, FECHA_INICIO_SINTOMA, FECHA_CONSULTA, FECHA_APERTURA, na.rm = TRUE),
  SEMANA_MIN = pmin(SEPI_APERTURA, SEPI_SINTOMA, SEPI_CONSULTA, SEPI_MUESTRA, na.rm = TRUE),
  ANIO_MIN = year(FECHA_MIN)
)

#Asignar valor numeríco a los resultados
vr_nominal <- vr_nominal %>% mutate(RESULTADO = case_when (RESULTADO %in% positivos ~ "1",
                                                           TRUE ~ "0"))


#Se analizan los registros con provincia de carga Chubut con anio minimo igual a 2025 y resultado positivo

vr_nominal<- vr_nominal %>% filter(PROVINCIA_CARGA == "Chubut" & 
                                     ANIO_MIN == anio_analisis & 
                                     RESULTADO == "1")

#-------------------------------------------------------------------
# 3.Determinación según tipo de agente 
#-------------------------------------------------------------------

vr_nominal <- vr_nominal %>% mutate(VIRUS_GRUPO = case_when (
  
  str_detect(DETERMINACION,"Parainfluenza") ~ "Parainfluenza",
  str_detect(DETERMINACION,"VSR") ~ "VSR",
  str_detect(DETERMINACION,"Influenza A") ~ "Influenza A",
  str_detect(DETERMINACION,"influenza A") ~ "Influenza A",
  str_detect(DETERMINACION,"Influenza B") ~ "Influenza B",
  str_detect(DETERMINACION,"influenza B") ~ "Influenza B",
  str_detect(DETERMINACION,"Metaneumovirus") ~ "Metaneumovirus",
  str_detect(DETERMINACION,"SARS-CoV-2") ~ "SARS-CoV-2",
  str_detect(DETERMINACION,"SARS CoV-2") ~ "SARS-CoV-2",
  str_detect(DETERMINACION,"SARS COV-2") ~ "SARS-CoV-2",
  str_detect(DETERMINACION,"Adenovirus") ~ "Adenovirus",
  str_detect(DETERMINACION,"ADV") ~ "Adenovirus",
  str_detect(DETERMINACION,"Rinovirus") ~ "Rinovirus",
  TRUE ~ "Otros virus"
)) %>%
  relocate(VIRUS_GRUPO,.after=RESULTADO)


#-------------------------------------------------------------------
# 3.Determinación según tipo de técnica
#-------------------------------------------------------------------

vr_nominal <- vr_nominal %>% mutate( prioridad = case_when(
  str_detect(DETERMINACION, regex("Genoma", ignore_case = TRUE)) ~ 1,
  str_detect(DETERMINACION, regex("Antígeno", ignore_case = TRUE)) ~ 2,
  TRUE ~ 3
)
) %>%
  relocate(prioridad,.after=VIRUS_GRUPO)

# quitar duplicados
vr_final <- vr_nominal %>%
  arrange(IDEVENTOCASO, VIRUS_GRUPO, prioridad) %>%
  distinct(IDEVENTOCASO, VIRUS_GRUPO, .keep_all = TRUE)



#PARAMETROS TEMPORALES PARA TEXTO AUTOMATIZADO

semana_analisis_maxima <- max(vr_final$SEMANA_MIN)

semana_analisis_minima <- min(vr_final$SEMANA_MIN)

