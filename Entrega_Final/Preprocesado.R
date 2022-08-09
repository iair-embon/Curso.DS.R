
#Saco la calibracion para cada participante, ver Fleming y Lau 2014
df_calibracion <- df_total %>%
  group_by(Participant, confidence_key) %>%
  summarise(prop_correcta = mean(discrimination_is_correct)) %>%
  rename(ConfidenceKey = confidence_key)

df_calibracion.calculada <- df_total %>% 
  pivot_longer(cols = starts_with("ConfKey")) %>%
  distinct(Participant, name, .keep_all = TRUE) %>% 
  select(Participant, name, value) %>% 
  mutate(ConfidenceKey = as.integer(str_extract(name, "\\d"))) %>%
  left_join(df_calibracion) %>%
  drop_na() %>%
  mutate(ConfidenceKey = (ConfidenceKey+2)/6) %>%
  group_by(Participant) %>%
  mutate(Conf.norm = value / sum(value)) %>%
  mutate( preCalibracion = value*(prop_correcta - ConfidenceKey)**2) %>%
  summarise(Calibracion  = mean(preCalibracion))

#Convierto el data frame por trials en un data frame por participantes
d <- df_total %>%
  select(!c(RelyOn,
            Problems,
            ConfKey1,
            ConfKey2,
            ConfKey3,
            ConfKey4,
            discrimination_is_correct,
            confidence_key,
            trials,
            PointDifference,
            ReacTime_DiscTask,
            ReacTime_ConfTask)) %>%
  distinct(Participant,.keep_all = TRUE)

# le agrego la variable calibracion
d <- d %>%
  mutate(Calibracion = df_calibracion.calculada$Calibracion)


filepath <- root$find_file("Entrega_Final/d.Rda")
save(d,file = filepath)
