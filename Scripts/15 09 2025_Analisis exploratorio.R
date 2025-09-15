#Importar base de datos 

#Renombrar base de datos
barrio20 <- Base_datos_analisis_AAR

#Importar lirerias
library(dplyr)
library(ggplot2)
library(tidyr)
install.packages("broom")
library(broom)  

barrio20_long <- barrio20 %>%
  pivot_longer(cols = starts_with("T_"), names_to = "Hora", values_to = "Temperatura") %>%
  mutate(Hora = as.numeric(gsub("T_", "", Hora)))  # elimina "T_" y convierte a número

#==============================================================================================================
  
#ANALISIS DESCRIPTIVO-Gráfico

# Resumen por vegetación
resumen <- barrio20_long %>%
  group_by(Vegetacion, Dia, Hora) %>%
  summarise(Media = mean(Temperatura, na.rm = TRUE),
            SD = sd(Temperatura, na.rm = TRUE))

# Visualización
ggplot(resumen, aes(x = Hora, y = Media, color = factor(Vegetacion), group = Vegetacion)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Media - SD, ymax = Media + SD, fill = factor(Vegetacion)), alpha = 0.2) +
  facet_wrap(~Dia, labeller = labeller(Dia = c(`1` = "Día medio", `2` = "Día extremo"))) +
  scale_x_continuous(breaks = 6:20) +  # forzar todas las horas en el eje x
  labs(color = "Vegetación", fill = "Vegetación", y = "Temperatura (°C)", x = "Hora") +
  theme_minimal()

#==============================================================================================================

#COMPARACIÓN VEGETACIÓN VS NO-VEGETACION

#T-Test promedio diario=======

barrio20 <- barrio20 %>%
  rowwise() %>%
  mutate(Temp_media_dia = mean(c_across(T_6:T_20)))

# Test día medio (Dia == 1)
t.test(Temp_media_dia ~ Vegetacion, data = barrio20 %>% filter(Dia == 1))

# Test día extremo (Dia == 2)
t.test(Temp_media_dia ~ Vegetacion, data = barrio20 %>% filter(Dia == 2))


# Resumen por grupo
resumen <- barrio20 %>%
  group_by(Dia, Vegetacion) %>%
  summarise(
    media = mean(Temp_media_dia, na.rm = TRUE),
    sd = sd(Temp_media_dia, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),   # error estándar
    .groups = "drop"
  )

# Gráfico
ggplot(resumen, aes(x = factor(Dia), y = media, fill = factor(Vegetacion))) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = c("0" = "orange", "1" = "darkgreen"),
                    labels = c("0" = "Sin vegetación", "1" = "Con vegetación")) +
  scale_x_discrete(labels = c("1" = "Día medio", "2" = "Día extremo")) +
  labs(x = "Tipo de día", y = "Temperatura media diaria (°C)",
       fill = "Vegetación") +
  theme_minimal(base_size = 14)


#T-Test por cada hora===============
resultados_ttest <- barrio20_long %>%
  group_by(Dia, Hora) %>%
  summarise(
    test = list(t.test(Temperatura ~ Vegetacion, data = cur_data())),
    .groups = "drop"
  ) %>%
  mutate(test = purrr::map(test, broom::tidy)) %>%
  unnest(test)

# Mira resultados
print(resultados_ttest)

# === Resumen por grupo (media, sd, se) ===
resumen_horas <- barrio20_long %>%
  group_by(Dia, Vegetacion, Hora) %>%
  summarise(
    media = mean(Temperatura, na.rm = TRUE),
    sd = sd(Temperatura, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# === Gráfico ejemplo: barras con error estándar por hora ===
ggplot(resumen_horas, aes(x = Hora, y = media, fill = factor(Vegetacion))) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                position = position_dodge(0.8), width = 0.2) +
  facet_wrap(~Dia, labeller = as_labeller(c("1" = "Día medio", "2" = "Día extremo"))) +
  scale_fill_manual(values = c("0" = "orange", "1" = "darkgreen"),
                    labels = c("0" = "Sin vegetación", "1" = "Con vegetación")) +
  labs(x = "Hora", y = "Temperatura (°C)", fill = "Vegetación") +
  theme_minimal(base_size = 14)

#==============================================================================================================

#Comparar entre sin/con vegetación; dia extremo/dia medio; considenrando la correlación de un mismo punto

install.packages("lme4")       # Modelos lineales mixtos
install.packages("lmerTest")   # P-values para modelos de lme4
library(lme4)
library(lmerTest)  
library(dplyr)
library(emmeans)

# Paquetes necesarios
install.packages("lme4")
install.packages("lmerTest")
install.packages("emmeans")
install.packages("dplyr")
install.packages("ggplot2")

library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(ggplot2)

# Asegurarnos de que Hora y Dia sean factores
barrio20_long <- barrio20_long %>%
  mutate(
    Hora = factor(Hora),
    Dia = factor(Dia, levels = c(1,2), labels = c("Día medio", "Día extremo"))
  )

# Modelo mixto considerando Día y Hora
modelo_total <- lmer(
  Temperatura ~ Vegetacion * Hora * Dia + (1 | Posicion),
  data = barrio20_long
)
summary(modelo_total)

# Comparaciones por hora y tipo de día
comparaciones <- emmeans(modelo_total, pairwise ~ Vegetacion | Hora * Dia)
contrastes <- as.data.frame(comparaciones$contrasts)

# Graficar diferencias por hora, facetas por tipo de día
ggplot(contrastes, aes(x = as.numeric(as.character(Hora)), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_errorbar(aes(ymin = estimate - SE * 1.96, ymax = estimate + SE * 1.96),
                width = 0.2, color = "black") +
  geom_point(aes(color = p.value < 0.05), size = 3) +
  facet_wrap(~Dia) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                     labels = c("FALSE" = "ns", "TRUE" = "p < 0.05")) +
  labs(x = "Hora", y = "Diferencia (Sin - Con vegetación, °C)",
       color = "Significancia",
       title = "Efecto horario de la vegetación sobre la temperatura") +
  theme_minimal(base_size = 14)



