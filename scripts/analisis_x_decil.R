## Analisis General de ENIGH para el X decil
# En realidad quiero hacer dos analisis
# 1) Cerrar el grupo para la gente que se parece a mí y ver la distribución de su ingreso:
# Hombre de 28 años que trabaja en CDMX con licenciatura
# 2) Distribución del ingreso en el décimo decil
# DAOA
# 23/08/2025

# Preparar Espacio --------------------------------------------------------

pacman::p_load(tidyverse, scales, janitor)
rm(list = ls())

# Cargar Datos ------------------------------------------------------------

poblacion <- read_csv('input/poblacion.csv')
concentrado_hogar <- read_csv('input/concentradohogar.csv')
ingresos <- read_csv('input/ingresos.csv')

# Analisis ----------------------------------------------------------------

ingresos %>% glimpse()
poblacion %>% glimpse()

# Hogares en CDMX

poblacion_cdmx <- poblacion %>% filter(entidad == '09') 
hogares_cdmx <- concentrado_hogar %>% filter(folioviv %in% poblacion_cdmx$folioviv) 

# Tamaño cdmx
hogares_cdmx %>% summarise(sum(factor*tot_integ))
# Perceptores cdmx
hogares_cdmx %>% summarise(sum(factor*percep_ing))
# Ocupados cdmx
hogares_cdmx %>% summarise(sum(factor*perc_ocupa))

# Quick plot

# 1 = Hombre
# 2 = Mujer

general_sexo_plot <- hogares_cdmx %>% 
  mutate(integrantes = tot_integ * factor, 
         perceptores = percep_ing * factor, 
         ocupados = perc_ocupa * factor, 
         ingreso_laboral = trabajo * factor
         )%>% 
  group_by(sexo_jefe) %>% 
  summarise(integrantes = sum(integrantes),
            perceptores = sum(perceptores), 
            ocupados = sum(ocupados), 
            ingreso_laboral = sum(ingreso_laboral), 
            tot = sum(factor)) %>% 
  mutate(sexo_jefe = ifelse(sexo_jefe == 1,  'Hombre', 'Mujer'), 
         p_perceptores = perceptores/integrantes, 
         p_integrantes = integrantes/tot,
         p_ocupados = ocupados/tot,
         p_perceptores = perceptores/tot,
         p_ingreso_lab = ingreso_laboral/ocupados) %>% 
  select(sexo_jefe, starts_with('p_')) %>% 
  pivot_longer(cols = 2:5, names_to = 'var', values_to = 'val')

# Head of house sex differences in general indicators in CDMX

general_sexo_plot %>% 
  ggplot(aes(var, val, fill = sexo_jefe))+
  geom_col(position = position_dodge())+
  geom_label(aes(label = comma(val)), position = position_dodge(widt = 1), color = 'white', family = 'mulish')+
  theme_minimal()+
  scale_fill_manual(values = c('firebrick', 'goldenrod'))+
  facet_wrap(~var, scales = 'free')



# HH income distribution in cdmx ------------------------------------------

income_mensual_laboral <- hogares_cdmx %>% mutate(ingreso_laboral = trabajo * factor, 
                        ocupados = perc_ocupa * factor, 
                        integrantes = tot_integ * factor, 
                        ) %>% 
  select(ingreso_laboral, factor, ocupados, integrantes) %>% 
  mutate(ingreso_l_trim_ocupados = ingreso_laboral/ocupados, 
         ingreso_l_mensual_ocupados = ingreso_l_trim_ocupados/3) %>% 
  filter(!is.na(ingreso_l_mensual_ocupados), ingreso_l_mensual_ocupados!= Inf)

income_mensual_laboral %>% mutate(decil = ntile(n =10,ingreso_l_mensual_ocupados)) %>% 
  group_by(decil) %>% 
  summarise(ingreso_p_decil = mean(ingreso_l_mensual_ocupados, na.rm = T)) %>% 
  ggplot(aes(decil, ingreso_p_decil, fill = factor(decil)))+
    geom_col(color = 'black')+
    geom_label(aes(label = comma(ingreso_p_decil)))+
  theme_minimal()


# # 10th decil income distribution -------------------------------------------
income_mensual_laboral <- hogares_cdmx %>% mutate(ingreso_laboral = trabajo * factor, 
                                                  ocupados = perc_ocupa * factor, 
                                                  integrantes = tot_integ * factor, 
) %>% 
  select(ingreso_laboral, factor, ocupados, integrantes) %>% 
  mutate(ingreso_l_trim_ocupados = ingreso_laboral/ocupados, 
         ingreso_l_mensual_ocupados = ingreso_l_trim_ocupados/3) %>% 
  filter(!is.na(ingreso_l_mensual_ocupados), ingreso_l_mensual_ocupados!= Inf)

income_mensual_laboral %>% filter(!is.na(ingreso_l_mensual_ocupados), ingreso_l_mensual_ocupados!= Inf) %>%
  mutate(decil = ntile(n =10,ingreso_l_mensual_ocupados)) %>% 
  filter(decil == 10) %>% 
  mutate(X_decil = ntile(ingreso_l_mensual_ocupados, 10)) %>% 
  group_by(X_decil) %>% 
  summarise(ingreso_l_mensual_ocupados = mean(ingreso_l_mensual_ocupados, na.rm = T), n = n()) %>% 
  pivot_longer(cols = 2:3, names_to = 'var', values_to = 'val') %>% 
  ggplot(aes(X_decil, val, fill = factor(X_decil)))+
  geom_col(color = 'black')+
  geom_label(aes(label = comma(val)))+
  theme_minimal()+
  facet_wrap(~var, scales = 'free')

## Tamaño del hogar por decil de ingreso -------


tamano_x_deciles <-  hogares_cdmx %>% mutate(decil = ntile(ing_cor, n=10)) %>% 
  mutate(ocupados = perc_ocupa * factor, 
         integrantes = tot_integ * factor) %>% 
  group_by(decil) %>% 
  summarise(ocupados = sum(ocupados), 
            integrantes = sum(integrantes), 
            tot = sum(factor)
            ) %>% 
  mutate(p_ocupados = ocupados/tot, 
         p_integ = integrantes/tot
         ) %>%select(decil, starts_with('p_')) %>% 
  pivot_longer(cols = 2:3)

tamano_x_deciles %>% ggplot(aes(decil, value, fill = factor(decil)))+
  geom_col(color = 'black')+
  geom_label(aes(label = comma(value, accuracy = 0.01)))+
  theme_minimal()+
  facet_wrap(~name)


# # 10th decil total income distribution -------------------------------------------
income_mensual <- hogares_cdmx %>% mutate(ingreso_laboral = trabajo * factor, 
                                                  ocupados = perc_ocupa * factor, 
                                                  integrantes = tot_integ * factor, 
                                                  ingreso_total = ing_cor * factor
) %>% 
  select(ingreso_laboral, ingreso_total, factor, ocupados, integrantes) %>% 
  mutate(ingreso_l_trim_ocupados = ingreso_laboral/ocupados, 
         ingreso_l_mensual_ocupados = ingreso_l_trim_ocupados/3, 
         ingreso_total_trim = ingreso_total/integrantes, 
         ingreso_total_mensual = ingreso_total_trim / 3) %>% 
  filter(!is.na(ingreso_l_mensual_ocupados), ingreso_l_mensual_ocupados!= Inf)

income_mensual %>% filter(!is.na(ingreso_total_mensual), ingreso_total_mensual!= Inf) %>%
  mutate(decil = ntile(n =10,ingreso_total_mensual)) %>% 
  filter(decil == 10) %>% 
  mutate(X_decil = ntile(ingreso_total_mensual, 10)) %>% 
  group_by(X_decil) %>% 
  summarise(ingreso_total_mensual = mean(ingreso_total_mensual, na.rm = T), n = n()) %>% 
  pivot_longer(cols = 2:3, names_to = 'var', values_to = 'val') %>% 
  ggplot(aes(X_decil, val, fill = factor(X_decil)))+
  geom_col(color = 'black')+
  geom_label(aes(label = comma(val)))+
  theme_minimal()+
  facet_wrap(~var, scales = 'free')


# Otros -------------------------------------------------------------------



ingresos %>% filter(entidad == '09') %>% # cdmx
  mutate(bulk_income = ing_tri * factor, 
                    )