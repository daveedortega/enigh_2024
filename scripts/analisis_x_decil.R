## Analisis General de ENIGH para el X decil
# En realidad quiero hacer dos analisis
# 1) Cerrar el grupo para la gente que se parece a mí y ver la distribución de su ingreso:
# Hombre de 28 años que trabaja en CDMX con licenciatura
# 2) Distribución del ingreso en el décimo decil
# DAOA
# 23/08/2025

# Preparar Espacio --------------------------------------------------------

dev.off()
pacman::p_load(tidyverse, scales, janitor, sf)
rm(list = ls())

# Cargar Datos ------------------------------------------------------------

poblacion <- read_csv('input/poblacion.csv')
concentrado_hogar <- read_csv('input/concentradohogar.csv')
ingresos <- read_csv('input/ingresos.csv')

entidades <- read_sf('~/Desktop/marco_geoestadistico/mg_2024_integrado/conjunto_de_datos/00ent.shp')

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
         p_ingreso_lab = ingreso_laboral/ocupados, 
         p_ingreso_lab = p_ingreso_lab/3 # mensual
         ) %>% 
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

# Distribución del ingreso laboral en méxico ------------------------------

distr_income <- concentrado_hogar %>% select(ingreso_laboral = trabajo, ocupados, factor) %>% 
  mutate(ingreso_mensual = ingreso_laboral/3, 
         ingreso_mensual_pp = ingreso_mensual/ocupados)

# Basic distribution plot (histogram)
ggplot(distr_income, aes(x = ingreso_mensual_pp, weight = factor)) +
  geom_histogram(binwidth = 1000, color = "black") +
  labs(x = "Monthly wage", y = "Number of households (weighted)")+
  geom_vline(xintercept = 42000, color = 'red', linetype = 'dashed')

# Density plot
ggplot(distr_income, aes(x = ingreso_mensual, weight = weights))+
  geom_density()

library(dplyr)
library(survey)
library(DescTools)  # For Winsorize()

# Taking the very top off
distr_income <- distr_income %>% 
  mutate(ing_cens = Winsorize(ingreso_mensual, val = quantile(ingreso_mensual, probs = c(0,0.99))))

ggplot(distr_income,aes(x = ing_cens, weight = weights))+
  geom_density()+
  geom_vline(xintercept = 42000, color = 'red', linetype = 'dashed')


mean(distr_income$ingreso_mensual <= 42000)

ggplot(distr_income,aes(x = ing_cens, weight = weights))+
  geom_histogram(binwidth = 1000, color = "black") +
  labs(x = "Monthly wage", y = "Number of households (weighted)")+
  geom_vline(xintercept = 42000, color = 'red', linetype = 'dashed')

ggplot(distr_income, aes(x = ingreso_mensual)) +
  stat_ecdf(geom = "step", color = "firebrick", linewidth = 1) +
  labs(
    title = "Cumulative Distribution Function of Monthly Labor Income",
    subtitle = 'In Mexico, during 2024, using 2024 MXN',
    x = "MXN",
    y = "CDF", 
    caption = 'DAOA with INEGI - ENIGH 2024 Data'
  ) +
  theme_minimal()+
  geom_vline(xintercept = 42000, color = 'red', linetype = 'dashed')+
  annotate("text", x = 42000, y = 0.9537 + 0.05,
           label = paste0("CDF = ", round(0.9537598 * 100, 3)),
           color = "black")



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
  ggplot(aes(factor(decil), ingreso_p_decil, fill = factor(decil)))+
    geom_col(color = 'black')+
    geom_label(aes(label = comma(ingreso_p_decil)))+
  theme_minimal()


# # 10th decil income distribution -------------------------------------------
income_mensual_laboral <- hogares_cdmx %>% mutate(ingreso_laboral = trabajo * factor, 
                                                  ocupados = perc_ocupa * factor, 
                                                  integrantes = tot_integ * factor
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
  mutate(X_decil = paste0('X-',X_decil)) %>% 
  pivot_longer(cols = 2:3, names_to = 'var', values_to = 'val') %>% 
  ggplot(aes(reorder(X_decil, val), val, fill = factor(X_decil)))+
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

tamano_x_deciles %>% ggplot(aes(factor(decil), value, fill = factor(decil)))+
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
  mutate(X_decil = paste0('X-',X_decil)) %>% 
  summarise(ingreso_total_mensual = mean(ingreso_total_mensual, na.rm = T), n = n()) %>% 
  pivot_longer(cols = 2:3, names_to = 'var', values_to = 'val') %>% 
  ggplot(aes(reorder(X_decil, val), val, fill = factor(X_decil)))+
  geom_col(color = 'black')+
  geom_label(aes(label = comma(val)))+
  theme_minimal()+
  facet_wrap(~var, scales = 'free')

## Ingreso corriente promedio por entidad del X decil -------

hogares_nac_x_decil <- concentrado_hogar %>% 
  left_join(poblacion %>% group_by(folioviv) %>% reframe(entidad = unique(entidad))) %>% 
  group_by(entidad) %>% 
  mutate(decil = ntile(ing_cor, n = 10)) %>% 
  filter(decil == 10) %>% 
  mutate(ingreso_laboral = trabajo * factor, 
         ocupados = perc_ocupa * factor, 
         integrantes = tot_integ * factor, 
         ingreso_total = ing_cor * factor) 

rel_ent <- entidades %>% as.data.frame() %>% select(entidad = CVEGEO, NOMGEO)

hogares_nac_x_decil %>% ungroup() %>% 
  mutate(p_ing_tot = ingreso_total/integrantes, 
         p_ing_lab = ingreso_laboral/ocupados
         )%>% 
  filter(p_ing_tot !=Inf, p_ing_lab!=Inf) %>% 
  mutate(p_ing_men = p_ing_tot/3, 
         p_lab_men = p_ing_lab/3, 
         ) %>%
  group_by(entidad) %>% 
  summarise(p_ing_tot = mean(p_ing_men, na.rm = T), 
            p_ing_lab = mean(p_lab_men, na.rm = T) 
            ) %>% 
  left_join(rel_ent) %>% 
  ggplot(aes(reorder(NOMGEO, p_ing_tot), p_ing_tot, fill = p_ing_tot))+
  geom_col(color = 'black')+
  geom_label(aes(label = comma(p_ing_tot)), color = 'white', family = 'mulish')+
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  theme_minimal()+
  coord_flip()

## De impacto chiapas, revisar -----

poblacion_chiapas <- poblacion %>% filter(entidad == '07') 
hogares_chiapas <- concentrado_hogar %>% filter(folioviv %in% poblacion_chiapas$folioviv) 

income_mensual <- hogares_chiapas %>% mutate(ingreso_laboral = trabajo * factor, 
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
  ggplot(aes(factor(X_decil), val, fill = val))+
  geom_col(color = 'black')+
  geom_label(aes(label = comma(val)), color = 'white')+
  theme_minimal()+
  facet_wrap(~var, scales = 'free')+
  scale_fill_gradient(low = 'lightgreen', high = 'darkgreen')


# % del ingreso que representa el trabajo por decil -----------------------

relacion_entidades <- poblacion %>% group_by(entidad) %>% reframe(folioviv = unique(folioviv)) %>% left_join(rel_ent)

# I do not understand this
concentrado_hogar  %>% mutate(decil = ntile(ing_cor, n = 10)) %>% 
  mutate(ingreso_laboral = trabajo * factor, 
             ocupados = perc_ocupa * factor, 
             integrantes = tot_integ * factor, 
             ingreso_total = ing_cor * factor) %>% 
  mutate(p_ing_lab = ingreso_laboral/ingreso_total) %>% 
  group_by(decil) %>% 
  summarise(por_ingreso_lab = mean(p_ing_lab, na.rm = T)) %>% 
  ggplot(aes(decil, por_ingreso_lab))+
  geom_col(aes(decil, por_ingreso_lab, fill = por_ingreso_lab), color = 'black')+
  geom_label(aes(label = comma(por_ingreso_lab * 100, accuracy = 0.01, suffix = '%')))+
  scale_fill_gradient(low = 'lightblue', high = 'darkblue')

## Composite of income sources-----
concentrado_hogar %>% glimpse()


p_ing <- concentrado_hogar %>% 
  select(ing_cor) %>% 
  mutate(decil = ntile(ing_cor, 10)) %>% 
  group_by(decil) %>% summarise(ing_cor = mean(ing_cor))

p_income <- concentrado_hogar %>% 
  select(ing_cor,sueldos:otros_ing) %>% 
  select(-transfer, -rentas, -(noagrop:pesca) )%>% 
  mutate(decil = ntile(ing_cor, 10)) %>% 
  select(-ing_cor) %>% 
  pivot_longer(cols = !decil, names_to = 'var', values_to = 'val') %>% 
  group_by(decil, var) %>%
  summarise(val = mean(val)) %>% 
  left_join(p_ing) %>% 
  mutate(p_val = val/ing_cor * 100)
  
# Too cluttered

p_income %>% ggplot(aes(factor(decil), p_val, fill = var))+
  geom_col(color = 'black', position = position_stack())+
  geom_label(aes(label = comma(p_val, accuracy = 0.1, suffix = '%')), position = position_stack())+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  coord_flip()

## Treemaps ----

library(treemapify)
?treemapify::geom_treemap()


# i decil
i_dec <- p_income %>% filter(decil ==1)

ggplot(i_dec, ggplot2::aes(area = p_val, fill = var)) +
  geom_treemap()+
  # geom_treemap_text(aes(label = var))+
  geom_treemap_text(
    aes(
      label = paste0(var, "\n", comma(p_val, accuracy = 0.1, suffix = "%"))
    ),
    place = "centre",   # can be "topleft", "topright", "bottomleft", "bottomright"
    align = "centre",   # controls justification
    colour = "white",
    reflow = TRUE       # allows text to resize to fit
  )

treemapify(x_dec, area = 'p_val')

# x) decil
x_dec <- p_income %>% filter(decil ==10)

ggplot(x_dec, ggplot2::aes(area = p_val, fill = var)) +
  geom_treemap()+
  # geom_treemap_text(aes(label = var))+
  geom_treemap_text(
    aes(
      label = paste0(var, "\n", comma(p_val, accuracy = 0.1, suffix = "%"))
    ),
    place = "centre",   # can be "topleft", "topright", "bottomleft", "bottomright"
    align = "centre",   # controls justification
    colour = "white",
    reflow = TRUE       # allows text to resize to fit
  )


# 1%

p_ing <- concentrado_hogar %>% 
  select(ing_cor) %>% 
  mutate(decil = ntile(ing_cor, 10)) %>% 
  filter(decil == 10) %>% 
  mutate(X_decil = ntile(ing_cor, 10)) %>% 
  group_by(X_decil) %>% summarise(ing_cor = mean(ing_cor))

p_income <- concentrado_hogar %>% 
  select(ing_cor,sueldos:otros_ing) %>% 
  select(-transfer, -rentas, -(noagrop:pesca) )%>% 
  mutate(decil = ntile(ing_cor, 10)) %>% 
  filter(decil == 10) %>% 
  mutate(X_decil = ntile(ing_cor, 10)) %>% 
  select(-ing_cor, -decil) %>% 
  pivot_longer(cols = !X_decil, names_to = 'var', values_to = 'val') %>% 
  group_by(X_decil, var) %>%
  summarise(val = mean(val)) %>% 
  left_join(p_ing) %>% 
  mutate(p_val = val/ing_cor * 100)

op_dec <- p_income %>% filter(X_decil ==10)

ggplot(op_dec, ggplot2::aes(area = p_val, fill = var)) +
  geom_treemap()+
  # geom_treemap_text(aes(label = var))+
  geom_treemap_text(
    aes(
      label = paste0(var, "\n", comma(p_val, accuracy = 0.1, suffix = "%"))
    ),
    place = "centre",   # can be "topleft", "topright", "bottomleft", "bottomright"
    align = "centre",   # controls justification
    colour = "white",
    reflow = TRUE       # allows text to resize to fit
  )


# Analisis de rubros de gasto --------------------------------------------------
















