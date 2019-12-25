library(tidyverse)
library(readxl)
library(moderndive)
library(ggpubr)
encuestasHumberto <- as_tibble(read_excel("encuestasHumberto.xlsx", 
                                          sheet = "Hoja1", range = "A1:CD291")) 

var_1 <- encuestasHumberto[,2:35]
namesvar1 <-c(paste("x1",seq(1:34),sep = ""))
names(var_1) <- namesvar1

var_2 <- encuestasHumberto[,36:75]
names(var_2) <- c(paste("x2",seq(1:40),sep = ""))

var_1 <- var_1 %>% mutate(sum_var_1 = apply(.,1,sum))
var_2 <- var_2 %>% mutate(sum_var_2 = apply(.,1,sum)) 

glimpse(var_2)

resultados_humberto <- as_tibble(
  data.frame(var_1 %>% 
               select(var_1 = sum_var_1),
             var_2 %>% 
               select(var_2 = sum_var_2),
             encuestasHumberto %>% 
               select(Escuela = ESCUELA,Edad = edad, 
                      `Género` = genero, Grado = semestre) %>%
               mutate(`Género` = recode_factor(`Género`,
                        `1` = "Femenino",
                        `2` = "Masculino"
                      ),
                      Grado = recode_factor(Grado,
                                            `1` = "Primero",
                                            `2` = "Segundo",
                                            `3` = "Tercero"
                                            ),
                      Escuela = recode(Escuela,
                                       `GDO SHU` = "Solosuchiapa",
                                       `LC REF` = "Reforma",
                                       `NH PICH` = "Pichucalco",
                                       `NH PN` = "Pueblo Nuevo",
                                       `VG IXT` = "Ixtacomitán",
                                       `VG JREZ` = "E. Juárez"
                                       )
                      )
             )
  )


glimpse(resultados_humberto)
get_correlation(resultados_humberto,var_1~var_2)
paleta <- c("Dark2")

gg_edad <- ggplot(data = resultados_humberto, 
                  aes(x = var_1, y = var_2, color = Edad)) +
  geom_jitter() + 
  geom_smooth(method = "lm",se = FALSE) +
  labs(x = "Motivación por las TIC", y = "Infraestructura TIC",
       title = "Clasificación por Edad")

gg_genero <- ggplot(data = resultados_humberto, 
                    aes(x = var_1, y = var_2, color = `Género`)) +
  geom_jitter() + 
  geom_smooth(method = "lm",se = FALSE) +
  scale_color_brewer(palette = paleta) +
  labs(x = "Motivación por las TIC", y = "Infraestructura TIC",
       title = "Clasificación por Género")

gg_escuela <- ggplot(data = resultados_humberto, 
                    aes(x = var_1, y = var_2, color = Escuela)) +
  geom_jitter() + 
  geom_smooth(method = "lm",se = FALSE) +
  scale_color_brewer(palette = paleta) +
  labs(x = "Motivación por las TIC", y = "Infraestructura TIC",
       title = "Clasificación por Escuela")

gg_semestre <- ggplot(data = resultados_humberto, 
                    aes(x = var_1, y = var_2, color = Grado)) +
  geom_jitter() + 
  geom_smooth(method = "lm",se = FALSE) +
  scale_color_brewer(palette = paleta) +
  labs(x = "Motivación por las TIC", y = "Infraestructura TIC",
       title = "Clasificación por Grado")

ggarrange(gg_semestre,gg_escuela,gg_edad,gg_genero,ncol = 2, nrow = 2)

glimpse(resultados_humberto)


ticEscuela <- resultados_humberto %>% select(var_1,var_2,Escuela)
modelo_escuela <- lm(formula = var_1 ~ var_2 + Escuela,data = ticEscuela)
get_regression_table(modelo_escuela)
get_regression_points(modelo_escuela)

get_regression_points(modelo_escuela) %>% summarise(sum(residual^2))


View(encuestasHumberto)
