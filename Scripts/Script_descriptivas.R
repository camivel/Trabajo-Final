#Descriptivas
setwd("C:/Users/Diana Contreras/OneDrive - Universidad de los Andes/1.Big Data/Trajabajo Final/Descriptivas")

require(pacman)
library(cowplot) #Gráficas en una sola
library(gtsummary)
library(officer)
library(flextable)

logit %>%
  tbl_regression()

df<- final2
tbl_descriptivas <- df %>%
  tbl_summary(
    statistic= list(
      all_continuous()~"{mean} ({sd})",
      all_categorical()~"{n} ({p}%)"),
    label = list(trabajo ~ "Niño trabaja", edad_nino ~ "Edad Niño",
             sexo ~ "Sexo niño", estudia ~ "Niño estudia",
             migra_ult3 ~ "Migró en los ult. 3 años", 
             dejo_estudiar ~ "Niño dejó de estudiar",
             RegionLb ~ "RegionLb", 
             t_personas ~ "No. personas en el hogar",
             tipo_vivienda~"Tipo vivienda", sp_estrato~"Estrato",
             familias_accion~"Familias en acción (ult. año)", 
             icbf~"Hogar beneficiario ICBF (ult.año)",
             sena~"Hogar beneficiario Sena (ult.año)",
             jovenes_accion~"Familias en acción (ult. año)",
             ayu_desplazados~"Recibió ayudas desplazados (ult. año)",
             padre_presente~"Padre vive en el hogar",
             madre_presente~"Madre vive en el hogar",
             ing_tot_kn~"Ingreso total del hogar"),
    type= edad_nino ~ "continuous") %>%
  modify_caption("**Tabla 1. Estadisticas descriptivas**") %>% 
  modify_footnote(all_stat_cols()~ "Promedio (Est.Desv)/ Frecuencia(%)")%>%
  as_flex_table() %>%
  flextable::save_as_docx(path="Descriptivas1.docx")