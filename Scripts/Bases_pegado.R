setwd("C:/Users/Diana Contreras/OneDrive - Universidad de los Andes/1.Big Data/Trajabajo Final")
niñosu<-Uninos6a16_2016
niñosr<-Rninos6a16_2016

urban<-colnames(niñosu)
rural<-colnames(niñosr)

interr<-intersect(rural, urban)

niñosr<- subset(niñosr, select=inter)
niñosu<- subset(niñosu, select=inter)
#quitamos labels para poder pegar
niñosu<-glimpse(niñosu %>% mutate_if(is.labelled, to_factor))
niñosr<-glimpse(niñosr %>% mutate_if(is.labelled, to_factor))
#identificador
niñosr$Sector<-"Rural"
niñosu$Sector<-"Urbano"
niños<-rbind(niñosr,niñosu)
niñosf<-select(niños,consecutivo, llave, hogar, llave_n16, hogar_n16, orden, llave_ID_lb,
               llaveper,llaveper_n16, edad_nino, parent_inform, padre_vive, orden_padre,
               edad_padre, educ_padre, trabajo_padre, madre_vive, orden_madre, edad_madre,
               educ_madre, trabajo_madre, trabajo)

save(niñosf, file="niñosf.rda")

#Personas------------------------------------------------------------------------
urban<-colnames(Upersonas)
rural<-colnames(Rpersonas)

inter2<-intersect(rural, urban)

Upersonasf<- subset(Upersonas, select=inter2)
Rpersonasf<- subset(Rpersonas, select=inter2)

Upersonasf<-remove_var_label(Upersonasf)
Rpersonasf<-remove_var_label(Rpersonasf)
#quitamos labels para poder pegar
Upersonasf<-glimpse(Upersonasf %>% mutate_if(is.labelled, to_factor))
Rpersonasf<-glimpse(Rpersonasf %>% mutate_if(is.labelled, to_factor))
#identificador
Rpersonasf$Sector<-"Rural"
Upersonasf$Sector<-"Urbano"
personas<-rbind(Upersonasf,Rpersonasf)

personas <-select(personas,consecutivo, llave, hogar, llave_n16, hogar_n16, orden, llave_ID_lb,
                  llaveper,llaveper_n16, sexo, etnia, estudia, razon_noestudia, nivel_educ,
                  grado_educ, edad_dejoestudio, migra_ult3, dejo_estudiar)

#Hogares------------------------------------------------------------------------
urban<-colnames(Uhogar)
rural<-colnames(Rhogar)

inter3<-intersect(rural, urban)

Rhogarf<- select(Rhogar, all_of(inter3), ing_trabagr, ing_trabnoagr)
Uhogarf<- select(Uhogar, all_of(inter3), ing_trabajo)

Uhogarf$ing_trabagr<-0
Uhogarf$ing_trabnoagr<- 0
Rhogarf$ing_trabajo<- 0

#quitamos labels para poder pegar
Uhogarf<-remove_var_label(Uhogarf)
Rhogarf<-remove_var_label(Rhogarf)
Uhogarf<-glimpse(Uhogarf %>% mutate_if(is.labelled, to_factor))
Rhogarf<-glimpse(Rhogarf %>% mutate_if(is.labelled, to_factor))

hogares<-rbind(Uhogarf,Rhogarf)

hogares <-select(hogares, RegionLb, hogar, llave_n16, hogar_n16,
                 t_personas, tipo_vivienda, sp_estrato, tenencia_vivienda,
                 subsidios, familias_accion, prg_adultomayor,sena, icbf, jovenes_accion, 
                 ayu_desplazados, ing_trabajo, ing_pensiones, ing_arriendos, 
                 ing_intereses_div, ing_ayudas,ing_otros_nrem, ing_trabagr, ing_trabnoagr)

hogares$ing_trabajo<-as.character(hogares$ing_trabajo)
hogares$ing_pensiones<-as.character(hogares$ing_pensiones)
hogares$ing_arriendos<-as.character(hogares$ing_arriendos)
hogares$ing_intereses_div<-as.character(hogares$ing_intereses_div)
hogares$ing_otros_nrem<-as.character(hogares$ing_otros_nrem)
hogares$ing_ayudas<-as.character(hogares$ing_ayudas)
hogares$ing_trabagr<-as.character(hogares$ing_trabagr)
hogares$ing_trabnoagr<-as.character(hogares$ing_trabnoagr)

hogares$ing_trabajo<-as.numeric(hogares$ing_trabajo) 
hogares$ing_pensiones<-as.numeric(hogares$ing_pensiones) 
hogares$ing_arriendos<-as.numeric(hogares$ing_arriendos) 
hogares$ing_intereses_div<-as.numeric(hogares$ing_intereses_div) 
hogares$ing_otros_nrem<-as.numeric(hogares$ing_otros_nrem)
hogares$ing_ayudas<-as.numeric(hogares$ing_ayudas)
hogares$ing_trabagr<-as.numeric(hogares$ing_trabagr)
hogares$ing_trabnoagr<-as.numeric(hogares$ing_trabnoagr)
                            
hogares$ing_pensiones<-as.numeric(hogares$ing_pensiones) 
hogares$ing_arriendos<-as.numeric(hogares$ing_arriendos) 
hogares$ing_intereses_div<-as.numeric(hogares$ing_intereses_div) 
hogares$ing_otros_nrem<-as.numeric(hogares$ing_otros_nrem)
hogares$ing_ayudas<-as.numeric(hogares$ing_ayudas)
hogares$ing_trabagr<-as.numeric(hogares$ing_trabagr)
hogares$ing_trabnoagr<-as.numeric(hogares$ing_trabnoagr)

hogares$ing_trabajo<-ifelse(is.na(hogares$ing_trabajo), 0, hogares$ing_trabajo)
hogares$ing_arriendos<-ifelse(is.na(hogares$ing_arriendos), 0, hogares$ing_arriendos)
hogares$ing_pensiones<-ifelse(is.na(hogares$ing_pensiones), 0, hogares$ing_pensiones)
hogares$ing_intereses_div<-ifelse(is.na(hogares$ing_intereses_div), 0, hogares$ing_intereses_div)
hogares$ing_otros_nrem<-ifelse(is.na(hogares$ing_otros_nrem), 0, hogares$ing_otros_nrem)
hogares$ing_ayudas<-ifelse(is.na(hogares$ing_ayudas), 0, hogares$ing_ayudas)

hogares$ing_tot<-hogares$ing_trabajo + hogares$ing_pensiones + hogares$ing_arriendos +
  hogares$ing_intereses_div + hogares$ing_ayudas + hogares$ing_otros_nrem + 
  hogares$ing_trabagr+ hogares$ing_trabnoagr

hogares <-select(hogares, RegionLb, hogar, llave_n16, hogar_n16,
                 t_personas, tipo_vivienda, sp_estrato, tenencia_vivienda,
                 subsidios, familias_accion, prg_adultomayor,sena, icbf, jovenes_accion, 
                 ayu_desplazados, ing_tot)
#PEGAR BASES-------------------------------------------------------

unidas1<-merge(niñosf, personas)
unidas<-merge(unidas1, hogares, by="llave_n16", all.x=TRUE, all.y=FALSE)

save(unidas, file="unidas.rda")