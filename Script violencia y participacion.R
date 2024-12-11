rm(list=ls())
require(pacman)
p_load(tidyverse, rio, skimr, janitor, ggplot2, haven, dplyr, stargazer,broom,
       xlsx,readxl, data.table, stringr, openxlsx) 

setwd("/Users/valeria/Desktop/Hec Proyecto")


#Bases de datos de elecciones a presidente 
elec_pres_1986 <- read_dta("1986_presidencia.dta")
elec_pres_1990 <- read_dta("1990_presidencia.dta")
elec_pres_1994 <- read_dta("1994_Presidencia_Segunda_Vuelta.dta")
elec_pres_1998 <- read_dta("1998_Presidencia_Segunda_Vuelta.dta")
elec_pres_2002 <- read_dta("2002_Presidencia.dta")
elec_pres_2006 <- read_dta("2006_Presidencia.dta")
elec_pres_2010 <- read_dta("2010_Presidencia_Primera_Vuelta.dta")
elec_pres_2014 <- read_dta("2014_Presidencia_Segunda_Vuelta.dta")
elec_pres_2018 <- read_dta("2018_Presidencia_Primera_Vuelta.dta")


#Bases de datos de población 
poblacion_1985_1994 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_1985-1994.xlsx")
poblacion_1995_2004 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_1995-2004.xlsx")
poblacion_2005_2017 <- read_excel("anexo-area-sexo-edad-proyecciones-poblacion-Municipal_2005-2017.xlsx")
poblacion_2018_2026 <- read_excel("anexo-proyecciones-poblacion-Municipal_2018-2026.xlsx")



#Limpieza bases de datos de población 


#Calcular poblacion mayor de edad para 1985 - 1994
poblacion_1985_1994 <- mutate(.data = poblacion_1985_1994, total_mayores18 = Total_18 + Total_19 + 
                                Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                                Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                                Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                                Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                                Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                                Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                                Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                                Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                                Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                                Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                                Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                                Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                                Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                                Total_98 + Total_99 + Total_100_y_más
)


poblacion_1985_1994 <- mutate(.data = poblacion_1985_1994,
                              poblacion_total = Total_0 + Total_1 + Total_2 + Total_3+
                                Total_4 + Total_5 + Total_6 + Total_7 + Total_8 + Total_9+
                                Total_10 + Total_11 +Total_12 + Total_13 + Total_14 + Total_15+
                                Total_16 + Total_17 + Total_18 + Total_19 + 
                                Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                                Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                                Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                                Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                                Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                                Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                                Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                                Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                                Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                                Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                                Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                                Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                                Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                                Total_98 + Total_99 + Total_100_y_más
)


poblacion_1985_1994 <-poblacion_1985_1994[poblacion_1985_1994$ÁREA_GEOGRÁFICA == "Total", ]  
poblacion_1985_1994 <- subset(poblacion_1985_1994, select = c(MPIO,DPMP, AÑO, ÁREA_GEOGRÁFICA,total_mayores18, poblacion_total))
poblacion_1985_1994 <-subset(poblacion_1985_1994, AÑO == 1986 | AÑO == 1990 | AÑO == 1994)


poblacion_1995_2004 <- mutate(.data = poblacion_1995_2004, total_mayores18 = Total_18 + Total_19 + 
                                Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                                Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                                Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                                Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                                Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                                Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                                Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                                Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                                Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                                Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                                Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                                Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                                Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                                Total_98 + Total_99 + Total_100_y_más
)

poblacion_1995_2004 <- mutate(.data = poblacion_1995_2004,
                              poblacion_total = Total_0 + Total_1 + Total_2 + Total_3+
                                Total_4 + Total_5 + Total_6 + Total_7 + Total_8 + Total_9+
                                Total_10 + Total_11 +Total_12 + Total_13 + Total_14 + Total_15+
                                Total_16 + Total_17 + Total_18 + Total_19 + 
                                Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                                Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                                Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                                Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                                Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                                Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                                Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                                Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                                Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                                Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                                Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                                Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                                Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                                Total_98 + Total_99 + Total_100_y_más
)

poblacion_1995_2004 <-poblacion_1995_2004[poblacion_1995_2004$ÁREA_GEOGRÁFICA == "Total", ]  
poblacion_1995_2004 <- subset(poblacion_1995_2004, select = c(MPIO,DPMP, AÑO, ÁREA_GEOGRÁFICA,total_mayores18, poblacion_total ))
poblacion_1995_2004 <-subset(poblacion_1995_2004,   AÑO == 1998  | AÑO == 2002 )






poblacion_2005_2017 <- mutate(.data = poblacion_2005_2017, total_mayores18 = Total_18 + Total_19 + 
                                Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                                Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                                Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                                Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                                Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                                Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                                Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                                Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                                Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                                Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                                Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                                Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                                Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                                Total_98 + Total_99 + Total_100_y_más
)

poblacion_2005_2017 <- mutate(.data = poblacion_2005_2017,
                              poblacion_total = Total_0 + Total_1 + Total_2 + Total_3+
                                Total_4 + Total_5 + Total_6 + Total_7 + Total_8 + Total_9+
                                Total_10 + Total_11 +Total_12 + Total_13 + Total_14 + Total_15+
                                Total_16 + Total_17 + Total_18 + Total_19 + 
                                Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                                Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                                Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                                Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                                Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                                Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                                Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                                Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                                Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                                Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                                Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                                Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                                Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                                Total_98 + Total_99 + Total_100_y_más
)



poblacion_2005_2017 <-poblacion_2005_2017[poblacion_2005_2017$ÁREA_GEOGRÁFICA == "Total", ]  
poblacion_2005_2017 <- subset(poblacion_2005_2017, select = c(MPIO,DPMP, AÑO, ÁREA_GEOGRÁFICA,total_mayores18, poblacion_total))
poblacion_2005_2017 <-subset(poblacion_2005_2017, AÑO == 2006 | AÑO == 2010 | AÑO == 2014 )





poblacion_2018_2026 <- mutate(.data = poblacion_2018_2026, total_mayores18 = Total_18 + Total_19 + 
                                Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                                Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                                Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                                Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                                Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                                Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                                Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                                Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                                Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                                Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                                Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                                Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                                Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                                Total_98 + Total_99 + Total_100_y_más
)


poblacion_2018_2026 <- mutate(.data = poblacion_2018_2026,
                              poblacion_total = Total_0 + Total_1 + Total_2 + Total_3+
                                Total_4 + Total_5 + Total_6 + Total_7 + Total_8 + Total_9+
                                Total_10 + Total_11 +Total_12 + Total_13 + Total_14 + Total_15+
                                Total_16 + Total_17 + Total_18 + Total_19 + 
                                Total_20 + Total_21 + Total_22 + Total_23 + Total_24 + Total_25 +
                                Total_26 + Total_27 + Total_28 + Total_29 + Total_30 + Total_31 +
                                Total_32 + Total_33 + Total_34 + Total_35 + Total_36 + Total_37 +
                                Total_38 + Total_39 + Total_40 + Total_41 + Total_42 + Total_43 + 
                                Total_44 + Total_45 + Total_46 + Total_47 + Total_48 + Total_49 + 
                                Total_50 + Total_51 + Total_52 + Total_53 + Total_54 + Total_55 + 
                                Total_56 + Total_57 + Total_58 + Total_59 + Total_60 + Total_61 + 
                                Total_62 + Total_63 + Total_64 + Total_65 + Total_66 + Total_67 + 
                                Total_68 + Total_69 + Total_70 + Total_71 + Total_72 + Total_73 + 
                                Total_74 + Total_75 + Total_76 + Total_77 + Total_78 + Total_79 + 
                                Total_80 + Total_81 + Total_82 + Total_83 + Total_84 + Total_85 + 
                                Total_86 + Total_87 + Total_88 + Total_89 + Total_90 + Total_91 +
                                Total_92 + Total_93 + Total_94 + Total_95 + Total_96 + Total_97 +
                                Total_98 + Total_99 + Total_100_y_más
)



poblacion_2018_2026 <-poblacion_2018_2026[poblacion_2018_2026$ÁREA_GEOGRÁFICA == "Total", ]  
poblacion_2018_2026 <- subset(poblacion_2018_2026, select = c(MPIO,DPMP, AÑO, ÁREA_GEOGRÁFICA,total_mayores18, poblacion_total))
poblacion_2018_2026 <-subset(poblacion_2018_2026, AÑO == 2018 )



poblacion_1986_2018 <- rbind(poblacion_1985_1994, poblacion_1995_2004, poblacion_2005_2017, poblacion_2018_2026)
poblacion_1986_2018 = arrange(.data=poblacion_1986_2018 , DPMP)

rm(poblacion_1985_1994, poblacion_1995_2004, poblacion_2005_2017, poblacion_2018_2026)

poblacion_1986_2018$codmpio <- poblacion_1986_2018$DPMP
poblacion_1986_2018$ano <- poblacion_1986_2018$AÑO
poblacion_1986_2018$codmpio <- as.numeric(as.character(poblacion_1986_2018$codmpio))
poblacion_1986_2018 <- subset(poblacion_1986_2018, select = c(MPIO,codmpio, ano,total_mayores18, poblacion_total ))


#Limpieza bases de datos presidencia 

elec_pres_1986 <- subset(elec_pres_1986, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1990 <- subset(elec_pres_1990, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1994 <- subset(elec_pres_1994, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_1998 <- subset(elec_pres_1998, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2002 <- subset(elec_pres_2002, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2006 <- subset(elec_pres_2006, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2010 <- subset(elec_pres_2010, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2014 <- subset(elec_pres_2014, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))
elec_pres_2018 <- subset(elec_pres_2018, select = c(ano, fecha_eleccion, codmpio, municipio, primer_apellido, nombres, votos))



total_votos_pres_1986 <- aggregate(votos ~ codmpio, data = elec_pres_1986, sum)
ano <- rep(1986,nrow(total_votos_pres_1986))
total_votos_pres_1986 <- cbind(total_votos_pres_1986, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_1986))
total_votos_pres_1986<- cbind(total_votos_pres_1986, eleccion)
total_votos_pres_1986 <- subset(total_votos_pres_1986, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_1990 <- aggregate(votos ~ codmpio, data = elec_pres_1990, sum)
ano <- rep(1990,nrow(total_votos_pres_1990))
total_votos_pres_1990 <- cbind(total_votos_pres_1990, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_1990))
total_votos_pres_1990<- cbind(total_votos_pres_1990, eleccion)
total_votos_pres_1990 <- subset(total_votos_pres_1990, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_1994 <- aggregate(votos ~ codmpio, data = elec_pres_1994, sum)
ano <- rep(1994,nrow(total_votos_pres_1994))
total_votos_pres_1994 <- cbind(total_votos_pres_1994, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_1994))
total_votos_pres_1994<- cbind(total_votos_pres_1994, eleccion)
total_votos_pres_1994 <- subset(total_votos_pres_1994, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_1998 <- aggregate(votos ~ codmpio, data = elec_pres_1998, sum)
ano <- rep(1998,nrow(total_votos_pres_1998))
total_votos_pres_1998 <- cbind(total_votos_pres_1998, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_1998))
total_votos_pres_1998<- cbind(total_votos_pres_1998, eleccion)
total_votos_pres_1998 <- subset(total_votos_pres_1998, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2002 <- aggregate(votos ~ codmpio, data = elec_pres_2002, sum)
ano <- rep(2002,nrow(total_votos_pres_2002))
total_votos_pres_2002 <- cbind(total_votos_pres_2002, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2002))
total_votos_pres_2002<- cbind(total_votos_pres_2002, eleccion)
total_votos_pres_2002 <- subset(total_votos_pres_2002, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2006 <- aggregate(votos ~ codmpio, data = elec_pres_2006, sum)
ano <- rep(2006,nrow(total_votos_pres_2006))
total_votos_pres_2006 <- cbind(total_votos_pres_2006, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2006))
total_votos_pres_2006<- cbind(total_votos_pres_2006, eleccion)
total_votos_pres_2006 <- subset(total_votos_pres_2006, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2010 <- aggregate(votos ~ codmpio, data = elec_pres_2010, sum)
ano <- rep(2010,nrow(total_votos_pres_2010))
total_votos_pres_2010 <- cbind(total_votos_pres_2010, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2010))
total_votos_pres_2010<- cbind(total_votos_pres_2010, eleccion)
total_votos_pres_2010 <- subset(total_votos_pres_2010, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2014 <- aggregate(votos ~ codmpio, data = elec_pres_2014, sum)
ano <- rep(2014,nrow(total_votos_pres_2014))
total_votos_pres_2014 <- cbind(total_votos_pres_2014, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2014))
total_votos_pres_2014<- cbind(total_votos_pres_2014, eleccion)
total_votos_pres_2014 <- subset(total_votos_pres_2014, select = c(ano,codmpio,votos, eleccion))


total_votos_pres_2018 <- aggregate(votos ~ codmpio, data = elec_pres_2018, sum)
ano <- rep(2018,nrow(total_votos_pres_2018))
total_votos_pres_2018 <- cbind(total_votos_pres_2018, ano)
eleccion <- rep("presidencia", nrow(total_votos_pres_2018))
total_votos_pres_2018<- cbind(total_votos_pres_2018, eleccion)
total_votos_pres_2018 <- subset(total_votos_pres_2018, select = c(ano,codmpio,votos, eleccion))


rm( elec_pres_1986, elec_pres_1990, elec_pres_1994,elec_pres_1998, elec_pres_2002, elec_pres_2006,
    elec_pres_2010, elec_pres_2014, elec_pres_2018)

votos_totales <- rbind(total_votos_pres_1986, total_votos_pres_1990, total_votos_pres_1994, 
                       total_votos_pres_1998, total_votos_pres_2002, total_votos_pres_2006, 
                       total_votos_pres_2010, total_votos_pres_2014, total_votos_pres_2018)



votos_totales <- arrange(.data=votos_totales , codmpio)

rm(total_votos_pres_1986, total_votos_pres_1990,
   total_votos_pres_1994, 
   total_votos_pres_1998, 
   total_votos_pres_2002, 
   total_votos_pres_2006, total_votos_pres_2010, 
   total_votos_pres_2014, 
   total_votos_pres_2018 )

poblacion_y_votos <- left_join(x=poblacion_1986_2018, y=votos_totales, by=c("codmpio","ano"))

rm(votos_totales, poblacion_1986_2018)

poblacion_y_votos <- mutate(.data = poblacion_y_votos, participacion = votos/total_mayores18)
poblacion_y_votos <- na.omit(poblacion_y_votos)

poblacion_y_votos <- subset(poblacion_y_votos, total_mayores18 != 0 &  votos != 0 & participacion < 1)



#Control ingreso


buen_gob <- read_dta("PANEL_BUEN_GOBIERNO(2022).dta")

buen_gob<- subset(buen_gob, select = c(codmpio,ano,y_total))


poblacion_votos_ingreso <- left_join(x=poblacion_y_votos, y=buen_gob, by=c("codmpio","ano"))

rm(buen_gob, poblacion_y_votos)


# 1. Evaluamos cualuiqer actividad letal de grupos armados al margen de la ley.

# 1.1 Violencia sexual - victimas

vic_VS <- read_excel("VictimasVS_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona","Situación Actual de la Víctima","Año", "Mes", "Día", "Calidad de la Víctima o la Baja"), 
           new =c("codmpio","id_persona","vic_stat", "ano","mes","dia", "tipo")) %>% 
  filter(ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00") %>% 
  select(id_persona,codmpio,Edad,vic_stat,Municipio,ano,mes,dia, tipo)

# Filtramos para los muertos

vic_VS <- vic_VS %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric) %>% 
  filter(vic_stat !="VIVA" )


# Creamos la variable que tome los valores del número total de muertes

vic_VS <- vic_VS %>% 
  count(ano, codmpio, mes, name = "tot_M_VS")


# 1.2 Victimas - secuestro

# Como estamos analizando la letalidad (muertos producidos) de los ataques, no vamos a incluir esta variable
# dentro de este apartado, se analizará después. Además, en la base no se identifica algun muerto producido 
# mientras se realizaba el secuestro.


# 1.3 Victimas - Reclutamiento Forzado de menores


# Como de momento estamos analizando ataques que provoquen una disminución en la población en edad de votar
# analizaremos esto después.


# 1.4 Victimas - Minas

vic_MI <- read_excel("VictimasMI_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona","Situación Actual de la Víctima", "Mes", "Día", "Año", "Calidad de la Víctima o la Baja"), 
           new =c("codmpio","id_persona","vic_stat","mes", "dia", "ano", "tipo")) %>% 
  select(codmpio,id_persona,vic_stat, ano, mes, dia, Edad, Municipio, tipo) %>% 
  filter(ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00")  

# Filtramos para los muertos

vic_MI <- vic_MI %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric) %>% 
  filter(vic_stat != "HERIDA" )  


# Creamos la variable que tome los valores del número total de muertes

vic_MI <- vic_MI %>% 
  count(ano, codmpio, mes,name = "tot_M_MI")



# 1.5 Victimas - Masacre (esta base considera que todas las observaciones son personas asesinadas)

vic_MA <- read_excel("VictimasMA_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona", "Mes", "Día", "Año", "Calidad de la Víctima o la Baja"), 
           new =c("codmpio","id_persona","mes", "dia", "ano", "tipo")) %>% 
  select(codmpio,id_persona, ano, mes, dia, Edad, Municipio, tipo) %>% 
  filter( ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00")  %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric)

# Creamos la variable que tome los valores del número total de muertes

vic_MA <- vic_MA %>% 
  count(ano, codmpio, mes, name = "tot_M_MA")



# 1.6 Victima - Acciones Belicas - Civiles (esta base considera que todas las observaciones son personas asesinadas)

vic_AB <-  read_excel("VictimasAB_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona", "Mes", "Día", "Año","Calidad de la Víctima o la Baja"),
           new =c("codmpio","id_persona","mes", "dia", "ano","tipo")) %>% 
  filter(ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00") %>% 
  select(codmpio,id_persona, ano, mes, dia, Edad, Municipio,tipo) %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric)

# Creamos la variable que tome los valores del número total de muertes

vic_AB <- vic_AB %>% 
  count(ano, codmpio, mes, name = "tot_M_AB")


# 1.7 Victimas - Ataques Poblaciones 


# Esta base toma en cuenta ataques que implican "Las modalidades de violencia que conforman los ataques a la población 
# civil se organizan en tres categorías, definidas en función de la naturaleza del daño infligido, a saber, 
# la integridad física y la vida, la libertad y los bienes".

# Como conisdera más ampliamente atentados contra la población civil, no especifica la Situacion Actual de la 
# victima, y es ambiguo de si el abandono de tierras o desplazamiento es especificamente de los civiles, 
# la tomamos en el siguiente apartado.

# 1.8 Victimas - Atentado Terrorista 


# Esta base toma en cuenta ataques que implican "Se entiende como todo ataque perpetrado mediante el uso de explosivos, 
# los cuales ocurren en zonas densamente pobladas y en los que hay afectación plural a personas o a bienes civiles, 
# independientemente de si el objetivo de la acción es civil o militar.".

# Como conisdera más ampliamente atentados contra la población civil, no especifica la Situacion Actual de la 
# victima, y es ambiguo de si el abandono de tierras o desplazamiento es especificamente de los civiles, 
# la tomamos en el siguiente apartado.


# 1.9 Victimas - Daños a Bienes Civiles

# Como de momento estamos analizando ataques que provoquen una disminución en la población en edad de votar
# analizaremos esto después.


# 1.10 Victimas - Desaparicion Forzada

# No vamos a incluir esta variable para este apartado ya que solo considera un ataque donde en su procedimiento
# no hubo muertos (segun la base). Hay muertos, pero son vicitmas que aparecieron muertas. Estas consideraciones
# se toman en cuenta en el siguiente apartado.


# 1.11 Victimas - Asesinato selectivo (esta base considera que todas las observaciones son personas asesinadas)

vic_AS <- read_excel("VictimasAS_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona", "Mes", "Día", "Año","Calidad de la Víctima o la Baja"),
           new =c("codmpio","id_persona","mes", "dia", "ano","tipo")) %>% 
  filter(ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00") %>% 
  select(codmpio,id_persona, ano, mes, dia, Edad, Municipio,tipo) %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric)


# Creamos la variable que tome los valores del número total de muertes

vic_AS <- vic_AS %>% 
  count(ano, codmpio, mes, name = "tot_M_AS")


# Unimos las bases    

u_pob_EV <- list(vic_AB,vic_MA,vic_MI,vic_VS,vic_AS) %>% 
  reduce(full_join, by = c("ano","codmpio", "mes" ))




#=======================================================================================================================================================================================#        

# 2. Evaluamos cualuiqer actividad  de grupos armados al margen de la ley que genere cualquier
#    tipo de desincentivo a votar. En este caso, consideramos todos los ataques no letales.


# 2.1 Violencia Sexual

vic_VS_b <-   read_excel("VictimasVS_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona","Situación Actual de la Víctima","Año", "Mes", "Día", "Calidad de la Víctima o la Baja"), 
           new =c("codmpio","id_persona","vic_stat", "ano","mes","dia", "tipo")) %>% 
  filter( ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "0000" & mes != "00") %>% 
  select(id_persona,codmpio,Edad,vic_stat,Municipio,ano,mes,dia, tipo)


# Filtramos para los que no murieron

vic_VS_b <- vic_VS_b %>% 
  filter(vic_stat !="MUERTA" ) 

# Creamos la variable que indique el número de casos

vic_VS_b <- vic_VS_b %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric) %>% 
  count(ano, codmpio, mes,name = "tot_VS") 


# 2.2 Secuestro


vic_SE_b <- read_excel("VictimasSE_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "Calidad de la Víctima o la Baja","ID Persona","Situación Actual de la Víctima", "Días de Cautiverio", "Mes", "Día", "Año"), 
           new =c("codmpio","tipo","id_persona","vic_stat", "dias_caut","mes", "dia", "ano")) %>% 
  filter( ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00") %>% 
  select(codmpio,id_persona,vic_stat, dias_caut, ano, mes, dia, Edad, Municipio) 


# Creamos la variable que tome los valores del número total de secuestros 

vic_SE_b <- vic_SE_b %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric) %>% 
  count(ano, codmpio, mes, name = "tot_SE")



# 2.3 - Reclutamiento - Menores

vic_RF_b <- read_excel("VictimasRU_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio","ID Persona", "Mes", "Día", "Año"), 
           new =c("codmpio","id_persona","mes", "dia", "ano")) %>% 
  filter( ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00") %>% 
  select(codmpio,id_persona, ano, mes, dia, Municipio) 

# Creamos la variable que indique el número de casos

vic_RF_b <- vic_RF_b %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric) %>% 
  count(ano, codmpio, mes,name = "tot_RF") 


# 2.4 - Victimas - Minas

vic_MI_b <- read_excel("VictimasMI_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona","Situación Actual de la Víctima", "Mes", "Día", "Año", "Calidad de la Víctima o la Baja"), 
           new =c("codmpio","id_persona","vic_stat","mes", "dia", "ano", "tipo")) %>% 
  select(codmpio,id_persona,vic_stat, ano, mes, dia, Edad, Municipio, tipo) %>% 
  filter( ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00")  


# Filtramos para los que no murieron

vic_MI_b <- vic_MI_b %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric) %>% 
  filter(vic_stat !="MUERTA" )

# Creamos la variable que tome los valores del número total de muertes y afectados

vic_MI_b <- vic_MI_b %>% 
  count(ano, codmpio, mes,name = "tot_MI")


# 2.5 - Victimas - Masacre (esta base considera que todas las observaciones son personas asesinadas)

# No consideramos esta variable ya que todas las observaciones son muertos


# 2.6 Victima - Acciones Belicas - Civiles ( considera que todas las observaciones son personas asesinadas)


# No consideramos esta variable ya que todas las observaciones son muertos


# 2.7 Victimas - Ataques Poblaciones 


vic_AP_b <- read_excel("VictimasAP_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona", "Mes", "Día", "Año","Calidad de la Víctima o la Baja"),
           new =c("codmpio","id_persona","mes", "dia", "ano","tipo")) %>% 
  filter( ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00") %>% 
  select(codmpio,id_persona, ano, mes, dia, Edad, Municipio,tipo) %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric)

# Creamos la variable que tome los valores del número total de afrectados

vic_AP_b <- vic_AP_b %>% 
  count(ano, codmpio, mes, name = "tot_AP")


# 2.8 Victimas - Atentados terroristas

vic_AT_b <- read_excel("VictimasAT_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona", "Mes", "Día", "Año","Calidad de la Víctima o la Baja"),
           new =c("codmpio","id_persona","mes", "dia", "ano","tipo")) %>% 
  filter( ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00") %>% 
  select(codmpio,id_persona, ano, mes, dia, Edad, Municipio,tipo) %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric)

# Creamos la variable que tome los valores del número total de afectados

vic_AT_b <- vic_AT_b %>% 
  count(ano, codmpio, mes, name = "tot_AT")



# 2.9 Victimas - Daños bienes civiles

vic_DB_b <- read_excel("VictimasDB_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona", "Mes", "Día", "Año","Calidad de la Víctima o la Baja"),
           new =c("codmpio","id_persona","mes", "dia", "ano","tipo")) %>% 
  filter(ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00") %>% 
  select(codmpio,id_persona, ano, mes, dia, Edad, Municipio,tipo) %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric)

# Creamos la variable que tome los valores del número total de afectados

vic_DB_b <- vic_DB_b %>% 
  count(ano, codmpio, mes, name = "tot_DB")




# 2.10 - Desaparicion Forzada       

vic_DF_b <- read_excel("VictimasDF_202209.xlsx") %>% 
  setnames(old=c("Código DANE de Municipio", "ID Persona","Situación Actual de la Víctima", "Mes", "Día", "Año", "Calidad de la Víctima o la Baja"), 
           new =c("codmpio","id_persona","vic_stat","mes", "dia", "ano", "tipo")) %>% 
  select(codmpio,id_persona,vic_stat, ano, mes, dia, Edad, Municipio, tipo) %>% 
  filter( ano != "0000" & Municipio != "SIN INFORMACION" & codmpio != "0" & mes != "00")


# Creamos la variable que tome los valores del número total de muertes

vic_DF_b <- vic_DF_b %>% 
  mutate_at(.vars=c("ano","codmpio", "mes", "dia"), .funs=as.numeric) %>% 
  count(ano, codmpio, mes,name = "tot_DF")


# 2.11 - Victimas - Asesinamiento selectivo


# No consideramos esta variable ya que todas las observaciones son muertos



# Unimos las bases    

u_VOT <- list( vic_AP_b, vic_AT_b, vic_DB_b, vic_MI_b, vic_RF_b, vic_SE_b, vic_VS_b, vic_DF_b) %>% 
  reduce(full_join, by = c("ano","codmpio", "mes")) 




# 3. 


# Para cada base, creamos una variable que sea la suma de todas la variables que denoten ataques

# 1. Para u_pob_EV

u_pob_EV<- u_pob_EV %>% drop_na(codmpio, mes)
u_pob_EV <- u_pob_EV %>%  
  replace(is.na(u_pob_EV), 0) %>% 
  group_by(ano, codmpio, mes) %>% 
  mutate(ataque_letal= tot_M_AB + tot_M_AS 
         + tot_M_MA + tot_M_MI  + tot_M_VS)





# 2. Para  u_VOT

u_VOT<- u_VOT %>% drop_na(codmpio, mes)
u_VOT <- u_VOT %>%  
  replace(is.na(u_VOT), 0) %>% 
  group_by(ano, codmpio, mes) %>% 
  mutate(ataque_n_letal= tot_AP + tot_AT + 
           tot_DB + tot_DF + tot_MI + tot_RF +
           tot_SE + tot_VS) 



u_VOT <-  u_VOT[order(u_VOT$codmpio,u_VOT$ano, u_VOT$mes),]


# 3. Unimos las bases en una sola

ataques_killings <- full_join(x=u_VOT, y=u_pob_EV, by=c("ano","codmpio", "mes")) %>% 
  select(codmpio, ano, mes, ataque_letal, ataque_n_letal) 
ataques_killings <-  ataques_killings[order(ataques_killings$codmpio,ataques_killings$ano, ataques_killings$mes),]


#=======================================================================================================================================================================================#        


# Creamos la base de datos de violencia agregandola por el promedio de los 4 años anteriores a las elecciones


ataques_killings_mean4 <- ataques_killings %>% 
  group_by(codmpio, ano) %>% 
  summarise_at(c("ataque_letal", "ataque_n_letal"), sum, na.rm=T)

#=======================================================================================================================================================================================#        


# Creamos la variable que indique el promedio de ataques letales los cuatro años anteriores al las elecciones presidenciales

"1986_m4_l" <- ataques_killings_mean4 %>% subset(ano >= 1982 &  1986>=ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_1986_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_1986_m4_let"), .funs=as.numeric)

"1990_m4_l" <- ataques_killings_mean4 %>% subset(ano >= 1986 &  1990>=ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_1990_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_1990_m4_let"), .funs=as.numeric)

"1994_m4_l" <- ataques_killings_mean4 %>% subset(ano >= 1990 & 1994 >=ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_1994_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_1994_m4_let"), .funs=as.numeric)     


"1998_m4_l" <- ataques_killings_mean4 %>% subset(ano >= 1994 &  1998>= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_1998_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_1998_m4_let"), .funs=as.numeric)  

"2002_m4_l" <- ataques_killings_mean4 %>% subset(ano >= 1998 & 2002 >= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2002_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_2002_m4_let"), .funs=as.numeric)  

"2006_m4_l" <- ataques_killings_mean4 %>% subset(ano>= 2002 & 2006>=ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2006_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_2006_m4_let"), .funs=as.numeric) 


"2010_m4_l" <- ataques_killings_mean4 %>% subset(ano >= 2006 & 2010 >= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2010_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_2010_m4_let"), .funs=as.numeric) 


"2014_m4_l" <- ataques_killings_mean4 %>% subset(ano >= 2010 & 2014 >= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2014_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_2014_m4_let"), .funs=as.numeric) 


"2018_m4_l" <- ataques_killings_mean4 %>% subset(ano >= 2014 & 2018 >= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2018_m4_let"= mean(ataque_letal)) %>% 
  mutate_at(.vars=c("e_2018_m4_let"), .funs=as.numeric) 


# Unimos las bases



ataq_m4_l_anos <- list( `1986_m4_l`,  `1990_m4_l` , `1994_m4_l` , `1998_m4_l`  ,
                        `2006_m4_l` ,  `2010_m4_l` ,  `2014_m4_l` ,  `2018_m4_l` , `2002_m4_l`) %>% 
  reduce(full_join, by = c("codmpio")) 




# Creamos la variable que indique el promedio de ataques no letales los cuatro años anteriores al las elecciones presidenciales

"1986_m4_n_l" <- ataques_killings_mean4 %>% subset(ano >= 1982 &  1986>=ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_1986_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_1986_m4_n_let"), .funs=as.numeric)

"1990_m4_n_l" <- ataques_killings_mean4 %>% subset(ano >= 1986 &  1990>=ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_1990_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_1990_m4_n_let"), .funs=as.numeric)

"1994_m4_n_l" <- ataques_killings_mean4 %>% subset(ano >= 1990 & 1994 >=ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_1994_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_1994_m4_n_let"), .funs=as.numeric)     


"1998_m4_n_l" <- ataques_killings_mean4 %>% subset(ano >= 1994 &  1998>= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_1998_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_1998_m4_n_let"), .funs=as.numeric)  

"2002_m4_n_l" <- ataques_killings_mean4 %>% subset(ano >= 1998 & 2002 >= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2002_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_2002_m4_n_let"), .funs=as.numeric)  

"2006_m4_n_l" <- ataques_killings_mean4 %>% subset(ano>= 2002 & 2006>=ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2006_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_2006_m4_n_let"), .funs=as.numeric) 


"2010_m4_n_l" <- ataques_killings_mean4 %>% subset(ano >= 2006 & 2010 >= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2010_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_2010_m4_n_let"), .funs=as.numeric) 


"2014_m4_n_l" <- ataques_killings_mean4 %>% subset(ano >= 2010 & 2014 >= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2014_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_2014_m4_n_let"), .funs=as.numeric) 


"2018_m4_n_l" <- ataques_killings_mean4 %>% subset(ano >= 2014 & 2018 >= ano) %>% 
  group_by(codmpio) %>% 
  summarise("e_2018_m4_n_let"= mean(ataque_n_letal)) %>% 
  mutate_at(.vars=c("e_2018_m4_n_let"), .funs=as.numeric) 


# Unimos las bases



ataq_m4_n_l_anos <- list( `1986_m4_n_l`,  `1990_m4_n_l` , `1994_m4_n_l` , `1998_m4_n_l`  ,
                          `2006_m4_n_l` ,  `2010_m4_n_l` ,  `2014_m4_n_l` ,  `2018_m4_n_l` , `2002_m4_n_l`) %>% 
  reduce(full_join, by = c("codmpio")) 



# Hacemos un reshape a los datos

# Para ataques letales

ataq_l_m4 <- melt(setDT(ataq_m4_l_anos), id.vars = c("codmpio"), variable.name = "ano") %>% 
  rename(ataq_l=value) %>% 
  mutate_at(.vars=c("ano"), .funs = as.character) 



ataq_l_m4 <-  ataq_l_m4 %>%  mutate(ano=substring(ataq_l_m4$ano,3,6)) 

ataq_l_m4 <- ataq_l_m4 %>%  mutate_at(.vars=c("ano"), .funs=as.numeric)



# Para ataques no letales

ataq_n_l_m4 <- melt(setDT(ataq_m4_n_l_anos), id.vars = c("codmpio"), variable.name = "ano")%>% 
  rename(ataq_no_l=value)  

ataq_n_l_m4 <-  ataq_n_l_m4 %>%  mutate(ano=substring(ataq_l_m4$ano,1,6)) 

ataq_n_l_m4 <- ataq_n_l_m4 %>%  mutate_at(.vars=c("ano"), .funs=as.numeric)



# Unimos bases de datos

ataq_promedio = inner_join(x=ataq_l_m4, y=ataq_n_l_m4, by=c("codmpio","ano"))

# Por cada 100,000 habitantes

ataq_promedio <- ataq_promedio %>% 
  mutate(ataq_l_100k = (ataq_l/(poblacion_votos_ingreso$poblacion_total))*100000) %>% 
  mutate(ataq_no_l_100k = (ataq_no_l/(poblacion_votos_ingreso$poblacion_total))*100000) %>% 
  select(codmpio, ano, ataq_l_100k, ataq_no_l_100k)


#=======================================================================================================================================================================================#        

# Unimos la base con la de participacion     


particip_vio <- full_join(x=poblacion_votos_ingreso, y=ataq_promedio, by=c("codmpio","ano")) %>% 
  filter(!is.na(MPIO))


ipc86 <- rep(0.2095, nrow(particip_vio))
particip_vio<- cbind(particip_vio, ipc86)

particip_vio<- mutate(.data = particip_vio, 
                      ingresos_reales = (particip_vio$y_total)/particip_vio$ipc86)

particip_vio$ingresos_reales<-ifelse(particip_vio$ano==1986, particip_vio$ingresos_reales*0.2095,
                                     particip_vio$ingresos_reales)


openxlsx::write.xlsx(particip_vio, file="particip_vio.xlsx")

rm(list=ls() [! ls() %in% c("particip_vio")])


#=======================================================================================================================================================================================#       
rm(list=ls())


vio_partpc<- read_excel("particip_vio_total.xlsx")
particip_vio<- read_dta("particip_vio.dta")

#Estadísticas


#Estadisticas Descriptivas por Variables y por Controles
violencia <- subset(particip_vio, select=c(participacion,ataq_l_10k, ataq_no_l_10k, y_total))
summary(violencia)


#Grafico de Barras
#3.Participacion por Año
ggplot(vio_partpc, aes(x = ano, y = participacion)) + 
  geom_bar(stat = "summary", fun = "mean",col="LightSkyBlue") +
  labs(x = "Año", y = "Participación")


#Ataques Letales por Año
ggplot(vio_partpc, aes(x = ano, y = ataq_l)) + 
  geom_bar(stat = "summary", fun = "mean", col="LightSkyBlue")+
  labs(x = "Año", y = "Ataques Letales")

#1.Ataques Letales per 10k por Año
ggplot(particip_vio, aes(x = ano, y = ataq_l_10k)) + 
  geom_bar(stat = "summary", fun = "mean", col="LightSkyBlue") +
  labs(x = "Año", y = "Ataques Letales por cada 10k habitantes")

#Ataques No Letales por Año
ggplot(vio_partpc, aes(x = ano, y = ataq_no_l)) + 
  geom_bar(stat = "summary", fun = "mean", col="LightSkyBlue")+
  labs(x = "Año", y = "Ataques No Letales")

#2.Ataques No Letales 10k por Año
ggplot(particip_vio, aes(x = ano, y = ataq_no_l_10k)) + 
  geom_bar(stat = "summary", fun = "mean", col="LightSkyBlue")+
  labs(x = "Año", y = "Ataques No Letales por cada 10k habitantes")


promedios <- particip_vio %>%
  group_by(MPIO) %>%
  summarise(partmean = mean(ataq_l_10k))

mayores <- promedios %>% top_n(3,partmean )
menores <- promedios %>% top_n(-3,partmean )

promedioss <- vio_partpc %>%
  group_by(MPIO) %>%
  summarise(mean = mean(participacion))

#1.Ataques Letales per 10k por Año
ggplot(mayores, aes(x = MPIO, y = partmean)) + 
  geom_bar(stat = "summary", fun = "mean", col="LightSkyBlue") +
  labs(x = "Año", y = "Ataques Letales MPIO")


#Los 3 departamentos con menos participacion en promedio
top_n(vio_partpc, n=-3, participacion) %>%
  ggplot(vio_partpc, mapping= aes(x=MPIO, y=participacion))+
  geom_bar(stat = "summary", fun = "mean", col="LightSkyBlue")
#Los 3 departamentos con menos participacion en promedio
top_n(vio_partpc, n=3, participacion) %>%
  ggplot(vio_partpc, mapping= aes(x=MPIO, y=participacion))+
  geom_bar(stat = "summary", fun = "mean", col="LightSkyBlue")


#Graficos de Dispersión

#Ingreso total vs participacion
collapse(particip_vio , y_total + participacion ~ MPIO, fmean)
plot(particip_vio$y_total, vio_partpc$participacion, main = "Ingreso vs participacion ",
     xlab = "Ingreso", ylab = "Participacion",
     pch = 19, frame = FALSE)
abline (lm(participacion~y_total, data=particip_vio), col="LightSkyBlue")

#Ataques Letales vs participacion 
collapse(vio_partpc, ataq_l + participacion ~ MPIO, fmean)
plot(vio_partpc$ataq_l, vio_partpc$participacion, main = "Ataques Letales vs participacion ",
     xlab = "Ataques Letales", ylab = "Participacion",
     pch = 19, frame = FALSE)
abline (lm(participacion~ataq_l, data=vio_partpc), col="LightSkyBlue")

#4.Ataques Letales 10k vs participacion
collapse(vio_partpc, ataq_l_10k + participacion ~ MPIO, fmean)
plot(particip_vio$ataq_l_10k, vio_partpc$participacion, main = "Ataques Letales por cada 10k habitantes vs Participación ",
     xlab = "Ataques Letales", ylab = "Participacion",
     pch = 19, frame = FALSE)
abline (lm(participacion~ataq_l_10k, data=particip_vio), col="LightSkyBlue")

#Ataques No Letales vs participacion
collapse(vio_partpc, ataq_no_l + participacion ~ MPIO, fmean)
plot(vio_partpc$ataq_no_l, vio_partpc$participacion, main = "Ataques No Letales vs participacion ",
     xlab = "Ataques No Letales", ylab = "Participacion",
     pch = 19, frame = FALSE)
abline (lm(participacion~ataq_no_l, data=vio_partpc), col="LightSkyBlue")

#5.Ataques No Letales 10k vs participacion
collapse(particip_vio, ataq_no_l_10k + participacion ~ MPIO, fmean)
plot(particip_vio$ataq_no_l_10k, particip_vio$participacion, main = "Ataques No Letales por cada 10k habitantes vs Participación ",
     xlab = "Ataques No Letales", ylab = "Participacion",
     pch = 19, frame = FALSE)
abline (lm(participacion~ataq_no_l_10k, data=particip_vio), col="LightSkyBlue")






