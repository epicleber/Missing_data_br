
# 2021 --------------------------------------------------------------------


library(dplyr)
library(readr)
library(data.table)

#FONTE 2021 https://dados.gov.br/dataset/sistema-de-informacao-sobre-mortalidade/resource/da17c5f6-aa89-4e7d-b7e2-ec4b77a5dc31

getwd()
setwd("/Users/cleber/Downloads/2_sub/sim")

banco<- fread("/Users/cleber/Downloads/sim_preliminar_2021.csv", sep=";")


banco$ano <- substr(banco$DTOBITO, 5, 8)

banco$UF <- substr(banco$CODMUNRES, 1, 2)

banco$OCUP[banco$OCUP==""] <- NA
#banco$SEXO[banco$SEXO==""] <- NA
banco$UF[banco$UF==""] <- NA

table(banco$ano)

banco <- banco %>% 
  select(DTOBITO, OCUP,UF,SEXO)

banco$count = 1
banco <- banco %>% 
  mutate(UF =  case_when(UF == 11 ~"RO", 
                            UF == 12 ~"AC", 
                            UF == 13 ~"AM", 
                            UF == 14 ~"RR",
                            UF == 15 ~"PA",
                            UF == 16 ~"AP",
                            UF == 17 ~"TO",
                            UF == 21 ~"MA",
                            UF == 22 ~"PI",
                            UF == 23 ~"CE",
                            UF == 24 ~"RN",
                            UF == 25 ~"PB",
                            UF == 26 ~"PE", 
                            UF == 27 ~"AL", 
                            UF == 28 ~"SE",
                            UF == 29 ~"BA",
                            UF == 31 ~"MG",
                            UF == 32 ~"ES",
                            UF == 33 ~"RJ",
                            UF == 35 ~"SP", 
                            UF == 41 ~"PR", 
                            UF == 42 ~"SC",
                            UF == 43 ~"RS",
                            UF == 50 ~"MS",
                            UF == 51 ~"MT",
                            UF == 52 ~"GO", 
                            UF == 53 ~"DF")) %>% 
  mutate(regiao = case_when(UF == "AC" | UF == "AM" |UF == "AP" |UF == "RR"  | UF == "RO"| UF == "PA"| UF == "TO" ~ 'Norte',
                                                 UF == "MA" | UF == "PI" |UF == "CE" |UF == "RN" | UF == "PE" | UF == "PB" | UF == "SE" | UF == "AL" | UF == "BA" ~ 'Nordeste',
                                                 UF == "MT" | UF == "MS" |UF == "GO" |UF=="DF" ~ 'Centro-Oeste',
                                                 UF == "PR" | UF == "RS" |UF == "SC" ~ 'Sul',
                                                 UF == "SP" | UF == "RJ" |UF == "ES" |UF == "MG" ~ 'Sudeste')) # end function

# agrupando por estado e sexo -------
banco$OCUP[banco$OCUP==""] <- NA
banco$SEXO[banco$SEXO==""] <- NA
banco$UF[banco$UF==""] <- NA
banco$regiao[banco$regiao==""] <- NA



#agrupando ocupacao preenchido
banco_ocup <- banco[complete.cases(banco$OCUP),]
banco_ocup_group_UF_SEXO <- banco_ocup %>%
  group_by(UF,SEXO) %>%
  summarise(ocup_preenc = sum(count))

banco_ocup_group_UF_SEXO <- subset(banco_ocup_group_UF_SEXO, banco_ocup_group_UF_SEXO$SEXO != "0")


#head(banco_OCUP_group_UF_SEXO)

#agrupando denominador
banco_group_UF_SEXO <- banco %>%
  group_by(UF,SEXO) %>%
  summarise(casos = sum(count))

banco_group_UF_SEXO <- subset(banco_group_UF_SEXO, banco_group_UF_SEXO$SEXO != "0")


banco_sim_br <- merge(banco_group_UF_SEXO,banco_ocup_group_UF_SEXO, by=c("UF", "SEXO"))



write.csv2(banco_sim_br, "banco_sim_uf_sexo_2021.csv")



# agrupando por regiao e SEXO -------------

#agrupando OCUP preenchido

banco_ocup_group_regiao_sexo <- banco_ocup %>%
  group_by(regiao,SEXO) %>%
  summarise(ocup_preenc = sum(count))


banco_ocup_group_regiao_sexo <- subset(banco_ocup_group_regiao_sexo, banco_ocup_group_regiao_sexo$SEXO != "0")

#head(banco_OCUP_group_regiao_SEXO)

#agrupando denominador
#banco <- banco[complete.cases(banco$regiao),]
banco_group_regiao_sexo <- banco %>%
  group_by(regiao,SEXO) %>%
  summarise(casos = sum(count))

banco_group_regiao_SEXO <- subset(banco_group_regiao_sexo, banco_group_regiao_sexo$SEXO != "0")


banco_sim_br <- merge(banco_group_regiao_sexo,banco_ocup_group_regiao_sexo, by=c("regiao", "SEXO"))

write.csv2(banco_sim_br, "banco_sim_reg_sexo_2021.csv")





# 2020 --------------------------------------------------------------------

library(dplyr)
library(readr)
library(data.table)

#FONTE 2020 https://dados.gov.br/dataset/sistema-de-informacao-sobre-mortalidade/resource/8d947ac1-addb-49f2-85ab-824a7408a432

getwd()
setwd("/Users/cleber/Downloads/sim")

banco<- fread("/Users/cleber/Downloads/sim/sim_preliminar_2020.csv", sep=";")

banco$UF <- substr(banco$CODMUNRES, 1, 2)

banco$OCUP[banco$OCUP==""] <- NA
#banco$SEXO[banco$SEXO==""] <- NA
banco$UF[banco$UF==""] <- NA

#table(banco$ano)

banco <- banco %>% 
  select(DTOBITO, OCUP,UF,SEXO)

banco$count = 1
banco <- banco %>% 
  mutate(UF =  case_when(UF == 11 ~"RO", 
                         UF == 12 ~"AC", 
                         UF == 13 ~"AM", 
                         UF == 14 ~"RR",
                         UF == 15 ~"PA",
                         UF == 16 ~"AP",
                         UF == 17 ~"TO",
                         UF == 21 ~"MA",
                         UF == 22 ~"PI",
                         UF == 23 ~"CE",
                         UF == 24 ~"RN",
                         UF == 25 ~"PB",
                         UF == 26 ~"PE", 
                         UF == 27 ~"AL", 
                         UF == 28 ~"SE",
                         UF == 29 ~"BA",
                         UF == 31 ~"MG",
                         UF == 32 ~"ES",
                         UF == 33 ~"RJ",
                         UF == 35 ~"SP", 
                         UF == 41 ~"PR", 
                         UF == 42 ~"SC",
                         UF == 43 ~"RS",
                         UF == 50 ~"MS",
                         UF == 51 ~"MT",
                         UF == 52 ~"GO", 
                         UF == 53 ~"DF")) %>% 
  mutate(regiao = case_when(UF == "AC" | UF == "AM" |UF == "AP" |UF == "RR"  | UF == "RO"| UF == "PA"| UF == "TO" ~ 'Norte',
                            UF == "MA" | UF == "PI" |UF == "CE" |UF == "RN" | UF == "PE" | UF == "PB" | UF == "SE" | UF == "AL" | UF == "BA" ~ 'Nordeste',
                            UF == "MT" | UF == "MS" |UF == "GO" |UF=="DF" ~ 'Centro-Oeste',
                            UF == "PR" | UF == "RS" |UF == "SC" ~ 'Sul',
                            UF == "SP" | UF == "RJ" |UF == "ES" |UF == "MG" ~ 'Sudeste')) # end function

# agrupando por estado e sexo -------
banco$OCUP[banco$OCUP==""] <- NA
banco$SEXO[banco$SEXO==""] <- NA
banco$UF[banco$UF==""] <- NA
banco$regiao[banco$regiao==""] <- NA

#agrupando ocupacao preenchido
banco_ocup <- banco[complete.cases(banco$OCUP),]
banco_ocup_group_UF_SEXO <- banco_ocup %>%
  group_by(UF,SEXO) %>%
  summarise(ocup_preenc = sum(count))

banco_ocup_group_UF_SEXO <- subset(banco_ocup_group_UF_SEXO, banco_ocup_group_UF_SEXO$SEXO != "0")

#agrupando denominador
banco_group_UF_SEXO <- banco %>%
  group_by(UF,SEXO) %>%
  summarise(casos = sum(count))

banco_group_UF_SEXO <- subset(banco_group_UF_SEXO, banco_group_UF_SEXO$SEXO != "0")

banco_sim_br <- merge(banco_group_UF_SEXO,banco_ocup_group_UF_SEXO, by=c("UF", "SEXO"))

write.csv2(banco_sim_br, "banco_sim_uf_sexo_2020.csv")


# agrupando por regiao e SEXO -------------

#agrupando OCUP preenchido

banco_ocup_group_regiao_sexo <- banco_ocup %>%
  group_by(regiao,SEXO) %>%
  summarise(ocup_preenc = sum(count))


banco_ocup_group_regiao_sexo <- subset(banco_ocup_group_regiao_sexo, banco_ocup_group_regiao_sexo$SEXO != "0")


#agrupando denominador
banco_group_regiao_sexo <- banco %>%
  group_by(regiao,SEXO) %>%
  summarise(casos = sum(count))

banco_group_regiao_SEXO <- subset(banco_group_regiao_sexo, banco_group_regiao_sexo$SEXO != "0")


banco_sim_br <- merge(banco_group_regiao_sexo,banco_ocup_group_regiao_sexo, by=c("regiao", "SEXO"))

write.csv2(banco_sim_br, "banco_sim_reg_sexo_2020.csv")

