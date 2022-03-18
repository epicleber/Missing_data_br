setwd("/Users/cleber/Downloads/srag")

srag2020 <- fread("/Users/cleber/Downloads/srag/srag2020.csv", sep = ";", select = c("PAC_DSCBO", "SG_UF_NOT","CS_SEXO")) #lendo os csv e sele
srag2021 <- fread("/Users/cleber/Downloads/srag/srag 2021.csv",sep = ";", select = c("PAC_DSCBO", "SG_UF_NOT","CS_SEXO")) #lendo os csv e sele

banco <- rbind (srag2020,srag2021)

banco$PAC_DSCBO[banco$PAC_DSCBO==""] <- NA
banco$CS_SEXO[banco$CS_SEXO==""] <- NA
banco$SG_UF_NOT[banco$SG_UF_NOT==""] <- NA

banco$count = 1
banco <- banco %>% 
  mutate(regiao = case_when(SG_UF_NOT == "AC" | SG_UF_NOT == "AM" |SG_UF_NOT == "AP" |SG_UF_NOT == "RR"  | SG_UF_NOT == "RO"| SG_UF_NOT == "PA"| SG_UF_NOT == "TO" ~ 'Norte',
                            SG_UF_NOT == "MA" | SG_UF_NOT == "PI" |SG_UF_NOT == "CE" |SG_UF_NOT == "RN" | SG_UF_NOT == "PE" | SG_UF_NOT == "PB" | SG_UF_NOT == "SE" | SG_UF_NOT == "AL" | SG_UF_NOT == "BA" ~ 'Nordeste',
                            SG_UF_NOT == "MT" | SG_UF_NOT == "MS" |SG_UF_NOT == "GO" |SG_UF_NOT=="DF" ~ 'Centro-Oeste',
                            SG_UF_NOT == "PR" | SG_UF_NOT == "RS" |SG_UF_NOT == "SC" ~ 'Sul',
                            SG_UF_NOT == "SP" | SG_UF_NOT == "RJ" |SG_UF_NOT == "ES" |SG_UF_NOT == "MG" ~ 'Sudeste')) # end function


# agrupando por estado e sexo -------

banco_ocup <- banco[complete.cases(banco$PAC_DSCBO),]
banco_ocup_group_uf_sexo <- banco_ocup %>%
  group_by(SG_UF_NOT,CS_SEXO) %>%
  summarise(ocup_preenc = sum(count)) %>% 
  filter(CS_SEXO != "I")

#agrupando denominador
banco_group_uf_sexo <- banco %>%
  group_by(SG_UF_NOT,CS_SEXO) %>%
  summarise(casos = sum(count)) %>% 
  filter(CS_SEXO != "I")

banco_srag_br <- merge(banco_group_uf_sexo,banco_ocup_group_uf_sexo, by=c("SG_UF_NOT", "CS_SEXO"))

write.csv2(banco_srag_br, "banco_srag_uf_sexo.csv")



# agrupando por regiao e SEXO -------------

banco$regiao[banco$regiao==""] <- NA

#agrupando ocupacao preenchido
banco_ocup <- banco[complete.cases(banco$PAC_DSCBO),]
banco_ocup <- banco[complete.cases(banco$regiao),]
banco_ocup_group_regiao_sexo <- banco_ocup %>%
  group_by(regiao,CS_SEXO) %>%
  summarise(ocup_preenc = sum(count)) %>% 
  filter(CS_SEXO != "I")

#agrupando denominador
banco_group_regiao_sexo <- banco %>%
  group_by(regiao,CS_SEXO) %>%
  summarise(casos = sum(count)) %>% 
  filter(CS_SEXO != "I")

banco_srag_br <- merge(banco_group_regiao_sexo,banco_ocup_group_regiao_sexo, by=c("regiao", "CS_SEXO"))



write.csv2(banco_srag_br, "banco_srag_regiao_sexo.csv")





