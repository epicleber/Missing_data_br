
################################################################################
install.packages("dplyr")
library(dplyr)
library(readr)
library(data.table)

#2021
setwd("/Users/cleber/Downloads/sg/2021")
temp = list.files(pattern="*.csv") #criando um arquivo temporario com todos arquivos csv do diretório
for (i in 1:length(temp)) assign(temp[i], fread(temp[i], sep = ";",select = c("cbo","dataNotificacao", "estado","sexo"))) #lendo os csv e sele

sg_2021 <- rbind(ac.csv,
                 al.csv,
                 am.csv, ap.csv, ba1.csv,ba2.csv,ba3.csv,ce1.csv,ce2.csv,df.csv,es.csv,go1.csv,go2.csv,ma.csv,mg1.csv,mg2.csv,mg3.csv,mg4.csv,mg5.csv,mg6.csv,
                 ms.csv,mt.csv,ni.csv,pa.csv,pb1.csv,pb2.csv,pe1.csv,pe2.csv,pi.csv,pr.csv,rj1.csv,rj2.csv,rj3.csv,rj3.csv,rj4.csv,rn.csv,ro.csv,rr.csv,
                 rs1.csv,rs2.csv,rs3.csv,rs4.csv,rs5.csv, sc1.csv,sc2.csv,sc3.csv,sc4.csv,sc5.csv,sc6.csv,se.csv, sp1.csv,sp2.csv,sp3.csv,sp4.csv,sp5.csv,sp6.csv,sp7.csv,
                 sp8.csv,sp9.csv,sp10.csv,sp11.csv,sp12.csv,sp13.csv,sp14.csv,sp5.csv,sp16.csv,to.csv)

sg_2021$count <- 1

sg_2021 <- sg_2021 %>% mutate(regiao = case_when(estado == "Acre" | estado == "Amazonas" |estado == "Amapá" |estado == "Roraima"  | estado == "Rondônia"| estado == "Pará"| estado == "Tocantins" ~ 'Norte',
                                                 estado == "Maranhão" | estado == "Piauí" |estado == "Ceará" |estado == "Rio Grande do Norte" | estado == "Pernambuco" | estado == "Paraíba" | estado == "Sergipe" | estado == "Alagoas" | estado == "Bahia" ~ 'Nordeste',
                                                 estado == "Mato Grosso" | estado == "Mato Grosso do Sul" |estado == "Goiás" |estado=="Distrito Federal" ~ 'Centro-Oeste',
                                                 estado == "Paraná" | estado == "Rio Grande do Sul" |estado == "Santa Catarina" ~ 'Sul',
                                                 estado == "São Paulo" | estado == "Rio de Janeiro" |estado == "Espírito Santo" |estado == "Minas Gerais" ~ 'Sudeste')) # end function

# agrupando por estado e sexo -------------------------------------------------------------------------


sg_2021$cbo[sg_2021$cbo==""] <- NA
sg_2021$sexo[sg_2021$sexo==""] <- NA
sg_2021$estado[sg_2021$estado==""] <- NA
sg_2021$regiao[sg_2021$regiao==""] <- NA
sg_2021 <- sg_2021[sg_2021$sexo=="Masculino" | sg_2021$sexo=="Feminino"]

#agrupando cbo preenchido
sg_2021_cbo <- sg_2021[complete.cases(sg_2021$cbo),]
sg_2021_cbo_group_estado_sexo <- sg_2021_cbo %>%
  group_by(estado,sexo) %>%
  summarise(cbo_preenc = sum(count))

#agrupando denominador
sg_2021_group_estado_sexo <- sg_2021 %>%
  group_by(estado,sexo) %>%
  summarise(casos = sum(count))

banco_sg_br <- merge(sg_2021_group_estado_sexo,sg_2021_cbo_group_estado_sexo, by=c("estado", "sexo"))

write.csv2(banco_sg_br, "banco_sg_uf_sexo_2021.csv")

# agrupando por regiao e sexo -------------------------------------------------------------------------

#agrupando cbo preenchido

sg_2021_cbo_group_regiao_sexo <- sg_2021_cbo %>%
  group_by(regiao,sexo) %>%
  summarise(cbo_preenc = sum(count))

#head(sg_2021_cbo_group_regiao_sexo)

#agrupando denominador
#sg_2021 <- sg_2021[complete.cases(sg_2021$regiao),]
sg_2021_group_regiao_sexo <- sg_2021 %>%
  group_by(regiao,sexo) %>%
  summarise(casos = sum(count))

banco_sg_br <- merge(sg_2021_group_regiao_sexo,sg_2021_cbo_group_regiao_sexo, by=c("regiao", "sexo"))

write.csv2(banco_sg_br, "banco_sg_reg_sexo_2021.csv")

rm(list = ls())

#2020



setwd("/Users/cleber/Downloads/2_sub/sg/2020")
temp = list.files(pattern="*.csv") #criando um arquivo temporario com todos arquivos csv do diretório
for (i in 1:length(temp)) assign(temp[i], fread(temp[i], sep = ";",select = c("cbo","dataNotificacao", "estado","sexo"))) #lendo os csv e sele

sg_2020 <- rbind(ac.csv,
                 al.csv,
                 am.csv, ap.csv, ba1.csv,ba2.csv,ce.csv,df.csv,es.csv,go1.csv,go2.csv,ma.csv,mg1.csv,mg2.csv,mg3.csv,
                 ms.csv,mt.csv,ni.csv,pa.csv,pb.csv,pe.csv,pi.csv,pr1.csv, pr2.csv,rj1.csv,rj2.csv,rn.csv,ro.csv,rr.csv,
                 rs1.csv,rs2.csv,rs3.csv, sc1.csv,sc2.csv,sc3.csv,se.csv, sp1.csv,sp2.csv,sp3.csv,sp4.csv,sp5.csv,sp6.csv,sp7.csv,
                 to.csv)

sg_2020$count <- 1

sg_2020 <- sg_2020 %>% mutate(regiao = case_when(estado == "Acre" | estado == "Amazonas" |estado == "Amapá" |estado == "Roraima"  | estado == "Rondônia"| estado == "Pará"| estado == "Tocantins" ~ 'Norte',
                                                 estado == "Maranhão" | estado == "Piauí" |estado == "Ceará" |estado == "Rio Grande do Norte" | estado == "Pernambuco" | estado == "Paraíba" | estado == "Sergipe" | estado == "Alagoas" | estado == "Bahia" ~ 'Nordeste',
                                                 estado == "Mato Grosso" | estado == "Mato Grosso do Sul" |estado == "Goiás" |estado=="Distrito Federal" ~ 'Centro-Oeste',
                                                 estado == "Paraná" | estado == "Rio Grande do Sul" |estado == "Santa Catarina" ~ 'Sul',
                                                 estado == "São Paulo" | estado == "Rio de Janeiro" |estado == "Espírito Santo" |estado == "Minas Gerais" ~ 'Sudeste')) # end function

# agrupando por estado e sexo -------------------------------------------------------------------------


sg_2020$cbo[sg_2020$cbo==""] <- NA
sg_2020$sexo[sg_2020$sexo==""] <- NA
sg_2020$estado[sg_2020$estado==""] <- NA
sg_2020$regiao[sg_2020$regiao==""] <- NA
sg_2020 <- sg_2020[sg_2020$sexo=="Masculino" | sg_2020$sexo=="Feminino"]

#agrupando cbo preenchido
sg_2020_cbo <- sg_2020[complete.cases(sg_2020$cbo),]
sg_2020_cbo_group_estado_sexo <- sg_2020_cbo %>%
  group_by(estado,sexo) %>%
  summarise(cbo_preenc = sum(count))

#agrupando denominador
sg_2020_group_estado_sexo <- sg_2020 %>%
  group_by(estado,sexo) %>%
  summarise(casos = sum(count))


banco_sg_br <- merge(sg_2020_group_estado_sexo,sg_2020_cbo_group_estado_sexo, by=c("estado", "sexo"))

write.csv2(banco_sg_br, "banco_sg_uf_sexo_2020.csv")



# agrupando por regiao e sexo -------------------------------------------------------------------------


#agrupando cbo preenchido

sg_2020_cbo_group_regiao_sexo <- sg_2020_cbo %>%
  group_by(regiao,sexo) %>%
  summarise(cbo_preenc = sum(count))

#agrupando denominador
sg_2020_group_regiao_sexo <- sg_2020 %>%
  group_by(regiao,sexo) %>%
  summarise(casos = sum(count))

banco_sg_br <- merge(sg_2020_group_regiao_sexo,sg_2020_cbo_group_regiao_sexo, by=c("regiao", "sexo"))

write.csv2(banco_sg_br, "banco_sg_reg_sexo_2020.csv")

