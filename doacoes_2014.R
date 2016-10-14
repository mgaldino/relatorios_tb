## http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais


library(electionsBR)
library(dplyr)

### ACT
setwd("/Users/natalia/Documents/Manoel/reports/ACT")
unzip("prestacao_final_2014.zip", exdir = paste0("./", 2014))

year <- 2014
# list.files()
dir <- getwd()
list.files(paste(dir, year, sep="/"))
setwd(paste(dir, year, sep="/"))

# receitas_comites <-  tryCatch(read.table("receitas_comites_2014_brasil.txt", colClasses = "character", header = T, sep = ";", stringsAsFactors = F, fill = T,
#                                          fileEncoding = "windows-1252"), error = function(e) NULL)
# 
# receitas_partidos <-  tryCatch(read.table("receitas_partidos_2014_brasil.txt",  colClasses = "character", header = T, sep = ";", stringsAsFactors = F, fill = T,
#                                           fileEncoding = "windows-1252"), error = function(e) NULL)

receitas_candidatos <-  tryCatch(read.table("receitas_candidatos_2014_brasil.txt",  colClasses = "character", header = T, sep = ";", stringsAsFactors = F, fill = T,
                                            fileEncoding = "windows-1252"), error = function(e) NULL)
gc()
  
despesas_comites <-  tryCatch(read.table("despesas_comites_2014_brasil.txt", colClasses = "character", header = T, sep = ";", stringsAsFactors = F, fill = T,
                                         fileEncoding = "windows-1252"), error = function(e) NULL)

despesas_partidos <-  tryCatch(read.table("despesas_partidos_2014_brasil.txt", colClasses = "character", header = T, sep = ";", stringsAsFactors = F, fill = T,
                                          fileEncoding = "windows-1252"), error = function(e) NULL)

despesas_candidatos <-  tryCatch(read.table("despesas_partidos_2014_brasil.txt", colClasses = "character", header = T, sep = ";", stringsAsFactors = F, fill = T,
                                            fileEncoding = "windows-1252"), error = function(e) NULL)

setwd("/Users/natalia/Documents/Manoel/reports/ACT/")
save(receitas_candidatos, file="receitas_candidatos.RData")
load("receitas_candidatos.RData")

names(receitas_candidatos)
names(receitas_partidos)
names(receitas_comites)

## validando que receitas batem por candidato, comite e partido
valid_receita_candidatos <- receitas_candidatos %>%
  mutate(Valor.receita = as.numeric(Valor.receita)) %>%
  group_by(Cargo, UF, CPF.do.candidato, Sigla..Partido) %>%
  summarise(receita = sum(Valor.receita, na.rm=T))

### criando tabela doaçnoes totais por cnpj 2014
setwd("/Users/natalia/Documents/Manoel/reports/ACT")

lista_cnpjs <- read.table("lista_cnpjs_v2.csv", header=T, sep=",", 
                          colClasses = c("numeric", "character", "character"))


idx <- which(names(receitas_candidatos) %in% c("CPF.do.candidato", "CPF.CNPJ.do.doador.originário","CPF.CNPJ.do.doador.originário",
                                               "CPF.CNPJ.do.doador", "UF", "Sigla..Partido", "Cargo","Valor.receita"))

doadores_2014_secundario <- lista_cnpjs %>%
  inner_join(select(receitas_candidatos, idx), by = c("CNPJ" = "CPF.CNPJ.do.doador.originário"))

doadores_2014_secundario <- doadores_2014_secundario%>%
  select(-(ncol(dadores_2014_secundario)-1))


doadores_2014_primario <- lista_cnpjs %>%
  inner_join(select(receitas_candidatos, idx), by = c("CNPJ" = "CPF.CNPJ.do.doador")) 

doadores_2014_primario <- doadores_2014_primario %>%
  select(-ncol(doadores_2014_primario))


doadores_2014 <- bind_rows(doadores_2014_secundario, doadores_2014_primario)

## apenas pra dep. federal
doacoes_2014 <- doadores_2014 %>%
  mutate(Valor.receita = as.numeric(Valor.receita)) %>%
  filter( Cargo == "Deputado Federal") %>%
  group_by(CNPJ) %>%
  summarise(receita = sum(Valor.receita, na.rm=T),
            agrupador_empresa = max(agrupador_empresa)) 

## por cargo
doacoes_2014_c <- doadores_2014 %>%
  mutate(Valor.receita = as.numeric(Valor.receita)) %>%
  group_by(CNPJ, Cargo) %>%
  summarise(receita = sum(Valor.receita, na.rm=T),
            agrupador_empresa = max(agrupador_empresa)) 

## 2010
setwd("/Users/natalia/Documents/Manoel/reports/ACT")

doacoes_cand_2010 <- read.table("doacoes_empresas_candidatos.csv",
                           header=T, sep=",", colClasses= "character")


doacoes_2010 <- doacoes_cand_2010 %>%
  mutate(montante = as.numeric(montante)) %>%
  group_by(cgc) %>%
  summarise(receita = sum(montante, na.rm=T),
            nome = max(nome_doador))

View(doacoes_2010)

## juntando por cnpj
doacoes_2010_14 <- doacoes_2010 %>%
  full_join(doacoes_2014, by = c("cgc" = "CNPJ")) %>%
  full_join(lista_cnpjs, by = c("cgc" = "CNPJ") ) %>%
  rename( cnpj = cgc, receita_2010 = receita.x, receita_2014 = receita.y,
          agrupador_empresa = agrupador_empresa.y) %>%
  select(which(names(.) %in% c("cnpj", "receita_2010", "receita_2014",
                               "agrupador_empresa"))) %>%
  mutate(receita_2010 = replace(receita_2010, is.na(receita_2010), 0),
         receita_2014 = replace(receita_2014, is.na(receita_2014), 0)) %>%
  distinct(cnpj, .keep_all = T)

setwd("/Users/natalia/Documents/Manoel/reports/ACT/2014")
write.table(doacoes_2010_14, file = "doacoes_2010_14_por_cnpj.csv", sep=";", row.names=F)


## por cnpj e por cargo
doacoes_2010_2014_c <- doacoes_2010 %>%
  full_join(doacoes_2014_c, by = c("cgc" = "CNPJ")) %>%
  full_join(lista_cnpjs, by = c("cgc" = "CNPJ") ) %>%
  rename( cnpj = cgc, receita_2010 = receita.x, receita_2014 = receita.y,
          agrupador_empresa = agrupador_empresa.y, cargo2014 = Cargo) %>%
  select(which(names(.) %in% c("cnpj", "receita_2010", "receita_2014",
                               "agrupador_empresa", "cargo2014"))) %>%
  mutate(receita_2010 = replace(receita_2010, is.na(receita_2010), 0),
         receita_2014 = replace(receita_2014, is.na(receita_2014), 0)) %>%
  distinct(cnpj, .keep_all = T)

View(doacoes_2010_2014_c)

setwd("/Users/natalia/Documents/Manoel/reports/ACT/2014")
write.table(doacoes_2010_2014_c, file = "doacoes_2010_14_por_cnpj_e_cargo.csv", sep=";", row.names=F)


## juntando por grupo

doacoes_2010_g <- doacoes_2010 %>%
  inner_join(lista_cnpjs, by = c("cgc" = "CNPJ") ) %>%
  group_by(agrupador_empresa) %>%
  summarise(receita_2010 = sum(receita, na.rm=T))

# federal
doacoes_2014_g <- doacoes_2014  %>%
  inner_join(lista_cnpjs, by = "CNPJ" ) %>%
  ungroup() %>%
  group_by(agrupador_empresa.y) %>%
  summarise(receita_2014 = sum(receita, na.rm=T)) %>%
  mutate(receita_2014 = replace(receita_2014, is.na(receita_2014), 0)) %>%
  rename(agrupador_empresa = agrupador_empresa.y)

## por cargo
doacoes_2014_g_c <- doacoes_2014_c  %>%
  inner_join(lista_cnpjs, by = "CNPJ" ) %>%
  ungroup() %>%
  group_by(agrupador_empresa.y, Cargo) %>%
  summarise(receita_2014 = sum(receita, na.rm=T)) %>%
  mutate(receita_2014 = replace(receita_2014, is.na(receita_2014), 0)) %>%
  rename(agrupador_empresa = agrupador_empresa.y)


## joining por argo
doacoes_2010_2014g_c <- doacoes_2010_g %>%
  full_join(doacoes_2014_g_c, by= "agrupador_empresa") %>%
  mutate(receita_2010 = replace(receita_2010, is.na(receita_2010), 0),
         receita_2014 = replace(receita_2014, is.na(receita_2014), 0))

setwd("/Users/natalia/Documents/Manoel/reports/ACT/2014")
write.table(doacoes_2010_2014g_c, file = "doacoes_2010_14_por_grupo_e_cargo.csv", sep=";", row.names=F)


# joining federal
doacoes_2010_2014g <- doacoes_2010_g %>%
  full_join(doacoes_2014_g, by= "agrupador_empresa") %>%
  mutate(receita_2010 = replace(receita_2010, is.na(receita_2010), 0),
         receita_2014 = replace(receita_2014, is.na(receita_2014), 0))

setwd("/Users/natalia/Documents/Manoel/reports/ACT/2014")
write.table(doacoes_2010_2014g, file = "doacoes_2010_14_por_grupo.csv", sep=";", row.names=F)

### Tabela só 2014

setwd("/Users/natalia/Documents/Manoel/reports/ACT")

doador_candidato2010 <- read.table("doador_candidato2010.csv",
                                header=T, sep=",", colClasses= "character")

info_depfed_2014 <- candidate_fed_1(2014)

info_depfed_2014_final <- info_depfed_2014 %>%
  select(which(names(.) %in% c("CPF_CANDIDATO", "DESCRICAO_CARGO" , "DES_SITUACAO_CANDIDATURA", "DESCRICAO_SEXO",
                               "DESCRICAO_SEXO", "SIGLA_PARTIDO", "DATA_NASCIMENTO",
                               "NOME_URNA_CANDIDATO", "SIGLA_UE", "DESCRICAO_OCUPACAO", 
                               "DESCRICAO_COR_RACA", "DESCRICAO_GRAU_INSTRUCAO", "DESC_SIT_TOT_TURNO") )) 






doadores_2014_info <- doadores_2014 %>%
  full_join(info_depfed_2014_final, by = c("CPF.do.candidato" = "CPF_CANDIDATO"))

setwd("/Users/natalia/Documents/Manoel/reports/ACT")
write.table(doadores_2014_info, file = "info_background_candidatos_doadores_2014.csv", sep=";",
            row.names = F)
