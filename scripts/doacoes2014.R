# ACT

library(tidyr)
library(electionsBR)
library(dplyr)

# setando diretório
# setwd("C:\\Users\\mgaldino\\2016\\ACT\\arquivos")
# unzip("prestacao_final_2014.zip", exdir = paste0("./", 2014))
# 
# setwd("C:\\Users\\mgaldino\\2016\\ACT\\arquivos\\2014")
# 
# 
# receitas_candidatos <-  tryCatch(read.table("receitas_candidatos_2014_brasil.txt",  colClasses = "character", header = T, sep = ";", stringsAsFactors = F, fill = T,
#                                             fileEncoding = "windows-1252"), error = function(e) NULL)
# gc()


setwd("C:\\Users\\mgaldino\\2016\\ACT\\arquivos")
#save(receitas_candidatos, file="receitas_candidatos.RData")
load("receitas_candidatos.RData")

## criando base doadores
lista_cnpjs <- read.table("lista_cnpjs_v2.csv", header=T, sep=",", 
                          colClasses = c("numeric", "character", "character"))

## removendo cnpjs duplicados
lista_cnpjs <- lista_cnpjs %>%
  filter(!duplicated(CNPJ))

idx <- which(names(receitas_candidatos) %in% c("CPF.do.candidato", "CPF.CNPJ.do.doador.originário","CPF.CNPJ.do.doador.originário",
                                               "CPF.CNPJ.do.doador", "UF", "Sigla..Partido", "Cargo","Valor.receita"))

## pegando doacoes secundárias da lista de empresas
doadores_2014_secundario <- lista_cnpjs %>%
  inner_join(select(receitas_candidatos, idx), by = c("CNPJ" = "CPF.CNPJ.do.doador.originário"))

doadores_2014_secundario <- doadores_2014_secundario%>%
  select(-(ncol(doadores_2014_secundario)-1))

# pegando doacções primárias da listra de empresas
doadores_2014_primario <- lista_cnpjs %>%
  inner_join(select(receitas_candidatos, idx), by = c("CNPJ" = "CPF.CNPJ.do.doador")) 

doadores_2014_primario <- doadores_2014_primario %>%
  select(-ncol(doadores_2014_primario))

# juntando doações primárias e secundárias
doadores_2014 <- bind_rows(doadores_2014_secundario, doadores_2014_primario)


## número de dep e senadores que receberam doações das empresas listadas

doadores_2014 %>%
  group_by(Cargo, agrupador_empresa) %>%
  summarise(num_cand = n_distinct(CPF.do.candidato)) %>%
  filter(Cargo %in% c("Deputado Federal", "Senador"))

### 
# info_depfed_2014 <- banco
info_depfed_2014 <- candidate_fed1(2014)

info_depfed_2014_final <- info_depfed_2014 %>%
  select(which(names(.) %in% c("CPF_CANDIDATO", "DESCRICAO_CARGO" , "DES_SITUACAO_CANDIDATURA", "DESCRICAO_SEXO",
                               "DESCRICAO_SEXO", "SIGLA_PARTIDO", "DATA_NASCIMENTO",
                               "NOME_URNA_CANDIDATO", "SIGLA_UE", "DESCRICAO_OCUPACAO", 
                               "DESCRICAO_COR_RACA", "DESCRICAO_GRAU_INSTRUCAO", "DESC_SIT_TOT_TURNO") )) 

doadores_2014_info <- doadores_2014 %>%
  full_join(info_depfed_2014_final, by = c("CPF.do.candidato" = "CPF_CANDIDATO"))

setwd("C:\\Users\\mgaldino\\2016\\ACT\\arquivos")
write.table(doadores_2014_info, file = "info_background_candidatos_doadores_2014_v2.csv", sep=";",
            row.names = F)

## pegando doações de todas as empresas para os eleitos
 
# 71346376468 info_depfed_2014_final

## pegando deputados e senadores
doacoes_totais <- info_depfed_2014_final %>%
  filter(DESCRICAO_CARGO %in% c("DEPUTADO FEDERAL", "SENADOR"),
         DESC_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP"))

## dos deputados e senadores eleitos, pegar todas as receitas
doacoes_totais <- doacoes_totais %>%
  inner_join(select(receitas_candidatos, idx), by = c("CPF_CANDIDATO" = "CPF.do.candidato")) 


##
# stat descritivas

## receitas totais, das empresas da lista, e perc

receita_eleito <- doadores_2014_info %>%
  mutate( Valor.receita = as.numeric(gsub("," , "\\." , Valor.receita))) %>%
  filter(DESCRICAO_CARGO %in% c("DEPUTADO FEDERAL", "SENADOR"),
         DESC_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  group_by(CPF.do.candidato, agrupador_empresa, Sigla..Partido, NOME_URNA_CANDIDATO, SIGLA_UE, DESCRICAO_CARGO) %>%
  summarise(receita = sum(Valor.receita))

View(receita_eleito)
doadores_2014_info %>%
  filter(DESCRICAO_CARGO %in% c("DEPUTADO FEDERAL", "SENADOR"), # , "SENADOR"
         DESC_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  group_by(DES_SITUACAO_CANDIDATURA) %>%
  summarise(n_distinct(CPF.do.candidato))

doadores_2014_info %>%
  filter(DESCRICAO_CARGO %in% c("DEPUTADO FEDERAL")) %>%
  group_by(DESC_SIT_TOT_TURNO, DES_SITUACAO_CANDIDATURA) %>%
  summarise(n_distinct(CPF.do.candidato))


head(receita_eleito)

doacoes_totais_cpf <- doacoes_totais %>%
  mutate( Valor.receita = as.numeric(gsub("," , "\\." , Valor.receita))) %>%
  group_by(CPF_CANDIDATO) %>%
  summarise(total_todas_empresas = sum(Valor.receita)) 

doacoes_totais_cpf_com_empresas <- doacoes_totais_cpf %>%
  inner_join(receita_eleito, by = c("CPF_CANDIDATO" = "CPF.do.candidato")) %>%
  rename(receita_empresas_lista = receita) %>%
  mutate(perc_empre_list_over_todas_doacoes = round(receita_empresas_lista/total_todas_empresas, 4))

# doações das empresas todos os cargos, dep e senado
empresas <- doadores_2014_info %>%
  mutate( Valor.receita = as.numeric(gsub("," , "\\." , Valor.receita))) %>%
  mutate(DESCRICAO_CARGO = ifelse(!DESCRICAO_CARGO %in% c("DEPUTADO FEDERAL", "SENADOR"),
                                  "OUTROS", DESCRICAO_CARGO)) %>%
  group_by(agrupador_empresa, DESCRICAO_CARGO) %>%
  summarise(doacoes_empresas = sum(Valor.receita)) %>%
  spread(DESCRICAO_CARGO, doacoes_empresas)


receitas_final <- doacoes_totais_cpf_com_empresas %>%
  left_join(empresas, by="agrupador_empresa") %>%
  rename(total_arrecadado = total_todas_empresas)


setwd("C:\\Users\\mgaldino\\2016\\ACT\\arquivos")
write.table(receitas_final, file = "doadores_completo_2014_v2.csv", sep=";",
            row.names = F)

## valid

summary(receitas_final$total_todas_empresas)

# 71346376468 info_depfed_2014_final
receitas_final %>%
  filter(CPF_CANDIDATO == "71346376468")

receitas_final %>%
  filter(CPF_CANDIDATO == "00031226540")


receitas_candidatos %>%
  filter(CPF.do.candidato == "71346376468") %>%
  mutate(Valor.receita = as.numeric(gsub(",", "\\.", Valor.receita))) %>%
  summarise(sum(Valor.receita))

doadores_2014 %>%
  filter(CPF.do.candidato == "71346376468")

## total de doações
receitas_candidatos %>%
  mutate( Valor.receita = as.numeric(gsub("," , "\\." , Valor.receita))) %>%
  summarise(sum(Valor.receita))

## total de doações do congresso
receitas_candidatos %>%
  mutate( Valor.receita = as.numeric(gsub("," , "\\." , Valor.receita))) %>%
  filter(Cargo %in% c("Deputado Federal", "Senador")) %>%
  summarise(sum(Valor.receita))
  