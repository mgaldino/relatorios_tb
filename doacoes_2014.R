## http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais


library(electionsBR)
library(dplyr)
library(ggplot2)
library(scales)
library(extrafont)
loadfonts(device="win")



### ACT
setwd("C:/Users/mgaldino/2016/ACT/arquivos/prestacao_final_2014")


# receitas_candidatos <-  tryCatch(read.table("receitas_candidatos_2014_brasil.txt",  colClasses = "character", header = T, sep = ";",
#                                             stringsAsFactors = F, fill = T))

setwd("C:/Users/mgaldino/2016/ACT/arquivos")
# save(receitas_candidatos, file="receitas_candidatos.RData")
load("receitas_candidatos.RData")



### criando tabela doaçnoes totais por cnpj 2014
setwd("C:/Users/mgaldino/2016/ACT/arquivos")

# lista_cnpjs <- read.table("lista_cnpjs_v2.csv", header=T, sep=",", 
#                           colClasses = c("numeric", "character", "character"))

load(file="lista_cnpjs_v3.RData")

lista_cnpjs <- tabela_empresas
rm(tabela_empresas)

lista_cnpjs <- lista_cnpjs %>%
  distinct(CNPJ, .keep_all = T)

idx <- which(names(receitas_candidatos) %in% c("CPF.do.candidato", "CPF.CNPJ.do.doador.originário","CPF.CNPJ.do.doador.originário",
                                               "CPF.CNPJ.do.doador", "UF", "Sigla..Partido", "Cargo", "Nome.candidato", "Valor.receita"))

receitas_candidatos <- receitas_candidatos %>%
  select(idx)

receitas_candidatos <- receitas_candidatos %>%
  mutate(CNPJ = ifelse(CPF.CNPJ.do.doador.originário == "#NULO", CPF.CNPJ.do.doador,
                       ifelse(CPF.CNPJ.do.doador %in% lista_cnpjs$CNPJ, CPF.CNPJ.do.doador, CPF.CNPJ.do.doador.originário)))

doadores_2014_full <- receitas_candidatos %>%
  left_join(lista_cnpjs, by = "CNPJ") 

doadores_2014_full <- doadores_2014_full %>%
  mutate(Valor.receita = as.numeric(gsub(",", "\\.", Valor.receita)))

summary(doadores_2014_full$Valor.receita)


######################################
### Gráfico s Tabelas do doc
######################################

setwd("C:/Users/mgaldino/2016/ACT/charts")
### Gráfico 1
df1 <- doadores_2014_full %>%
  filter(!is.na(agrupador), Cargo %in% c("Deputado Federal", "Senador")) %>%
  group_by(Cargo, agrupador) %>%
  summarise(doacao = sum(Valor.receita)/1e6) %>%
  mutate(doacao_texto = gsub( "\\.", "," , as.character(round(doacao, 1))))

chart1 <- df1 %>%
  ggplot(aes(x=agrupador , y=doacao, fill = Cargo, label = doacao_texto)) + 
           geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(size = 3, position = position_dodge(width = 0.9), 
            vjust= -.5,check_overlap = TRUE) +
  theme_tb(base_family = "Garamond" ) + ylab("R$ milhões") + xlab("") +
  scale_y_continuous(labels = real_format()) + ylim(0, 100)
  
ggsave("grafico1.bmp", chart1, scale=.6, height = 6, width = 10, family="Garamond" )

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
