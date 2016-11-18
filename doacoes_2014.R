## http://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais


library(electionsBR)
library(dplyr)
library(ggplot2)
library(scales)
library(extrafont)
library(tidyr)
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


### Tabela eleito e nao-eleito 2014

# info_depfed_2014 <- candidate_fed1(2014)
# save(info_depfed_2014, file="info_depfed_2014.RData")

load("info_depfed_2014.RData")


info_depfed_2014_final <- info_depfed_2014 %>%
  select(which(names(.) %in% c("CPF_CANDIDATO", "DESCRICAO_CARGO" , "DES_SITUACAO_CANDIDATURA", "DESCRICAO_SEXO",
                               "DESCRICAO_SEXO", "SIGLA_PARTIDO", "DATA_NASCIMENTO",
                               "NOME_URNA_CANDIDATO", "SIGLA_UE", "DESCRICAO_OCUPACAO", 
                               "DESCRICAO_COR_RACA", "DESCRICAO_GRAU_INSTRUCAO", "DESC_SIT_TOT_TURNO") )) 

info_depfed_senador_2014 <- info_depfed_2014_final %>%
  filter(DESCRICAO_CARGO %in% c("DEPUTADO FEDERAL", "SENADOR", "2º SUPLENTE", "1º SUPLENTE"),
         DES_SITUACAO_CANDIDATURA %in% c("DEFERIDO", "DEFERIDO COM RECURSO"))

# nrow(info_depfed_senador_2014) == length(unique(info_depfed_senador_2014$CPF_CANDIDATO))

doadores_2014_full_bg <- doadores_2014_full %>%
  left_join(info_depfed_senador_2014, by = c("CPF.do.candidato" = "CPF_CANDIDATO"))

## depois calcular quanto foi doado pelas empresas para candidatos em outras situações (indeferido etc.)

######################################
### Gráficos Tabelas do doc
######################################

setwd("C:/Users/mgaldino/2016/ACT/charts")

### Gráfico 1
df1 <- doadores_2014_full %>%
  filter(!is.na(agrupador), Cargo %in% c("Deputado Federal", "Senador")) %>%
  group_by(Cargo, agrupador) %>%
  summarise(doacao = sum(Valor.receita)/1e6) %>%
  mutate(doacao_texto = gsub( "\\.", "," , as.character(round(doacao, 1))))

chart1 <- df1 %>%
  ggplot(aes(x=reorder(agrupador, -doacao) , y=doacao, fill = Cargo, label = doacao_texto)) + 
           geom_bar(stat = "identity", position = position_dodge(width = 0.9), width=0.8) +
  geom_text(size = 3, position = position_dodge(width = 0.9), 
            vjust= -.5,check_overlap = TRUE) +
  theme_tb(base_family = "Helvetica" , legend_size = 8) + ylab("R$ milhões") + xlab("") +
  scale_y_continuous(labels = real_format()) + ylim(0, 100) +  
  scale_fill_manual(values = c("#ffb959","#406fef")) 

chart1_alternativo <- df1 %>%
  ggplot(aes(x=reorder(agrupador, -doacao) , y=doacao, label = doacao_texto)) + 
  geom_bar(stat = "identity", fill= "#406fef", colour= "#406fef") +
  geom_text(size = 3, vjust= -.5,check_overlap = TRUE) +
  facet_grid(. ~ Cargo) +
  theme_tb(base_family = "Helvetica" , legend_size = 8) + ylab("R$ milhões") + xlab("") +
  scale_y_continuous(labels = real_format()) + ylim(0, 100)

  
  ## cinza e azul nos stacked

ggsave("grafico1.bmp", chart1, scale=.6, height = 6, width = 10, family="Helvetica" )

ggsave("grafico1_alternativa.bmp", chart1_alternativo, scale=.8, height = 6, width = 10, family="Helvetica" )

## chart 2 Total doado pelos conglomerados – 2010 e 2014


### Tabela 2 CANDIDATOS ELEITOS FAVORECIDOS – CONGRESSO 2014

candidatos_fav <- doadores_2014_full_bg %>%
  mutate(bol_status_eleito = 
           ifelse(DESC_SIT_TOT_TURNO %in% c( "ELEITO POR QP", "ELEITO POR MÉDIA", "ELEITO"), 
                  "eleito", "nao_eleito")) %>%
  filter(!is.na(agrupador)) 

# número de candidatos deferidos com prestação de contas (arrecadou algum real)
n_cand <- doadores_2014_full_bg %>%  # info_depfed_2014 doadores_2014_full_bg
  filter(Cargo == "Deputado Federal") %>%
  summarise(n_cand = n_distinct(CPF.do.candidato))

n_fav_dep <- candidatos_fav %>%
  filter(Cargo == "Deputado Federal") %>%
  summarise(n = n_distinct(CPF.do.candidato))

n_fav_eleito_dep <- candidatos_fav %>%
  filter(Cargo == "Deputado Federal", bol_status_eleito == "eleito") %>%
  summarise(n = n_distinct(CPF.do.candidato))

df2_tmp <- candidatos_fav %>%
  filter(Cargo == "Deputado Federal") %>%
  group_by(agrupador) %>%
  summarise(dep_favorecidos = n_distinct(CPF.do.candidato))

df2_tmp1 <- candidatos_fav %>%
  filter(Cargo == "Deputado Federal", bol_status_eleito == "eleito") %>%
  group_by(agrupador) %>%
  summarise(dep_eleitos = n_distinct(CPF.do.candidato))

df2 <- df2_tmp %>%
  inner_join(df2_tmp1, by="agrupador")



df2_1 <- bind_rows(df2,
          data.frame(agrupador = "total_favorecidos",
                     dep_favorecidos = n_fav_dep$n,
                     dep_eleitos = n_fav_eleito_dep$n),
          data.frame(agrupador = "total_candidatos",
                     dep_favorecidos = round(n_fav_dep$n/n_cand$n_cand, 2),
                     dep_eleitos = round(n_fav_eleito_dep$n/513,2)))


setwd("C:/Users/mgaldino/2016/ACT/tabelas")
write.table(df2_1, file="tabela_2.csv", sep=";", row.names=F)

## tabela 3 senadores favorecidos

# número de candidatos deferidos com prestação de contas (arrecadou algum real)
n_cand_senador <- doadores_2014_full_bg %>%  # info_depfed_2014 doadores_2014_full_bg
  filter(Cargo == "Senador") %>%
  summarise(n_cand = n_distinct(CPF.do.candidato))

n_fav_sendador <- candidatos_fav %>%
  filter(Cargo == "Senador") %>%
  summarise(n = n_distinct(CPF.do.candidato))

n_fav_eleito_sendador <- candidatos_fav %>%
  filter(Cargo == "Senador", bol_status_eleito == "eleito") %>%
  summarise(n = n_distinct(CPF.do.candidato))


df3_tmp <- candidatos_fav %>%
  filter(Cargo == "Senador") %>%
  group_by(agrupador) %>%
  summarise(sen_favorecidos = n_distinct(CPF.do.candidato))

df3_tmp1 <- candidatos_fav %>%
  filter(Cargo == "Senador", bol_status_eleito == "eleito") %>%
  group_by(agrupador) %>%
  summarise(sen_eleitos = n_distinct(CPF.do.candidato))

df3 <- df3_tmp %>%
  inner_join(df3_tmp1, by="agrupador")


df3_1 <- bind_rows(df3,
                   data.frame(agrupador = "total_favorecidos",
                              sen_favorecidos = n_fav_sendador$n,
                              sen_eleitos = n_fav_eleito_sendador$n),
                   data.frame(agrupador = "total_candidatos",
                              sen_favorecidos = round(n_fav_sendador$n/n_cand_senador$n_cand,2),
                              sen_eleitos = round(n_fav_eleito_sendador$n/27, 2)))

## GRÁFICO 3 e 4 - EFETIVIDADE DOAÇÕES DE CAMPANHA - DEPUTADOS FEDERAIS e Senadores (2014)

df_chart3 <- df2_1 %>%
  mutate(efetividade = dep_eleitos/dep_favorecidos,
         efetividade_texto = 
           paste(gsub( "\\.", "," , as.character(100*round(efetividade, 2))), "%", sep="")) %>%
  slice(-(7:8)) %>%
  mutate(cargo = "Deputado Federal")

df_chart3_alt <- bind_rows(df_chart3,
df3_1 %>%
  mutate(efetividade = sen_eleitos/sen_favorecidos,
         efetividade_texto = 
           paste(gsub( "\\.", "," , as.character(100*round(efetividade, 2))), "%", sep="")) %>%
  slice(-(7:8)) %>%
  mutate(cargo = "Senador"))

chart3_alternativo <- df_chart3_alt %>%
  ggplot(aes(x=reorder(agrupador, -efetividade) , y=efetividade, label = efetividade_texto)) + 
  geom_bar(stat = "identity", fill= "#406fef", colour= "#406fef") +
  geom_text(size = 3, vjust= -.5,check_overlap = TRUE) +
  facet_grid(. ~ cargo) +
  theme_tb(base_family = "Helvetica" , legend_size = 8) + ylab("Efetividade candidatos eleitos") + xlab("") +
  scale_y_continuous(labels = percent, limits = c(0, 1))

setwd("C:/Users/mgaldino/2016/ACT/charts")
ggsave("grafico3_alternativo.bmp", chart3_alternativo, scale=.8, height = 8, width = 12, family="Helvetica" )


df3_main <- candidatos_fav %>%
  filter(Cargo %in% c("Senador", "Deputado Federal")) %>%
  group_by(Cargo, agrupador, bol_status_eleito) %>%
  summarise(doacoes = sum(Valor.receita)/1e6) %>%
  ungroup() %>%
  mutate(efetividade_texto = 
           gsub( "\\.", "," , as.character(round(doacoes, 1))))
           
chart3_main <- df3_main %>%
  filter(Cargo == "Deputado Federal") %>%
  ggplot(aes(x=reorder(agrupador, -doacoes) , y=doacoes, fill=bol_status_eleito, label = efetividade_texto)) + 
  geom_bar(stat = "identity") + #, fill= c("#406fef", "#ffb959"), colour= "#406fef") +
  geom_text(size = 3, vjust= -.5, check_overlap = TRUE) +
  theme_tb(base_family = "Helvetica" , legend_size = 8) +
  ylab("Efetividade em milhões") + xlab("") +
  scale_y_continuous(labels = real_format()) 

chart4_main <- df3_main %>%
  filter(Cargo == "Senador") %>%
  ggplot(aes(x=reorder(agrupador, -doacoes) , y=doacoes, fill=bol_status_eleito, label = efetividade_texto)) + 
  geom_bar(stat = "identity") + #, fill= "#406fef", colour= "#406fef") +
  geom_text(size = 3, vjust= -1, check_overlap = F) +
  theme_tb(base_family = "Helvetica" , legend_size = 8) +
  ylab("Efetividade em milhões") + xlab("") +
  scale_y_continuous(labels = real_format()) 


#### Chart 5  e tabela 4 distribution of donoations
df5 <- candidatos_fav %>%
  filter(bol_status_eleito == "eleito") %>%
  group_by(CPF.do.candidato, agrupador) %>%
  summarise(receita = sum(Valor.receita),
            receita_milhoes = round(receita/1e6, 2)) %>%
  ungroup() %>%
  group_by(agrupador) %>%
  mutate( decile = ntile(receita, 10),
          total_doado = sum(receita)) %>%
  ungroup() %>%
  mutate(agrupador = reorder(agrupador, total_doado)) 
          


chart5_v2 <- df5 %>%
  ggplot(aes(x= decile , y=receita_milhoes)) + 
  stat_summary(fun.y=mean, geom="bar") +
  scale_x_continuous(breaks= seq(1, 10, 2)+1)  +
  facet_grid(. ~ agrupador, scales = "free_y")  +
  theme_tb(base_family = "Helvetica" , legend_size = 8) + ylab("Doação média por decil em R$ milhões") + xlab("decil")

ggsave("grafico5_decil_v3.bmp", chart5_v2, scale=.8, height = 8, width = 12, family="Helvetica" )

### Tabela 5 5 DEPUTADOS FEDERAIS MAIS BENEFICIADOS COM DOAÇÕES DE CAMPANHAR POR GRUPOS ALIMENTÍCIOS 

df_tab5 <-  candidatos_fav %>%
  filter(bol_status_eleito == "eleito") %>%
  group_by(CPF.do.candidato, SIGLA_PARTIDO, SIGLA_UE, Nome.candidato, agrupador) %>%
  summarise(receita = sum(Valor.receita),
            receita_milhoes = round(receita/1e6, 2)) %>%
  ungroup() %>%
  group_by(agrupador) %>%
  mutate(q3 = quantile(receita, .75),
         q1 = quantile(receita, .25),
         outlier_status = ifelse(receita > q3 + 1.5*(q3-q1), "outlier", "normal")) %>%
  filter(outlier_status == "outlier") %>%
  select(1, 5, 4, 2, 3, 6)

# doações totais dos favorecidos

candidatos_fav_all <- candidatos_fav %>%
  distinct(CPF.do.candidato, .keep_all = T) %>%
  inner_join(select(doadores_2014_full_bg, c(5, 7, 11)), by = "CPF.do.candidato") %>%
  group_by(CPF.do.candidato) %>%
  summarise(receita_total = sum(Valor.receita.y))

df_tab5_final <- df_tab5 %>%
  inner_join(candidatos_fav_all, by="CPF.do.candidato")

setwd("C:/Users/mgaldino/2016/ACT/tabelas")

write.table(df_tab5_final, file="tabela_5.csv", sep=";", row.names=F)


## Tabela 7 Hugo

df7 <-  candidatos_fav %>%
  filter(bol_status_eleito == "eleito") %>%
  group_by(SIGLA_PARTIDO,  agrupador, Cargo) %>%
  summarise(receita = sum(Valor.receita)) %>%
              spread(agrupador, receita, fill = 0)

# tabela 8 Hugo
df8 <-  candidatos_fav %>%
  filter(bol_status_eleito == "eleito") %>%
  group_by(SIGLA_UE,  Cargo) %>%
  summarise(receita = sum(Valor.receita), num_congressistas = n_distinct(CPF.do.candidato)) 


setwd("C:/Users/mgaldino/2016/ACT/tabelas")
write.table(df8, file="tabela_8_hugo.csv", sep=";", row.names=F)

##  tabela doações totais Jéssica

total_tudo <- sum(doadores_2014_full$Valor.receita)

df_jessica <- doadores_2014_full %>%
  group_by(agrupador) %>%
  summarise(total_doacoes = sum(Valor.receita)) %>%
  ungroup() %>%
  mutate(total_tudo = total_tudo,
         perc_total = total_doacoes/total_tudo)

df_jessica$agrupador[is.na(df_jessica$agrupador)] <- "outras doações"

setwd("C:/Users/mgaldino/2016/ACT/tabelas")
write.table(df_jessica, file="tabela2014_jessica.csv", sep=";", row.names=F)


## http://sunlightfoundation.com/2013/06/24/1pct_of_the_1pct/
## alternativa: classificar as doações em percentil
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






doadores_2014_info <- doadores_2014 %>%
  full_join(info_depfed_2014_final, by = c("CPF.do.candidato" = "CPF_CANDIDATO"))

setwd("/Users/natalia/Documents/Manoel/reports/ACT")
write.table(doadores_2014_info, file = "info_background_candidatos_doadores_2014.csv", sep=";",
            row.names = F)
