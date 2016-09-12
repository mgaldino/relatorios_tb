
library(electionsBR)
library(dplyr)

### Todo o Brasil

vote_mun_zone_local_legenda <- function(year) {
  {
    test_local_year(year)
    dados <- tempfile()
    sprintf("http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_partido_munzona/votacao_partido_munzona_%s.zip", 
            year) %>% download.file(dados)
    unzip(dados, exdir = paste0("./", year))
    unlink(dados)
    cat("Processing the data...")
    setwd(as.character(year))
    banco <- juntaDados()
    setwd("..")
    unlink(as.character(year), recursive = T)
    names(banco) <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", 
                      "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", 
                      "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "NUMERO_ZONA", 
                      "CODIGO_CARGO", "DESCRICAO_CARGO", "TIPO_LEGENDA", "NOME_COLIGACAO",
                      "COMPOSICAO_LEGENDA", 
                      "SIGLA_PARTIDO",  "NUMERO_PARTIDO", "NOME_PARTIDO", 
                      "QTDE_VOTOS_NOMINAIS", "QTDE_VOTOS_LEGENDA", "TRANSITO")
    cat("Done.")
    return(banco)
  }
}
voto_legenda <- vote_mun_zone_local_legenda(year=2012)
voto_legenda_vereador <- filter(voto_legenda, DESCRICAO_CARGO == "VEREADOR")

head(voto_legenda_vereador)

## computado votos válidos por uf e município
votos_validos <- voto_legenda_vereador %>%
  group_by(SIGLA_UF, NOME_MUNICIPIO) %>%
  summarise(total_nominal = sum(QTDE_VOTOS_NOMINAIS),
            total_legenda=sum(QTDE_VOTOS_LEGENDA)) %>%
  mutate(total_votos_validos = total_nominal + total_legenda)

head(votos_validos)

## votos nominais
voto_nominal_2012 <- vote_mun_zone_local(2012)

voto_nominal_vereador <- filter(voto_nominal_2012, DESCRICAO_CARGO == "VEREADOR")
head(voto_nominal_vereador)

## tipos de candidaturas
unique(voto_nominal_vereador$DESC_SIT_CAND_SUPERIOR)
unique(voto_nominal_vereador$DESC_SIT_CANDIDATO)
unique(voto_nominal_vereador$DESC_SIT_CAND_TOT)

# computando número de eleitos (cadeiras) por uf e municípip

num_cadeiras <- voto_nominal_vereador %>%
  filter(DESC_SIT_CAND_SUPERIOR== "APTO",
         DESC_SIT_CAND_TOT %in% c("ELEITO POR MÉDIA", "ELEITO POR QP")) %>%
  group_by(NOME_MUNICIPIO, SIGLA_UF) %>%
  summarise(cadeiras = n_distinct(SQ_CANDIDATO), cadeiras_v2 = n_distinct(NUMERO_CAND))

head(num_cadeiras)

## computado voto por candidato e QE
voto_nominal_vereador_v2 <- voto_nominal_vereador %>%
  filter(DESC_SIT_CAND_SUPERIOR== "APTO") %>%
  group_by(SIGLA_UF, NOME_MUNICIPIO, NUMERO_CAND) %>%
  summarise(total_nominal_nominal = sum(TOTAL_VOTOS),
            resultado = max(DESC_SIT_CAND_TOT)) %>%
  inner_join(num_cadeiras, by=c("NOME_MUNICIPIO", "SIGLA_UF")) %>%
  inner_join(votos_validos,  by=c("NOME_MUNICIPIO", "SIGLA_UF")) %>%
  mutate(quociente_eleitoral = total_votos_validos/cadeiras)

head(voto_nominal_vereador_v2)

## simulando o que aconteceria com regras atuais, paricularmente 10% do QE
voto_nominal_vereador_v2 <- voto_nominal_vereador_v2 %>%
  mutate(bol_10perc = as.numeric(total_nominal_nominal >= .1*as.integer(quociente_eleitoral))) 

## total
voto_nominal_vereador_v2 %>%
  group_by(resultado, bol_10perc) %>%
  summarise(total= n())

## quem são os que perderiam a vaga
voto_nominal_vereador_v2 %>%
  filter(bol_10perc == 0, resultado %in% c("ELEITO POR MÉDIA", "ELEITO POR QP"))

## cadeiras direto
votosp1 <- votosp %>%
  group_by(COMPOSICAO_LEGENDA) %>%
  summarise(num_cand_minimo_nominal = sum(bol_atingiu_10perc),
            total_votos = max(total_votos),
            quociente_eleitoral = max(quociente_eleitoral)) %>%
  mutate(quociente_lista = total_votos/quociente_eleitoral,
         vagas_qe = floor(quociente_lista))

View(votosp1)

