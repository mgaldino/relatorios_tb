# ajustes em funções do pacto ElectionsBR
## importar banco com colunas como character

candidate_fed1 <- function (year) 
{
  test_fed_year(year)
  dados <- tempfile()
  sprintf("http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_%s.zip", 
          year) %>% download.file(dados)
  unzip(dados, exdir = paste0("./", year))
  unlink(dados)
  cat("Processing the data...")
  setwd(as.character(year))
  banco <- juntaDados1()
  setwd("..")
  unlink(as.character(year), recursive = T)
  if (year < 2014) {
    names(banco) <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", 
                      "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", 
                      "DESCRICAO_UE", "CODIGO_CARGO", "DESCRICAO_CARGO", 
                      "NOME_CANDIDATO", "SEQUENCIAL_CANDIDATO", "NUMERO_CANDIDATO", 
                      "CPF_CANDIDATO", "NOME_URNA_CANDIDATO", "COD_SITUACAO_CANDIDATURA", 
                      "DES_SITUACAO_CANDIDATURA", "NUMERO_PARTIDO", "SIGLA_PARTIDO", 
                      "NOME_PARTIDO", "CODIGO_LEGENDA", "SIGLA_LEGENDA", 
                      "COMPOSICAO_LEGENDA", "NOME_COLIGACAO", "CODIGO_OCUPACAO", 
                      "DESCRICAO_OCUPACAO", "DATA_NASCIMENTO", "NUM_TITULO_ELEITORAL_CANDIDATO", 
                      "IDADE_DATA_ELEICAO", "CODIGO_SEXO", "DESCRICAO_SEXO", 
                      "COD_GRAU_INSTRUCAO", "DESCRICAO_GRAU_INSTRUCAO", 
                      "CODIGO_ESTADO_CIVIL", "DESCRICAO_ESTADO_CIVIL",
                      "CODIGO_NACIONALIDADE", "DESCRICAO_NACIONALIDADE", 
                      "SIGLA_UF_NASCIMENTO", "CODIGO_MUNICIPIO_NASCIMENTO", 
                      "NOME_MUNICIPIO_NASCIMENTO", "DESPESA_MAX_CAMPANHA", 
                      "COD_SIT_TOT_TURNO", "DESC_SIT_TOT_TURNO")
  }
  else {
    names(banco) <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", 
                      "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", 
                      "DESCRICAO_UE", "CODIGO_CARGO", "DESCRICAO_CARGO", 
                      "NOME_CANDIDATO", "SEQUENCIAL_CANDIDATO", "NUMERO_CANDIDATO", 
                      "CPF_CANDIDATO", "NOME_URNA_CANDIDATO", "COD_SITUACAO_CANDIDATURA", 
                      "DES_SITUACAO_CANDIDATURA", "NUMERO_PARTIDO", "SIGLA_PARTIDO", 
                      "NOME_PARTIDO", "CODIGO_LEGENDA", "SIGLA_LEGENDA", 
                      "COMPOSICAO_LEGENDA", "NOME_COLIGACAO", "CODIGO_OCUPACAO", 
                      "DESCRICAO_OCUPACAO", "DATA_NASCIMENTO", "NUM_TITULO_ELEITORAL_CANDIDATO", 
                      "IDADE_DATA_ELEICAO", "CODIGO_SEXO", "DESCRICAO_SEXO", 
                      "COD_GRAU_INSTRUCAO", "CODIGO_COR_RACA", "DESCRICAO_COR_RACA", 
                      "DESCRICAO_GRAU_INSTRUCAO", "CODIGO_ESTADO_CIVIL", 
                      "DESCRICAO_ESTADO_CIVIL", "CODIGO_NACIONALIDADE", 
                      "DESCRICAO_NACIONALIDADE", "SIGLA_UF_NASCIMENTO", 
                      "CODIGO_MUNICIPIO_NASCIMENTO", "NOME_MUNICIPIO_NASCIMENTO", 
                      "DESPESA_MAX_CAMPANHA", "COD_SIT_TOT_TURNO", "DESC_SIT_TOT_TURNO", 
                      "EMAIL_CANDIDATO")
  }
  cat("Done")
  return(banco)
}



juntaDados1 <- function () 
{
  banco <- Sys.glob("*.txt") %>% lapply(function(x) tryCatch(read.table(x, 
                                                                        header = F, sep = ";",colClasses = "character", stringsAsFactors = F, fill = T, 
                                                                        fileEncoding = "windows-1252"), error = function(e) NULL))
  nCols <- sapply(banco, ncol)
  banco <- banco[nCols == Moda(nCols)] %>% do.call("rbind", 
                                                   .)
  banco
}
