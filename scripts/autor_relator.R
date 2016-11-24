#### Relator e autor
library(dplyr)
library(rstanarm)

relator <- read.table("clipboard", header=T, sep="\t", colClasses = "character")
head(relator)

autor <- read.table("clipboard", header=T, sep="\t", colClasses = "character")
head(autor)

autor_relator <- autor %>%
  left_join(relator, by="PL")

head(autor_relator)

## Números

# 1 percentual de proposições de favorecidos e não favorecidos
autor %>%
  mutate(total_linhas = n(),
         fav = ifelse(Favorecido == "Não", "Não", "Sim")) %>%
  group_by(fav) %>%
  summarise(total = n(),
            perc = round(total/max(total_linhas),2))
## 62% nao, 38% fav

# 2 percentual de relatores favorecidos e não favorecidos

relator %>%
  filter(!is.na(Partido_uf)) %>%
  mutate(total_linhas = n(),
         fav = ifelse(Favorecido == "Não", "Não", "Sim")) %>%
  group_by(fav) %>%
  summarise(total = n(),
            perc = round(total/max(total_linhas),2))
# 32% não, 67% fav

## 3 na relatoria, crosst tba entre favorecimento e parecer

ctab_parecer_fav <- autor_relator %>%
  filter(!is.na(Partido_uf.y), Agenda == "Positiva", Parecer != "sem parecer") %>%
  mutate(total_linhas = n_distinct(id_relator),
         fav = ifelse(Favorecido.y == "Não", "Não", "Sim")) %>%
  group_by(fav, Parecer) %>%
  summarise(total = n_distinct(id_relator))

ctab_parecer_fav %>%
  spread(Parecer, total) %>%
  mutate(total_linhas = Contrário + favorável,
         perc_contra = round(Contrário/total_linhas, 2),
         perc_fav = round(favorável/total_linhas, 2))

## logistic
df_reg <- autor_relator %>%
  filter(!is.na(Partido_uf.y), Agenda == "Positiva", Parecer != "sem parecer") %>%
  mutate(total_linhas = n_distinct(id_relator),
         fav = as.factor(ifelse(Favorecido.y == "Não", "Não", "Sim")),
         parecer= ifelse(Parecer == "Contrário", 0, 1))

reg1 <- glm(parecer ~ fav, family=binomial, data = df_reg)
summary(reg1)

t_prior <- student_t(df = 7, location = 0, scale = 2.5)
fit1 <- stan_glm(parecer ~ fav, data = df_reg, 
                 family = binomial(link = "logit"), 
                 prior = normal(-.5, 1), prior_intercept = t_prior,  
                 chains = 4, cores = 4, iter = 2000)


round(posterior_interval(fit1, prob = 0.80), 2)

autor %>%
  mutate(total_linhas = n(),
         fav = ifelse(Favorecido == "Não", "Não", "Sim")) %>%
  group_by(fav, Agenda) %>%
  summarise(total = n_distinct(id_autor),
            perc = round(total/max(total_linhas), 4))


length(unique(autor$id_autor)) # 34

## Números
length(unique(relator$id_relator)) # 34

### Totais




autor_relator %>%
  filter(!is.na(Relator)) %>%
  group_by(Agenda, Parecer) %>%
  summarise(n_pls = n_distinct(PL),
            n_relatores = n_distinct(id_relator),
            n_autores = n_distinct(id_autor))

## fuzzy match
autor <- autor %>%
  mutate(nome_autor = trimws(gsub("Deputado ", "", Autor)),
         nome_autor = gsub("Senador ", "", nome_autor))

x <- unique(candidatos_fav$NOME_URNA_CANDIDATO)
dist.name <- adist(autor$nome_autor, x, partial = TRUE, ignore.case = TRUE)

min.name <- apply(dist.name, 1, min)

adist(x[2], autor$nome_autor[1], partial = TRUE, ignore.case = TRUE)
dim(dist.name)
head(dist.name)
  