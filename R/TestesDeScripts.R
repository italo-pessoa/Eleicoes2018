library(tidyverse)
#library(ggstatsplot)


dados <- read.csv2('bweb_1t_RO_101020182022.csv')

dados <-  filter(dados, DS_CARGO_PERGUNTA == "Presidente")


zona = 6
cod_mun = 35
cod_local = 1040
num_cand_PSL = 17
num_cand_PT = 13



dados_zona <- filter(dados, NR_ZONA == zona) %>%
  select(NR_ZONA, CD_MUNICIPIO, NR_LOCAL_VOTACAO, NR_SECAO,
         QT_COMPARECIMENTO, NR_VOTAVEL, NM_VOTAVEL, QT_VOTOS)

dados_zona_agrupados <- dados_zona %>%
  select (NR_ZONA, CD_MUNICIPIO, NR_LOCAL_VOTACAO) %>%
  distinct()



dados_local <- filter(dados_zona, NR_ZONA == zona, CD_MUNICIPIO == cod_mun,
                      NR_LOCAL_VOTACAO == cod_local)




secoes_local <- dados_local %>% group_by(NR_ZONA, CD_MUNICIPIO, NR_LOCAL_VOTACAO) %>%
  summarise(QTD_SECOES = n_distinct(dados_local$NR_SECAO))

painel_zona <- tibble(NR_ZONA = integer(), CD_MUNICIPIO = integer(),
                      NR_LOCAL_VOTACAO = integer(),
                      QT_SECOES = integer(), P_VALOR = double())
