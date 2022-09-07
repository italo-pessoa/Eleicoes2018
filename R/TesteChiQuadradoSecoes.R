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
  select(CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO,
         QT_COMPARECIMENTO, NR_VOTAVEL, NM_VOTAVEL, QT_VOTOS)

dados_local <- filter(dados_zona, NR_ZONA == zona, CD_MUNICIPIO == cod_mun,
                      NR_LOCAL_VOTACAO == cod_local)

secoes_local <- dados_local %>% group_by(NR_ZONA, CD_MUNICIPIO, NR_LOCAL_VOTACAO, NR_SECAO) %>%
  summarise(n())

n_secoes <- nrow(secoes_local)

votacao_local <- c()

matriz_votacao <- c()



for (i in 1:n_secoes) {
  qt_votos_secao <- c()
  secao <- secoes_local$NR_SECAO[i]

  dados_secao <- filter(dados_local, NR_SECAO == secao)

  nulos_brancos <- filter(dados_secao, NR_VOTAVEL %in% c(95, 96)) %>%
    group_by(CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO,
             QT_COMPARECIMENTO, NR_VOTAVEL = 99, NM_VOTAVEL = "Nulos e Brancos") %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))
  nulos_brancos <- as.data.frame(nulos_brancos)

  cand_PSL <- filter(dados_secao, NR_VOTAVEL == num_cand_PSL)

  cand_PT <- filter(dados_secao, NR_VOTAVEL == num_cand_PT)

  demais_cand <- filter(dados_secao, !(NR_VOTAVEL == num_cand_PT |NR_VOTAVEL == num_cand_PSL |
                                         NR_VOTAVEL == 95 | NR_VOTAVEL == 96)) %>%
    group_by(CD_MUNICIPIO, NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO,
             QT_COMPARECIMENTO, NR_VOTAVEL = 00, NM_VOTAVEL = "Demais Candidatos") %>%
    summarise(QT_VOTOS = sum(QT_VOTOS))

  demais_cand <- as.data.frame(demais_cand)

  votacao_local <- rbind(votacao_local, nulos_brancos, demais_cand, cand_PSL, cand_PT)

  qt_votos_secao <- rbind(nulos_brancos, demais_cand, cand_PSL, cand_PT) %>%
    select(NM_VOTAVEL, QT_VOTOS)

  qt_votos_secao <- qt_votos_secao %>% pivot_wider(names_from = NM_VOTAVEL,
                                                   values_from = QT_VOTOS)

  # qt_votos_secao <- rbind(qt_votos_secao, c(10,20,30,40))

  matriz_votacao <- rbind(matriz_votacao, qt_votos_secao)

}

matriz_votacao <- matriz_votacao %>%
  mutate(demaisvotos_brancos_nulos = `Nulos e Brancos` + `Demais Candidatos`)

matriz_votacao <- select(matriz_votacao, -`Nulos e Brancos`, -`Demais Candidatos`)

#matriz_votacao <- as.matrix(matriz_votacao, ncol=4)



#mat_alunos <- rbind(c(8,210,72,10,75,125),c(20,180,55,10,90,145))

chisq.test(matriz_votacao)


## plot with statistical results
#ggpiestats(
#  data = df,
#  x = NM_VOTAVEL,
#  bf.message = FALSE
#)


