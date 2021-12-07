#Pacote que ajuda a extrair os dados da CNES
devtools::install_github("rfsaldanha/microdatasus", force = TRUE)

#Bibliotecas
library(microdatasus)
library(stringr)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(extrafont)
library(remotes)

#Configuraçoes de fonte:
options(OutDec=",")
options(digits = 4, scipen = 100)
windowsFonts(Times=windowsFont("TT Times New Roman"))

#Extraindo os dados de profissionais:
#Ano 2019 - Março:
mar_19 <- fetch_datasus(month_start = 3, month_end = 3,
                        year_start = 2019, year_end = 2019, 
                        uf = "PB", information_system = "CNES-PF")

#Processando os dados: nomeando as variaveis
mar_19 <- process_cnes_pf(mar_19)

#Filtrando os tipos de unidades: 
dados_mar_19 <- mar_19 %>% 
  filter(str_detect(TP_UNID, "Hospital|Pronto"))

#Filtrando apenas os profissionais da saúde
dados_mar_19$CBO <- as.character(dados_mar_19$CBO)

dados_mar_19 %<>% filter(str_starts(CBO, "223") | str_starts(CBO, "225") |
                           str_starts(CBO, "322"))

#Identificando o profissional por estabelecimento 
dados_mar_19 %<>% 
  group_by(CNS_PROF, CNES) %>%
  mutate(id = row_number()) %>%
  ungroup()

#Extraindo os dados relativos a quantidade de leitos no hospital:

#Classificando os hospitais e pronto atendimento de acordo com a 
#quantidade de leitos disponiveis: 

leitos <- fetch_datasus(month_start = 3, month_end = 3,
                        year_start = 2019, year_end = 2019, 
                        uf = "PB", information_system = "CNES-LT")

#Somatório do número de leitos por estabelecimento
leitos %<>%
  group_by(CNES) %>%
  summarise(TOTAL = sum(QT_EXIST))

#Unindo as duas bases anteriores
dados_mar_19 <- left_join(dados_mar_19, leitos, by = "CNES")

#Substituiçao: O NA na base corresponde ao fato que nao tem pleito no hospital.
#Portanto, NA = 0

dados_mar_19 %<>%
  mutate_at(vars(TOTAL), ~replace_na(., 0))

###########------------------------------------------------###################

#Fazendo o mesmo para dezembro de 2019

dez_19 <- fetch_datasus(month_start = 12, month_end = 12,
                        year_start = 2019, year_end = 2019, 
                        uf = "PB", information_system = "CNES-PF")

#Processando os dados: nomeando as variaveis
dez_19 <- process_cnes_pf(dez_19)

#Filtrando os tipos de unidades: 
dados_dez_19 <- dez_19 %>% 
  filter(str_detect(TP_UNID, "Hospital|Pronto"))

#Filtrando apenas os profissionais da saúde

dados_dez_19$CBO <- as.character(dados_dez_19$CBO)

dados_dez_19 %<>% filter(str_starts(CBO, "223") | str_starts(CBO, "225") |
                           str_starts(CBO, "322"))

#Identificando o profissional por estabelecimento 
dados_dez_19 %<>% 
  group_by(CNS_PROF, CNES) %>%
  mutate(id = row_number()) %>%
  ungroup

#OBS: Deixando informaçoes em dezembro que tem em março:

#Semi-join: retorna todas as linhas de  onde há valores correspondentes de y,
#mantendo apenas as colunas de x: 

dados_dez_19 <- semi_join(dados_dez_19, dados_mar_19, by = "CNES")

#Baixando a informaçao sobre a qtde de leitos:
leitos <- fetch_datasus(month_start = 12, month_end = 12,
                        year_start = 2019, year_end = 2019, 
                        uf = "PB", information_system = "CNES-LT")
leitos %<>%
  group_by(CNES) %>%
  summarise(TOTAL = sum(QT_EXIST))

dados_dez_19 <- left_join(dados_dez_19, leitos, by = "CNES")

#NA == 0:
dados_dez_19 %<>%
  mutate_at(vars(TOTAL), ~replace_na(., 0))

#Preparando a base - março

d0 <-  dados_mar_19 %>% 
  select(CNES, CNS_PROF) %>%
  group_by(CNS_PROF, CNES) %>%
  mutate(t0 = 1,
         id = row_number()) %>%
  ungroup()

#Preparando a base - dezembro

d1 <-  dados_dez_19 %>% 
  select(CNES, CNS_PROF) %>%
  group_by(CNS_PROF, CNES) %>%
  mutate(t1 = 1,
         id = row_number()) %>%
  ungroup()

#Unindo as informaçoes dos dois meses: 
d <- full_join(d0, d1, by = c("CNES", "CNS_PROF", "id"))

#Onde nao tem informaçao, substituir por 0
d[is.na(d)] = 0

#Se trabalhou nos dois periodos continuou no emprego, menor do que 2 trocou:
d <- d %>% 
  mutate(t = t0 + t1,
         rotat = if_else(t < 2, 1,0))

#Voltando para a base original:
df0 <- left_join(dados_mar_19,d, by = c("CNES", "CNS_PROF", "id" )) %>%
  mutate(tempo = 0)

df1<- left_join(dados_dez_19,d, by = c("CNES", "CNS_PROF", "id")) %>%
  mutate(tempo = 1)

#Uniao dos dois meses:
uniao_19 = bind_rows(df0, df1)

#Criando variavel sobre a quantidade média de vinculos de trabalhos de um
#profissional: quantidade de vinculos entre março e dezembro.

uniao_19 %<>%
  group_by(CNS_PROF, tempo) %>%
  mutate(VINCULOS = n())

#Fazendo análises por público e privado: 
#Descobrindo quem sao os hospitais públicos e privados pela natureza juridica: 

uniao_19$NAT_JUR <- as.character(uniao_19$NAT_JUR)

uniao_19 %<>% mutate(PUBLICO = if_else(NAT_JUR == "2011"|
                                        NAT_JUR == "Autarquia Federal"|
                                        NAT_JUR == "Autarquia Municipal"|
                                        NAT_JUR == "Município"|
                                        NAT_JUR == "Orgao Público do Poder Executivo Estadual ou do Distrito Federal",1,0))

#Renomeando algumas profissionais que podem ser divididas em varias categorias:
uniao_19  %<>% 
  mutate(PROF = case_when(str_detect(OCUPACAO, "Enfermeiro") ~ "Enfermeiro",
                          str_detect(OCUPACAO, "Médico") ~ "Médico",
                          str_detect(OCUPACAO, "Técnico de enfermagem") ~ "Técnico de enfermagem",
                          str_detect(OCUPACAO, "Psicólogo") ~ "Psicólogo",
                          str_detect(OCUPACAO, "Fisioterapeuta") ~ "Fisioterapeuta",
                          str_detect(OCUPACAO, "Cirurgiao dentista") ~ "Cirurgiao dentista",
                          str_detect(OCUPACAO, "Auxiliar de enfermagem") ~ "Técnico de enfermagem",
                          str_detect(OCUPACAO, "Farmaceutico") ~ "Farmaceutico",
                          TRUE ~ as.character(OCUPACAO)))


#Estatísticas descritivas sobre 2019: 
#N total e proporçao de funcionários por tipo de hospital 
uniao_19 %>%
  group_by(tempo, PUBLICO) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100)

#N total e proporçao de funcionários por tipo de hospital e por grupo ocupacional
#Alternar as opçoes de publico e tempo:
#Exemplo: PUBLICO == 1 & tempo == 1 - hospitais privados em dezembro

uniao_19 %>%
  filter(PUBLICO == 1 & tempo == 1) %>%
  group_by(PROF) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(-prop)

#N total e proporçao de pessoas por carga horária: 
#Por horas trabalhadas
uniao_19$H_TOTAL <- uniao_19$HORAOUTR + uniao_19$HORAHOSP + uniao_19$HORA_AMB

#Criando dummies para total de horas trabalhadas:
uniao_19 %<>% mutate(D_HORAS = case_when(H_TOTAL %in% 0:10 ~ 1,
                                         H_TOTAL %in% 11:20 ~ 2,
                                         H_TOTAL %in% 21:30 ~ 3,
                                         H_TOTAL > 30 ~ 4))
#Estatística descritiva: 
uniao_19 %>%
  filter(PUBLICO == 0 & tempo == 0) %>%
  group_by(D_HORAS) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100)

#Importando os dados sobre as mesorregioes: 
codigo <- read.csv2("codigo_meso.csv", sep = ";")

#Transformando em factor
codigo$mun_trab <- as.factor(codigo$mun_trab)

#Unindo com a base uniao_19: 
uniao_19<- inner_join(uniao_19,codigo, by = c("CODUFMUN" = "mun_trab"))

#Estatística descritiva por mesorregiao
uniao_19 %>%
  filter(PUBLICO == 1 & tempo == 1) %>%
  group_by(meso_trab) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(-prop)

#Fazendo a análise por porte de hospital: 
uniao_19  %<>% mutate(PORTE = case_when(TOTAL <= 50 ~ 1,
                                        TOTAL %in% 51:150 ~ 2,
                                        TOTAL %in% 151:500 ~ 3,
                                        TOTAL > 500 ~ 4))

uniao_19 %>%
  filter(PUBLICO == 1 & tempo == 1) %>%
  group_by(PORTE) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100)
###########------------------------------------------------###################

#PARA 2020:
mar_20 <- fetch_datasus(month_start = 3, month_end = 3,
                        year_start = 2020, year_end = 2020, 
                        uf = "PB", information_system = "CNES-PF")

#Processando os dados: nomeando as variaveis
mar_20 <- process_cnes_pf(mar_20)

#Filtrando os tipos de unidades: 
dados_mar_20 <- mar_20 %>% 
  filter(str_detect(TP_UNID, "Hospital|Pronto"))

#Filtrando apenas os profissionais da saúde
dados_mar_20$CBO <- as.character(dados_mar_20$CBO)

dados_mar_20 %<>% filter(str_starts(CBO, "223") | str_starts(CBO, "225") |
                           str_starts(CBO, "322"))

#Identificando o profissional dentro do estabelecimento 
dados_mar_20 %<>% 
  group_by(CNS_PROF, CNES) %>%
  mutate(id = row_number()) %>%
  ungroup()

#Deixando apenas os hospitais que tem em 2019
dados_mar_20 <- semi_join(dados_mar_20, dados_mar_19, by = "CNES")

#Leitos
leitos <- fetch_datasus(month_start = 3, month_end = 3,
                        year_start = 2020, year_end = 2020, 
                        uf = "PB", information_system = "CNES-LT")

#Somatório de leitos por hospital
leitos %<>%
  group_by(CNES) %>%
  summarise(TOTAL = sum(QT_EXIST))

dados_mar_20 <- left_join(dados_mar_20, leitos, by = "CNES")

#onde há NA é porque nao tem leitos para internaçao

dados_mar_20 %<>%
  mutate_at(vars(TOTAL), ~replace_na(., 0))

####################
#Fazendo o mesmo para dezembro de 2020
dez_20 <- fetch_datasus(month_start = 12, month_end = 12,
                        year_start = 2020, year_end = 2020, 
                        uf = "PB", information_system = "CNES-PF")

#Processando os dados: nomeando as variaveis
dez_20 <- process_cnes_pf(dez_20)

#Filtrando os tipos de unidades: 
dados_dez_20 <- dez_20 %>% 
  filter(str_detect(TP_UNID, "Hospital|Pronto"))

#Filtrando apenas os profissionais da saúde
dados_dez_20$CBO <- as.character(dados_dez_20$CBO)

dados_dez_20 %<>% filter(str_starts(CBO, "223") | str_starts(CBO, "225") |
                           str_starts(CBO, "322"))

dados_dez_20 %<>% 
  group_by(CNS_PROF, CNES) %>%
  mutate(id = row_number()) %>%
  ungroup()

#Deixando informaçoes em dezembro que tem em 2019 (mes de referencia = mar/2019)
dados_dez_20 <- semi_join(dados_dez_20, dados_mar_19, by = "CNES")

#Leitos
leitos <- fetch_datasus(month_start = 12, month_end = 12,
                        year_start = 2020, year_end = 2020, 
                        uf = "PB", information_system = "CNES-LT")
leitos %<>%
  group_by(CNES) %>%
  summarise(TOTAL = sum(QT_EXIST))

dados_dez_20 <- left_join(dados_dez_20, leitos, by = "CNES")

#onde há NA é porque nao tem leitos para internaçao
dados_dez_20 %<>%
  mutate_at(vars(TOTAL), ~replace_na(., 0))

###########------------------------------------------------###################

#Preparando a base - março
d0 <-  dados_mar_20 %>% 
  select(CNES, CNS_PROF) %>%
  group_by(CNS_PROF, CNES) %>%
  mutate(t0 = 1,
         id = row_number()) %>%
  ungroup()

#Preparando a base - dezembro
d1 <-  dados_dez_20 %>% 
  select(CNES, CNS_PROF) %>%
  group_by(CNS_PROF, CNES) %>%
  mutate(t1 = 1,
         id = row_number()) %>%
  ungroup()

#Unindo as informaçoes dos dois meses: 
d <- full_join(d0, d1, by = c("CNES", "CNS_PROF", "id"))

#Onde nao tem informaçao, substituir por 0
d[is.na(d)] = 0

#Se trabalhou nos dois periodos continuou no emprego, menor do que 2 trocou:
d <- d %>% 
  mutate(t = t0 + t1,
         rotat = if_else(t < 2, 1,0))

#Voltando para a base original:
df0 <- left_join(dados_mar_20,d, by = c("CNES", "CNS_PROF", "id" )) %>%
  mutate(tempo = 0)

df1<- left_join(dados_dez_20,d, by = c("CNES", "CNS_PROF", "id")) %>%
  mutate(tempo = 1)

#Uniao dos dois meses:
uniao_20 = bind_rows(df0, df1)

#Criando variavel sobre a quantidade de vinculos de trabalhos de um
#profissional 
uniao_20 %<>%
  group_by(CNS_PROF, tempo) %>%
  mutate(VINCULOS = n())

#Fazendo análises para os hospitais públicos e privados: 

#Descobrindo quem sao os hospitais públicos e privados pela natureza juridica: 
uniao_20$NAT_JUR <- as.character(uniao_20$NAT_JUR)

uniao_20 %<>% mutate(PUBLICO = if_else(NAT_JUR == "2011"|
                                         NAT_JUR == "Autarquia Federal"|
                                         NAT_JUR == "Autarquia Municipal"|
                                         NAT_JUR == "Município"|
                                         NAT_JUR == "Orgao Público do Poder Executivo Estadual ou do Distrito Federal",1,0))

#Renomeando algumas profissionais que podem ser divididas em varias categorias:
uniao_20  %<>% 
  mutate(PROF = case_when(str_detect(OCUPACAO, "Enfermeiro") ~ "Enfermeiro",
                          str_detect(OCUPACAO, "Médico") ~ "Médico",
                          str_detect(OCUPACAO, "Técnico de enfermagem") ~ "Técnico de enfermagem",
                          str_detect(OCUPACAO, "Psicólogo") ~ "Psicólogo",
                          str_detect(OCUPACAO, "Fisioterapeuta") ~ "Fisioterapeuta",
                          str_detect(OCUPACAO, "Cirurgiao dentista") ~ "Cirurgiao dentista",
                          str_detect(OCUPACAO, "Auxiliar de enfermagem") ~ "Técnico de enfermagem",
                          str_detect(OCUPACAO, "Farmaceutico") ~ "Farmaceutico",
                          TRUE ~ as.character(OCUPACAO)))

#Construindo algumas estatísticas descritivas: 
#N total e proporçao de funcionários por tipo de hospital 
uniao_20 %>%
  group_by(tempo, PUBLICO) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100)

#N total e proporçao de funcionários por tipo de hospital e por grupo ocupacional
#Alternar as opçoes de publico e tempo:
#Exemplo: PUBLICO == 1 & tempo == 1 - hospitais privados em dezembro
uniao_20 %>%
  filter(PUBLICO == 1 & tempo == 1) %>%
  group_by(PROF) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(-prop) %>%
  head()

#N total e proporçao de pessoas por carga horária: 
#Por horas trabalhadas
uniao_20$H_TOTAL <- uniao_20$HORAOUTR + uniao_20$HORAHOSP + uniao_20$HORA_AMB

#Criando dummies para total de horas trabalhadas:
uniao_20 %<>% mutate(D_HORAS = case_when(H_TOTAL %in% 0:10 ~ 1,
                                         H_TOTAL %in% 11:20 ~ 2,
                                         H_TOTAL %in% 21:30 ~ 3,
                                         H_TOTAL > 30 ~ 4))
#Estatística descritiva: 
t <- uniao_20 %>%
  filter(PUBLICO == 0 & tempo == 0) %>%
  group_by(D_HORAS) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100)

#Separando por mesorregiao: 
codigo <- read.csv2("codigo_meso.csv", sep = ";")

#Transformando em factor
codigo$mun_trab <- as.factor(codigo$mun_trab)

#Unindo com a base uniao_20: 
uniao_20<- inner_join(uniao_20,codigo, by = c("CODUFMUN" = "mun_trab"))

#Estatística descritiva por mesorregiao
uniao_20 %>%
  filter(PUBLICO == 1 & tempo == 0) %>%
  group_by(meso_trab) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(-prop)

#Estatística descritiva para porte de hospital
uniao_20  %<>% mutate(PORTE = case_when(TOTAL <= 50 ~ 1,
                                        TOTAL %in% 51:150 ~ 2,
                                        TOTAL %in% 151:500 ~ 3,
                                        TOTAL > 500 ~ 4))
uniao_20 %>%
  filter(PUBLICO == 1 & tempo == 1) %>%
  group_by(PORTE) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n)*100)
###########------------------------------------------------###################
#Analisando finalmente os Índices de rotatividade em hospitais 
#Públicos e privados no Estado da Paraíba.

#Rotatividade em hospital privado em 2019
uniao_19 %>% 
  filter(PUBLICO == 0) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Intervalo de confiança
prop.test(540,4590)

#Em hospital Público
uniao_19 %>% 
  filter(PUBLICO == 1) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Intervalo de confiança
prop.test(1910,17013)

###############
#Rotatividade em hospital privado em 2020
uniao_20 %>% 
  filter(PUBLICO == 0) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Intervalo de confiança
prop.test(226,4827)

#Em hospital Público
uniao_20 %>% 
  filter(PUBLICO == 1) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Intervalo de confiança
prop.test(1599,17543)

#Testando se os Índices de rotatividade sao estatisticamente diferentes:
#Ho: diferença das proporçoes = 0
#H1: diferença das proporçoes é diferente de 0. 
#Nível de confiança: 95%.

#Para hospitais privados: 
dif <- 0.11764706 - 0.04681997; dif 

prop.test(x = c (540, 226), n = c(4590, 4827))

#Como p-valor foi igual a zero, tem-se que a hipotese nula foi rejeitada
#Isto é, a diferença dos Índices de 2019 e 2020 nao é igual a zero.
#A rejeiçao tambem é vista pelo intervalo de confiança, dado que 0
#nao pertence ao intervalo. 

#Para hospitais Públicos:
dif <- 0.11226709 - 0.09114747 ; dif 

prop.test(x = c (1910, 1599), n = c(17013,17543))


#Visualizaçao dos Índices de rotatividade por gráfico: 
ano <- c(2019,2019, 2020, 2020)
ind <- c(11.76,11.22, 4.68,9.11)
hosp<- c("Privado", "Público","Privado", "Público")
data <- cbind(ano, ind, hosp)
data <- as_tibble(data)

data %<>% mutate(ano = as.factor(ano),
                 hosp = as.factor(hosp),
                 porc = paste0(ind, "%"),
                 ind = as.numeric(ind))

data$porc<- gsub("\\.", ",", data$porc)


png("Graf_1.png", width = 7, height = 4, units = 'in', res = 300)
ggplot(data, aes(x=hosp, y=ind, fill=ano)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.4)+
  geom_text(aes(label = porc), position = position_dodge(0.4),
            vjust = -0.3, hjust = 0.5, size = 3.8, color = "black") +
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite", size=0.7, 
                                         linetype="blank")) +
  scale_fill_manual(values = c("#7AACBF", "#010D26")) +
  labs(x = "", y = "Índice de Rotatividade (%)",
       fill = "")
dev.off()

#Calculando o Índice de Rotatividade para as mesorregioes de acordo
#com o tipo de hospital 
#Para 2019
d0 <- uniao_19 %>% 
  group_by(PUBLICO, meso_trab) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2019)) %>%
  arrange(-sumdez)

#Para 2020
d1 <- uniao_20 %>% 
  group_by(PUBLICO, meso_trab) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2020)) %>%
  arrange(-sumdez)

uniao <- rbind(d0, d1) %>%
  mutate(ind_r = round(ind_r, 2),
         porc = paste0(ind_r, "%"))

uniao$porc<- gsub("\\.", ",", uniao$porc)


uniao %<>% mutate(PUBLICO = factor(PUBLICO, levels= c(0, 1),
                                   labels = c("Privado", "Público")))

#Mostrando esses resultados em formato de gráfico
png("Graf_3.png", width = 10, height = 4, units = 'in', res = 300)
ggplot(uniao, aes(x=as.factor(meso_trab), y=ind_r, fill=ano)) +
  geom_bar(position = "dodge", stat = "identity", width=0.9) +
  facet_wrap(~as.factor(PUBLICO) ) +
  geom_text(aes(label = porc), size = 2.8, color = "black",
            vjust = -0.2, position = position_dodge(0.8)) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) + 
  theme(axis.text.x = element_text(hjust =0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite", size=0.7,  linetype="blank"),
        strip.text = element_text(size = 13, face = "bold")) +
  scale_fill_manual(values = c("#7AACBF", "#010D26")) +
  labs(x = "", y = "Índice de Rotatividade (%)",
       fill = "")
dev.off()

#Testando a hipotese anterior, porém para cada mesorregiao 
#Primeiramente para hospital privado 
#Para encontrar os resultados é so trocar a base. Ex: uniao_19 ou uniao_20
uniao_20 %>% 
  filter(PUBLICO == 0) %>%
  group_by(meso_trab) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  arrange(-sumdez)

#Mata Paraibana	
dif <- 0.13191206 - 0.04643963 ; dif
prop.test(x = c(396,150), n = c(3002,3230))

#Agreste Paraibano
dif <- 0.07724719 - 0.04259003; dif
prop.test(x = c(110, 61.5), n = c(1424,1444))

#Sertao Paraibano
dif <- 0.11965812 - 0.05357143; dif
prop.test(x = c(14, 6), n = c(117,112))

#Borborema
dif <- 0.4042553 - 0.2317073; dif
prop.test(x = c(19, 9.5), n = c(47,41))


#Realizando o mesmo procedimento para hospitais Públicos: 
uniao_20 %>% 
  filter(PUBLICO == 1) %>%
  group_by(meso_trab) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  arrange(-sumdez)

#Para Mata Paraibana
dif <- 0.09747682 - 0.10201383; dif
prop.test(x = c(904, 1003), n = c(9274,9832))


#Agreste Paraibano
dif <- 0.12091954 - 0.06766398; dif
prop.test(x = c(526,294), n = c(4350,4345))

#Sertao Paraibano 
dif <- 0.15680473 - 0.08724832 ; dif
prop.test(x = c(424, 234), n = c(2704, 2682))

#Borborema: 
dif <- 0.08175182 - 0.09795322 ; dif
prop.test(x = c(56,67), n = c(685,684))

####Índice de Rotatividade por horas trabalhadas
#Por horas trabalhadas

#análise gráfica: 
d0 <- uniao_19 %>% 
  group_by(D_HORAS, PUBLICO) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2019)) 

#Para 2020
d1 <- uniao_20 %>% 
  group_by(D_HORAS, PUBLICO) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2020)) 

uniao <- rbind(d0, d1) %>%
  mutate(ind_r = round(ind_r, 2),
         porc = paste0(ind_r, "%"))

uniao$porc <- gsub("\\.", ",", uniao$porc)

uniao %<>% mutate(PUBLICO = factor(PUBLICO, levels= c(0, 1),
                                   labels = c("Privado", "Público")))

uniao %<>% mutate(D_HORAS = factor(D_HORAS, levels= c(1, 2,3,4),
                                   labels = c("At???? 10 horas", "11h a 20 horas",
                                              "21h a 30 horas","31 horas ou mais")))

#Mostrando esses resultados em formato de gráfico
png("Graf_5.png", width = 10, height = 4, units = 'in', res = 300)
ggplot(uniao, aes(x=as.factor(D_HORAS), y=ind_r, fill=ano)) +
  geom_bar(position = "dodge", stat = "identity", width=0.9) +
  facet_wrap(~as.factor(PUBLICO) ) +
  geom_text(aes(label = porc), size = 2.8, color = "black",
            vjust = -0.2, position = position_dodge(0.8)) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) + 
  theme(axis.text.x = element_text(hjust =0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite", size=0.7,  linetype="blank"),
        strip.text = element_text(size = 13, face = "bold")) +
  scale_fill_manual(values = c("#7AACBF", "#010D26")) +
  labs(x = "", y = "Índice de Rotatividade (%)",
       fill = "")
dev.off()

#Teste de hipotese, considerando as horas trabalhadas: 
uniao_19 %>% 
  filter(PUBLICO == 0) %>%
  group_by(D_HORAS) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Hospital Privado
#até 10h
dif <- 0.07820513 - 0.06155989; dif
prop.test(x = c(122,110.5), n = c(1560,1795))

#11h a 20h
dif <- 0.09201624 - 0.04161074; dif
prop.test(x = c(68,31), n = c(739,745))

#21h a 30h 
dif <- 0.2332762 - 0.0493311; dif
prop.test(x = c(136,29.5), n = c(583,598))

#31h ou mais
dif <- 0.12470726 - 0.03285968; dif
prop.test(x = c(213,55.5), n = c(1708,1689))

#Hospital Público: 
uniao_20 %>% 
  filter(PUBLICO == 1) %>%
  group_by(D_HORAS) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Para até 10h:
dif <- 0.09381663 - 0.04631696; dif
prop.test(x = c(88,41.5), n = c(938,896))

#11h a 20h 
dif <- 0.1218530 - 0.1049724; dif
prop.test(x = c(484,418), n = c(3972,3982))

#21h a 30h
dif <- 0.10495553 - 0.07733169 ; dif
prop.test(x = c(826,626), n = c(7870,8095))

#31h ou mais 
dif <- 0.1209544 - 0.1124726; dif
prop.test(x = c(512,514), n = c(4233,4570))

#Índice de rotatividade para as profiss????es 
#Visualizando graficamente: 
d0 <- uniao_19 %>% 
  group_by(PROF, PUBLICO) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2019)) %>%
  filter(PROF == "Técnico de enfermagem"|PROF == "Médico"|PROF == "Enfermeiro"|PROF == "Fisioterapeuta"| PROF == "Farmaceutico")

#Para 2020
d1 <- uniao_20 %>% 
  group_by(PUBLICO, PROF) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2020)) %>%
  filter(PROF == "Técnico de enfermagem"|PROF == "Médico"|PROF == "Enfermeiro"|PROF == "Fisioterapeuta" | PROF == "Farmaceutico")


uniao <- rbind(d0, d1) %>%
  mutate(ind_r = round(ind_r, 2),
         porc = paste0(ind_r, "%"))

uniao$porc <- gsub("\\.", ",", uniao$porc)

uniao %<>% mutate(PUBLICO = factor(PUBLICO, levels= c(0, 1),
                                   labels = c("Privado", "Público")))

#Mostrando esses resultados em formato de gráfico
png("Graf_4.png", width = 10, height = 4, units = 'in', res = 300)

ggplot(uniao, aes(x=as.factor(PROF), y=ind_r, fill=ano)) +
  geom_bar(position = "dodge", stat = "identity", width=0.9) +
  facet_wrap(~as.factor(PUBLICO) ) +
  geom_text(aes(label = porc), size = 2.8, color = "black",
            vjust = -0.2, position = position_dodge(0.8)) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) + 
  theme(axis.text.x = element_text(hjust =0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite", size=0.7,  linetype="blank"),
        strip.text = element_text(size = 13, face = "bold")) +
  scale_fill_manual(values = c("#7AACBF", "#010D26")) +
  labs(x = "", y = "Índice de Rotatividade (%)",
       fill = "")

dev.off()

#Testes de hipoteses:
uniao_19 %>% 
  filter(PUBLICO == 0) %>%
  group_by(PROF) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  arrange(-sumdez) %>%
  head()

#Para Hospital Privado
#Para tecnico de enfermagem 
dif <- 0.10277033 - 0.06050319; dif
prop.test(x = c(230, 151.5), n = c(2238, 2504))

#Para Médicos
dif <- 0.10504732 - 0.02884615; dif
prop.test(x = c(166.5, 45), n = c(1585, 1560))

#Para enfermeiro
dif <- 0.16949153 - 0.03634085; dif
prop.test(x = c(70, 14.5), n = c(413, 399))

#Para fisioterapeuta
dif <- 0.22928177 - 0.04812834; dif
prop.test(x = c(41.5, 9), n = c(181, 187))

#Para Farmaceutico
dif <- 0.19135802 - 0.06024096; dif
prop.test(x = c(15.5, 5), n = c(81, 83))

#Para hospitais Públicos
uniao_20 %>% 
  filter(PUBLICO == 1) %>%
  group_by(PROF) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  arrange(-sumdez) %>%
  head()

#Para Técnico de Enfermagem 
dif <- 0.08871473 - 0.08239363; dif

prop.test(x = c(566, 548), n = c(6380, 6651))

#Para Médicos 
dif <- 0.1456311 - 0.1042310 ; dif
prop.test(x = c(795, 574), n = c(5459, 5507))

#Para enfermeiros
dif <- 0.10714286 - 0.09028387 ; dif
prop.test(x = c(354, 308.5), n = c(3304, 3417))

#Para fisioterapeutas: 
dif <- 0.09035222 - 0.087904367 ; dif
prop.test(x = c(59, 62.5), n = c(653, 711))

#Para farmaceuticos: 
dif <- 0.12102473 - 0.08761062 ; dif
prop.test(x = c(68.5, 49.5), n = c(566, 565))

x = c(566, 795, 354, 59, 68.5)
n = c(6380, 5459, 3304,653, 566)


x = c(548, 574, 308, 62.5, 49,5)
n = c(6651,5507, 3417, 711, 565)


#Índice de rotatividade por porte do hospital/pronto atendimento:
#análise gráfica: 
d0 <- uniao_19 %>% 
  group_by(PORTE, PUBLICO) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2019)) 

#Para 2020
d1 <- uniao_20 %>% 
  group_by(PORTE, PUBLICO) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2020)) 

uniao <- rbind(d0, d1) %>%
  mutate(ind_r = round(ind_r, 2),
         porc = paste0(ind_r, "%"))

uniao$porc <- gsub("\\.", ",", uniao$porc)

uniao %<>% mutate(PUBLICO = factor(PUBLICO, levels= c(0, 1),
                                   labels = c("Privado", "Público")))

uniao %<>% mutate(PORTE = factor(PORTE, levels= c(1, 2,3),
                                 labels = c("Pequeno", "M????dio",
                                            "Grande")))

#Mostrando esses resultados em formato de gráfico
png("Graf_2.png", width = 7, height = 4, units = 'in', res = 300)

ggplot(uniao, aes(x=as.factor(PORTE), y=ind_r, fill=ano)) +
  geom_bar(position = "dodge", stat = "identity", width=0.9) +
  facet_wrap(~as.factor(PUBLICO) ) +
  geom_text(aes(label = porc), size = 2.8, color = "black",
            vjust = -0.2, position = position_dodge(0.8)) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) + 
  theme(axis.text.x = element_text(hjust =0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite", size=0.7,  linetype="blank"),
        strip.text = element_text(size = 13, face = "bold")) +
  scale_fill_manual(values = c("#7AACBF", "#010D26")) +
  labs(x = "", y = "Índice de Rotatividade (%)",
       fill = "")

dev.off()

#Testes de hipoteses: 
#Para 2019
uniao_19 %>% 
  filter(PUBLICO == 0) %>%
  group_by(PORTE) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) 

#Para 2020
uniao_20 %>% 
  filter(PUBLICO == 0) %>%
  group_by(PORTE) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Para hospitais privados
#Para porte pequeno
dif <- 0.16384181 - 0.05665349; dif
prop.test(x = c(116,43), n = c(708,759))

#Para porte m????dio
dif <- 0.10464691 - 0.05022672; dif
prop.test(x = c(286,144), n = c(2733,2867))

#Para porte grande
dif <- 0.11923412 - 0.03288926; dif
prop.test(x = c(137,39.5), n = c(1149,1201))

#Para hospitais publicos
uniao_19 %>% 
  filter(PUBLICO == 1) %>%
  group_by(PORTE) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) 

#Para 2020
uniao_20 %>% 
  filter(PUBLICO == 1) %>%
  group_by(PORTE) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Para porte pequeno
dif <- 0.1735294 - 0.1633097; dif
prop.test(x = c(708,600), n = c(4080,3674))

#Para porte m????dio
dif <- 0.09349135 - 0.08144691; dif
prop.test(x = c(497,349), n = c(5316,4285))

#Para porte grande
dif <- 0.09268741 - 0.06782137; dif
prop.test(x = c(706,650), n = c(7617,9584))

#vínculos de trabalho: 
uniao_19  %<>% mutate(D_VINC = case_when(VINCULOS == 1 ~ "1 vínculo",
                                         VINCULOS == 2 ~ "2 vínculos",
                                         VINCULOS >= 3 ~ "3 ou mais vínculos"))

d0 <- uniao_19 %>% 
  group_by(D_VINC, PUBLICO) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2019)) 

#Para 2020
uniao_20  %<>% mutate(D_VINC = case_when(VINCULOS == 1 ~ "1 vínculo",
                                         VINCULOS == 2 ~ "2 vínculos",
                                         VINCULOS >= 3 ~ "3 ou mais vínculos"))

d1 <- uniao_20 %>% 
  group_by(D_VINC, PUBLICO) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) %>%
  mutate(ano = as.factor(2020)) 

uniao <- rbind(d0, d1) %>%
  mutate(ind_r = round(ind_r, 2),
         porc = paste0(ind_r, "%"))

uniao$porc <- gsub("\\.", ",", uniao$porc)

uniao %<>% mutate(PUBLICO = factor(PUBLICO, levels= c(0, 1),
                                   labels = c("Privado", "Público")))

#Mostrando esses resultados em formato de gráfico
png("Graf_6.png", width = 7, height = 4, units = 'in', res = 300)

ggplot(uniao, aes(x=as.factor(D_VINC), y=ind_r, fill=ano)) +
  geom_bar(position = "dodge", stat = "identity", width=0.9) +
  facet_wrap(~as.factor(PUBLICO) ) +
  geom_text(aes(label = porc), size = 2.8, color = "black",
            vjust = -0.2, position = position_dodge(0.8)) +
  theme_minimal(base_family = "Times New Roman", base_size = 12) + 
  theme(axis.text.x = element_text(hjust =0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite", size=0.7,  linetype="blank"),
        strip.text = element_text(size = 13, face = "bold")) +
  scale_fill_manual(values = c("#7AACBF", "#010D26")) +
  labs(x = "", y = "Índice de Rotatividade (%)",
       fill = "")

dev.off()

#Testes de hipoteses: 
#Para 2019
uniao_19 %>% 
  filter(PUBLICO == 0) %>%
  group_by(D_VINC) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) 

#Para 2020
uniao_20 %>% 
  filter(PUBLICO == 0) %>%
  group_by(D_VINC) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Para hospitais privados
#Para um vínculo
dif <- 0.12844867 - 0.04101996 ; dif
prop.test(x = c(284,92.5), n = c(2211,2255))

#Para dois vínculos
dif <- 0.1151832 - 0.0514214; dif
prop.test(x = c(132,61.5), n = c(1146,1196))

#Para tres vínculos ou mais
dif <- 0.10056772 - 0.05268895; dif
prop.test(x = c(124,72.5), n = c(1233,1376))

#Para hospitais Públicos
#Para 2019
uniao_19 %>% 
  filter(PUBLICO == 1) %>%
  group_by(D_VINC) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100) 

#Para 2020
uniao_20 %>% 
  filter(PUBLICO == 1) %>%
  group_by(D_VINC) %>%
  summarise(sumdez = sum(tempo == "1"),
            sumrotat = sum(rotat == "1"),
            mediarot = sumrotat/2,
            ind_r = (mediarot/sumdez)*100)

#Para um vínculo
dif <- 0.11115207 - 0.09726766 ; dif
prop.test(x = c(1206,1100), n = c(10850,11309))

#Para dois vínculos
dif <- 0.11233797 - 0.09052334; dif
prop.test(x = c(468,384), n = c(4166,4242))

#Para tres vínculos ou mais
dif <- 0.11817727 - 0.05773092; dif
prop.test(x = c(236,115), n = c(1997,1992))

#########-----------------------------------------------------------------#
#Preparando a base para estimar o modelo econometrico

#Identificando o ano da base
#2019
uniao_19$ano <- "2019"
#2020
uniao_20$ano <- "2020"

#Unindo as bases de dados: 
base = bind_rows(uniao_19, uniao_20)

#Modificando algumas variaveis 
#Criando dummies para as mesorregioes
base$meso_trab <- factor(x = base$meso_trab, 
                         levels = c("Sertao Paraibano", "Agreste Paraibano",
                                    "Mata Paraibana", "Borborema"),
                         labels = c("1", "2", "3", "4"))

#Criando dummies para as cinco principais profissoes com maior n de profissionais
base %<>% mutate(MED = if_else(PROF == "Médico", 1,0),
                 TEC = if_else(PROF == "Técnico de enfermagem", 1,0),
                 ENF = if_else(PROF == "Enfermeiro", 1,0),
                 FISIO = if_else(PROF == "Fisioterapeuta",1,0),
                 FARM = if_else(PROF == "Farmaceutico",1,0))


base$PORTE <- as.factor(base$PORTE)
base$D_HORAS <- as.factor(base$D_HORAS)

#Exportando a base: 
write.csv2(base, file = "base.csv", row.names = F)

#########-----------------------------------------------------------------#
#Estimando de fato o modelo econometrico: 

#Modelo Logit Hierarquico:
#Modelo 1: logit considerando um dos níveis 
m1 <- glmer(rotat ~ + (1 |CNES), family = binomial("logit"), data = base) 
summary(m1)   #Variância entre os hospitais nao é igual a 0

#Modelo 2: logit considerando dois dos níveis 
m2 <- glmer(rotat ~ + (1 |CNES) + (1 |CODUFMUN) , family = binomial("logit"), data = base) 
summary(m2)   #Variância entre os municípios é bem menor, mas ainda nao é igual a 0

#Incluindo as variaveis explicativas:
#Modelo 3:
m3 <- glmer(rotat ~ ano + D_HORAS + MED + TEC + FISIO + ENF + FARM + VINCULOS + (1 |CNES) + (1 |CODUFMUN), family = binomial("logit"), data = base)
summary(m3)

#Modelo 4:
m4 <- glmer(rotat ~ ano + D_HORAS + MED + TEC + FISIO + ENF + FARM + VINCULOS + (1 |CNES) + PUBLICO + PORTE +  (1 |CODUFMUN), family = binomial("logit"), data = base)
summary(m4)

#Modelo 5:
m5 <- glmer(rotat ~ ano + D_HORAS + MED + TEC + FISIO + ENF + FARM + VINCULOS + (1 |CNES) + PUBLICO + PORTE +  (1 |CODUFMUN) + meso_trab, family = binomial("logit"), data = base)
summary(m5)

#Tabela com as estimativas com intervalos de confiança a um nível de 95%
#Erros-Padrao
se <- sqrt(diag(vcov(m6)))
#Tabela
tab <- cbind(COEF = fixef(m6), LI = fixef(m6) - 1.96 * se, LS = fixef(m6) + 1.96 *
               se)

#Razao de chances
exp(tab)