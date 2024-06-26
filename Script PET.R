# Tratamento banco de dados Perfil 2021

# 1.Importando o banco de dados
# 1.1 - Maneira mais simples
library(readxl)
perfil21 <- read_excel("Pesquisa.Ingressantes.2021.xlsx")
View(Pesquisa_Ingressantes_2021)

#1.2 - Caso aquela não funcione, precisa direcionar o R para o diretorio
#Instalar e carregar os pacotes necessarios
install.packages("data.table")
library(data.table)
install.packages("here")
library(here)
#Rodar esse código para descobrir qual a pasta do pc colocar o arquivo
getwd()
#Criar o objeto e importar o banco de dados
perfil21 <- read_excel(here("Pesquisa.Ingressantes.2021.xlsx"))

# 2. Ajustes no banco
# 2.1 selecionar as variaveis 
install.packages("tidyverse")
library(tidyverse)
perfil21teste <- perfil21 %>%
  select(`Nr. USP`, `P1. Data de Nascimento`, `P2. Cor (Critério IBGE)`,
         `P3a. Grau de escolaridade de sua mãe (biológica ou, se não cresceu com ela, a de criação)`,
         `P3b. Grau de escolaridade de seu pai (biológico ou, se não cresceu com ele, o de criação)`,
         `P4a. Qual gênero foi atribuído a você ao nascer?`, `P4b. Qual é a sua identidade de gênero?`,
         `P5. Qual é a sua orientação sexual?`, `P6. Seu estado conjugal (situação de fato)`,
         `P7. Filhos?`)

#2.2 Renomear as variaveis
perfil21teste <- perfil21teste %>% 
  rename(NUSP = `Nr. USP`,
         P1 = `P1. Data de Nascimento`, 
         P2 = `P2. Cor (Critério IBGE)`, 
         P3a = `P3a. Grau de escolaridade de sua mãe (biológica ou, se não cresceu com ela, a de criação)`,
         P3b = `P3b. Grau de escolaridade de seu pai (biológico ou, se não cresceu com ele, o de criação)`,
         P4a = `P4a. Qual gênero foi atribuído a você ao nascer?`, 
         P4b = `P4b. Qual é a sua identidade de gênero?`, 
         P5 = `P5. Qual é a sua orientação sexual?`, 
         P6 = `P6. Seu estado conjugal (situação de fato)`,
         P7 = `P7. Filhos?`)

#2.3 Codificar as variaveis 
install.packages("dplyr)
library(dplyr)

#P2
perfil21testecodf <- perfil21teste %>% 
  mutate(P2_codificado = case_when(
    P2 == "Branca" ~ 1,
    P2 == "Preta" ~ 2,
    P2 == "Parda" ~ 3,
    P2 == "Amarela" ~ 4,
    tolower(P2) == "Indígena" ~ 5,
    TRUE ~ 6)) %>% 
  mutate(P2_outra = ifelse(P2 == "Cabocla", "Cabocla", "")) 

#P3a
perfil21testecodf <- perfil21testecodf%>% 
  mutate(P3a_codificado = case_when(
    P3a == "Não frequentou escola" ~ 1,
    P3a == "de 1a. a 4a. do Fundamental" ~ 2,
    P3a == "de 5a. a 8a. do Fundamental" ~ 3,
    P3a == "Médio incompleto (1o. ou 2o. col.)" ~ 4,
    P3a == "Médio completo" ~ 5,
    P3a == "Superior incompleto" ~ 6, 
    P3a == "Superior completo" ~ 7, 
    P3a == "Pós-graduada" ~ 8, 
    P3a == "Não sabe" ~ 9))

#P3b 
perfil21testecodf <- perfil21testecodf %>%
  mutate(P3b_codificado = case_when(
    P3b == "Não frequentou escola" ~ 1,
    P3b == "de 1a. a 4a. do Fundamental" ~ 2,
    P3b == "de 5a. a 8a. do Fundamental" ~ 3,
    P3b == "Médio incompleto (1o. ou 2o. col.)" ~ 4,
    P3b == "Médio completo" ~ 5,
    P3b == "Superior incompleto" ~ 6, 
    P3b == "Superior completo" ~ 7, 
    P3b == "Pós-graduado" ~ 8, 
    P3b == "Não sabe" ~ 9))

#P4a 
perfil21testecodf <- perfil21testecodf %>% 
  mutate(P4a_codificado = case_when(
    P4a == "Masculino" ~ 1, 
    P4a == "Feminino" ~ 2))

#P4b 
perfil21testecodf <- perfil21testecodf %>% 
  mutate(P4b_codificado = case_when(
    P4b == "Homem CIS" ~ 1, 
    P4b == "Homem TRANS" ~ 2, 
    P4b == "Mulher CIS" ~ 3, 
    P4b == "Mulher TRANS" ~ 4, 
    P4b == "Não-binário" ~ 5, 
    TRUE ~ 6)) %>%
  mutate(P4b_outra = ifelse(P4b == "Queer", "Queer", ""))

#P5
perfil21testecodf <- perfil21testecodf %>% 
  mutate(P5_codificado = case_when(
    P5 == "Assexual" ~ 1, 
    P5 == "Bissexual" ~ 2, 
    P5 == "Heterossexual" ~ 3, 
    P5 == "Homossexual" ~ 4, 
    P5 == "Pansexual" ~ 5, 
    TRUE ~ 6)) %>%
  mutate(P5_outra = ifelse(P5 == "Questionando", "Questionando", ""))

#P6 
perfil21testecodf <- perfil21testecodf %>% 
  mutate(P6_codificado = case_when(
    P6 == "Solteiro(a)" ~ 1, 
    P6 == "Casado(a) ou mora junto" ~ 2, 
    P6 == "Separado(a)" ~ 3, 
    P6 == "Viúvo(a)" ~ 4))

#P7 
perfil21testecodf <- perfil21testecodf %>% 
  mutate(P7_codificado = case_when(
    P7 == "Não" ~ 9, 
    P7 == "2.0" ~ 2))

#3. Criação do banco final 
perfil21codificado <- perfil21testecodf %>%
  select(NUSP, P1, P2_codificado, P2_outra, P3a_codificado, P3b_codificado, P4a_codificado,
         P4b_codificado, P4b_outra, P5_codificado, P5_outra, P6_codificado, P7_codificado)
view(perfil21codificado)

#3.1 Exportação do banco final em excel 
write.csv(perfil21codificado, file = "perfil21codificado.csv")
