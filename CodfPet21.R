#Codificação banco 2021 PET

#Importação do banco
library(readxl)
perfil21 <- read_excel("Pesquisa.Ingressantes.2021.xlsx")
View(Pesquisa_Ingressantes_2021)

#Selecionando as variáveis 
library(tidyverse)
perfil21cod <- perfil21 %>%
  select("P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Consumo de tabaco", 
         "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Consumo de bebidas alcoólicas",
         "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Consumo de drogas ilícitas", 
         "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Uso de medicamentos industrializados sem prescrição", 
         "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Uso de medicamentos industrializados prescritos", 
         "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Uso de medicamentos naturais", 
         "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Quantidade da alimentação", 
         "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Qualidade da alimentação", 
         "P53a. Recebeu atendimento médico de urgência",
         "P53b. Realizou consulta médica", 
         "P53c. Realizou consulta odontológica", 
         "P53d. Realizou sessão de terapia", 
         "P54. Durante o período de distanciamento, com que frequência você se sentiu triste?",
         "P55. Você já teve acesso a algum serviço de saúde mental (atendimento psicológico, psicoterápico, etc.)?",
         "P56. Você deu início a este atendimento durante o período de distanciamento social?",
         "P57. Você conhece algum serviço de assistência psicológica oferecido pela USP?",
         "P58. Você já procurou algum desses serviços de assistência psicológica oferecidos pela USP?",
         "P58a. Qual(is) serviços de assistência psicológica oferecidos pela USP você procurou?",
         "P59. Você realizou algum exame de detecção de infecção por Sars-CoV-2 2 (exame de Covid)?",
         "P60. E o resultado de algum destes exames foi positivo?",
         "P61. Algum familiar ou pessoa próxima teve Covid-19?",
         "P62. Qual a sua relação com estas pessoas? (MARQUE MAIS DE UMA ALTERNATIVA SE NECESSÁRIO)",
         "P63. Alguns desses familiares/pessoas próximas chegou a falecer em decorrência da doença?",
         "P64. Qual a sua relação com estas pessoas? (MARQUE MAIS DE UMA ALTERNATIVA SE NECESSÁRIO)",
         "P65. Durante o período de distanciamento, a sua renda pessoal:",
         "P66. Durante o período de distanciamento, a renda da sua família:",
         "P67. Você exerceu algum tipo de atividade remunerada (trabalho) nos últimos 60 dias?",
         "P68. Quantas horas em média por semana você dedicou a essas atividades?",
         "P69. Desde o início do período de distanciamento, em comparação ao período pré-pandemia, você considera que a sua carga nesse trabalho:",
         "P70. Você considera que esse trabalho vem atrapalhando seu rendimento acadêmico?",
         "P71. Você exerceu no seu domicílio algum tipo de atividade relacionada ao trabalho doméstico e/ou ao cuidado de pessoas (crianças, enfermos, idosos, etc.) nos últimos 60 dias?", 
         "P72. Quantas horas em média por semana você dedicou a estas atividades?", 
         "P73. Tendo em mente o domicílio em que você reside, desde o início do período de distanciamento, em comparação ao período pré-pandemia, você considera que a sua carga de trabalho doméstico e/ou cuidado com pessoas (crianças, enfermos, idosos, etc.):", 
         "P74. Você considera que o trabalho doméstico e/ou de cuidado de pessoas (crianças, enfermos, idosos, etc.) que você exerce no seu domicílio vem atrapalhando seu rendimento acadêmico?", 
         "P75. Com relação ao tempo que você gasta nas atividades do dia-a-dia, você considera que:")

#P.54 
library(dplyr)
perfil21cod <- perfil21cod %>%
  rename(P.54 = "P54. Durante o período de distanciamento, com que frequência você se sentiu triste?") %>%
  mutate(P.54 = case_when(
    P.54 == "Nunca" ~ 1, 
    P.54 == "Raramente" ~ 2, 
    P.54 == "Algumas vezes" ~ 3, 
    P.54 == "Frequentemente" ~ 4, 
    P.54 == "Sempre ou quase sempre" ~ 5))

#P.55 
perfil21cod <- perfil21cod %>%
  rename(P.55 = "P55. Você já teve acesso a algum serviço de saúde mental (atendimento psicológico, psicoterápico, etc.)?") %>%
  mutate(P.55 = case_when(
    P.55 == "Sim, privado" ~ 1,
    P.55 == "Sim, público" ~ 2, 
    P.55 == "Sim, privado e público" ~ 3, 
    P.55 == "Não" ~ 4))

#P.56
perfil21cod <- perfil21cod %>%
  rename(P.56 = "P56. Você deu início a este atendimento durante o período de distanciamento social?") %>%
  mutate(P.56 = case_when(
    P.56 == "Sim" ~ 1, 
    P.56 == "Não" ~ 2))

#P.57 
perfil21cod <- perfil21cod %>%
  rename(P.57 = "P57. Você conhece algum serviço de assistência psicológica oferecido pela USP?") %>%
  mutate(P.57_qual = ifelse(P.57 == "Não", "", P.57),  P.57 = ifelse(P.57 == "Não", 2, 1))

#P.58 
perfil21cod <- perfil21cod %>%
  rename(P.58 = "P58. Você já procurou algum desses serviços de assistência psicológica oferecidos pela USP?") %>%
  mutate(P.58 = case_when(
    P.58 == "Sim, e fui/sou atendido" ~ 1,
    P.58 == "Sim, mas não consegui atendimento" ~ 2, 
    P.58 == "Sim, mas decidi interromper o atendimento" ~ 3, 
    P.58 == "Não" ~ 4))

#P.58a 
perfil21cod <- perfil21cod %>%
  rename(P.58a = "P58a. Qual(is) serviços de assistência psicológica oferecidos pela USP você procurou?")
