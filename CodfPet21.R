#Codificação banco 2021 PET

#Importação do banco
library(readxl)
perfil21 <- read_excel("Pesquisa.Ingressantes.2021.xlsx")

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

#P.52a
library(dplyr)
perfil21cod <- perfil21cod %>%
  rename(P.52a = "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Consumo de tabaco") %>%
  mutate(P.52a = case_when(
    P.52a == "Aumentou muito" ~ 1, 
    P.52a == "Aumentou um pouco" ~ 2, 
    P.52a == "Manteve-se igual" ~ 3, 
    P.52a == "Diminuiu um pouco" ~ 4, 
    P.52a == "Diminuiu muito" ~ 5, 
    P.52a == "Não se aplica" ~ 88))

#P.52b
perfil21cod <- perfil21cod %>%
  rename(P.52b = "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Consumo de bebidas alcoólicas") %>%
  mutate(P.52b = case_when(
    P.52b == "Aumentou muito" ~ 1, 
    P.52b == "Aumentou um pouco" ~ 2, 
    P.52b == "Manteve-se igual" ~ 3, 
    P.52b == "Diminuiu um pouco" ~ 4, 
    P.52b == "Diminuiu muito" ~ 5, 
    P.52b == "Não se aplica" ~ 88))

#P.52c
perfil21cod <- perfil21cod %>%
  rename(P.52c = "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Consumo de drogas ilícitas") %>%
  mutate(P.52c = case_when(
    P.52c == "Aumentou muito" ~ 1, 
    P.52c == "Aumentou um pouco" ~ 2, 
    P.52c == "Manteve-se igual" ~ 3, 
    P.52c == "Diminuiu um pouco" ~ 4, 
    P.52c == "Diminuiu muito" ~ 5, 
    P.52c == "Não se aplica" ~ 88))

#P.52d
perfil21cod <- perfil21cod %>%
  rename(P.52d = "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Uso de medicamentos industrializados sem prescrição") %>%
  mutate(P.52d = case_when(
    P.52d == "Aumentou muito" ~ 1, 
    P.52d == "Aumentou um pouco" ~ 2, 
    P.52d == "Manteve-se igual" ~ 3, 
    P.52d == "Diminuiu um pouco" ~ 4, 
    P.52d == "Diminuiu muito" ~ 5, 
    P.52d == "Não se aplica" ~ 88))

#P.52e
perfil21cod <- perfil21cod %>%
  rename(P.52e = "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Uso de medicamentos industrializados prescritos") %>%
  mutate(P.52e = case_when(
    P.52e == "Aumentou muito" ~ 1, 
    P.52e == "Aumentou um pouco" ~ 2, 
    P.52e == "Manteve-se igual" ~ 3, 
    P.52e == "Diminuiu um pouco" ~ 4, 
    P.52e == "Diminuiu muito" ~ 5, 
    P.52e == "Não se aplica" ~ 88))

#P.52f
perfil21cod <- perfil21cod %>%
  rename(P.52f = "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Uso de medicamentos naturais") %>%
  mutate(P.52f = case_when(
    P.52f == "Aumentou muito" ~ 1, 
    P.52f == "Aumentou um pouco" ~ 2, 
    P.52f == "Manteve-se igual" ~ 3, 
    P.52f == "Diminuiu um pouco" ~ 4, 
    P.52f == "Diminuiu muito" ~ 5, 
    P.52f == "Não se aplica" ~ 88))

#P.52g
perfil21cod <- perfil21cod %>%
  rename(P.52g = "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Quantidade da alimentação") %>%
  mutate(P.52g = case_when(
    P.52g == "Aumentou muito" ~ 1, 
    P.52g == "Aumentou um pouco" ~ 2, 
    P.52g == "Manteve-se igual" ~ 3, 
    P.52g == "Diminuiu um pouco" ~ 4, 
    P.52g == "Diminuiu muito" ~ 5, 
    P.52g == "Não se aplica" ~ 88))

#P.52h
perfil21cod <- perfil21cod %>%
  rename(P.52h = "P52. Comparando a sua rotina antes e durante o período de distanciamento, o que você diria a respeito dos seguintes hábitos e práticas:: Qualidade da alimentação") %>%
  mutate(P.52h = case_when(
    P.52h == "Aumentou muito" ~ 1, 
    P.52h == "Aumentou um pouco" ~ 2, 
    P.52h == "Manteve-se igual" ~ 3, 
    P.52h == "Diminuiu um pouco" ~ 4, 
    P.52h == "Diminuiu muito" ~ 5, 
    P.52h == "Não se aplica" ~ 88))

#P.53a
perfil21cod <- perfil21cod %>%
  rename(P.53a = "P53a. Recebeu atendimento médico de urgência") %>%
  mutate(
    P.53a_vezes = ifelse(P.53a == "Não", "", P.53a),  
    P.53a = ifelse(P.53a == "Não", 2, 1))

#P.53b
perfil21cod <- perfil21cod %>%
  rename(P.53b = "P53b. Realizou consulta médica") %>%
  mutate(
    P.53b_vezes = ifelse(P.53b == "Não", "", P.53b),  
    P.53b = ifelse(P.53b == "Não", 2, 1))

#P.53c
perfil21cod <- perfil21cod %>%
  rename(P.53c = "P53c. Realizou consulta odontológica") %>%
  mutate(
    P.53c_vezes = ifelse(P.53c == "Não", "", P.53c),  
    P.53c = ifelse(P.53c == "Não", 2, 1))

#P.53d
perfil21cod <- perfil21cod %>%
  rename(P.53d = "P53d. Realizou sessão de terapia") %>%
  mutate(
    P.53d_vezes = ifelse(P.53d == "Não", "", P.53d),  
    P.53d = ifelse(P.53d == "Não", 2, 1))

#P.54 
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

#P.59
perfil21cod <- perfil21cod %>%
  rename(P.59 = "P59. Você realizou algum exame de detecção de infecção por Sars-CoV-2 2 (exame de Covid)?") %>%
  mutate(P.59 = case_when(
    P.59 == "Sim, mais de uma vez" ~ 1, 
    P.59 == "Sim, uma vez" ~ 2, 
    P.59 == "Não" ~ 3))

#P.60
perfil21cod <- perfil21cod %>%
  rename(P.60 = "P60. E o resultado de algum destes exames foi positivo?") %>%
  mutate(P.60 = case_when(
    P.60 == "Sim" ~ 1, 
    P.60 == "Não" ~ 2))

perfil21cod <- perfil21cod %>%
  mutate(P.60 = ifelse(P.59 == 3 & is.na(P.60), 88, P.60))

#P.61
perfil21cod <- perfil21cod %>%
  rename(P.61 = "P61. Algum familiar ou pessoa próxima teve Covid-19?") %>%
  mutate(P.61 = case_when(
    P.61 == "Sim" ~ 1, 
    P.61 == "Não" ~ 2))

#P.62 
perfil21cod <- perfil21cod %>%
  rename(P.62 = "P62. Qual a sua relação com estas pessoas? (MARQUE MAIS DE UMA ALTERNATIVA SE NECESSÁRIO)") %>%
  mutate(P.62 = ifelse(P.61 == 2, 88, P.62),  
         P.62 = ifelse(is.na(P.62), 99, P.62))

perfil21cod <- perfil21cod %>%
  separate(P.62, into = paste0("P.62_", 1:10), sep = ";")

perfil21cod <- perfil21cod %>%
  mutate(
    P.62_2 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_2), 88, P.62_2), ifelse(is.na(P.62_2), 2, P.62_2)),
    P.62_3 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_3), 88, P.62_3), ifelse(is.na(P.62_3), 2, P.62_3)),
    P.62_4 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_4), 88, P.62_4), ifelse(is.na(P.62_4), 2, P.62_4)),
    P.62_5 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_5), 88, P.62_5), ifelse(is.na(P.62_5), 2, P.62_5)),
    P.62_6 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_6), 88, P.62_6), ifelse(is.na(P.62_6), 2, P.62_6)),
    P.62_7 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_7), 88, P.62_7), ifelse(is.na(P.62_7), 2, P.62_7)),
    P.62_8 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_8), 88, P.62_8), ifelse(is.na(P.62_8), 2, P.62_8)),
    P.62_9 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_9), 88, P.62_9), ifelse(is.na(P.62_9), 2, P.62_9)),
    P.62_10 = ifelse(P.62_1 == 88, ifelse(is.na(P.62_10), 88, P.62_10), ifelse(is.na(P.62_10), 2, P.62_10))
  )

alternativas <- c(
  "Companheiro/companheira (com quem você compartilha o domicílio)",
  "Companheiro/companheira (com quem você NÃO compartilha o domicílio)",
  "Filho/filha/enteado/enteada",
  "Pai/mãe/padrasto/madrasta",
  "Irmã/irmão",
  "Avô/avó",
  "Sogro/sogra",
  "Tio/tia",
  "Primo/prima",
  "Amigo/amiga"
)

perfil21cod <- perfil21cod %>%
  mutate(across(starts_with("P.62"), ~ifelse(. %in% alternativas, 1, .)))

#P.63 
perfil21cod <- perfil21cod %>%
  rename(P.63 = "P63. Alguns desses familiares/pessoas próximas chegou a falecer em decorrência da doença?") %>%
  mutate(P.63 = case_when(
    P.63 == "Sim" ~ 1, 
    P.63 == "Não" ~ 2))

#P.64 
perfil21cod <- perfil21cod %>%
  rename(P.64 = "P64. Qual a sua relação com estas pessoas? (MARQUE MAIS DE UMA ALTERNATIVA SE NECESSÁRIO)") %>%
  mutate(P.64 = ifelse(P.63 == 2, 88, P.64),  
         P.64 = ifelse(is.na(P.64), 99, P.64))

perfil21cod <- perfil21cod %>%
  separate(P.64, into = paste0("P.64_", 1:10), sep = ";")

perfil21cod <- perfil21cod %>%
  mutate(
    P.64_2 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_2), 88, P.64_2), ifelse(is.na(P.64_2), 2, P.64_2)),
    P.64_3 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_3), 88, P.64_3), ifelse(is.na(P.64_3), 2, P.64_3)),
    P.64_4 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_4), 88, P.64_4), ifelse(is.na(P.64_4), 2, P.64_4)),
    P.64_5 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_5), 88, P.64_5), ifelse(is.na(P.64_5), 2, P.64_5)),
    P.64_6 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_6), 88, P.64_6), ifelse(is.na(P.64_6), 2, P.64_6)),
    P.64_7 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_7), 88, P.64_7), ifelse(is.na(P.64_7), 2, P.64_7)),
    P.64_8 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_8), 88, P.64_8), ifelse(is.na(P.64_8), 2, P.64_8)),
    P.64_9 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_9), 88, P.64_9), ifelse(is.na(P.64_9), 2, P.64_9)),
    P.64_10 = ifelse(P.64_1 == 88, ifelse(is.na(P.64_10), 88, P.64_10), ifelse(is.na(P.64_10), 2, P.64_10))
  )

alternativas <- c(
  "Companheiro/companheira (com quem você compartilha o domicílio)",
  "Companheiro/companheira (com quem você NÃO compartilha o domicílio)",
  "Filho/filha/enteado/enteada",
  "Pai/mãe/padrasto/madrasta",
  "Irmã/irmão",
  "Avô/avó",
  "Sogro/sogra",
  "Tio/tia",
  "Primo/prima",
  "Amigo/amiga"
)

perfil21cod <- perfil21cod %>%
  mutate(across(starts_with("P.64"), ~ifelse(. %in% alternativas, 1, .)))

#P.65 
perfil21cod <- perfil21cod %>%
  rename(P.65 = "P65. Durante o período de distanciamento, a sua renda pessoal:") %>%
  mutate(P.65 = case_when(
    P.65 == "Aumentou" ~ 1,
    P.65 == "Continuou a mesma" ~ 2, 
    P.65 == "Diminuiu" ~ 3, 
    P.65 == "Perdi totalmente minha renda" ~ 4, 
    P.65 == "Não tinha nenhuma renda e continuei sem renda" ~ 5))

#P.66 
perfil21cod <- perfil21cod %>%
  rename(P.66 = "P66. Durante o período de distanciamento, a renda da sua família:") %>%
  mutate(P.66 = case_when(
    P.66 == "Aumentou" ~ 1,
    P.66 == "Continuou a mesma" ~ 2,
    P.66 == "Diminuiu" ~ 3,
    P.66 == "Perdemos totalmente nossa renda" ~ 4))

#P.67 
perfil21cod <- perfil21cod %>%
  rename(P.67 = "P67. Você exerceu algum tipo de atividade remunerada (trabalho) nos últimos 60 dias?") %>%
  mutate(P.67 = case_when(
    P.67 == "Sim" ~ 1, 
    P.67 == "Não" ~ 2))

#P.68 - mantém pois é aberta 
perfil21cod <- perfil21cod %>%
  rename(P.68 = "P68. Quantas horas em média por semana você dedicou a essas atividades?")

#P.69 
perfil21cod <- perfil21cod %>%
  rename(P.69 = "P69. Desde o início do período de distanciamento, em comparação ao período pré-pandemia, você considera que a sua carga nesse trabalho:") %>%
  mutate(P.69 = case_when(
    P.69 == "Aumentou" ~ 1,
    P.69 == "Diminuiu" ~ 2, 
    P.69 == "Manteve-se a mesma" ~ 3))

perfil21cod <- perfil21cod %>%
  mutate(P.69 = ifelse(P.67 == 2 & is.na(P.69), 88, P.69))

#P.70
perfil21cod <- perfil21cod %>%
  rename(P.70 = "P70. Você considera que esse trabalho vem atrapalhando seu rendimento acadêmico?") %>%
  mutate(P.70 = case_when(
    P.70 == "Sim, atrapalha consideravelmente o meu rendimento acadêmico" ~ 1, 
    P.70 == "Sim, atrapalha um pouco o meu rendimento acadêmico" ~ 2, 
    P.70 == "Não atrapalha o meu rendimento acadêmico" ~ 3))

perfil21cod <- perfil21cod %>%
  mutate(P.70 = ifelse(P.67 == 2 & is.na(P.70), 88, P.70))

#P.71
perfil21cod <- perfil21cod %>%
  rename(P.71 = "P71. Você exerceu no seu domicílio algum tipo de atividade relacionada ao trabalho doméstico e/ou ao cuidado de pessoas (crianças, enfermos, idosos, etc.) nos últimos 60 dias?") %>%
  mutate(P.71 = case_when(
    P.71 == "Sim" ~ 1, 
    P.71 == "Não" ~ 2))

#P.72 - mantém pois é aberta
perfil21cod <- perfil21cod %>%
  rename(P.72 = "P72. Quantas horas em média por semana você dedicou a estas atividades?")

#P.73
perfil21cod <- perfil21cod %>%
  rename(P.73 = "P73. Tendo em mente o domicílio em que você reside, desde o início do período de distanciamento, em comparação ao período pré-pandemia, você considera que a sua carga de trabalho doméstico e/ou cuidado com pessoas (crianças, enfermos, idosos, etc.):") %>%
  mutate(P.73 = case_when(
    P.73 == "Aumentou" ~ 1, 
    P.73 == "Diminuiu" ~ 2, 
    P.73 == "Manteve-se a mesma" ~ 3))

perfil21cod <- perfil21cod %>%
  mutate(P.73 = ifelse(P.71 == 2 & is.na(P.73), 88, P.73))

#P.74 
perfil21cod <- perfil21cod %>%
  rename(P.74 = "P74. Você considera que o trabalho doméstico e/ou de cuidado de pessoas (crianças, enfermos, idosos, etc.) que você exerce no seu domicílio vem atrapalhando seu rendimento acadêmico?") %>%
  mutate(P.74 = case_when(
    P.74 == "Sim, atrapalha consideravelmente o meu rendimento acadêmico" ~ 1,
    P.74 == "Sim, atrapalha um pouco o meu rendimento acadêmico" ~ 2, 
    P.74 == "Não atrapalha o meu rendimento acadêmico" ~ 3))

perfil21cod <- perfil21cod %>%
  mutate(P.74 = ifelse(P.71 == 2 & is.na(P.74), 88, P.74))

#P.75 
perfil21cod <- perfil21cod %>%
  rename(P.75 = "P75. Com relação ao tempo que você gasta nas atividades do dia-a-dia, você considera que:") %>%
  mutate(P.75 = case_when(
    P.75 == "A maior parte do tempo é dedicado a atividades relacionadas ao estudo" ~ 1, 
    P.75 == "A maior parte do tempo é dedicado a atividades relacionadas ao trabalho" ~ 2,
    P.75 == "A maior parte do tempo é dedicado a atividades relacionadas ao trabalho doméstico ou de cuidado" ~ 3,
    P.75 == "A maior parte do tempo é dedicado a atividades de lazer" ~ 4, 
    P.75 == "O tempo é equilibrado entre estudo, trabalho e lazer" ~ 5))

#N/A
perfil21cod <- perfil21cod %>%
  mutate_at(vars(-matches("P.58|P.68|P.72")), ~ifelse(is.na(.), 99, .))

#Organizando o banco de dados
perfil21cod <- perfil21cod %>%
  select(P.52a, P.52b, P.52c, P.52d, P.52e, P.52f, P.52g, P.52h, P.53a, P.53b, 
         P.53c, P.53d, P.54, P.55, P.56, P.57, P.57_qual, P.58, P.58a, P.59, 
         P.60, P.61, P.62_1, P.62_2, P.62_3, P.62_4, P.62_5, P.62_6, P.62_7,
         P.62_8, P.62_9, P.62_10, P.63, P.64_1, P.64_2, P.64_3, P.64_4, P.64_5,
         P.64_6, P.64_7, P.64_8, P.64_9, P.64_10, P.65, P.66, P.67, P.68, P.69,
         P.70, P.71, P.72, P.73, P.74, P.75)

#Exportando o banco de dados
write.csv(perfil21cod, file = "perfil21cod.csv")
