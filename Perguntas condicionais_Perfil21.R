#Script perguntas condicionais perfil2021 

#p8b
  mutate(P8b = ifelse(P8a == 2, p8a == 3 & is.na(P8b), 88, P8b))

#p10 
  mutate(P10 = ifelse(P9 == 0 & is.na(P10), 88, P10))

#p12 
mutate(P12 = ifelse(P9 == 1 & is.na(P12), 88, P12))

#p18 
mutate(P18 = ifelse(P17 == 2, P17 == 3, P17 == 4, P17 == 5, P17 == 6, 
                    P17 == 7 & is.na(P18), 88, P18))

#p20b
mutate(P20b = ifelse(P20a == 1 & is.na(P20b), 88, P20b))

#p22 
mutate(P22 = ifelse(P21 == 2, P21 == 3, P21 == 4, P21 == 5, 
                    P21 == 6 & is.na(P22), 88, P22))

#p31b 
mutate(P31b = ifelse(P31a == 0 & is.na(P31b), 88, P31b))

#p31c 
mutate(P31c = ifelse(P31a == 1 & is.na(P31c), 88, P31c))

#p39 
mutate(P39 = ifelse(P38 == 0 & is.na(P39), 88, P39))

#p40 
mutate(P40 = ifelse(P39 == 2, P39 == 3 & is.na(P40), 88, P40))

#p42 
mutate(P42 = ifelse(P41 == 0 & is.na(P42), 88, P42))

#p43 
mutate(P43 = ifelse(P42 == 2, P42 == 3 & is.na(P43), 88, P43))

#p51a
mutate(P51a = ifelse(P51 == 0 & is.na(P51a), 88, P51a))

#p51b 
mutate(P51b = ifelse(P51 == 0 & is.na(P51b), 88, P51b))

#p60
mutate(P.60 = ifelse(P.59 == 3 & is.na(P.60), 88, P.60))

#p.62 
perfil21cod <- perfil21cod %>% #precisa trocar pelo nome q vc colocou no banco 
  rename(P.62 = "P62. Qual a sua relação com estas pessoas? (MARQUE MAIS DE UMA ALTERNATIVA SE NECESSÁRIO)") %>%
  mutate(P.62 = ifelse(P.61 == 0, 88, P.62),  
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

#p63 
mutate(P.63 = ifelse(P.61 == 0 & is.na(P.63), 88, P.63))

#p67 
perfil21cod <- perfil21cod %>% #precisa mudar pro nome que vc colou no banco 
  rename(P.64 = "P64. Qual a sua relação com estas pessoas? (MARQUE MAIS DE UMA ALTERNATIVA SE NECESSÁRIO)") %>%
  mutate(P.64 = ifelse(P.63 == 0, 88, P.64),  
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

#p69
mutate(P.69 = ifelse(P.67 == 0 & is.na(P.69), 88, P.69))

#p70
mutate(P.70 = ifelse(P.67 == 0 & is.na(P.70), 88, P.70))

#p73
mutate(P.73 = ifelse(P.71 == 0 & is.na(P.73), 88, P.73))

#p74
mutate(P.74 = ifelse(P.71 == 0 & is.na(P.74), 88, P.74))

#N/A - pensei em passar esse código ao final do script, mas precisa adicionar as outras perguntas abertas
perfil21cod <- perfil21cod %>%
  mutate_at(vars(-matches("P.58|P.68|P.72")), ~ifelse(is.na(.), 99, .))