
library(tidyverse)
library(ggcpesrthemes)

fin <- read.csv("fr-esr-operateurs-indicateurs-financiers.csv",sep=";",quote='"') %>%
  mutate(exercice = as.factor(exercice)) %>%
  mutate(groupe = case_when(
    startsWith(groupe,"université") ~ "université",
    groupe == "école d'ingénieurs" ~ "écoles d'ingénieurs",
    groupe == "communauté d'universités et établissements" ~ "COMUE",
    groupe == "autres établissements d'enseignement et de recherche" ~ "autres",
    str_length(groupe) == 0 ~ "autres",
    TRUE ~ groupe
  )) %>%
  mutate(etablissement = case_when(
    str_length(etablissement) == 0 ~ uai...identifiant,
    TRUE ~ etablissement
  )) %>%
  mutate(
    groupe = as.factor(groupe),
    etablissement = as.factor(etablissement))

etiquettes <- read.csv("etiquettes.csv") %>%
  mutate(étiquettes = factor(case_when(
    IDEx ~ "IDEx",
    ISITE ~ "ISITE",
    TRUE ~ "NINI"),
    levels=c("IDEx","ISITE","NINI","Non Université"))
  ) 


anr <- fin %>% 
  group_by(exercice) %>%
  transmute(
    UAI = as.character(uai...identifiant),
    etablissement = etablissement,
    groupe = groupe,
    exercice = as.numeric(as.character(exercice)),
    SCSP = Produits.de.fonctionnement.encaissables - Ressources.propres,
    ANR.PIA = ANR.investissements.d.avenir,
    ANR.hors.PIA = ANR.hors.investissements.d.avenir,
    Autres.RP = Ressources.propres - ANR.PIA - ANR.hors.PIA,
    PFE = Produits.de.fonctionnement.encaissables,
    rang = rank(desc(ANR.PIA))
  ) %>%
  ungroup() %>%
  #left_join(ens) %>%
  #left_join(etab) %>%
  left_join(kpiESR::esr %>% transmute(
    UAI = as.character(UAI),
    exercice=as.numeric(as.character(Rentrée))+1,
    titulaires = kpi.ENS.S.titulaires,
    étudiants = kpi.ETU.P.effectif,
    sigle = case_when(
      Sigle!="" ~ Sigle,
      TRUE ~ gsub("Université (de )?","",Libellé))
  ) ) %>%
  left_join(etiquettes %>% select(UAI,étiquettes)) %>%
  mutate(
    exercice = factor(exercice),
    UAI = factor(UAI)) %>%
  mutate(
    SCSP = ifelse(SCSP < 0, NA, SCSP)
  ) %>%
  group_by(UAI) %>%
  fill(sigle) %>%
  ungroup() %>%
  mutate(groupe = as.factor(ifelse(etablissement == "Université Paris-Dauphine", "université", as.character(groupe)))) %>%
  mutate(étiquettes= factor(case_when(
    groupe != "université" ~ "Non université",
    is.na(étiquettes) ~ "NINI",
    TRUE ~ as.character(étiquettes)),
    levels=c("IDEx","ISITE","NINI","Non université")))

anr.pivot <- anr %>%
  filter(!is.na(ANR.PIA)) %>%
  pivot_longer(c(SCSP,ANR.PIA,ANR.hors.PIA,Autres.RP,PFE), names_to = "Indicateur", values_to = "Valeur") %>%
  mutate(Indicateur = factor(Indicateur,
                             levels=c("ANR.PIA","ANR.hors.PIA","Autres.RP","SCSP","PFE"),
                             labels=c("ANR PIA","ANR hors PIA","Autres RP","SCSP","PFE")))

anr.pivot.2 <- anr %>%
  filter(!is.na(ANR.PIA)) %>%
  mutate(Autres.RP = Autres.RP + ANR.hors.PIA) %>%
  select(-ANR.hors.PIA) %>%
  pivot_longer(c(SCSP,ANR.PIA,Autres.RP,PFE), names_to = "Indicateur", values_to = "Valeur") %>%
  mutate(Indicateur = factor(Indicateur,
                             levels=c("ANR.PIA","Autres.RP","SCSP","PFE"),
                             labels=c("ANR PIA","Autres RP","SCSP","PFE")))

