## Carrega pacotes --------------------------------------

# devtools::install_github(repo = "ajdamico/lodown")
# devtools::install_github("ajdamico/lodown" , dependencies = TRUE)

library(lodown)
library(tidyverse)
library(stringr)
library(survey)
library(srvyr)
library(janitor)
library(data.table)

options(scipen = 999)

## Carrega svydesign modificado --------------------------

setwd("C:/Users/ahpvi/Documents/doutorado/artigos-prelo/educacao-mercado")
source("./script/svydesign_modified.R")

## Carrega dados (LER APENAS UMA POR VEZ) ----------------

c10 <- readRDS("./dados/tb-censo2010-completo.rds")

c00 <- fread("./dados/tb-censo2000-completo.csv", 
             sep = ",", 
             # nrows = 10, 
             select = c('V0102', 'AREAP', 'P001', 'V0300', 'V0401', 
                        'V4752', 'V0408', 'V4452', 'V0447', 'V4513', 'V4354'))

## Define desenho amostral do CENSO 2010 -----------------

# Define desenho amostral "padrao"

c10 <- c10 %>% 
      inner_join(dplyr::summarise(dplyr::group_by(c10, v0011), fpc = sum(v0010))) %>% 
      dplyr::mutate(one = 1)

c10_design <- svydesign_modified(ids = ~v0300,
                                 strata = ~v0011,
                                 data = c10,
                                 weights = ~v0010,
                                 fpc = ~fpc)

save(c10_design, file = "tb-censo2010-design.RData")
rm(c10); gc()

# O meu PC nao tem MEMORIA SUFICIENTE para executar os dois passos seguintes

# censo_wgts <-
#       survey::bootweights(
#             strata = censo_pt1$v0011,
#             psu = censo_pt1$v0300,
#             replicates = 50,
#             fpc = censo_pt1$fpc)

# save(censo_wgts, file = "censo2010_wgts.RData")

# gc()
# 
# c10_design <-
#       survey::svrepdesign(
#             weight = ~ v0010,
#             repweights = censo_wgts$repweights,
#             type = "bootstrap",
#             combined.weights = FALSE ,
#             scale = 0.01268176, # censo_wgts$scale,
#             rscales = rep(1, 80), # censo_wgts$rscales,
#             data = censo_pt1
#       )
# 
# gc()

# saveRDS(censo_df, "tb-censo2010-pt1.rds")
# saveRDS(censo_df, "tb-censo2010-pt2.rds")
# write.csv2(censo_df, "tb-censo2010-pt1.csv", row.names = F, quote = F)
# saveRDS(c10_final, "tb-censo2010-educacao.rds")

rm(dom_stru, file_names, pes_stru, columns_to_import, these_columns_to_import, 
   this_state, this_variable, censo_cat)

## Modifica colunas do CENSO 2010 -------------------------

c10_design <- srvyr::as_survey_design(c10_design)

c10_design <- dplyr::filter(c10_design, v6352 %in% c(140, 142, 143, 144, 145, 146))

c10_design <- c10_design %>% 
      dplyr::filter(!v0606 %in% c(3, 5, 9)) %>% 
      dplyr::filter(!is.na(v0606)) %>% 
      dplyr::rename_all(.funs = tolower) %>% 
      dplyr::select(v0011, v0010, v0300, one, uf = v0001,
                    gender = v0601, race = v0606, age = v6036,
                    occup = v6461, occup_pos = v0648,
                    income_main_job = v6513, field_study = v6352) %>% 
      dplyr::mutate(
            field_study = recode(field_study,
                                 `140` = "General Courses",
                                 `142` = "Education Sciences",
                                 `143` = "Early Child. Edu.",
                                 `144` = "Basic Education",
                                 `145` = "Specific Subjects",
                                 `146` = "Professional Subjects"),
            field_study = factor(field_study),
            race = case_when(race == 2 ~ "Black",
                             race == 4 ~ "Pardo",
                             race == 1 ~ "White"),
            race = factor(race),
            gender = case_when(gender == 2 ~ "Woman", 
                               gender == 1 ~ "Man"),
            gender = factor(gender)
      )

save(c10_design, file = "./dados/tb-censo2010-design-final.RData")

## Define desenho amostral do CENSO 2000 -------------------

# Calcula FPC

c00 <- c00 %>% 
      inner_join(dplyr::summarise(dplyr::group_by(c00, AREAP), fpc = sum(P001))) %>% 
      dplyr::mutate(one = 1)

# Define desenho amostral "padrao"

# c00_design <- c00 %>% 
#       as_survey_design(
#             ids = ~V0300,
#             strata = ~AREAP,
#             data = c00,
#             weights = ~P001,
#             fpc = ~fpc, 
#             out = 
#        )

c00_design <- svydesign_modified(ids = ~V0300,
                                 strata = ~AREAP,
                                 data = c00,
                                 weights = ~P001,
                                 fpc = ~fpc)

save(c00_design, file = "./dados/tb-censo2000-design.RData")
rm(c00, fpc)

## Modifica colunas do CENSO 2000 -------------------------

load("./dados/tb-censo2000-design.RData")

c00_design <- srvyr::as_survey_design(c00_design)

c00_design <- dplyr::filter(c00_design, V4354 %in% c(140, 142, 143, 144, 145, 146))

c00_design <- c00_design %>% 
      dplyr::filter(!V0408 %in% c(3, 5, 9)) %>% 
      dplyr::filter(!is.na(V0408)) %>% 
      dplyr::rename_all(.funs = tolower) %>% 
      dplyr::select(areap, p001, v0300, one, uf = v0102,
                    gender = v0401, race = v0408, age = v4752,
                    occup = v4452, occup_pos = v0447,
                    income_main_job = v4513, field_study = v4354) %>% 
      dplyr::mutate(
            field_study = recode(field_study,
                                 `140` = "General Courses",
                                 `142` = "Education Sciences",
                                 `143` = "Early Child. Edu.",
                                 `144` = "Basic Education",
                                 `145` = "Specific Subjects",
                                 `146` = "Professional Subjects"),
            field_study = factor(field_study),
            race = case_when(race == 2 ~ "Black",
                             race == 4 ~ "Pardo",
                             race == 1 ~ "White"),
            race = factor(race),
            gender = case_when(gender == 2 ~ "Woman", 
                               gender == 1 ~ "Man"),
            gender = factor(gender)
      )

save(c00_design, file = "./dados/tb-censo2000-design-final.RData")
