## Carrega pacotes ---------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, stringr, reshape2, DescTools, janitor, 
               lodown, readr, haven, sqldf, survey, srvyr, logmult)
options(max.print = 1000, scipen = 999)

## Carrega dados -----------------------------

setwd("C:/Users/ahpvi/Documents/doutorado/artigos-prelo/education-stratification")
load("./dados/tb-censo2000-design-final.RData")
load("./dados/tb-censo2010-design-final.RData")

## Estatisticas descritivas ------------------

# Total de individuos na populacao

c00_design %>%
      summarize(n_total = survey_total(one, vartype = "ci"))

# Total de individuos por area de Educacao

c00_design %>%
      group_by(field_study) %>% 
      summarize(n_total = survey_total(one, vartype = "ci"))

# --> Remover "Early childhood" e "General Courses"? Comparar com 2010.

# % de individuos por area de Educacao

c00_design %>%
      group_by(field_study) %>% 
      summarize(prop_field = survey_mean())

# % de individuos por sexo

c00_design %>%
      group_by(gender) %>% 
      summarize(prop_gender = survey_mean())

# % de individuos por raca

c00_design %>%
      group_by(race) %>% 
      summarize(prop_race = survey_mean())

# % de individuos por area e raca

c00_design %>%
      group_by(field_study, race) %>% 
      summarize(prop = survey_mean())

c00_design %>%
      group_by(field_study, race) %>% 
      summarize(prop = survey_mean())

# Medias e medianas de rendimento do trabalho principal por area

c10_design %>% 
      group_by(field_study) %>% 
      summarize(mean_inc = survey_mean(income_main_job, na.rm = T))

c10_design %>% 
      group_by(field_study) %>% 
      summarize(median_inc = survey_median(income_main_job, na.rm = T))

#' Ordem decrescente de renda: 
#' General Courses > Professional > Specific > Early > Education > Basic

## Modelos log-lineares ----------------------

# Tabelas e modelos - Area de estudo e Genero

tab_fg00 <- c00_design %>% 
      filter(!field_study %in% c("Early Child. Edu.", "General Courses")) %>% 
      svytable(~field_study + gender, .)

tab_fg10 <- c10_design %>% 
      filter(!field_study %in% c("Early Child. Edu.", "General Courses")) %>% 
      svytable(~field_study + gender, .)

tab_gender <- rbind(tab_fg00, tab_fg10) %>% 
      cbind(year = c(rep(2000, 6), rep(2010, 6))) %>% 
      data.frame %>%
      rownames_to_column %>%
      dplyr::rename(field_study = rowname) %>% 
      melt(id.vars = c("year", "field_study")) %>% 
      filter(!value == 0) %>% 
      dplyr::rename(freq = value, gender = variable) %>% 
      dplyr::mutate(field_study = str_replace_all(field_study, ".1", "")) %>% 
      arrange(year) %>% 
      xtabs(freq ~ field_study+gender+year, .)

models <- list()

# Conditional independence
models$indep <- gnm(Freq ~ year + field_study + gender + field_study:year + gender:year, 
                    data = tab_gender, 
                    family = poisson, 
                    eliminate = field_study:year)

# Associacao constante
models$stable <- update(models$indep, ~ . + field_study:gender)

# UNIDIFF
models$unidiff <- unidiff(tab_gender, trace = TRUE, checkEstimability = FALSE)

# Estatisticas de ajuste dos modelos
models.summary(models)

## Figura X: modelo de associacao constante

boxOdds_gender <- exp(cbind(OR = coef(models$stable), confint(models$stable)))
boxOdds_gender %>% 
      data.frame %>% 
      rownames_to_column %>%
      slice(7:9) %>% 
      dplyr::rename(yaxis = rowname, odds = OR, cilow = `X2.5..`, cihigh = `X97.5..`) %>% 
      ggplot(aes(x = odds, y = yaxis)) +
      geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
      geom_errorbarh(aes(xmax = cihigh, xmin = cilow), size = .5, height = .2, color = "gray50") +
      geom_point(size = 2, color = "grey40") +
      theme_bw() +
      coord_trans(x = "log10") +
      ylab("") +
      xlab("Odds ratio (log scale)")

# Tabelas e modelos - Area de estudo e Raca

tab_fr00 <- c00_design %>% 
      filter(!field_study %in% c("Early Child. Edu.", "General Courses")) %>% 
      svytable(~field_study + race, .)

tab_fr10 <- c10_design %>% 
      filter(!field_study %in% c("Early Child. Edu.", "General Courses")) %>% 
      svytable(~field_study + race, .)

tab_race <- rbind(tab_fr00, tab_fr10) %>% 
      cbind(year = c(rep(2000, 6), rep(2010, 6))) %>% 
      data.frame %>%
      rownames_to_column %>%
      dplyr::rename(field_study = rowname) %>% 
      melt(id.vars = c("year", "field_study")) %>% 
      filter(!value == 0) %>% 
      dplyr::rename(freq = value, race = variable) %>% 
      dplyr::mutate(field_study = str_replace_all(field_study, ".1", "")) %>% 
      arrange(year) %>% 
      xtabs(freq ~ field_study+race+year, .)

models_race <- list()

# Conditional independence
models_race$indep <- gnm(Freq ~ year + field_study + race + field_study:year + race:year, 
                    data = tab_race, 
                    family = poisson, 
                    eliminate = field_study:year)

# Associacao constante
models_race$stable <- update(models_race$indep, ~ . + field_study:race)

# UNIDIFF
models_race$unidiff <- unidiff(tab_race, trace = TRUE, checkEstimability = FALSE)

# Estatisticas de ajuste dos modelos
models.summary(models_race)

boxOdds_race <- exp(cbind(OR = coef(models_race$stable), confint(models_race$stable)))
boxOdds_race %>% 
      data.frame %>% 
      rownames_to_column %>%
      slice(9:14) %>% 
      # glimpse
      dplyr::rename(yaxis = rowname, odds = OR, cilow = `X2.5..`, cihigh = `X97.5..`) %>% 
      ggplot(aes(x = odds, y = yaxis)) +
      geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
      geom_errorbarh(aes(xmax = cihigh, xmin = cilow), size = .5, height = .2, color = "gray50") +
      geom_point(size = 2, color = "grey40") +
      theme_bw() +
      coord_trans(x = "log10") +
      ylab("") +
      xlab("Odds ratio (log scale)")

#' Ordem decrescente de renda: 
#' General Courses > Professional > Specific > Early > Education > Basic

## 

## Referencias -------------------------------

#' https://www.dadosaleatorios.com.br/post/2014-06-13-trabalhando-com-amostras-complexas-no-r-usando-o-pacote-survey/
#' https://cran.r-project.org/web/packages/survey/vignettes/domain.pdf
#' https://rpubs.com/trjohns/survey-cluster
#' https://www.statmodel.com/download/usersguide/Chapter9.pdf
#' https://eventos.ibge.gov.br/downloads/smi2017/apresentacoes/minicursos/MC4%20_Parte2_Pedro%20Silva_Ricardo%20Cardoso_Sonia%20Oliveira.pdf
#' https://freakonometrics.hypotheses.org/tag/gnm
#' https://rstudio-pubs-static.s3.amazonaws.com/308591_6dff566d383946c881cb5b6a735a79fd.html

# GNM
#' https://github.com/nalimilan
#' https://data.library.virginia.edu/an-introduction-to-loglinear-models/
#' https://www.udemy.com/linear-regression-glms-and-gams-with-r/
#' https://www.heatherturner.net/index.html
#' https://www.zhrcourses.uzh.ch/en/fruehere-kurse/programm/datenanalyse/intro-nonlinear.html
#' https://www.r-project.org/conferences/useR-2009/tutorials/Turner.html
#' https://www.r-project.org/conferences/useR-2009/tutorials/gnmCourse-3up.pdf
#' https://github.com/hturner/gnm-day-course/blob/master/gnmDayCourse.R
#' http://www.jscarlton.net/post/2015-10-24VisualizingLogistic/
#' 
      