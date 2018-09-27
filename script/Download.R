## Carrega pacotes --------------------------------------

# devtools::install_github(repo = "ajdamico/lodown")
# devtools::install_github("ajdamico/lodown" , dependencies = TRUE)

library(lodown)
library(tidyverse)
library(stringr)
library(survey)
library(janitor)

options(scipen = 999)

## Baixa CENSO DEMOGRAFICO 2010  ------------------------

# examine all available CENSO microdata files

setwd("C:/Users/ahpvi/Documents/doutorado/artigos-prelo/educacao-mercado")

censo_cat <- get_catalog("censo", 
                         output_dir = file.path(path.expand("~"), "CENSO"))

# 2010 only
# setwd("C:/Users/ahpvi/Documents/CENSO/2010")

censo_cat <- subset(censo_cat, year == 2010)

# download the microdata to your local computer
# censo_cat <- lodown("censo", censo_cat)

#' fpc1: v0011
#' fpc2: v0010
#' fpc3: v0001
#' fpc4: v0300
#' fpc5: NA

# choose columns to import from both household and person files
columns_to_import <-
      c('v0001', 'v0011', 'v1001', 'v1006',
        'v0502', 'v0601', 'v6036', 'v0606', 'v0629', 'v0634',
        'v0635', 'v6352', 'v6461', 'v6471', 'v0648', 'v6513',
        'v6521', 'v6525', 'v6527', 'v6529', 'v6531', 'v0653')

# initiate a data.frame to stack all downloaded censo states

file_names <- data.frame(dom_file = character(28), pes_file = character(28)) %>% 
      dplyr::mutate(dom_file = paste0("Amostra_Domicilios_", 
                                      paste0(as.character(rep(
                                            c(12, 27, 13, 16, 29, 23, 53, 32,
                                    52, 21, 31, 50, 51, 15, 25, 26, 22,
                                    41, 33, 24, 11, 14, 43, 42, 28, 
                                    "35_outras", "35_RMSP", 17))), ".txt")),
                    pes_file = paste0("Amostra_Pessoas_", 
                                      paste0(as.character(rep(
                                            c(12, 27, 13, 16, 29, 23, 53, 32,
                                              52, 21, 31, 50, 51, 15, 25, 26, 22,
                                              41, 33, 24, 11, 14, 43, 42, 28, 
                                              "35_outras", "35_RMSP", 17))), ".txt")))

censo_cat <- censo_cat %>% 
      # glimpse
      cbind(file_names) %>% 
      # glimpse
      dplyr::mutate(dom_file = paste("C:/Users/ahpvi/Documents/doutorado/artigos-prelo/educacao-mercado/CENSO/2010", 
                                     c("AC", "AL", "AM", "AP", "BA", "CE",
                                       "DF", "ES", "GO", "MA", "MG", "MS",
                                       "MT", "PA", "PB", "PE", 
                                       "PI", "PR", "RJ", "RN", "RO", "RR",
                                       "RS", "SC", "SE", "SP1", "SP2-RM",
                                       "TO"),
                                     dom_file, sep = "/"),
                    pes_file = paste("C:/Users/ahpvi/Documents/doutorado/artigos-prelo/educacao-mercado/CENSO/2010", 
                                     c("AC", "AL", "AM", "AP", "BA", "CE",
                                       "DF", "ES", "GO", "MA", "MG", "MS",
                                       "MT", "PA", "PB", "PE", 
                                       "PI", "PR", "RJ", "RN", "RO", "RR",
                                       "RS", "SC", "SE", "SP1", "SP2-RM",
                                       "TO"),
                                     pes_file, sep = "/"))

# censo_cat <- censo_cat[1:14, ]
censo_cat <- censo_cat[15:28, ]

censo_df <- data.frame(NULL)

# only construct one censo design at a time (2000 and 2010 should not be stacked)
stopifnot(length(unique(censo_cat[, 'year'])) == 1)

# loop through all downloaded censo states
for (this_state in seq(nrow(censo_cat))) {
      
      # add the design information to the columns to import
      these_columns_to_import <-
            unique( 
                  c( 
                        columns_to_import , 
                        as.character( 
                              censo_cat[2, c('weight', paste0('fpc', 1:5))] 
                        ) 
                  ) 
            )
      
      # remove NAs
      these_columns_to_import <- these_columns_to_import[ !is.na( these_columns_to_import ) ]
      
      # load structure files, lowercase variable names, set unwanted columns to missing
      dom_stru <- SAScii::parse.SAScii( censo_cat[ this_state , 'dom_sas' ] )
      dom_stru$varname <- tolower( dom_stru$varname )
      
      pes_stru <- SAScii::parse.SAScii( censo_cat[ this_state , 'pes_sas' ] )
      pes_stru$varname <- tolower( pes_stru$varname )
      
      # import fixed-width files
      this_censo_dom_df <- 
            data.frame( readr::read_fwf(
                  censo_cat[ this_state , 'dom_file' ] ,
                  readr::fwf_widths( 
                        abs( dom_stru$width ) , col_names = dom_stru[ , 'varname' ] 
                  ) ,
                  col_types = 
                        paste0( 
                              ifelse( !( dom_stru$varname %in% these_columns_to_import ) , 
                                      "_" , 
                                      ifelse( dom_stru$char , "c" , "d" ) 
                              ) , 
                              collapse = "" 
                        )
            ) )
      
      this_censo_pes_df <- 
            data.frame( readr::read_fwf(
                  censo_cat[ this_state , 'pes_file' ] ,
                  readr::fwf_widths( 
                        abs( pes_stru$width ) , col_names = pes_stru[ , 'varname' ] 
                  ) ,
                  col_types = 
                        paste0( 
                              ifelse( !( pes_stru$varname %in% these_columns_to_import ) , 
                                      "_" , 
                                      ifelse( pes_stru$char , "c" , "d" ) 
                              ) , 
                              collapse = "" 
                        )
            ) )
      
      # add decimals
      for( this_variable in these_columns_to_import ) {
            
            if( 
                  ( this_variable %in% names( this_censo_dom_df ) ) & 
                  !isTRUE( all.equal( 1 , dom_stru[ dom_stru$varname == this_variable , 'divisor' ] ) ) 
            ){
                  this_censo_dom_df[ , this_variable ] <- 
                        dom_stru[ dom_stru$varname == this_variable , 'divisor' ] * 
                        this_censo_dom_df[ , this_variable ]
            }
            
            if( 
                  ( this_variable %in% names( this_censo_pes_df ) ) & 
                  !isTRUE( all.equal( 1 , pes_stru[ pes_stru$varname == this_variable , 'divisor' ] ) ) 
            ){
                  this_censo_pes_df[ , this_variable ] <- 
                        pes_stru[ pes_stru$varname == this_variable , 'divisor' ] * 
                        this_censo_pes_df[ , this_variable ]
            }
            
      }
      
      # merge household and person tables
      this_censo_df <- merge( this_censo_dom_df , this_censo_pes_df )
      
      # confirm one record per person, with household information merged on
      stopifnot( nrow( this_censo_df ) == nrow( this_censo_pes_df ) )
      
      rm( this_censo_dom_df , this_censo_pes_df ) ; gc()
      
      # stack the merged tables
      censo_df <- rbind( censo_df , this_censo_df )
      
      rm( this_censo_df ) ; gc()
      
}

#' A leitura dos dados foi feita em duas etapas, com 14 arquivos lidos cada.
#' Dois arquivos foram criados (tb-censo2010-pt1.rds e tb-censo2010-pt2.rds). 
#' Em seguida, os arquivos foram empilhados e salvos (tb-ceenso2010-completo.rds).

# censo_pt1 <- readRDS("./dados/tb-censo2010-pt1.rds"); gc()
# 
# censo_pt1 <- dplyr::select(censo_pt1,
#                            v0001, v0011, v0010, v0300, v0601, v6036,
#                            v0606, v6461, v0648, v6513, v6352)
# 
# gc()
#
# censo_pt2 <- readRDS("./dados/tb-censo2010-pt2.rds"); gc()
# 
# censo_pt2 <- dplyr::select(censo_pt2,
#                            v0001, v0011, v0010, v0300, v0601, v6036,
#                            v0606, v6461, v0648, v6513, v6352)
# 
# gc()
#
# censo_pt1 <- rbind(censo_pt1, censo_pt2); gc()
# 
# rm(censo_pt2)
# saveRDS(censo_pt1, "tb-censo2010-completo.rds")
# write_csv(censo_pt1, "tb-censo2010-completo.csv")

# c10$v0001 <- as.integer(c10$v0001)
# c10$v0011 <- as.numeric(c10$v0011)
# c10$v0601 <- as.integer(c10$v0601)
# c10$v0606 <- as.integer(c10$v0606)
# c10$v6352 <- as.integer(c10$v6352)

# fpc_sums <- c10 %>% 
#       dplyr::group_by(v0011) %>% 
#       dplyr::summarise(fpc = sum(v0010))
# 
# c10 <- inner_join(c10, fpc_sums)
# rm(fpc_sums)
# gc()
# c10$one <- 1

# saveRDS(c10, "tb-censo2010-temp.rds")

## Baixa e carrega CENSO DEMOGRAFICO 2000  ---------------

# examine all available CENSO microdata files

censo_cat <- get_catalog("censo", output_dir = file.path(path.expand("~"), "CENSO"))

# 2010 onlY
censo_cat <- subset(censo_cat, year == 2000)

# download the microdata to your local computer
# censo_cat <- lodown("censo", censo_cat) # -- > DONE BEFORE AT THE BEGINNING OF THE CODE

#' fpc1: v0011
#' fpc2: v0010
#' fpc3: v0001
#' fpc4: v0300
#' fpc5: NA

# choose columns to import from both household and person files

columns_to_import <- c('v0102', 'areap', 'p001', 'v0300', 'v0400', 
                       'v1001', 'v4752', 'v1005', 'v1006', 'v0401', 'v0402', 
                       'V4752', 'v0408', 'v0430', 'v0432', 'v0434', 
                       'v4452', 'v4462', 'v0447', 'v4513', 'v4354')

# initiate a data.frame to stack all downloaded censo states

file_names <- data.frame(dom_file = character(27), pes_file = character(27)) %>% 
      dplyr::mutate(dom_file = paste0("Dom", 
                                      paste0(as.character(rep(
                                            c(12, 27, 13, 16, 29, 23, 53, 32,
                                              52, 21, 31, 50, 51, 15, 25, 26, 
                                              22, 41, 33, 24, 11, 14, 43, 42, 
                                              28, 35, 17))), ".txt")),
                    pes_file = paste0("Pes", 
                                      paste0(as.character(rep(
                                            c(12, 27, 13, 16, 29, 23, 53, 32,
                                              52, 21, 31, 50, 51, 15, 25, 26, 
                                              22, 41, 33, 24, 11, 14, 43, 42, 
                                              28, 35, 17))), ".txt")))

censo_cat <- censo_cat %>% 
      # glimpse
      cbind(file_names) %>% 
      # glimpse
      dplyr::mutate(dom_file = paste("C:/Users/ahpvi/Documents/doutorado/artigos-prelo/educacao-mercado/CENSO/2000", 
                                     c("AC", "AL", "AM", "AP", "BA", "CE",
                                       "DF", "ES", "GO", "MA", "MG", "MS",
                                       "MT", "PA", "PB", "PE", "PI", "PR", 
                                       "RJ", "RN", "RO", "RR", "RS", "SC", 
                                       "SE", "SP", "TO"),
                                     dom_file, sep = "/"),
                    pes_file = paste("C:/Users/ahpvi/Documents/doutorado/artigos-prelo/educacao-mercado/CENSO/2000", 
                                     c("AC", "AL", "AM", "AP", "BA", "CE",
                                       "DF", "ES", "GO", "MA", "MG", "MS",
                                       "MT", "PA", "PB", "PE", "PI", "PR", 
                                       "RJ", "RN", "RO", "RR", "RS", "SC", 
                                       "SE", "SP", "TO"),
                                     pes_file, sep = "/"))

censo_df <- data.frame(NULL)

# only construct one censo design at a time (2000 and 2010 should not be stacked)
stopifnot(length(unique(censo_cat[, 'year'])) == 1)

# censo_cat <- censo_cat[1:14, ]
# censo_cat <- censo_cat[15:28, ]
# censo_cat <- censo_cat[19, ]


# loop through all downloaded censo states
for(this_state in seq(nrow(censo_cat[19, ]))) {
      
      # add the design information to the columns to import
      these_columns_to_import <-
            unique( 
                  c( 
                        columns_to_import , 
                        as.character( 
                              censo_cat[2, c('weight', paste0('fpc', 1:5))] 
                        ) 
                  ) 
            )
      
      # remove NAs
      these_columns_to_import <- these_columns_to_import[ !is.na( these_columns_to_import ) ]
      
      # load structure files, lowercase variable names, set unwanted columns to missing
      dom_stru <- SAScii::parse.SAScii( censo_cat[ this_state , 'dom_sas' ] )
      dom_stru$varname <- tolower( dom_stru$varname )
      
      pes_stru <- SAScii::parse.SAScii( censo_cat[ this_state , 'pes_sas' ] )
      pes_stru$varname <- tolower( pes_stru$varname )
      
      # import fixed-width files
      this_censo_dom_df <- 
            data.frame( readr::read_fwf(
                  censo_cat[ this_state , 'dom_file' ] ,
                  readr::fwf_widths( 
                        abs( dom_stru$width ) , col_names = dom_stru[ , 'varname' ] 
                  ) ,
                  col_types = 
                        paste0( 
                              ifelse( !( dom_stru$varname %in% these_columns_to_import ) , 
                                      "_" , 
                                      ifelse( dom_stru$char , "c" , "d" ) 
                              ) , 
                              collapse = "" 
                        )
            ) )
      
      this_censo_pes_df <- 
            data.frame( readr::read_fwf(
                  censo_cat[ this_state , 'pes_file' ] ,
                  readr::fwf_widths( 
                        abs( pes_stru$width ) , col_names = pes_stru[ , 'varname' ] 
                  ) ,
                  col_types = 
                        paste0( 
                              ifelse( !( pes_stru$varname %in% these_columns_to_import ) , 
                                      "_" , 
                                      ifelse( pes_stru$char , "c" , "d" ) 
                              ) , 
                              collapse = "" 
                        )
            ) )
      
      # add decimals
      for( this_variable in these_columns_to_import ) {
            
            if( 
                  ( this_variable %in% names( this_censo_dom_df ) ) & 
                  !isTRUE( all.equal( 1 , dom_stru[ dom_stru$varname == this_variable , 'divisor' ] ) ) 
            ){
                  this_censo_dom_df[ , this_variable ] <- 
                        dom_stru[ dom_stru$varname == this_variable , 'divisor' ] * 
                        this_censo_dom_df[ , this_variable ]
            }
            
            if( 
                  ( this_variable %in% names( this_censo_pes_df ) ) & 
                  !isTRUE( all.equal( 1 , pes_stru[ pes_stru$varname == this_variable , 'divisor' ] ) ) 
            ){
                  this_censo_pes_df[ , this_variable ] <- 
                        pes_stru[ pes_stru$varname == this_variable , 'divisor' ] * 
                        this_censo_pes_df[ , this_variable ]
            }
            
      }
      
      # merge household and person tables
      this_censo_dom_df$v0400 <- NULL
      this_censo_df <- inner_join(this_censo_dom_df, this_censo_pes_df,
                                  by = c("v0102", "v0300", "v1001", "areap", 
                                         "v1005", "v1006", "p001"))
      
      # confirm one record per person, with household information merged on
      stopifnot(nrow(this_censo_df) == nrow(this_censo_pes_df))
      
      rm(this_censo_dom_df, this_censo_pes_df)
      gc()
      
      # stack the merged tables
      censo_df <- rbind(censo_df, this_censo_df)
      
      rm(this_censo_df)
      gc()
      
}

# glimpse(censo_df)

# keep only education graduates

censo_df <- dplyr::filter(censo_df, v6352 %in% c("140", "142", "143", "144", "145", "146"))

# add a column of ones
censo_df[, 'one' ] <- 1

# calculate the finite population correction for each stratum to construct a
# sampling design with weighting areas as strata and households as psu

# the real censo design is stratified with "setor censitarios" rather than 
# "area de ponderacao" but those are not disclosed due to confidentiality

# v0010 is the person or household weight
# v0011 is the weighting area identifier
# both of these are specified inside `censo_cat[ c( 'fpc1' , 'weight' ) ]`

fpc_sums <- aggregate(v0010 ~ v0011, data = censo_df, sum)

names(fpc_sums)[2] <- 'fpc'

censo_df <- left_join(censo_df, fpc_sums, by = "v0011")

c00 <- haven::read_sav(file = "C:/Users/ahpvi/Documents/doutorado/artigos-prelo/educacao-mercado/Censo.2000.brasil.amostra.10porcento.sav")

str(c00$V4754)
