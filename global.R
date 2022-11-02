library(dplyr)
library(readr)
library(pins)
library(bizdays)
library(fixedincome)
library(rb3)
library(dtplyr)
library(httr)
library(furrr)
library(stringr)
library(tsibble)
library(ggplot2)
library(plm) 

board <- board_folder("dados_tese/",versioned = T)


####### Variáveis Base ############

CNPJ_GESTOR_PREV <- c("27.665.207/0001-31",
                      "51.990.695/0001-37",
                      "03.730.204/0001-76",
                      "62.465.117/0001-06",
                      "00.436.923/0001-90",
                      "87.376.109/0001-06")

BANCOES_VEREJO <- c("30.822.936/0001-69",
                    "62.375.134/0001-44",
                    "40.430.971/0001-96",
                    "42.040.639/0001-40",
                    "10.231.177/0001-52",
                    "60.701.190/0001-04",
                    "33.311.713/0001-25",
                    "03.795.072/0001-60",
                    "90.400.888/0001-42")

ITAU <- tibble(CPF_CNPJ_GESTOR = c("60.701.190/0001-04",
                                   "40.430.971/0001-96",
                                   "33.311.713/0001-25",
                                   "33.311.713/0001-25"),
               GESTOR = c("ITAU UNIBANCO ASSET MANAGEMENT LTDA.",
                          "ITAU UNIBANCO S.A.",
                          "ITAU UNIBANCO S.A.",
                          "ITAU DTVM S.A."))                                                                             

SPX <- tibble(CPF_CNPJ_GESTOR = c("14.595.392/0001-93",
                                  "12.330.774/0001-60",
                                  "34.293.150/0001-52"),
              GESTOR = c("SPX EQUITIES GESTÃO DE RECURSOS LTDA",
                         "SPX GESTÃO DE RECURSOS LTDA",
                         "SPX CRÉDITO GESTÃO DE RECURSOS LTDA."))

SANTANDER <- tibble(CPF_CNPJ_GESTOR = c("10.231.177/0001-52",
                                        "90.400.888/0001-42"),
                    GESTOR = c("SANTANDER BRASIL GESTÃO DE RECURSOS LTDA",
                               "BANCO SANTANDER (BRASIL) S.A."))

BTG <- tibble(CPF_CNPJ_GESTOR = c("60.451.242/0001-23",
                                  "29.650.082/0001-00"),
              GESTOR = c("BTG PACTUAL WM GESTÃO DE RECURSOS LTDA",
                         "BTG PACTUAL ASSET MANAGEMENT S/A DTVM"))





