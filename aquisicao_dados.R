source("global.R")
##### Dados Macroeconômicos #####

###### Expactativas de Inflação 12 meses ######

expectativa_inflacao_12M <- rbcb::get_market_expectations("inflation-12-months", "IPCA", 
                                                          end_date = "2022-12-31")  |> 
  select(Data,Mediana) |> 
  rename(DT_COMPTC = Data) |> 
  mutate(MÊS = yearmonth(DT_COMPTC)) |> 
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  mutate(TRIMESTRE = yearquarter(DT_COMPTC)) |> 
  mutate(SEMESTRE = paste0(ANO," S",lubridate::semester(DT_COMPTC)))


pin_write(board,
          expectativa_inflacao_12M,
          "expectativa_inflacao_12M",
          type = "qs")

###### Expactativas de Inflação 12 meses ######



###### Abertura da Curva de Juros ######

df <- futures_mget(
  first_date = "2005-01-01",
  last_date = "2022-04-27",
  by = 1
)

pin_write(board,
          df,
          "contratos_futuros",
          type = "qs")

di1_futures <- df |> 
  lazy_dt() |> 
  filter(refdate > "2005-01-01") |> 
  filter(commodity == "DI1") |>
  mutate(maturity_code = str_replace(maturity_code,"JAN","F")) |>
  mutate(maturity_code = str_replace(maturity_code,"FEV","G")) |>
  mutate(maturity_code = str_replace(maturity_code,"MAR","H")) |>
  mutate(maturity_code = str_replace(maturity_code,"ABR","J")) |>
  mutate(maturity_code = str_replace(maturity_code,"MAI","K")) |>
  mutate(maturity_code = str_replace(maturity_code,"JUN","M")) |>
  mutate(maturity_code = str_replace(maturity_code,"JUL","N")) |>
  mutate(maturity_code = str_replace(maturity_code,"AGO","Q")) |>
  mutate(maturity_code = str_replace(maturity_code,"SET","U")) |>
  mutate(maturity_code = str_replace(maturity_code,"OUT","V")) |>
  mutate(maturity_code = str_replace(maturity_code,"NOV","X")) |>
  mutate(maturity_code = str_replace(maturity_code,"DEZ","Z")) |>
  mutate(maturity_code = str_replace(maturity_code,"(?<=[:alpha:])\\d{1,1}$",
                                     paste0("0",
                                            str_extract(maturity_code,"\\d")))) |> 
  mutate(maturity_code = str_replace(maturity_code,"00","10")) |> 
  filter(str_detect(maturity_code,"F")) |> 
  mutate(maturity_date = maturity2date(maturity_code)) |>
  mutate(fixing = following(maturity_date, "Brazil/ANBIMA"),
         business_days = bizdays(refdate, maturity_date, "Brazil/ANBIMA"),
         adjusted_tax = (100000 / price) ^ (252 / business_days) - 1) |> 
  collect()

di1_futures |> 
  filter(str_detect(maturity_code,"F")) |> 
  select(refdate,maturity_code,adjusted_tax) |> 
  group_by(refdate) |> 
  tidyr::nest() |> 
  mutate(data = lapply(data,function(df) tidyr::pivot_wider(df,
                                                            names_from = maturity_code,
                                                            values_from = adjusted_tax))) |> 
  mutate(data = purrr::map(data,function(df) df %>% mutate(Abertura_Curva5A = .[[6]]-.[[1]]))) |> 
  tidyr::unnest(data) |> 
  select(-starts_with("DI1")) -> Aberturas_Curva

pin_write(board,
          Aberturas_Curva,
          "Aberturas_Curva",
          type = "qs")


###### Curva DIxPRE ######

yc_mget(
  first_date = "2005-01-01",
  last_date = "2022-04-27",
  by = 1
) -> DI_PRE

pin_write(board,
          DI_PRE,
          "DI_PRE",
          type = "qs")

##### Câmbio ########

dolar <- rbcb::get_series(c(USDBRL = 1)) |> 
  rename(DT_COMPTC = date) |> 
  mutate(MÊS = yearmonth(DT_COMPTC)) |> 
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  mutate(TRIMESTRE = yearquarter(DT_COMPTC)) |> 
  mutate(SEMESTRE = paste0(ANO," S",lubridate::semester(DT_COMPTC))) |>
  filter(ANO >= 2004)

pin_write(board,
          dolar,
          "dolar",
          type = "qs")

####### Selic #######
selic <- rbcb::get_series(432) |> 
  rename(SELIC = `432`) |> 
  rename(DT_COMPTC = date) |> 
  mutate(MÊS = yearmonth(DT_COMPTC)) |> 
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  mutate(TRIMESTRE = yearquarter(DT_COMPTC)) |> 
  mutate(SEMESTRE = paste0(ANO," S",lubridate::semester(DT_COMPTC))) |>
  filter(ANO >= 2004)


pin_write(board,
          selic,
          "selic",
          type = "qs")


###### IBOV #########

IBOV1 <- read_delim("D:/Tese/Dados/IBOV/Evolucao_Mensal.csv", 
                    delim = ";", 
                    escape_double = FALSE, 
                    locale = locale(decimal_mark = ",", 
                                    grouping_mark = ".", 
                                    encoding = "WINDOWS-1252"), 
                    trim_ws = TRUE)

IBOV2 <- read_delim("D:/Tese/Dados/IBOV/Evolucao_Mensal (1).csv", 
                    delim = ";", 
                    escape_double = FALSE, 
                    locale = locale(decimal_mark = ",", 
                                    grouping_mark = ".", 
                                    encoding = "WINDOWS-1252"), 
                    trim_ws = TRUE)


bind_rows(IBOV1,IBOV2) |> 
  mutate(COMPETENCIA = zoo::as.yearmon(paste0(as.character(lubridate::month(Mês,label = T))," ",Ano))) |> 
  select(COMPETENCIA,Valor) |> 
  rename(IBOV = Valor) -> IBOV

pin_write(board,
          IBOV,
          "IBOV",
          type = "qs")

########### IPCA #############

IPCA <- rbcb::get_series(13522) |> 
  rename(IPCA = `13522`) |> 
  rename(DT_COMPTC = date) |> 
  mutate(MÊS = yearmonth(DT_COMPTC)) |> 
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  mutate(TRIMESTRE = yearquarter(DT_COMPTC)) |> 
  mutate(SEMESTRE = paste0(ANO," S",lubridate::semester(DT_COMPTC)))

pin_write(board,
          IPCA,
          "IPCA",
          type = "qs")

############## DADOS BALANCETES #################
plan(multisession, workers = 6)

meses_download <- format(seq(as.Date("2005-01-01"),as.Date("2014-12-31"),by = "month"),"%Y%m")

future_map(meses_download, function(mes){
  tryCatch({
    url <- paste0("https://dados.cvm.gov.br/dados/FI/DOC/BALANCETE/DADOS/HIST/balancete_fi_",mes,".zip")
    destfile <- paste0("./balancetes/",mes,".zip")
    download.file(url,destfile)
  }, error = function(e){
    url <- paste0("https://dados.cvm.gov.br/dados/FI/DOC/BALANCETE/DADOS/HIST/balancete_fi_",mes,".zip")
    destfile <- paste0("./balancetes/",mes,".zip")
    download.file(url,destfile)
  })
})

meses_download <- format(seq(as.Date("2015-01-01"),as.Date("2021-06-01"),by = "month"),"%Y%m")

future_map(meses_download, function(mes){
  tryCatch({
    url <- paste0("https://dados.cvm.gov.br/dados/FI/DOC/BALANCETE/DADOS/balancete_fi_",mes,".zip")
    destfile <- paste0("./balancetes/",mes,".zip")
    download.file(url,destfile)
  }, error = function(e){
    url <- paste0("https://dados.cvm.gov.br/dados/FI/DOC/BALANCETE/DADOS/balancete_fi_",mes,".zip")
    destfile <- paste0("./balancetes/",mes,".zip")
    download.file(url,destfile)
  })
})

zipfiles <- list.files("./balancetes/",full.names = T)

future_map(zipfiles,function(path){
  unzip(path,exdir = "./balancetes/")
})

file.remove(zipfiles)


balancetes_path <- list.files("./balancetes/",full.names = T,pattern = ".csv")

future_map(balancetes_path,function(balancentes){
  read_delim(balancentes, 
             delim = ";", 
             escape_double = FALSE, 
             locale = locale(), 
             trim_ws = TRUE) |> 
    lazy_dt() |> 
    dplyr::filter(CD_CONTA_BALCTE == "49983205") |> 
    dplyr::as_tibble() |> 
    dplyr::as_tibble()
}) -> taxa_performance_fundos


taxa_performance_fundos |> 
  dplyr::bind_rows() -> taxa_performance_fundos

pin_write(board,taxa_performance_fundos,"taxa_performance_fundos")


############ INFORME DIARIO #######################

anos_download <- unique(format(seq(as.Date("2005-01-01"),as.Date("2020-12-31"),by = "month"),"%Y"))
meses_download <- format(seq(as.Date("2021-01-01"),as.Date("2021-06-01"),by = "month"),"%Y%m")


future_map(anos_download, function(ano){
  url <- paste0("https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/HIST/inf_diario_fi_",ano,".zip")
  destfile <- paste0("./informe_diario/",ano,".zip")
  download.file(url,destfile)
})

future_map(meses_download, function(mes){
  url <- paste0("https://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/inf_diario_fi_",mes,".zip")
  destfile <- paste0("./informe_diario/",mes,".zip")
  download.file(url,destfile)
})

zipfiles <- list.files("./informe_diario/",full.names = T)

future_map(zipfiles,function(path){
  unzip(path,exdir = "./informe_diario/")
})

file.remove(zipfiles)


informes_path <- list.files("./informe_diario/",full.names = T,pattern = ".csv")


future_map(informes_path,function(informe){
  read_delim(informe, 
             delim = ";", 
             escape_double = FALSE,
             col_types = cols(DT_COMPTC = col_date(format = "%Y-%m-%d")), 
             locale = locale(), 
             trim_ws = TRUE)
}) -> informe_diario_fundos


bind_rows(informe_diario_fundos) -> informe_diario_fundos

pin_write(board,informe_diario_fundos,"informe_diario_fundos",type = "rds")

##########################  EXTRATO DE FUNDOS  ####################################################

download.file("https://dados.cvm.gov.br/dados/FI/DOC/EXTRATO/DADOS/extrato_fi.csv",
              "./extrato/extrato_fi_atual.csv")

extrato_fi_atual <- read_delim("extrato/extrato_fi_atual.csv", 
                               delim = ";", escape_double = FALSE, col_types = cols(DT_COMPTC = col_date(format = "%Y-%m-%d")), 
                               locale = locale(encoding = "latin1"), 
                               trim_ws = TRUE)

pin_write(board,extrato_fi_atual,"extrato_fi_atual",type = "rds")

######################## CASDASTRO DE FUNDOS ##########################################################
download.file("https://dados.cvm.gov.br/dados/FI/CAD/DADOS/cad_fi.csv",
              "./extrato/cad_fi.csv")

cad_fi <- read_delim("extrato/cad_fi.csv", 
                     delim = ";", 
                     escape_double = FALSE, 
                     col_types = cols(DT_REG = col_date(format = "%Y-%m-%d"), 
                                      DT_CONST = col_date(format = "%Y-%m-%d"), 
                                      DT_CANCEL = col_date(format = "%Y-%m-%d"), 
                                      DT_INI_SIT = col_date(format = "%Y-%m-%d"), 
                                      DT_INI_ATIV = col_date(format = "%Y-%m-%d"), 
                                      DT_INI_EXERC = col_date(format = "%Y-%m-%d"), 
                                      DT_FIM_EXERC = col_date(format = "%Y-%m-%d"), 
                                      DT_PATRIM_LIQ = col_date(format = "%Y-%m-%d")), 
                     locale = locale(encoding = "latin1"), 
                     trim_ws = TRUE)

pin_write(board,cad_fi,"cad_fi",type = "rds")


############## IMAS ############################

rbcb::get_series(12461) |> 
  rename(IRF_M = `12461`) |> 
  rename(DT_COMPTC = date) |> 
  mutate(MÊS = yearmonth(DT_COMPTC)) |> 
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  mutate(TRIMESTRE = yearquarter(DT_COMPTC)) |> 
  mutate(SEMESTRE = paste0(ANO," S",lubridate::semester(DT_COMPTC))) -> IRF_M

rbcb::get_series(12462) |> 
  rename(IMA_S = `12462`) |> 
  rename(DT_COMPTC = date) |> 
  mutate(MÊS = yearmonth(DT_COMPTC)) |> 
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  mutate(TRIMESTRE = yearquarter(DT_COMPTC)) |> 
  mutate(SEMESTRE = paste0(ANO," S",lubridate::semester(DT_COMPTC))) -> IMA_S

rbcb::get_series(12466) |> 
  rename(IMA_B = `12466`) |> 
  rename(DT_COMPTC = date) |> 
  mutate(MÊS = yearmonth(DT_COMPTC)) |> 
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  mutate(TRIMESTRE = yearquarter(DT_COMPTC)) |> 
  mutate(SEMESTRE = paste0(ANO," S",lubridate::semester(DT_COMPTC))) -> IMA_B

pin_write(board,
          IRF_M,
          "IRF_M",
          type = "qs")

pin_write(board,
          IMA_S,
          "IMA_S",
          type = "qs")

pin_write(board,
          IMA_B,
          "IMA_B",
          type = "qs")


