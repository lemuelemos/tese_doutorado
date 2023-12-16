# Carregamento de dados e Board -------------------------------------------
source("global.R")
board <- board_folder("dados_tese/",versioned = T)
cad_fi <- pin_read(board,"cad_fi")
extrato_fi_atual <- pin_read(board,"extrato_fi_atual")
pin_read(board,"taxa_performance_fundos") -> taxa_performance_fundos

# Carregamento dos dados de Informes Diários ------------------------------

cad_fi |>
  filter(TP_FUNDO == "FI") |>
  filter(SIT == "EM FUNCIONAMENTO NORMAL") |>
  select(TP_FUNDO,
         CNPJ_FUNDO,
         CPF_CNPJ_GESTOR,SIT,
         DT_CANCEL,
         DT_CONST,
         DT_INI_EXERC,
         FUNDO_COTAS,
         CLASSE,
         GESTOR,
         PF_PJ_GESTOR,
         ENTID_INVEST,
         TRIB_LPRAZO,
         CONDOM,
         FUNDO_EXCLUSIVO,
         RENTAB_FUNDO) -> cad_fi


extrato_fi_atual |>
  filter(CNPJ_FUNDO %in% cad_fi$CNPJ_FUNDO) |>
  select(CNPJ_FUNDO,
         DENOM_SOCIAL,
         DT_COMPTC,
         NEGOC_MERC,
         MERCADO,
         TP_PRAZO,
         PRAZO,
         PUBLICO_ALVO,
         REG_ANBIMA,
         CLASSE_ANBIMA,
         DISTRIB,
         POLIT_INVEST,
         FUNDO_COTAS,
         FUNDO_ESPELHO,
         FUNDO_COTAS,
         TAXA_SAIDA_PAGTO_RESGATE,
         EXISTE_TAXA_PERFM,
         TAXA_PERFM,
         PARAM_TAXA_PERFM,
         CALC_TAXA_PERFM) |>
  left_join(cad_fi, by = "CNPJ_FUNDO") |>
  filter(REG_ANBIMA == "S") |>
  filter(PF_PJ_GESTOR == "PJ") |>
  filter(!((FUNDO_COTAS.x == "S" & FUNDO_COTAS.y == "N") |
             (FUNDO_COTAS.x == "N" & FUNDO_COTAS.y == "S"))) |>
  filter(!(CLASSE == "Fundo de Ações" & str_detect(CLASSE_ANBIMA,"RENDA FIXA"))) |>
  filter(!(CLASSE == " Renda Fixa" & str_detect(CLASSE_ANBIMA,"AÇÕES"))) |>
  select(-contains(".y")) |>
  filter(CLASSE %in% c("Fundo de Ações","Fundo Multimercado","Fundo de Renda Fixa")) -> extrato_fi_atual


# Dados de Informes Diários -----------------------------------------------

informe_diario_fundos <- pin_read(board,"informe_diario_fundos") |>
  select(-TP_FUNDO) |>
  lazy_dt()

informe_diario_fundos |>
  filter(bizdays::is.bizday(DT_COMPTC,"Brazil/ANBIMA")) |>
  filter(CNPJ_FUNDO %in% extrato_fi_atual$CNPJ_FUNDO) -> informe_diario_fundos

informe_diario_fundos |>
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo de Ações") |>
  collect() -> informe_diario_fundos_acoes

informe_diario_fundos |>
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo Multimercado") |> collect() -> informe_diario_fundos_multimercado

informe_diario_fundos |>
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo de Renda Fixa") |> collect() -> informe_diario_fundos_rf

rm(informe_diario_fundos)

# Funções - Helpers------------------------------------


criar_sequencias_dias <- function(dia,dias,uteis){
  seq(as.Date(dia)-lubridate::days(dias-1),
      as.Date(dia)+lubridate::days(dias),by = 1) -> sequencia
  sequencia[is.bizday(sequencia,
                      cal = "Brazil/ANBIMA")] -> sequencia
  sequencia[(which(sequencia == dia)-(uteis-1)):(which(sequencia == dia)+uteis)]
}

# Fundos de Ações - Volatilidade ------------------------------------
# 

informe_diario_fundos <- pin_read(board,"informe_diario_fundos") |>
  select(-TP_FUNDO) |> 
  lazy_dt()

informe_diario_fundos |> 
  filter(bizdays::is.bizday(DT_COMPTC,"Brazil/ANBIMA")) |> 
  filter(CNPJ_FUNDO %in% extrato_fi_atual$CNPJ_FUNDO) -> informe_diario_fundos

informe_diario_fundos |> 
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo de Ações") |>
  collect() -> informe_diario_fundos_acoes

rm(informe_diario_fundos)

informe_diario_fundos_acoes |> 
  select(DT_COMPTC) |> 
  distinct() |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência) |> 
  summarise_all(dplyr::last) |> 
  mutate(Periodos = purrr::map(DT_COMPTC,
                               function(data) criar_sequencias_dias(data,60,30))) -> periodos_acoes_30

informe_diario_fundos_acoes |> 
  select(DT_COMPTC) |> 
  distinct() |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência) |> 
  summarise_all(dplyr::last) |> 
  mutate(Periodos = purrr::map(DT_COMPTC,
                               function(data) criar_sequencias_dias(data,120,60))) -> periodos_acoes_60

informe_diario_fundos_acoes |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_QUOTA) -> base_volatilidade_acoes

rm(informe_diario_fundos_acoes)

# Fundos de Ações - Volatilidade - 30 Dias------------------------------------

plan(multisession, workers = 10)  
future_map(unique(base_volatilidade_acoes$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(periodos_acoes_30$Periodos,
              periodos_acoes_30$DT_COMPTC,function(datas,marco){
    base_volatilidade_acoes |>
      filter(CNPJ_FUNDO == CNPJ) |> 
      filter(DT_COMPTC %in% datas) |>
      mutate(Marco = case_when(DT_COMPTC <= marco ~ "30 Dias Antes",
                               DT_COMPTC > marco ~ "30 Dias Depois")) |> 
      group_by(Marco) |> 
      mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
      summarise(
        `Retorno Anual` = dplyr::last((na.omit(`Retorno Diário`)+1))^(252/30),
        `Volatilidade` = sd(`Retorno Diário`,na.rm = T)*(sqrt(252))
        ) |> 
      mutate(CNPJ_FUNDO = CNPJ,
             DT_COMPTC = marco)
  }) |> 
    bind_rows()
},.progress = T) -> diferenca_vol_acoes_30_dias
  
diferenca_vol_acoes_30_dias |>   
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,
                     values_from = Volatilidade) -> diferenca_vol_acoes_30_dias

pin_write(board,diferenca_vol_acoes_30_dias,"diferenca_vol_acoes_30_dias")

# Fundos de Ações - Volatilidade - 60 dias------------------------------------

plan(multisession, workers = 10)  
future_map(unique(base_volatilidade_acoes$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(periodos_acoes_60$Periodos,
              periodos_acoes_60$DT_COMPTC,function(datas,marco){
                base_volatilidade_acoes |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "60 Dias Antes",
                                           DT_COMPTC > marco ~ "60 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)*(sqrt(252))) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco)
              }) |> 
    bind_rows()
},.progress = T) -> diferenca_vol_acoes_60_dias

diferenca_vol_acoes_60_dias |>   
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,
                     values_from = Volatilidade) -> diferenca_vol_acoes_60_dias

pin_write(board,diferenca_vol_acoes_60_dias,"diferenca_vol_acoes_60_dias")

rm(base_volatilidade_acoes)
rm(periodos_acoes_60)
rm(periodos_acoes_30)
rm(diferenca_vol_acoes_60_dias)
rm(diferenca_vol_acoes_30_dias)

# Fundos Multimercado - Volatilidade ------------------------------------

informe_diario_fundos <- pin_read(board,"informe_diario_fundos") |>
  select(-TP_FUNDO) |>
  lazy_dt()

informe_diario_fundos |>
  filter(bizdays::is.bizday(DT_COMPTC,"Brazil/ANBIMA")) |>
  filter(CNPJ_FUNDO %in% extrato_fi_atual$CNPJ_FUNDO) -> informe_diario_fundos

informe_diario_fundos |>
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo Multimercado") |> collect() -> informe_diario_fundos_multimercado

rm(informe_diario_fundos)

informe_diario_fundos_multimercado |> 
  select(DT_COMPTC) |> 
  distinct() |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência) |> 
  summarise_all(dplyr::last) |> 
  mutate(Periodos = purrr::map(DT_COMPTC,
                               function(data) criar_sequencias_dias(data,60,30))) -> periodos_multimercado_30

informe_diario_fundos_multimercado |> 
  select(DT_COMPTC) |> 
  distinct() |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência) |> 
  summarise_all(dplyr::last) |> 
  mutate(Periodos = purrr::map(DT_COMPTC,
                               function(data) criar_sequencias_dias(data,120,60))) -> periodos_multimercado_60

informe_diario_fundos_multimercado |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_QUOTA) |> 
  lazy_dt() -> base_volatilidade_multimercado

rm(informe_diario_fundos_multimercado)

# Fundos Multimercado - Volatilidade - 30 Dias------------------------------------

plan(multisession, workers = 14)

future_map(unique(collect(base_volatilidade_multimercado)$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(periodos_multimercado_30$Periodos,
              periodos_multimercado_30$DT_COMPTC,function(datas,marco){
                base_volatilidade_multimercado |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "30 Dias Antes",
                                           DT_COMPTC > marco ~ "30 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)*(sqrt(252))) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco) |> 
                  collect()
              }) |> 
    bind_rows()
},.progress = T,
  .options  = furrr_options(packages = c("data.table",
                                         "dtplyr"))) -> diferenca_vol_multimercado_30_dias

diferenca_vol_multimercado_30_dias |>   
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,
                     values_from = Volatilidade) -> diferenca_vol_multimercado_30_dias

pin_write(board,diferenca_vol_multimercado_30_dias,"diferenca_vol_multimercado_30_dias")

# Fundos Multimercado - Volatilidade - 60 Dias------------------------------------

plan(multisession, workers = 14)  

future_map(unique(collect(base_volatilidade_multimercado)$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(periodos_multimercado_60$Periodos,
              periodos_multimercado_60$DT_COMPTC,function(datas,marco){
                base_volatilidade_multimercado |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "60 Dias Antes",
                                           DT_COMPTC > marco ~ "60 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)*(sqrt(252))) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco) |> 
                  collect()
              }) |> 
    bind_rows()
},.progress = T,
  .options  = furrr_options(packages = c("data.table",
                                         "dtplyr"))) -> diferenca_vol_multimercado_60_dias

diferenca_vol_multimercado_60_dias |>   
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,
                     values_from = Volatilidade) -> diferenca_vol_multimercado_60_dias

pin_write(board,diferenca_vol_multimercado_60_dias,"diferenca_vol_multimercado_60_dias")

rm(base_volatilidade_acoes)
rm(periodos_multimercado_30)
rm(periodos_multimercado_60)
rm(diferenca_vol_multimercado_60_dias)
rm(diferenca_vol_multimercado_30_dias)
# Fundos Renda Fixa - Volatilidade ------------------------------------

informe_diario_fundos <- pin_read(board,"informe_diario_fundos") |>
  select(-TP_FUNDO) |>
  lazy_dt()

informe_diario_fundos |>
  filter(bizdays::is.bizday(DT_COMPTC,"Brazil/ANBIMA")) |>
  filter(CNPJ_FUNDO %in% extrato_fi_atual$CNPJ_FUNDO) -> informe_diario_fundos

informe_diario_fundos |>
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo de Renda Fixa") |> 
  collect() -> informe_diario_fundos_rf

rm(informe_diario_fundos)


informe_diario_fundos_rf |> 
  select(DT_COMPTC) |> 
  distinct() |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência) |> 
  summarise_all(dplyr::last) |> 
  mutate(Periodos = purrr::map(DT_COMPTC,
                               function(data) criar_sequencias_dias(data,60,30))) -> periodos_rf_30

informe_diario_fundos_rf |> 
  select(DT_COMPTC) |> 
  distinct() |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência) |> 
  summarise_all(dplyr::last) |> 
  mutate(Periodos = purrr::map(DT_COMPTC,
                               function(data) criar_sequencias_dias(data,120,60))) -> periodos_rf_60

informe_diario_fundos_rf |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_QUOTA) |> 
  lazy_dt() -> base_volatilidade_rf

rm(informe_diario_fundos_rf)

# Fundos Renda Fixa - Volatilidade - 30 Dias------------------------------------

plan(multisession, workers = 14)  

future_map(unique(collect(base_volatilidade_rf)$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(periodos_rf_30$Periodos,
              periodos_rf_30$DT_COMPTC,function(datas,marco){
                base_volatilidade_rf |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "30 Dias Antes",
                                           DT_COMPTC > marco ~ "30 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)*(sqrt(252)),
                            Qtd = n()) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco) |> 
                  collect()
              }) |> 
    bind_rows()
},.progress = T,
  .options  = furrr_options(packages = c("data.table",
                                         "dtplyr"))) -> diferenca_vol_rf_30_dias

diferenca_vol_rf_30_dias |>   
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,
                     values_from = Volatilidade) -> diferenca_vol_rf_30_dias

pin_write(board,diferenca_vol_rf_30_dias,"diferenca_vol_rf_30_dias")

# Fundos Renda Fixa - Volatilidade - 60 Dias------------------------------------

plan(multisession, workers = 10)  

future_map(unique(collect(base_volatilidade_rf)$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(periodos_rf_60$Periodos,
              periodos_rf_60$DT_COMPTC,function(datas,marco){
                base_volatilidade_rf |>
                  filter(CNPJ_FUNDO == CNPJ) |>
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "60 Dias Antes",
                                           DT_COMPTC > marco ~ "60 Dias Depois")) |>
                  group_by(Marco) |>
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |>
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)*(sqrt(252)),
                            Qtd = n()) |>
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco) |>
                  collect()
              }) |> 
    bind_rows()
},.progress = T,
  .options  = furrr_options(packages = c("data.table",
                                         "dtplyr"))) -> diferenca_vol_rf_60_dias

diferenca_vol_rf_60_dias |>   
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,
                     values_from = Volatilidade) -> diferenca_vol_rf_60_dias

pin_write(board,diferenca_vol_rf_60_dias,"diferenca_vol_rf_60_dias")


rm(base_volatilidade_rf)
rm(periodos_rf_30)
rm(periodos_rf_60)
rm(diferenca_vol_rf_60_dias)
rm(diferenca_vol_rf_30_dias)
