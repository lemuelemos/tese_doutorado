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

criar_sequencias_dias <- function(ano,dias){
  seq(as.Date(ano)-lubridate::days(dias-1),
      as.Date(ano)+lubridate::days(dias),by = 1)}

# Fundos de Ações - Volatilidade ------------------------------------

taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% 
           unique(informe_diario_fundos_acoes$CNPJ_FUNDO)) -> taxa_performance_fundos_acoes

informe_diario_fundos_acoes |>
  filter(DT_COMPTC >= "2014-11-01") |> 
  filter(CNPJ_FUNDO %in% unique(taxa_performance_fundos_acoes$CNPJ_FUNDO)) |> 
  select(CNPJ_FUNDO,
         DT_COMPTC,
         TP_FUNDO,
         TAXA_PERFM,
         EXISTE_TAXA_PERFM,
         NR_COTST,
         VL_QUOTA) |> 
  left_join(taxa_performance_fundos_acoes,by = c("CNPJ_FUNDO",
                                                 "DT_COMPTC")) |> 
  filter(VL_QUOTA > 0) |> 
  distinct_all() -> base_volatilidade_acoes

# Fundos de Ações - Volatilidade - 30 Dias------------------------------------
plan(multisession, workers = 10)
base_volatilidade_acoes |> 
  select(CNPJ_FUNDO,DT_COMPTC) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  group_by(CNPJ_FUNDO,Competencia) |> 
  summarise_all(dplyr::last) |> 
  select(-Competencia) |> 
  mutate(Periodos = future_map(DT_COMPTC,function(ano) criar_sequencias_dias(ano,30))) -> periodos_por_fundo

plan(multisession, workers = 10)  
future_map(unique(periodos_por_fundo$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$Periodos,
             filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$DT_COMPTC,function(datas,marco){
    base_volatilidade_acoes |>
      filter(CNPJ_FUNDO == CNPJ) |> 
      filter(DT_COMPTC %in% datas) |>
      mutate(Marco = case_when(DT_COMPTC <= marco ~ "30 Dias Antes",
                               DT_COMPTC > marco ~ "30 Dias Depois")) |> 
      group_by(Marco) |> 
      mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
      summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)^(n()/360)) |> 
      mutate(CNPJ_FUNDO = CNPJ,
             DT_COMPTC = marco)
  }) |> 
    bind_rows()
}) |> 
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,values_from = Volatilidade) -> diferenca_vol_acoes_30_dias

# Fundos de Ações - Volatilidade - 60 dias------------------------------------

plan(multisession, workers = 10)  
base_volatilidade_acoes |> 
  select(CNPJ_FUNDO,DT_COMPTC) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  group_by(CNPJ_FUNDO,Competencia) |> 
  summarise_all(dplyr::last) |> 
  select(-Competencia) |> 
  mutate(Periodos = future_map(DT_COMPTC,function(ano) criar_sequencias_dias(ano,60))) -> periodos_por_fundo

plan(multisession, workers = 10)  
future_map(unique(periodos_por_fundo$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$Periodos,
              filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$DT_COMPTC,function(datas,marco){
                base_volatilidade_acoes |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "60 Dias Antes",
                                           DT_COMPTC > marco ~ "60 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)^(n()/360)) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco)
              }) |> 
    bind_rows()
}) |> 
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,values_from = Volatilidade) -> diferenca_vol_acoes_60_dias

# Fundos Multimercado - Volatilidade ------------------------------------

taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% 
           unique(informe_diario_fundos_multimercado$CNPJ_FUNDO)) -> taxa_performance_fundos_multimercado

informe_diario_fundos_multimercado |>
  filter(DT_COMPTC >= "2014-11-01") |> 
  filter(CNPJ_FUNDO %in% unique(taxa_performance_fundos_multimercado$CNPJ_FUNDO)) |> 
  select(CNPJ_FUNDO,
         DT_COMPTC,
         TP_FUNDO,
         TAXA_PERFM,
         EXISTE_TAXA_PERFM,
         NR_COTST,
         VL_QUOTA) |> 
  left_join(taxa_performance_fundos_multimercado,by = c("CNPJ_FUNDO",
                                                 "DT_COMPTC")) |> 
  filter(VL_QUOTA > 0) |> 
  distinct_all() -> base_volatilidade_multimercado


# Fundos Multimercado - Volatilidade - 30 Dias------------------------------------

plan(multisession, workers = 10)
base_volatilidade_multimercado |> 
  select(CNPJ_FUNDO,DT_COMPTC) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  group_by(CNPJ_FUNDO,Competencia) |> 
  summarise_all(dplyr::last) |> 
  select(-Competencia) |> 
  mutate(Periodos = future_map(DT_COMPTC,function(ano) criar_sequencias_dias(ano,30))) -> periodos_por_fundo

plan(multisession, workers = 10)  
future_map(unique(periodos_por_fundo$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$Periodos,
              filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$DT_COMPTC,function(datas,marco){
                base_volatilidade_multimercado |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "30 Dias Antes",
                                           DT_COMPTC > marco ~ "30 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)^(n()/360)) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco)
              }) |> 
    bind_rows()
}) |> 
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,values_from = Volatilidade) -> diferenca_vol_multimercado_30_dias

# Fundos Multimercado - Volatilidade - 60 Dias------------------------------------

plan(multisession, workers = 10)
base_volatilidade_multimercado |> 
  select(CNPJ_FUNDO,DT_COMPTC) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  group_by(CNPJ_FUNDO,Competencia) |> 
  summarise_all(dplyr::last) |> 
  select(-Competencia) |> 
  mutate(Periodos = future_map(DT_COMPTC,function(ano) criar_sequencias_dias(ano,60))) -> periodos_por_fundo

plan(multisession, workers = 10)  
future_map(unique(periodos_por_fundo$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$Periodos,
              filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$DT_COMPTC,function(datas,marco){
                base_volatilidade_multimercado |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "60 Dias Antes",
                                           DT_COMPTC > marco ~ "60 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)^(n()/360)) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco)
              }) |> 
    bind_rows()
}) |> 
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,values_from = Volatilidade) -> diferenca_vol_multimercado_60_dias

# Fundos Renda Fixa - Volatilidade ------------------------------------

taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% 
           unique(informe_diario_fundos_rf$CNPJ_FUNDO)) -> taxa_performance_fundos_rf

informe_diario_fundos_rf |>
  filter(DT_COMPTC >= "2014-11-01") |> 
  filter(CNPJ_FUNDO %in% unique(taxa_performance_fundos_rf$CNPJ_FUNDO)) |> 
  select(CNPJ_FUNDO,
         DT_COMPTC,
         TP_FUNDO,
         TAXA_PERFM,
         EXISTE_TAXA_PERFM,
         NR_COTST,
         VL_QUOTA) |> 
  left_join(taxa_performance_fundos_rf,by = c("CNPJ_FUNDO",
                                                        "DT_COMPTC")) |> 
  filter(VL_QUOTA > 0) |> 
  distinct_all() -> base_volatilidade_rf

# Fundos Renda Fixa - Volatilidade - 30 Dias------------------------------------

plan(multisession, workers = 10)
base_volatilidade_rf |> 
  select(CNPJ_FUNDO,DT_COMPTC) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  group_by(CNPJ_FUNDO,Competencia) |> 
  summarise_all(dplyr::last) |> 
  select(-Competencia) |> 
  mutate(Periodos = future_map(DT_COMPTC,function(ano) criar_sequencias_dias(ano,30))) -> periodos_por_fundo

plan(multisession, workers = 10)  
future_map(unique(periodos_por_fundo$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$Periodos,
              filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$DT_COMPTC,function(datas,marco){
                base_volatilidade_rf |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "30 Dias Antes",
                                           DT_COMPTC > marco ~ "30 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)^(n()/360)) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco)
              }) |> 
    bind_rows()
}) |> 
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,values_from = Volatilidade) -> diferenca_vol_rf_30_dias

# Fundos Renda Fixa - Volatilidade - 30 Dias------------------------------------

plan(multisession, workers = 10)
base_volatilidade_rf |> 
  select(CNPJ_FUNDO,DT_COMPTC) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  group_by(CNPJ_FUNDO,Competencia) |> 
  summarise_all(dplyr::last) |> 
  select(-Competencia) |> 
  mutate(Periodos = future_map(DT_COMPTC,function(ano) criar_sequencias_dias(ano,60))) -> periodos_por_fundo

plan(multisession, workers = 10)  
future_map(unique(periodos_por_fundo$CNPJ_FUNDO), function(CNPJ){
  purrr::map2(filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$Periodos,
              filter(periodos_por_fundo,CNPJ_FUNDO == CNPJ)$DT_COMPTC,function(datas,marco){
                base_volatilidade_rf |>
                  filter(CNPJ_FUNDO == CNPJ) |> 
                  filter(DT_COMPTC %in% datas) |>
                  mutate(Marco = case_when(DT_COMPTC <= marco ~ "60 Dias Antes",
                                           DT_COMPTC > marco ~ "60 Dias Depois")) |> 
                  group_by(Marco) |> 
                  mutate(`Retorno Diário` = c(NA_integer_,diff(log(VL_QUOTA)))) |> 
                  summarise(`Volatilidade` = sd(`Retorno Diário`,na.rm = T)^(n()/360)) |> 
                  mutate(CNPJ_FUNDO = CNPJ,
                         DT_COMPTC = marco)
              }) |> 
    bind_rows()
}) |> 
  bind_rows() |> 
  tidyr::pivot_wider(names_from = Marco,values_from = Volatilidade) -> diferenca_vol_rf_60_dias
