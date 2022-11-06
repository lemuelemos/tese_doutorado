
# Carregamento de dados e Board -------------------------------------------

source("global.R")
board <- board_folder("dados_tese/",versioned = T)
cad_fi <- pin_read(board,"cad_fi")
extrato_fi_atual <- pin_read(board,"extrato_fi_atual")

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
  filter(CNPJ_FUNDO %in% extrato_fi_atual$CNPJ_FUNDO) -> informe_diario_fundos

informe_diario_fundos |> 
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo de Ações") |> collect() -> informe_diario_fundos_acoes

informe_diario_fundos |> 
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo Multimercado") |> collect() -> informe_diario_fundos_multimercado

informe_diario_fundos |> 
  left_join(select(extrato_fi_atual,-DT_COMPTC), by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo de Renda Fixa") |> collect() -> informe_diario_fundos_rf

rm(informe_diario_fundos)

