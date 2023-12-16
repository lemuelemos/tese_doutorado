source("global.R")
board <- board_folder("dados_tese/",versioned = T)
cad_fi <- pin_read(board,"cad_fi")
extrato_fi_atual <- pin_read(board,"extrato_fi_atual")

#### Tratamento de inconsistências ####

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
  filter(CLASSE %in% c("Fundo de Ações",
                       "Fundo Multimercado",
                       "Fundo de Renda Fixa")) -> extrato_fi_atual


#### Pareamento com a Base de Informes diários #### 

informe_diario_fundos <- pin_read(board,"informe_diario_fundos") |>
  select(-TP_FUNDO) |> 
  lazy_dt()

informe_diario_fundos |> 
  filter(CNPJ_FUNDO %in% 
           extrato_fi_atual$CNPJ_FUNDO) -> informe_diario_fundos


##### Fundos Multimercados #### 

informe_diario_fundos |> 
  left_join(select(extrato_fi_atual,-DT_COMPTC), 
            by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo Multimercado") |> 
  filter(!str_detect(CLASSE_ANBIMA,"AÇÕES|RENDA FIXA"),
         CONDOM == "Aberto") |>
  collect() -> informe_diario_fundos_multimercado

informe_diario_fundos_multimercado |> 
  distinct(DT_COMPTC,CNPJ_FUNDO,.keep_all = T) |> 
  group_by(CNPJ_FUNDO) |>
  mutate(Qtd = n()) |> 
  ungroup() |> 
  filter(Qtd > max(Qtd)/2) |>
  filter(!CNPJ_FUNDO %in% 
           distinct(filter(informe_diario_fundos_multimercado,
                           VL_PATRIM_LIQ < 1000000),
                    CNPJ_FUNDO)$CNPJ_FUNDO) -> informe_diario_fundos_multimercado

informe_diario_fundos_multimercado |> 
  filter(CONDOM == "Aberto") |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000) |> 
  mutate(COMPETÊNCIA = yearmonth(DT_COMPTC)) |> 
  group_by(CNPJ_FUNDO,COMPETÊNCIA) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA)) |> 
  ungroup() |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(Qtd = n()) |> 
  ungroup() |> 
  filter(Qtd > Qtd/2) |> 
  ungroup() |> 
  mutate(CPT_ZERO = case_when(CAPTC_LIQ == 0 ~ "S",
                              T ~ "N")) |> 
  count(CNPJ_FUNDO,CPT_ZERO) |> 
  tidyr::pivot_wider(names_from = CPT_ZERO,
                     values_from = n,
                     values_fill = 0) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(PROP_ZERO = (S/(S+N))*100) |> 
  ungroup() |> 
  filter(PROP_ZERO < 50) |> 
  distinct(CNPJ_FUNDO) -> fundos_elegiveis_mm

informe_diario_fundos_multimercado |> 
  filter(CNPJ_FUNDO %in% 
           fundos_elegiveis_mm$CNPJ_FUNDO) -> informe_diario_fundos_multimercado

informe_diario_fundos_multimercado |> 
  lazy_dt() |> 
  select(DT_COMPTC,
         CNPJ_FUNDO,
         TRIB_LPRAZO,
         CLASSE,
         CLASSE_ANBIMA,
         PUBLICO_ALVO,
         FUNDO_COTAS.x,
         EXISTE_TAXA_PERFM,
         TAXA_PERFM,
         CPF_CNPJ_GESTOR,
         PF_PJ_GESTOR,
         ENTID_INVEST,
         CONDOM,
         FUNDO_ESPELHO,
         VL_PATRIM_LIQ,
         CAPTC_DIA,
         RESG_DIA,
         VL_QUOTA) |> 
  rename(FUNDO_COTAS = FUNDO_COTAS.x) |> 
  left_join(tibble(CPF_CNPJ_GESTOR = BANCOES_VEREJO,
                   GRUPO = "Grandes Bancos"), 
            by = "CPF_CNPJ_GESTOR") |> 
  mutate(GRUPO = case_when(is.na(GRUPO) ~ "Demais Instituições",
                           T ~ GRUPO)) |> 
  mutate(MÊS = yearmonth(DT_COMPTC)) |> 
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  mutate(TRIMESTRE = yearquarter(DT_COMPTC)) |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  mutate(TRIB_LPRAZO = case_when(is.na(TRIB_LPRAZO) | 
                                   TRIB_LPRAZO == "N/A" |
                                   TRIB_LPRAZO == "N" ~ 0,
                                 TRIB_LPRAZO == "S" ~ 1)) |>
  mutate(FUNDO_ESPELHO = case_when(FUNDO_ESPELHO == "N" |
                                     is.na(FUNDO_ESPELHO) ~ 0,
                                   FUNDO_ESPELHO == "S" ~ 1)) |> 
  mutate(FUNDO_COTAS = case_when(FUNDO_COTAS == "N" ~ 0,
                                 FUNDO_COTAS == "S" ~ 1)) |> 
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "N" ~ 0,
                                       T  ~ 1)) |> 
  relocate(ANO,.after = DT_COMPTC) |> 
  relocate(SEMESTRE,.after = ANO) |>
  relocate(TRIMESTRE,.after = SEMESTRE) |>
  relocate(`MÊS`,.after = TRIMESTRE) |>
  collect() -> dados_painel_mm


dados_painel_mm |> 
  lazy_dt() |> 
  group_by(MÊS,CNPJ_FUNDO) |> 
  mutate(CAPTC_DIA = cumsum(CAPTC_DIA),
         RESG_DIA = cumsum(RESG_DIA)) |> 
  summarise_all(dplyr::last) |> 
  group_by(CNPJ_FUNDO) |>
  mutate(RETORNO = ((VL_QUOTA/dplyr::lag(VL_QUOTA))-1),
         CAPTC_LIQ = (CAPTC_DIA-RESG_DIA)/1000000) |> 
  collect() -> dados_painel_mm_mensal

dados_painel_mm_mensal |> 
  ungroup() |> 
  ggplot(aes(CAPTC_LIQ,RETORNO)) + 
  geom_point()
