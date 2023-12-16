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

##### Fundos de Ações #####

informe_diario_fundos |> 
  left_join(select(extrato_fi_atual,-DT_COMPTC), 
            by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo de Ações") |> 
  filter(!CLASSE_ANBIMA %in% c("AÇÕES - FUNDOS FECHADOS"),
         CONDOM == "Aberto") |> 
  collect() -> informe_diario_fundos_acoes

informe_diario_fundos_acoes |> 
  distinct(DT_COMPTC,CNPJ_FUNDO,.keep_all = T) |> 
  group_by(CNPJ_FUNDO) |>
  mutate(Qtd = n()) |> 
  ungroup() |> 
  filter(Qtd > max(Qtd)/2) |> 
  filter(!CNPJ_FUNDO %in% 
           distinct(count(filter(informe_diario_fundos_acoes,
                                 VL_PATRIM_LIQ < 1000000),
                          CNPJ_FUNDO),
                    CNPJ_FUNDO)$CNPJ_FUNDO) -> informe_diario_fundos_acoes

informe_diario_fundos_acoes |>  
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
  distinct(CNPJ_FUNDO) -> fundos_elegiveis_acoes

informe_diario_fundos_acoes |> 
  filter(CNPJ_FUNDO %in% 
           fundos_elegiveis_acoes$CNPJ_FUNDO) -> informe_diario_fundos_acoes

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

##### Fundos Renda Fixa #####

informe_diario_fundos |> 
  left_join(select(extrato_fi_atual,-DT_COMPTC),
            by = "CNPJ_FUNDO") |>
  filter(CLASSE == "Fundo de Renda Fixa") |>
  filter(!str_detect(CLASSE_ANBIMA,"AÇÕES|MULTIMERCADO"),
         CONDOM == "Aberto") |>
  collect() -> informe_diario_fundos_rf

informe_diario_fundos_rf |>
  distinct(DT_COMPTC,CNPJ_FUNDO,.keep_all = T) |> 
  group_by(CNPJ_FUNDO) |>
  mutate(Qtd = n()) |> 
  ungroup() |> 
  filter(Qtd > max(Qtd)/2) |>
  filter(!CNPJ_FUNDO %in% 
           distinct(filter(informe_diario_fundos_rf ,
                           VL_PATRIM_LIQ < 1000000),
                    CNPJ_FUNDO)$CNPJ_FUNDO) -> informe_diario_fundos_rf

informe_diario_fundos_rf |> 
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
  distinct(CNPJ_FUNDO) -> fundos_elegiveis_rf

informe_diario_fundos_rf |> 
  filter(CNPJ_FUNDO %in% 
           fundos_elegiveis_rf$CNPJ_FUNDO) -> informe_diario_fundos_rf

rm(informe_diario_fundos)
rm(fundos_elegiveis_rf)
rm(fundos_elegiveis_mm)
rm(fundos_elegiveis_acoes)

#### Manipulação de dados por Perídos ####
##### Fundos de Ações #####

informe_diario_fundos_acoes |> 
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
  collect() -> dados_painel_acoes

##### Fundos Multimercados #####

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

##### Fundos de Renda Fixa #####

informe_diario_fundos_rf |> 
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
  collect() -> dados_painel_rf

#### Manipulação de Dados ####

##### Manipulação de Dados fundos de Ações #####

dados_painel_acoes |> 
  lazy_dt() |> 
  select(-SEMESTRE,-TRIMESTRE,-MÊS) |> 
  mutate(ANO = as.character(ANO)) |>
  group_by(ANO,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                               VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                               VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                               VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  ungroup() -> dados_painel_acoes_ano

dados_painel_acoes |> 
  lazy_dt() |> 
  select(-ANO,-TRIMESTRE,-MÊS) |> 
  group_by(SEMESTRE,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(SEMESTRE = SEMESTRE) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                               VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                               VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                               VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  ungroup() -> dados_painel_acoes_semestre

dados_painel_acoes |> 
  lazy_dt() |> 
  select(-SEMESTRE,-ANO,-MÊS) |> 
  group_by(TRIMESTRE,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                               VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                               VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                               VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  mutate(TRIMESTRE = as.character(TRIMESTRE)) |> 
  ungroup() -> dados_painel_acoes_trimestre

##### Manipulação de dados de Fundos Multimercados #####

dados_painel_mm |> 
  lazy_dt() |> 
  select(-SEMESTRE,-TRIMESTRE,-MÊS) |> 
  mutate(ANO = as.character(ANO)) |>
  group_by(ANO,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                               VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                               VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                               VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  ungroup() -> dados_painel_mm_ano

dados_painel_mm |> 
  lazy_dt() |> 
  select(-ANO,-TRIMESTRE,-MÊS) |> 
  group_by(SEMESTRE,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(SEMESTRE = as.character(SEMESTRE)) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                               VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                               VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                               VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  ungroup() -> dados_painel_mm_semestre

dados_painel_mm |> 
  lazy_dt() |> 
  filter(CONDOM == "Aberto") |>
  select(-SEMESTRE,-ANO,-MÊS) |> 
  mutate(TRIMESTRE = as.character(TRIMESTRE)) |>
  group_by(TRIMESTRE,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                               VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                               VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                               VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  ungroup() -> dados_painel_mm_trimestre

##### Manipulação de dados fundos de Renda Fixa #####

dados_painel_rf |> 
  lazy_dt() |> 
  select(-SEMESTRE,-TRIMESTRE,-MÊS) |> 
  mutate(ANO = as.character(ANO)) |>
  group_by(ANO,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                             VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                             VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                             VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  ungroup() -> dados_painel_rf_ano

dados_painel_rf |> 
  lazy_dt() |> 
  select(-ANO,-TRIMESTRE,-MÊS) |> 
  group_by(SEMESTRE,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(SEMESTRE = as.character(SEMESTRE)) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                               VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                               VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                               VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  ungroup() -> dados_painel_rf_semestre

dados_painel_rf |> 
  lazy_dt() |> 
  select(-SEMESTRE,-ANO,-MÊS) |> 
  mutate(TRIMESTRE = as.character(TRIMESTRE)) |>
  group_by(TRIMESTRE,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = ((dplyr::last(VL_QUOTA)/
                                dplyr::first(VL_QUOTA))-1)*100,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA),
            FUNDO_ESPELHO = dplyr::first(FUNDO_ESPELHO),
            TRIB_LPRAZO = dplyr::first(TRIB_LPRAZO)) |> 
  mutate(RENTABILIDADE = case_when(RENTABILIDADE == Inf ~ 0,
                                   T ~ RENTABILIDADE)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                               VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                               VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                               VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000 & 
                                   CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000 & 
                                   CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000 & 
                                   CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & 
                                   CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & 
                                   CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & 
                                   CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | 
                                   CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & 
                               TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & 
                               TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) |> 
  collect() |> 
  ungroup() -> dados_painel_rf_trimestre

#### Tratamento das Variáveis Econômicas ####

##### Dados Macro Período Anual #####

pin_read(board,"dolar") |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) Dólar` = (dplyr::last(USDBRL)/
                                      dplyr::first(USDBRL))-1,
            USDBRL = dplyr::last(USDBRL)) -> dolar_anual

pin_read(board,"IRF_M") |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) IRF-M` = (dplyr::last(IRF_M)/
                                      dplyr::first(IRF_M))-1,
            IRF_M = dplyr::last(IRF_M)) -> IRF_M_anual

pin_read(board,"IMA_S") |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) IMA-S` = (dplyr::last(IMA_S)/
                                      dplyr::first(IMA_S))-1,
            IMA_S = dplyr::last(IMA_S)) -> IMA_S_anual

pin_read(board,"IMA_B") |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) IMA-B` = (dplyr::last(IMA_B)/
                                      dplyr::first(IMA_B))-1,
            IMA_B = dplyr::last(IMA_B)) -> IMA_B_anual

pin_read(board,"IBOV") |> 
  arrange(COMPETENCIA) |> 
  distinct_all() |> 
  mutate(ANO = lubridate::year(zoo::as.Date(COMPETENCIA))) |>
  group_by(ANO) |>
  summarise(`VAR ANUAL(%) IBOV` = (dplyr::last(IBOV)/
                                     dplyr::first(IBOV))-1,
            IBOV = dplyr::last(IBOV)) -> ibov_anual

pin_read(board,"IPCA") |> 
  filter(ANO > 2004) |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) IPCA` = (dplyr::last(IPCA)/
                                     dplyr::first(IPCA))-1,
            IPCA = dplyr::last(IPCA)) -> ipca_anual


pin_read(board,"expectativa_inflacao_12M") |> 
  filter(ANO > 2004) |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) Expectativa IPCA` = (dplyr::last(Mediana)/
                                                 dplyr::first(Mediana))-1,
            `Expectativa IPCA 12M` = dplyr::last(Mediana)) -> expectativa_ipca_12M_anual

pin_read(board,"selic") |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) SELIC` = (dplyr::last(SELIC)/
                                      dplyr::first(SELIC))-1,
            `SELIC` = dplyr::last(SELIC)) -> selic_anual

pin_read(board,"expectativa_selic") |> 
  filter(str_detect(Reuniao,"R8")) |> 
  mutate(ANO = lubridate::year(Data)) |> 
  group_by(ANO) |> 
  arrange(Data) |> 
  summarise_all(dplyr::last) |> 
  select(ANO,Mediana) |> 
  mutate(`VAR ANUAL(%) Expec.SELIC` = (Mediana/
                                         dplyr::lag(Mediana))-1) |> 
  rename(`Expec.SELIC` = Mediana) -> expectativa_selic_anual

pin_read(board,"expectativa_cambio") |> 
  filter(baseCalculo == 0) |> 
  mutate(DataReferencia = yearmonth(parse_date(DataReferencia,"%m/%Y")),
         Competencia = yearmonth(Data)) |> 
  relocate(Competencia,.after = DataReferencia) |> 
  mutate(Dif_mes = DataReferencia-Competencia) |> 
  filter(Dif_mes == 12) |> 
  arrange(Data) |> 
  group_by(Competencia) |> 
  summarise_all(dplyr::last) |> 
  mutate(ANO = lubridate::year(Data)) |> 
  filter(ANO > 2004) |> 
  group_by(ANO) |> 
  arrange(Data) |> 
  summarise_all(dplyr::last) |> 
  select(ANO,DataReferencia,Mediana) |> 
  mutate(`VAR ANUAL(%) Expec.CAMBIO` = (Mediana/
                                          dplyr::lag(Mediana))-1) |> 
  rename(`Expec.CAMBIO` = Mediana) -> expectativa_cambio_anual

pin_read(board,"IBC_BR") |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) IBC-Br` = (dplyr::last(`IBC-Br`)/
                                       dplyr::first(`IBC-Br`))-1,
            `IBC-Br` = dplyr::last(`IBC-Br`)) -> ibc_br_anual

pin_read(board,"CDI") |> 
  group_by(ANO) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR ANUAL(%) CDI` = (prod((CDI/100)+1)-1)*100,
            CDI = dplyr::last(CDI)) -> CDI_anual

dolar_anual |> 
  left_join(ibov_anual) |> 
  left_join(ipca_anual) |> 
  left_join(expectativa_ipca_12M_anual) |> 
  left_join(selic_anual) |> 
  left_join(expectativa_selic_anual) |>
  left_join(expectativa_cambio_anual) |> 
  left_join(ibc_br_anual) |> 
  left_join(IRF_M_anual) |> 
  left_join(IMA_S_anual) |> 
  left_join(IMA_B_anual) |> 
  left_join(CDI_anual) |> 
  filter(ANO > 2004) |> 
  mutate(ANO = ANO+1) |> 
  mutate(ANO = as.character(ANO)) |> 
  mutate(across(starts_with("VAR"),~.x*100))  -> variaveis_macro_anual

##### Dados macro Período Semestral #####

pin_read(board,"dolar") |>
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR SEMESTRAL(%) Dólar` = (dplyr::last(USDBRL)/
                                          dplyr::first(USDBRL))-1,
            USDBRL = dplyr::last(USDBRL)) -> dolar_semestral

pin_read(board,"IRF_M") |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR SEMESTRAL(%) IRF-M` = (dplyr::last(IRF_M)/
                                          dplyr::first(IRF_M))-1,
            IRF_M = dplyr::last(IRF_M)) -> IRF_M_semestral

pin_read(board,"IMA_S") |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR SEMESTRAL(%) IMA-S` = (dplyr::last(IMA_S)/
                                          dplyr::first(IMA_S))-1,
            IMA_S = dplyr::last(IMA_S)) -> IMA_S_semestral

pin_read(board,"IMA_B") |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR SEMESTRAL(%) IMA-B` = (dplyr::last(IMA_B)/
                                          dplyr::first(IMA_B))-1,
            IMA_B = dplyr::last(IMA_B)) -> IMA_B_semestral


pin_read(board,"IBOV") |> 
  arrange(COMPETENCIA) |> 
  distinct_all() |> 
  mutate(SEMESTRE = lubridate::semester(COMPETENCIA,with_year = T)) |> 
  group_by(SEMESTRE) |>
  summarise(`VAR SEMESTRAL(%) IBOV` = (dplyr::last(IBOV)/
                                         dplyr::first(IBOV))-1,
            IBOV = dplyr::last(IBOV)) -> ibov_semestral

pin_read(board,"IPCA") |> 
  filter(ANO > 2004) |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR SEMESTRAL(%) IPCA` = (dplyr::last(IPCA)/
                                         dplyr::first(IPCA))-1,
            IPCA = dplyr::last(IPCA)) -> ipca_semestral


pin_read(board,"expectativa_inflacao_12M") |> 
  filter(ANO > 2004) |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR SEMESTRAL(%) Expectativa IPCA` = (dplyr::last(Mediana)/dplyr::first(Mediana))-1,
            `Expectativa IPCA 12M` = dplyr::last(Mediana)) -> expectativa_ipca_12M_semestral

pin_read(board,"selic") |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR SEMESTRAL(%) SELIC` = (dplyr::last(SELIC)/
                                          dplyr::first(SELIC))-1,
            `SELIC` = dplyr::last(SELIC)) -> selic_semestral

pin_read(board,"expectativa_selic") |> 
  filter(str_detect(Reuniao,"R8")) |> 
  mutate(SEMESTRE = lubridate::semester(Data,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(Data) |> 
  summarise_all(dplyr::last) |> 
  select(SEMESTRE,Mediana) |> 
  mutate(`VAR SEMESTRAL(%) Expec.SELIC` = (Mediana/
                                             dplyr::lag(Mediana))-1) |> 
  rename(`Expec.SELIC` = Mediana) -> expectativa_selic_semestral

pin_read(board,"expectativa_cambio") |> 
  filter(baseCalculo == 0) |> 
  mutate(DataReferencia = yearmonth(parse_date(DataReferencia,"%m/%Y")),
         Competencia = yearmonth(Data)) |> 
  relocate(Competencia,.after = DataReferencia) |> 
  mutate(Dif_mes = DataReferencia-Competencia) |> 
  filter(Dif_mes == 12) |> 
  arrange(Data) |> 
  group_by(Competencia) |> 
  summarise_all(dplyr::last) |> 
  mutate(SEMESTRE = lubridate::semester(Data,with_year = T)) |>
  select(Data,SEMESTRE,Competencia,DataReferencia,Mediana) |> 
  group_by(SEMESTRE) |>
  summarise_all(dplyr::last) |>
  mutate(`VAR SEMESTRAL(%) Expec.CAMBIO` = (Mediana/
                                              dplyr::lag(Mediana))-1) |> 
  rename(`Expec.CAMBIO` = Mediana) -> expectativa_cambio_semestral

pin_read(board,"IBC_BR") |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR SEMESTRAL(%) IBC-Br` = (dplyr::last(`IBC-Br`)/
                                           dplyr::first(`IBC-Br`))-1,
            `IBC-Br` = dplyr::last(`IBC-Br`)) -> ibc_br_semestral

pin_read(board,"CDI") |> 
  mutate(SEMESTRE = lubridate::semester(DT_COMPTC,with_year = T)) |> 
  group_by(SEMESTRE) |> 
  arrange(DT_COMPTC) |>  
  summarise(`VAR SEMESTRAL(%) CDI` = (prod((CDI/100)+1)-1)*100,
            CDI = dplyr::last(CDI)) -> CDI_semestral

dolar_semestral |> 
  left_join(ibov_semestral) |> 
  left_join(ipca_semestral) |> 
  left_join(expectativa_ipca_12M_semestral) |> 
  left_join(selic_semestral) |> 
  left_join(expectativa_selic_semestral) |>
  left_join(expectativa_cambio_semestral) |> 
  left_join(ibc_br_semestral) |>
  left_join(IRF_M_semestral) |> 
  left_join(IMA_S_semestral) |> 
  left_join(IMA_B_semestral) |> 
  left_join(CDI_semestral) |> 
  mutate(across(starts_with("VAR"),~.x*100)) |> 
  mutate(SEMESTRE = case_when(str_detect(as.character(SEMESTRE),
                                         "\\.1") ~ SEMESTRE+0.1,
                              T ~ (SEMESTRE+1)-0.1)) |> 
  mutate(SEMESTRE = as.character(SEMESTRE)) -> variaveis_macro_semestral

##### Dados macro Período Trimestral #####

pin_read(board,"dolar") |> 
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR TRIMESTRAL(%) Dólar` = (dplyr::last(USDBRL)/
                                           dplyr::first(USDBRL))-1,
            USDBRL = dplyr::last(USDBRL)) -> dolar_trimestral

pin_read(board,"IRF_M") |> 
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR TRIMESTRAL(%) IRF-M` = (dplyr::last(IRF_M)/
                                           dplyr::first(IRF_M))-1,
            IRF_M = dplyr::last(IRF_M)) -> IRF_M_trimestral

pin_read(board,"IMA_S") |> 
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR TRIMESTRAL(%) IMA-S` = (dplyr::last(IMA_S)/
                                           dplyr::first(IMA_S))-1,
            IMA_S = dplyr::last(IMA_S)) -> IMA_S_trimestral

pin_read(board,"IMA_B") |> 
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR TRIMESTRAL(%) IMA-B` = (dplyr::last(IMA_B)/
                                           dplyr::first(IMA_B))-1,
            IMA_B = dplyr::last(IMA_B)) -> IMA_B_trimestral

pin_read(board,"IBOV") |> 
  arrange(COMPETENCIA) |> 
  distinct_all() |> 
  mutate(TRIMESTRE = yearquarter(zoo::as.Date(COMPETENCIA))) |> 
  group_by(TRIMESTRE) |>
  summarise(`VAR TRIMESTRAL(%) IBOV` = (dplyr::last(IBOV)/
                                          dplyr::first(IBOV))-1,
            IBOV = dplyr::last(IBOV)) -> ibov_trimestral

pin_read(board,"IPCA") |> 
  filter(DT_COMPTC > "2004-01-01") |> 
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR TRIMESTRAL(%) IPCA` = (dplyr::last(IPCA)/
                                          dplyr::first(IPCA))-1,
            IPCA = dplyr::last(IPCA)) -> ipca_trimestral


pin_read(board,"expectativa_inflacao_12M") |> 
  filter(DT_COMPTC > "2004-01-01") |>
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR TRIMESTRAL(%) Expectativa IPCA` = (dplyr::last(Mediana)/dplyr::first(Mediana))-1,
            `Expectativa IPCA 12M` = dplyr::last(Mediana)) -> expectativa_ipca_12M_trimestral

pin_read(board,"selic") |> 
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR TRIMESTRAL(%) SELIC` = (dplyr::last(SELIC)/
                                           dplyr::first(SELIC))-1,
            `SELIC` = dplyr::last(SELIC)) -> selic_trimestral

pin_read(board,"expectativa_selic") |> 
  filter(str_detect(Reuniao,"R8")) |> 
  mutate(TRIMESTRE = yearquarter(Data)) |> 
  group_by(TRIMESTRE) |> 
  arrange(Data) |> 
  summarise_all(dplyr::last) |> 
  dplyr::select(TRIMESTRE,Mediana) |> 
  mutate(`VAR TRIMESTRAL(%) Expec.SELIC` = (Mediana/
                                              dplyr::lag(Mediana))-1) |> 
  rename(`Expec.SELIC` = Mediana) -> expectativa_selic_trimestral

pin_read(board,"expectativa_cambio") |> 
  filter(baseCalculo == 0) |> 
  mutate(DataReferencia = yearmonth(parse_date(DataReferencia,"%m/%Y")),
         Competencia = yearmonth(Data)) |> 
  relocate(Competencia,.after = DataReferencia) |> 
  mutate(Dif_mes = DataReferencia-Competencia) |> 
  filter(Dif_mes == 12) |> 
  arrange(Data) |> 
  group_by(Competencia) |> 
  summarise_all(dplyr::last) |> 
  dplyr::select(Data,Competencia,DataReferencia,Mediana) |> 
  mutate(TRIMESTRE = yearquarter(Data)) |>   
  group_by(TRIMESTRE) |> 
  arrange(Data) |> 
  summarise_all(dplyr::last) |> 
  dplyr::select(TRIMESTRE,DataReferencia ,Mediana) |> 
  mutate(`VAR TRIMESTRAL(%) Expec.CAMBIO` = (Mediana/
                                               dplyr::lag(Mediana))-1) |> 
  rename(`Expec.CAMBIO` = Mediana) -> expectativa_cambio_trimestral

pin_read(board,"IBC_BR") |> 
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR TRIMESTRAL(%) IBC-Br` = (dplyr::last(`IBC-Br`)/
                                            dplyr::first(`IBC-Br`))-1,
            `IBC-Br` = dplyr::last(`IBC-Br`)) -> ibc_br_trimestral

pin_read(board,"CDI") |> 
  group_by(TRIMESTRE) |> 
  arrange(DT_COMPTC) |>  
  summarise(`VAR TRIMESTRAL(%) CDI` = (prod((CDI/100)+1)-1)*100,
            CDI = dplyr::last(CDI)) -> CDI_trimestral

dolar_trimestral |> 
  left_join(ibov_trimestral) |> 
  left_join(ipca_trimestral) |> 
  left_join(expectativa_ipca_12M_trimestral) |> 
  left_join(selic_trimestral) |> 
  left_join(expectativa_selic_trimestral) |>
  left_join(expectativa_cambio_trimestral) |> 
  left_join(ibc_br_trimestral) |> 
  left_join(IRF_M_trimestral) |>
  left_join(IMA_S_trimestral) |> 
  left_join(IMA_B_trimestral) |> 
  left_join(CDI_trimestral) |> 
  mutate(TRIMESTRE = as.character(yearquarter(TRIMESTRE)+
                                    lubridate::quarter(1)
  )) |> 
  mutate(TRIMESTRE = as.character(TRIMESTRE)) |> 
  filter(TRIMESTRE >= "2005 Q1") |> 
  mutate(across(starts_with("VAR"),~.x*100)) -> variaveis_macro_trimestral

#### Junção das bases ####

## Ano ##

dados_painel_acoes_ano |> 
  left_join(variaveis_macro_anual,
            by = "ANO") -> dados_painel_acoes_ano

dados_painel_mm_ano |> 
  left_join(variaveis_macro_anual,
            by = "ANO") -> dados_painel_mm_ano

dados_painel_rf_ano |> 
  left_join(variaveis_macro_anual,
            by = "ANO") -> dados_painel_rf_ano

## Semestre ##

dados_painel_acoes_semestre |> 
  mutate(SEMESTRE = as.character(SEMESTRE)) |> 
  left_join(variaveis_macro_semestral,
            by = "SEMESTRE") -> dados_painel_acoes_semestre

dados_painel_mm_semestre |> 
  mutate(SEMESTRE = as.character(SEMESTRE)) |>
  left_join(variaveis_macro_semestral,
            by = "SEMESTRE") -> dados_painel_mm_semestre

dados_painel_rf_semestre |> 
  mutate(SEMESTRE = as.character(SEMESTRE)) |>
  left_join(variaveis_macro_semestral,
            by = "SEMESTRE") -> dados_painel_rf_semestre

## Trimestre ##

dados_painel_acoes_trimestre |> 
  left_join(variaveis_macro_trimestral,
            by = "TRIMESTRE") -> dados_painel_acoes_trimestre

dados_painel_mm_trimestre |> 
  left_join(variaveis_macro_trimestral,
            by = "TRIMESTRE") -> dados_painel_mm_trimestre

dados_painel_rf_trimestre |> 
  left_join(variaveis_macro_trimestral,
            by = "TRIMESTRE") -> dados_painel_rf_trimestre

#### Modelando os Dados ####

##### Renda Fixa #####

###### Trimestral ######

dados_painel_rf_trimestre |>  
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(CAPTC_LIQ_ACUM = cumsum(CAPTC_LIQ)) |>
  mutate(RENTABILIDADE_ANO = (roll_trimestre(((RENTABILIDADE/
                                                 100)+1))-1)*100) |> 
  mutate(SUP = case_when(lag(RENTABILIDADE)>`VAR TRIMESTRAL(%) CDI` ~ 1,
                         T ~ 0)) |> 
  mutate(DANOS = as.factor(TRIMESTRE)) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VART_IBOV = `VAR TRIMESTRAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VART_DOLAR = `VAR TRIMESTRAL(%) Dólar`,
         VART_EXPEC_IPCA = `VAR TRIMESTRAL(%) Expectativa IPCA`,
         VART_EXPEC_DOLAR = `VAR TRIMESTRAL(%) Expec.CAMBIO`,
         VART_EXPEC_IBC = `VAR TRIMESTRAL(%) IBC-Br`,
         VART_CDI = `VAR TRIMESTRAL(%) CDI`,
         VART_IMAB = `VAR TRIMESTRAL(%) IMA-B`,
         VART_IRFM = `VAR TRIMESTRAL(%) IRF-M`,
         VART_IMAS = `VAR TRIMESTRAL(%) IMA-S`) |> 
  mutate(OLR = case_when(RENTABILIDADE > 25 ~ "Fora",
                        RENTABILIDADE < -25 ~ "Fora",
                        T ~ ""),
         OLC = case_when(CAPTC_LIQ > 20000 ~ "Fora",
                         CAPTC_LIQ < -20000 ~ "Fora",
                         T ~ "")) -> dados_painel_rf_trimestre_reg

dados_painel_rf_trimestre_reg |> 
  filter(!CNPJ_FUNDO %in% unique(filter(dados_painel_rf_trimestre_reg,
                                        OLR == "Fora")$CNPJ_FUNDO)) |>
  filter(!CNPJ_FUNDO %in% unique(filter(dados_painel_rf_trimestre_reg,
                                        OLC == "Fora")$CNPJ_FUNDO)) |>
  select(-OLR,-OLC) |> 
  filter(TRIMESTRE >= "2006 Q1") -> dados_painel_rf_trimestre_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE,1)*GRANDES_BANCOS*PG+
      lag(RENTABILIDADE,2)*GRANDES_BANCOS*PG+
      IRF_M+
      IMA_B+
      IMA_S+
      CDI+
      IPCA+
      USDBRL+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      Expec.CAMBIO+
      IBC_Br+
      IBOV+
      PG+
      GRANDES_BANCOS+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PREV+
      TRIB_LPRAZO+
      FUNDO_ESPELHO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_rf_trimestre_reg, 
    model="within") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="white1", 
                                  type="HC3"))


plm(CAPTC_LIQ ~
      lag(RENTABILIDADE)*PG+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*FUNDO_COTAS+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG+
      lag(RENTABILIDADE)*GRANDES_BANCOS*IQ+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      SELIC*PG+
      IBOV*PG+
      VART_IBOV*PG+
      IPCA*PG+
      EXPEC_IPCA_12M+
      VART_EXPEC_IPCA+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      VART_DOLAR+
      VART_EXPEC_DOLAR+
      VART_EXPEC_IBC+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_rf_trimestre_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE,2)*PG+
      lag(RENTABILIDADE,2)*IQ+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG*GIGANTE+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      log(SELIC)*PG+
      RENTABILIDADE_ANO*PG+
      RENTABILIDADE_ANO*IQ+
      log(IBOV)+
      log(IPCA)+
      log(EXPEC_IPCA_12M)+
      log(Expec.SELIC)+
      log(USDBRL)+
      log(Expec.CAMBIO)+
      log(IBC_Br)+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_rf_trimestre_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, 
                                  method="arellano", 
                                  type="HC3",
                                  cluster = "group"))

plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE,2)*PG+
      lag(RENTABILIDADE,2)*IQ+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG*GIGANTE+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      SELIC*PG+
      RENTABILIDADE_ANO*PG+
      RENTABILIDADE_ANO*IQ+
      IBOV+
      IPCA+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_rf_trimestre_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3",
                                  cluster = "group"))

plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE)+
      lag(RENTABILIDADE)+
      log(IRF_M)+
      log(IMA_B)+
      log(IMA_S)+
      SELIC+
      RENTABILIDADE_ANO+
      RENTABILIDADE_ANO+
      IBOV+
      IPCA+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      logIBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_rf_trimestre_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

###### Semestral ######

dados_painel_rf_semestre |>  
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VARS_IBOV = `VAR SEMESTRAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VARS_DOLAR = `VAR SEMESTRAL(%) Dólar`,
         VARS_EXPEC_IPCA = `VAR SEMESTRAL(%) Expectativa IPCA`,
         VARS_EXPEC_DOLAR = `VAR SEMESTRAL(%) Expec.CAMBIO`,
         VARS_EXPEC_IBC = `VAR SEMESTRAL(%) IBC-Br`) |> 
  mutate(SEMESTRE = as.character(SEMESTRE)) -> dados_painel_rf_semestre_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE,1)+
      lag(RENTABILIDADE,2)+
      IRF_M+
      IMA_B+
      IMA_S+
      CDI+
      IPCA+
      USDBRL+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      Expec.CAMBIO+
      IBC_Br+
      IBOV+
      PG+
      GRANDES_BANCOS+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PREV+
      TRIB_LPRAZO+
      FUNDO_ESPELHO,
    index = c("CNPJ_FUNDO","SEMESTRE"),
    data=dados_painel_rf_semestre_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))


plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE,2)*PG+
      lag(RENTABILIDADE,2)*IQ+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG*GIGANTE+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      log(CDI)*PG+
      RENTABILIDADE_ANO*PG+
      RENTABILIDADE_ANO*IQ+
      log(IBOV)+
      log(IPCA)+
      log(EXPEC_IPCA_12M)+
      log(Expec.SELIC)+
      log(USDBRL)+
      log(Expec.CAMBIO)+
      log(IBC_Br)+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","SEMESTRE"),
    data=dados_painel_rf_semestre_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE,2)*PG+
      lag(RENTABILIDADE,2)*IQ+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG*GIGANTE+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      log(CDI)*PG+
      RENTABILIDADE_ANO*PG+
      RENTABILIDADE_ANO*IQ+
      log(IBOV)+
      log(IPCA)+
      log(EXPEC_IPCA_12M)+
      log(Expec.SELIC)+
      log(USDBRL)+
      log(Expec.CAMBIO)+
      log(IBC_Br)+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","SEMESTRE"),
    data=dados_painel_rf_semestre_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

###### Anual ######

dados_painel_rf_ano |> 
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(DANOS = as.factor(ANO)) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VARA_IBOV = `VAR ANUAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VARA_DOLAR = `VAR ANUAL(%) Dólar`,
         VARA_EXPEC_IPCA = `VAR ANUAL(%) Expectativa IPCA`,
         VARA_EXPEC_DOLAR = `VAR ANUAL(%) Expec.CAMBIO`,
         VARA_EXPEC_IBC = `VAR ANUAL(%) IBC-Br`) -> dados_painel_rf_ano_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE,1)+
      lag(RENTABILIDADE,2)+
      # IRF_M+
      # IMA_B+
      IMA_S+
      CDI+
      IPCA+
      USDBRL+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      Expec.CAMBIO+
      IBC_Br+
      IBOV+
      PG+
      GRANDES_BANCOS+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PREV+
      TRIB_LPRAZO+
      FUNDO_ESPELHO,
    index = c("CNPJ_FUNDO","ANO"),
    data=dados_painel_rf_ano_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE)*PG+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG+
      lag(RENTABILIDADE)*GRANDES_BANCOS*IQ+
      IRF_M*PG+
      IMA_B*PG+
      IMA_S*PG+
      SELIC*PG+
      IBOV*PG+
      VARA_IBOV*PG+
      # IPCA*PG+
      EXPEC_IPCA_12M+
      VARA_EXPEC_IPCA+
      # Expec.SELIC+
      # USDBRL+
      # Expec.CAMBIO+
      # VARA_DOLAR+
      # VARA_EXPEC_DOLAR+
      # VARA_EXPEC_IBC+
      IBC_Br+
      # GIGANTE+
      # GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","ANO"),
    data=dados_painel_rf_ano_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

##### Multimercado #####

###### Trimestral ######

dados_painel_mm_trimestre |> 
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(CAPTC_LIQ_ACUM = cumsum(CAPTC_LIQ)) |>
  mutate(RENTABILIDADE_ANO = (roll_trimestre(((RENTABILIDADE/
                                                 100)+1))-1)*100) |> 
  mutate(SUP = case_when(lag(RENTABILIDADE)>`VAR TRIMESTRAL(%) CDI` ~ 1,
                         T ~ 0)) |> 
  mutate(DANOS = as.factor(TRIMESTRE)) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VART_IBOV = `VAR TRIMESTRAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VART_DOLAR = `VAR TRIMESTRAL(%) Dólar`,
         VART_EXPEC_IPCA = `VAR TRIMESTRAL(%) Expectativa IPCA`,
         VART_EXPEC_DOLAR = `VAR TRIMESTRAL(%) Expec.CAMBIO`,
         VART_EXPEC_IBC = `VAR TRIMESTRAL(%) IBC-Br`,
         VART_CDI = `VAR TRIMESTRAL(%) CDI`,
         VART_IMAB = `VAR TRIMESTRAL(%) IMA-B`,
         VART_IRFM = `VAR TRIMESTRAL(%) IRF-M`,
         VART_IMAS = `VAR TRIMESTRAL(%) IMA-S`) |> 
  mutate(OLR = case_when(RENTABILIDADE > 25 ~ "Fora",
                         RENTABILIDADE < -25 ~ "Fora",
                         T ~ ""),
         OLC = case_when(CAPTC_LIQ > 20000 ~ "Fora",
                         CAPTC_LIQ < -20000 ~ "Fora",
                         T ~ "")) -> dados_painel_mm_trimestre_reg

dados_painel_mm_trimestre_reg |> 
  filter(!CNPJ_FUNDO %in% unique(filter(dados_painel_mm_trimestre_reg,
                                        OLR == "Fora")$CNPJ_FUNDO)) |>
  filter(!CNPJ_FUNDO %in% unique(filter(dados_painel_mm_trimestre_reg,
                                        OLC == "Fora")$CNPJ_FUNDO)) |>
  select(-OLR,-OLC) |> 
  filter(TRIMESTRE >= "2006 Q1") -> dados_painel_mm_trimestre_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE,1)*GRANDES_BANCOS+
      lag(RENTABILIDADE,2)*GRANDES_BANCOS+
      IRF_M+
      IMA_B+
      IMA_S+
      CDI+
      IPCA+
      USDBRL+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      Expec.CAMBIO+
      IBC_Br+
      IBOV+
      # PG+
      # GRANDES_BANCOS+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PREV+
      TRIB_LPRAZO+
      FUNDO_ESPELHO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_mm_trimestre_reg, 
    model="random") -> plm_mm_fixo

summary(plm_mm_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE,1)*GRANDES_BANCOS*PG+
      lag(RENTABILIDADE,2)*GRANDES_BANCOS*PG+
      IRF_M+
      IMA_B+
      IMA_S+
      SELIC+
      IBOV+
      VART_IBOV+
      IPCA+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PEQUENO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_mm_trimestre_reg, 
    model="random") -> plm_mm_fixo

summary(plm_mm_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE,2)+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG*GIGANTE+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      log(SELIC)*PG+
      RENTABILIDADE_ANO*PG+
      log(VL_PATRIM_LIQ)+
      log(IBOV)+
      log(IPCA)+
      log(EXPEC_IPCA_12M)+
      log(Expec.SELIC)+
      log(USDBRL)+
      log(Expec.CAMBIO)+
      log(IBC_Br)+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_mm_trimestre_reg, 
    model="random") -> plm_mm_fixo

summary(plm_mm_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE,2)*PG+
      lag(RENTABILIDADE,2)*IQ+
      lag(RENTABILIDADE,1)*TRIB_LPRAZO+
      lag(RENTABILIDADE,1)*GRANDES_BANCOS+
      lag(RENTABILIDADE,1)*GRANDES_BANCOS*PG*GIGANTE+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      log(SELIC)*PG+
      RENTABILIDADE_ANO*PG+
      RENTABILIDADE_ANO*IQ+
      log(IBOV)+
      log(IPCA)+
      log(EXPEC_IPCA_12M)+
      log(Expec.SELIC)+
      log(USDBRL)+
      log(Expec.CAMBIO)+
      log(IBC_Br)+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_mm_trimestre_reg, 
    model="random") -> plm_mm_fixo

summary(plm_mm_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC4"))

plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE,2)*PG+
      lag(RENTABILIDADE,2)*IQ+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG*GIGANTE+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      CDI*PG+
      RENTABILIDADE_ANO*PG+
      RENTABILIDADE_ANO*IQ+
      IBOV+
      IPCA+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_mm_trimestre_reg, 
    model="random") -> plm_mm_fixo

summary(plm_mm_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

###### Semestral ######

dados_painel_mm_semestre |> 
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(DANOS = as.factor(SEMESTRE)) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VARS_IBOV = `VAR SEMESTRAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VARS_DOLAR = `VAR SEMESTRAL(%) Dólar`,
         VARS_EXPEC_IPCA = `VAR SEMESTRAL(%) Expectativa IPCA`,
         VARS_EXPEC_DOLAR = `VAR SEMESTRAL(%) Expec.CAMBIO`,
         VARS_EXPEC_IBC = `VAR SEMESTRAL(%) IBC-Br`) -> dados_painel_mm_semestre_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE,1)*GRANDES_BANCOS+
      lag(RENTABILIDADE,2)*GRANDES_BANCOS+
      IRF_M+
      IMA_B+
      IMA_S+
      CDI+
      IPCA+
      USDBRL+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      Expec.CAMBIO+
      IBC_Br+
      IBOV+
      # PG+
      # GRANDES_BANCOS+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PREV+
      TRIB_LPRAZO+
      FUNDO_ESPELHO,
    index = c("CNPJ_FUNDO","SEMESTRE"),
    data=dados_painel_mm_semestre_reg, 
    model="random") -> plm_mm_fixo

summary(plm_mm_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

###### Anual ###### 

dados_painel_rf_ano |> 
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(DANOS = as.factor(ANO)) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VARA_IBOV = `VAR ANUAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VARA_DOLAR = `VAR ANUAL(%) Dólar`,
         VARA_EXPEC_IPCA = `VAR ANUAL(%) Expectativa IPCA`,
         VARA_EXPEC_DOLAR = `VAR ANUAL(%) Expec.CAMBIO`,
         VARA_EXPEC_IBC = `VAR ANUAL(%) IBC-Br`) -> dados_painel_rf_ano_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE)*PG+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*FUNDO_COTAS+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG+
      lag(RENTABILIDADE)*GRANDES_BANCOS*IQ+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      SELIC*PG+
      IBOV*PG+
      VARA_IBOV*PG+
      IPCA*PG+
      EXPEC_IPCA_12M+
      VARA_EXPEC_IPCA+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      VARA_DOLAR+
      VARA_EXPEC_DOLAR+
      VARA_EXPEC_IBC+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PEQUENO,
    index = c("CNPJ_FUNDO","ANO"),
    data=dados_painel_rf_ano_reg, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo)
summary(plm_rf_fixo, vcov = function(x) vcovHC(x, method="arellano", type="HC1"))

##### Ações #####

###### Trimestral ######

dados_painel_acoes_trimestre |> 
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(CAPTC_LIQ_ACUM = cumsum(CAPTC_LIQ)) |>
  mutate(RENTABILIDADE_ANO = (roll_trimestre(((RENTABILIDADE/
                                                 100)+1))-1)*100) |> 
  mutate(SUP = case_when(lag(RENTABILIDADE)>`VAR TRIMESTRAL(%) CDI` ~ 1,
                         T ~ 0)) |> 
  mutate(DANOS = as.factor(TRIMESTRE)) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VART_IBOV = `VAR TRIMESTRAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VART_DOLAR = `VAR TRIMESTRAL(%) Dólar`,
         VART_EXPEC_IPCA = `VAR TRIMESTRAL(%) Expectativa IPCA`,
         VART_EXPEC_DOLAR = `VAR TRIMESTRAL(%) Expec.CAMBIO`,
         VART_EXPEC_IBC = `VAR TRIMESTRAL(%) IBC-Br`,
         VART_CDI = `VAR TRIMESTRAL(%) CDI`,
         VART_IMAB = `VAR TRIMESTRAL(%) IMA-B`,
         VART_IRFM = `VAR TRIMESTRAL(%) IRF-M`,
         VART_IMAS = `VAR TRIMESTRAL(%) IMA-S`) |> 
  mutate(OLR = case_when(RENTABILIDADE > 50 ~ "Fora",
                         RENTABILIDADE < -50 ~ "Fora",
                         T ~ ""),
         OLC = case_when(CAPTC_LIQ > 10000 ~ "Fora",
                         CAPTC_LIQ < -10000 ~ "Fora",
                         T ~ "")) -> dados_painel_acoes_trimestre_reg

dados_painel_acoes_trimestre_reg |> 
  filter(TRIMESTRE >= "2006 Q1") |> 
  filter(!CNPJ_FUNDO %in% unique(filter(dados_painel_acoes_trimestre_reg,
                                        OLR == "Fora")$CNPJ_FUNDO)) |>
  filter(!CNPJ_FUNDO %in% unique(filter(dados_painel_acoes_trimestre_reg,
                                        OLC == "Fora")$CNPJ_FUNDO)) |>
  select(-OLR,-OLC) -> dados_painel_acoes_trimestre_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE,1)*GRANDES_BANCOS*PG+
      lag(RENTABILIDADE,2)*GRANDES_BANCOS*PG+
      IRF_M+
      IMA_B+
      IMA_S+
      CDI+
      IPCA+
      USDBRL+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      Expec.CAMBIO+
      IBC_Br+
      IBOV+
      # PG+
      # GRANDES_BANCOS+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PREV+
      FUNDO_ESPELHO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_acoes_trimestre_reg, 
    model="random") -> plm_acoes_fixo

summary(plm_acoes_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE)+
      lag(RENTABILIDADE)+
      log(IRF_M)+
      log(IMA_B)+
      log(IMA_S)+
      CDI+
      RENTABILIDADE_ANO+
      RENTABILIDADE_ANO+
      IBOV+
      IPCA+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=dados_painel_acoes_trimestre_reg, 
    model="random") -> plm_acoes_fixo

summary(plm_acoes_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))
###### Semestral ######

dados_painel_acoes_semestre |>  
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(DANOS = as.factor(SEMESTRE)) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VARS_IBOV = `VAR SEMESTRAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VARS_DOLAR = `VAR SEMESTRAL(%) Dólar`,
         VARS_EXPEC_IPCA = `VAR SEMESTRAL(%) Expectativa IPCA`,
         VARS_EXPEC_DOLAR = `VAR SEMESTRAL(%) Expec.CAMBIO`,
         VARS_EXPEC_IBC = `VAR SEMESTRAL(%) IBC-Br`) -> dados_painel_acoes_semestre_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE)*PG+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*FUNDO_COTAS+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG+
      lag(RENTABILIDADE)*GRANDES_BANCOS*IQ+
      IRF_M*PG+
      IMA_B*PG+
      IMA_S*PG+
      SELIC*PG+
      IBOV*PG+
      VARS_IBOV*PG+
      IPCA*PG+
      EXPEC_IPCA_12M+
      VARS_EXPEC_IPCA+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      VARS_DOLAR+
      VARS_EXPEC_DOLAR+
      VARS_EXPEC_IBC+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PEQUENO,
    index = c("CNPJ_FUNDO","SEMESTRE"),
    data=dados_painel_acoes_semestre_reg, 
    model="within") -> plm_acoes_fixo

summary(plm_acoes_fixo)

###### Anual ###### 

dados_painel_acoes_ano |>  
  mutate(MICRO = if_else(TAMANHO == "Micro",1,0),
         PEQUENO = if_else(TAMANHO == "Pequeno",1,0),
         `MÉDIO` = if_else(TAMANHO == "Médio",1,0),
         GRANDE = if_else(TAMANHO == "Grande",1,0),
         GIGANTE = if_else(TAMANHO == "Gigante",1,0),
         GRANDES_BANCOS = if_else(GRUPO == "Grandes Bancos",1,0),
         IQ = if_else(PUBLICO_ALVO == "INVESTIDORES QUALIFICADOS",1,0),
         PG = if_else(PUBLICO_ALVO == "PÚBLICO EM GERAL",1,0),
         IP = if_else(PUBLICO_ALVO == "INVESTIDORES PROFISSIONAIS",1,0),
         PREV = if_else(PUBLICO_ALVO == "PREVIDENCIÁRIO",1,0)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(DANOS = as.factor(ANO)) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         IBOV = IBOV/1000) |> 
  select(-TAMANHO,
         -GRUPO,
         -TAMANHO_CAP,
         -PUBLICO_ALVO) |> 
  rename(VARA_IBOV = `VAR ANUAL(%) IBOV`,
         EXPEC_IPCA_12M = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`,
         VARA_DOLAR = `VAR ANUAL(%) Dólar`,
         VARA_EXPEC_IPCA = `VAR ANUAL(%) Expectativa IPCA`,
         VARA_EXPEC_DOLAR = `VAR ANUAL(%) Expec.CAMBIO`,
         VARA_EXPEC_IBC = `VAR ANUAL(%) IBC-Br`) -> dados_painel_acoes_ano_reg

plm(CAPTC_LIQ ~
      lag(RENTABILIDADE)*PG+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*FUNDO_COTAS+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG+
      lag(RENTABILIDADE)*GRANDES_BANCOS*IQ+
      IRF_M*PG+
      IMA_B*PG+
      IMA_S*PG+
      SELIC*PG+
      IBOV*PG+
      VARA_IBOV*PG+
      IPCA*PG+
      EXPEC_IPCA_12M+
      VARA_EXPEC_IPCA+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      VARA_DOLAR+
      VARA_EXPEC_DOLAR+
      VARA_EXPEC_IBC+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO+
      PEQUENO,
    index = c("CNPJ_FUNDO","ANO"),
    data=dados_painel_acoes_ano_reg, 
    model="within") -> plm_acoes_fixo

summary(plm_acoes_fixo)


##### Total #####

bind_rows(mutate(dados_painel_rf_trimestre_reg,CLASSIFICACAO = "RF"),
          mutate(dados_painel_mm_trimestre_reg,CLASSIFICACAO = "MM"),
          mutate(dados_painel_acoes_trimestre_reg,CLASSIFICACAO = "ACOES")) |> 
  mutate(CLASSIFICACAO = as.factor(CLASSIFICACAO)) -> plm_td_classes_trimestral_reg


plm(CAPTC_LIQ ~
      lag(CAPTC_LIQ)+
      lag(RENTABILIDADE,2)*PG+
      lag(RENTABILIDADE,2)*IQ+
      lag(RENTABILIDADE)*TRIB_LPRAZO+
      lag(RENTABILIDADE)*GRANDES_BANCOS+
      lag(RENTABILIDADE)*GRANDES_BANCOS*PG*GIGANTE+
      log(IRF_M)*PG+
      log(IMA_B)*PG+
      log(IMA_S)*PG+
      CDI*PG+
      RENTABILIDADE_ANO*PG+
      RENTABILIDADE_ANO*IQ+
      IBOV+
      IPCA+
      EXPEC_IPCA_12M+
      Expec.SELIC+
      USDBRL+
      Expec.CAMBIO+
      IBC_Br+
      GIGANTE+
      GRANDE+
      MÉDIO,
    index = c("CNPJ_FUNDO","TRIMESTRE"),
    data=plm_td_classes_trimestral_reg, 
    model="random") -> plm_td_fixo

summary(plm_td_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

lmtest::coeftest(plm_td_fixo,vcov. = vcovHC(plm_td_fixo, 
                                            method="arellano", 
                                            type="HC4",
                                            cluster = "group"))

cov <- vcovHC(plm_td_fixo, 
              method="arellano", 
              type="HC4",
              cluster = "group")
robust.se <- sqrt(diag(cov))

stargazer(plm_td_fixo,
          se = list(robust.se),
          title="Results", 
          align=TRUE,
          column.sep.width = "1pt",
          font.size = "small",  
          single.row=TRUE)


plm_td_classes_trimestral_reg |> 
  tidyr::pivot_wider(names_from = CLASSIFICACAO,
                     values_from = CAPTC_LIQ,
                     names_prefix = "CAPTC_LIQ_") -> plm_td_classes_trimestral_reg_pvar

var_fundos_pvar <- pvargmm(
  dependent_vars = c("CAPTC_LIQ_RF","CAPTC_LIQ_ACOES"),
  lags = 1,
  exog_vars = c("IBOV"),
  transformation = "fd",
  data = data.frame(plm_td_classes_trimestral_reg_pvar),
  panel_identifier = c("CNPJ_FUNDO","TRIMESTRE"),
  steps = "twostep",
  system_instruments = T,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = F,
  progressbar = T)

bind_rows(mutate(dados_painel_acoes,CLASSIFICACAO = "ACOES"),
          mutate(dados_painel_mm,CLASSIFICACAO = "MM"),
          mutate(dados_painel_rf,CLASSIFICACAO = "RF")) -> plm_td_classes_semestral_reg_pvar



plm_td_classes_semestral_reg_pvar |> 
  mutate(COMPETENCIA = as.yearmon(DT_COMPTC)) |> 
  group_by(COMPETENCIA ,CLASSIFICACAO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  dplyr::select(-CAPTC_DIA,-RESG_DIA) |> 
  mutate(CAPTC_LIQ = CAPTC_LIQ/1000000000) |> 
  tidyr::pivot_wider(names_from = CLASSIFICACAO,
                     values_from = CAPTC_LIQ,
                     names_prefix = "CAPTC_LIQ_") -> plm_td_classes_semestral_reg_pvar

plm_td_classes_semestral_reg_pvar |> 
  dplyr::select(COMPETENCIA,CAPTC_LIQ_ACOES,CAPTC_LIQ_MM,CAPTC_LIQ_RF) -> var_teste

var_teste |> 
  ungroup() |> 
  # mutate(TRIMESTRE = yearquarter(TRIMESTRE)) |> 
  # mutate(TRIMESTRE = as.Date(TRIMESTRE)) |> 
  timetk::tk_xts() -> var_teste

plm_td_classes_semestral_reg_pvar |> 
  dplyr::select(TRIMESTRE,CDI,IBOV,USDBRL,Expec.CAMBIO) |> 
  mutate(across(where(is.numeric),~log(.x))) |> 
  ungroup() |> 
  mutate(TRIMESTRE = yearquarter(TRIMESTRE)) |> 
  mutate(TRIMESTRE = as.Date(TRIMESTRE)) |> 
  timetk::tk_xts() -> exotest

VARselect(var_teste)
summary(VAR(var_teste,p=1))

var_fundos_pvar <- pvargmm(
  dependent_vars = c("CAPTC_LIQ_RF","CAPTC_LIQ_MM","CAPTC_LIQ_ACOES"),
  lags = 2,
  # exog_vars = c("CDI"),
  transformation = "fd",
  data = data.frame(plm_td_classes_semestral_reg_pvar),
  panel_identifier = c("TAMANHO","SEMESTRE"),
  steps = "twostep",
  system_instruments = T,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = F,
  progressbar = T)

##### Modelo Geral #####


variaveis_macro_trimestral |> 
  select(TRIMESTRE,IMA_S,IMA_B,IRF_M,SELIC) |> 
  mutate(IMA_S = cumprod(c(0,diff(log(IMA_S)))+1),
         IMA_B = cumprod(c(0,diff(log(IMA_B)))+1),
         IRF_M = cumprod(c(0,diff(log(IRF_M)))+1),
         SELIC = SELIC) |> 
  mutate(TRIMESTRE = yearquarter(TRIMESTRE)) |> 
  ggplot() +
  geom_line(aes(TRIMESTRE,IMA_S),colour = "blue") +
  geom_line(aes(TRIMESTRE,IMA_B),colour = "red") +
  geom_line(aes(TRIMESTRE,IRF_M),colour = "orange") +
  geom_line(aes(TRIMESTRE,SELIC),colour = "black")


