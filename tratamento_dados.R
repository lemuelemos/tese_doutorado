board <- board_folder("dados_tese/",versioned = T)

cad_fi <- pin_read(board,"cad_fi")
extrato_fi_atual <- pin_read(board,"extrato_fi_atual")


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
  # select(CNPJ_FUNDO,contains(".x"),contains(".y"),FUNDO_ESPELHO) |> 
  filter(!((FUNDO_COTAS.x == "S" & FUNDO_COTAS.y == "N") | (FUNDO_COTAS.x == "N" & FUNDO_COTAS.y == "S"))) |> 
  filter(!(CLASSE == "Fundo de Ações" & str_detect(CLASSE_ANBIMA,"RENDA FIXA"))) |> 
  filter(!(CLASSE == " Renda Fixa" & str_detect(CLASSE_ANBIMA,"AÇÕES"))) |>
  filter(CLASSE %in% c("Fundo de Ações","Fundo Multimercado","Fundo de Renda Fixa")) 

tibble(CPF_CNPJ_GESTOR = BANCOES_VEREJO,
       GRUPO = "Grandes Bancos")

informe_diario_fundos_multimercado |> 
  select(DT_COMPTC,CPF_CNPJ_GESTOR,CAPTC_DIA,RESG_DIA) |> 
  left_join(tibble(CPF_CNPJ_GESTOR = BANCOES_VEREJO,
                   GRUPO = "Grandes Bancos"), 
            by = "CPF_CNPJ_GESTOR") |> 
  mutate(GRUPO = case_when(is.na(GRUPO) ~ "Demais Instituições",
                           T ~ GRUPO)) |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência,GRUPO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA)) |> 
  group_by(Competência,GRUPO) |> 
  mutate(CAPTC_LIQ = (CAPTC_DIA-RESG_DIA)/100000000)  |> 
  ggplot(aes(CAPTC_LIQ,color=GRUPO,fill=GRUPO)) +
  geom_histogram(bins = 10,
                 position="identity",
                 alpha=0.5) -> hist_multi


hist_multi +
  scale_color_grey() +
  scale_fill_grey() +
  theme_classic()

informe_diario_fundos_rf |> 
  select(DT_COMPTC,CPF_CNPJ_GESTOR,CAPTC_DIA,RESG_DIA) |> 
  left_join(tibble(CPF_CNPJ_GESTOR = BANCOES_VEREJO,
                   GRUPO = "Grandes Bancos"), 
            by = "CPF_CNPJ_GESTOR") |> 
  mutate(GRUPO = case_when(is.na(GRUPO) ~ "Demais Instituições",
                           T ~ GRUPO)) |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência,GRUPO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA)) |> 
  group_by(Competência,GRUPO) |> 
  mutate(CAPTC_LIQ = (CAPTC_DIA-RESG_DIA)/100000000)  |> 
  ggplot(aes(CAPTC_LIQ,color=GRUPO,fill=GRUPO)) +
  geom_histogram(bins = 10,
                 position="identity",
                 alpha=0.5) -> hist_rf



hist_rf +
  scale_color_grey() +
  scale_fill_grey() +
  theme_classic()



informe_diario_fundos_acoes |> 
  select(DT_COMPTC,CPF_CNPJ_GESTOR,CAPTC_DIA,RESG_DIA) |> 
  left_join(tibble(CPF_CNPJ_GESTOR = BANCOES_VEREJO,
                   GRUPO = "Grandes Bancos"), 
            by = "CPF_CNPJ_GESTOR") |> 
  mutate(GRUPO = case_when(is.na(GRUPO) ~ "Demais Instituições",
                           T ~ GRUPO)) |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência,GRUPO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA)) |> 
  group_by(Competência,GRUPO) |> 
  mutate(CAPTC_LIQ = (CAPTC_DIA-RESG_DIA)/100000000)  |> 
  ggplot(aes(CAPTC_LIQ,color=GRUPO,fill=GRUPO)) +
  geom_histogram(bins = 10,
                 position="identity",
                 alpha=0.5) -> hist_acoes

hist_acoes +
  scale_color_grey() +
  scale_fill_grey() +
  theme_classic() +
  labs(title="Captação Líquida Fundos de Ações",
       x="Captação Líquida - Valores em 100 milhões", 
       y = "Quantidade de Fundos")


informe_diario_fundos_rf |> 
  select(DT_COMPTC,CPF_CNPJ_GESTOR,CNPJ_FUNDO,CAPTC_DIA,RESG_DIA) |> 
  left_join(tibble(CPF_CNPJ_GESTOR = BANCOES_VEREJO,
                   GRUPO = "Grandes Bancos"), 
            by = "CPF_CNPJ_GESTOR") |> 
  mutate(GRUPO = case_when(is.na(GRUPO) ~ "Demais Instituições",
                           T ~ GRUPO)) |> 
  mutate(Competência = yearmonth(DT_COMPTC)) |> 
  group_by(Competência,CNPJ_FUNDO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA)) |> 
  group_by(Competência,CNPJ_FUNDO) |>
  mutate(CAPTC_LIQ = (CAPTC_DIA-RESG_DIA)/1000) |> 
  filter(CAPTC_LIQ != 0) -> hist_fundos


informe_diario_fundos_multimercado 
informe_diario_fundos_rf

informe_diario_fundos_multimercado |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_PATRIM_LIQ) |> 
  lazy_dt() |>
  mutate(ANO = lubridate::year(DT_COMPTC)) |> 
  group_by(ANO,CNPJ_FUNDO) |> 
  summarise_all(dplyr::first) |>  
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 & VL_PATRIM_LIQ < 50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 & VL_PATRIM_LIQ < 250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 ~ "Grande",
                             T ~ "Micro")) |> 
  select(-VL_PATRIM_LIQ,-DT_COMPTC) |> 
  ungroup() |>
  collect() -> fundos_mm_tamanho


fundos_mm_tamanho |> 
  count(ANO,TAMANHO) |> 
  group_by(ANO) |> 
  mutate(`Part(%)` = round((n/sum(n))*100,2)) |> 
  select(-n) |> 
  tidyr::pivot_wider(names_from = ANO,values_from = `Part(%)`) 

informe_diario_fundos_multimercado |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_PATRIM_LIQ) |> 
  lazy_dt() |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 & VL_PATRIM_LIQ < 50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 & VL_PATRIM_LIQ < 250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 ~ "Grande",
                             T ~ "Micro")) |> 
  ungroup() |> 
  collect() |>
  count(CNPJ_FUNDO,TAMANHO) |>
  tidyr::pivot_wider(names_from = TAMANHO,values_from = n,values_fill = 0) |> 
  filter(Grande > 0 & Micro == 0,Pequeno == 0,Médio == 0) -> fundos_n_mudam_tamanho_grande


informe_diario_fundos_multimercado |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_PATRIM_LIQ) |> 
  lazy_dt() |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 & VL_PATRIM_LIQ < 50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 & VL_PATRIM_LIQ < 250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 ~ "Grande",
                             T ~ "Micro")) |> 
  ungroup() |> 
  collect() |>
  count(CNPJ_FUNDO,TAMANHO) |>
  tidyr::pivot_wider(names_from = TAMANHO,values_from = n,values_fill = 0) |> 
  filter(Grande == 0 & Micro > 0,Pequeno == 0,Médio ==0) |> 
  bind_rows(fundos_n_mudam_tamanho) -> fundos_n_mudam_tamanho


informe_diario_fundos_multimercado |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_PATRIM_LIQ) |> 
  lazy_dt() |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 & VL_PATRIM_LIQ < 50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 & VL_PATRIM_LIQ < 250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 ~ "Grande",
                             T ~ "Micro")) |> 
  ungroup() |> 
  collect() |>
  count(CNPJ_FUNDO,TAMANHO) |>
  tidyr::pivot_wider(names_from = TAMANHO,values_from = n,values_fill = 0) |> 
  filter(Grande == 0 & Micro == 0,Pequeno > 0,Médio ==0) |> 
  bind_rows(fundos_n_mudam_tamanho) -> fundos_n_mudam_tamanho

informe_diario_fundos_multimercado |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_PATRIM_LIQ) |> 
  lazy_dt() |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 & VL_PATRIM_LIQ < 50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 & VL_PATRIM_LIQ < 250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 ~ "Grande",
                             T ~ "Micro")) |> 
  ungroup() |> 
  collect() |>
  count(CNPJ_FUNDO,TAMANHO) |>
  tidyr::pivot_wider(names_from = TAMANHO,values_from = n,values_fill = 0) |> 
  filter(Grande == 0 & Micro == 0,Pequeno == 0,Médio > 0) |> 
  bind_rows(fundos_n_mudam_tamanho)-> fundos_n_mudam_tamanho


fundos_n_mudam_tamanho |> 
  tidyr::pivot_longer(names_to = "TAMANHO",cols = Micro:Grande) |> 
  select(CNPJ_FUNDO,TAMANHO) |> 
  distinct_all() -> fundos_n_mudam_tamanho

informe_diario_fundos_multimercado |> 
  select(DT_COMPTC,VL_PATRIM_LIQ,VL_TOTAL,CAPTC_DIA,RESG_DIA) |> 
  group_by(DT_COMPTC) |> 
  summarise(VL_PATRIM_LIQ = sum(VL_PATRIM_LIQ,na.rm = T),
            CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA)) |> 
  filter(VL_PATRIM_LIQ > 0) |> 
  mutate(VL_PATRIM_LIQ = VL_PATRIM_LIQ/1000000,
         CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(CAPTC_LIQ_ACUM = cumsum(CAPTC_LIQ)/1000000) |> 
  ggplot(aes(x=DT_COMPTC)) +
  geom_line(aes(y=VL_PATRIM_LIQ)) +

    geom_line(aes(y=CAPTC_LIQ_ACUM))




informe_diario_fundos_multimercado |> 
  select(DT_COMPTC,
         CNPJ_FUNDO,
         TRIB_LPRAZO,
         REG_ANBIMA,
         CLASSE,
         CLASSE_ANBIMA,
         MERCADO,
         PUBLICO_ALVO,
         FUNDO_COTAS.x,
         EXISTE_TAXA_PERFM,
         TAXA_PERFM,
         CPF_CNPJ_GESTOR,
         PF_PJ_GESTOR,
         ENTID_INVEST,
         CONDOM,
         FUNDO_ESPELHO)



dados_painel_acoes_semestre |>
  ungroup() |> 
  select(SEMESTRE,CNPJ_FUNDO,TAMANHO) |> 
  mutate(GIGANTE = case_when(TAMANHO == "Gigante" ~ 1,
                             T ~ 0)) |> 
  mutate(GRANDE = case_when(TAMANHO == "Grande" ~ 1,
                             T ~ 0)) |> 
  mutate(MEDIO = case_when(TAMANHO == "Médio" ~ 1,
                             T ~ 0)) |> 
  mutate(PEQUENO = case_when(TAMANHO == "Pequeno" ~ 1,
                             T ~ 0))

dados_painel_acoes_semestre |> 
  ungroup() |> 
  select(SEMESTRE,CNPJ_FUNDO,GRUPO) |>
  mutate(BCO_GRANDE = case_when(GRUPO == "Grandes Bancos" ~ 1,
                             T ~ 0)) |> 
  mutate(DEM_INST = case_when(GRUPO == "Demais Instituições" ~ 1,
                            T ~ 0))
  
dados_painel_acoes_semestre |> 
  group_by(CNPJ_FUNDO) |> 
  summarise(Qtd = n()) |> 
  filter(Qtd == 33) -> fundos_periodo_completo 


dados_painel_acoes |> 
  lazy_dt() |> 
  filter(CONDOM == "Aberto") |>
  mutate(`MÊS` = as.character(`MÊS`)) |>
  select(-SEMESTRE,-ANO,-TRIMESTRE,-DT_COMPTC) |> 
  group_by(`MÊS`,CNPJ_FUNDO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                             VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                             VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                             VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  # filter(TAMANHO == "Micro") |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000   & CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000  & CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000   & CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  collect() |> 
  ungroup() |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) -> dados_painel_acoes_mes2
  

dados_painel_acoes_mes2 |> 
  filter(TAMANHO == "Gigante") |>
  mutate(CAPTC_LIQ = CAPTC_LIQ/1000000) |> 
  ggplot(aes(CAPTC_LIQ)) +
  geom_histogram(bins = 5,
                 position="identity",
                 alpha=0.5)

###########################################################################################################

dados_painel_acoes |> 
  lazy_dt() |> 
  select(-SEMESTRE,-ANO,-MÊS) |> 
  group_by(`TRIMESTRE`,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ),
            RENTABILIDADE = (dplyr::last(VL_QUOTA)/
                               dplyr::first(VL_QUOTA))-1,
            FUNDO_COTAS = dplyr::first(FUNDO_COTAS),
            PUBLICO_ALVO = dplyr::first(PUBLICO_ALVO),
            EXISTE_TAXA_PERFM = dplyr::first(EXISTE_TAXA_PERFM),
            CLASSE_ANBIMA = dplyr::first(CLASSE_ANBIMA)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                             VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                             VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                             VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  # filter(TAMANHO == "Micro") |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000   & CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000  & CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000   & CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  collect() |> 
  ungroup() |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) -> dados_painel_acoes_trimestre2


dados_painel_acoes_trimestre2 |> 
  filter(TAMANHO == "Médio") |>
  mutate(CAPTC_LIQ = CAPTC_LIQ/1000000) |> 
  ggplot(aes(CAPTC_LIQ)) +
  geom_histogram(bins = 10,
                 position="identity",
                 alpha=0.5)
  
######################################################################################################################

dados_painel_acoes |> 
  lazy_dt() |> 
  select(-TRIMESTRE,-ANO,-MÊS,-DT_COMPTC) |> 
  group_by(`SEMESTRE`,CNPJ_FUNDO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            GRUPO = dplyr::last(GRUPO),
            VL_PATRIM_LIQ = dplyr::first(VL_PATRIM_LIQ)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  mutate(TAMANHO = case_when(VL_PATRIM_LIQ >= 15000000 &
                             VL_PATRIM_LIQ <  50000000 ~ "Pequeno",
                             VL_PATRIM_LIQ >= 50000000 &
                             VL_PATRIM_LIQ <  250000000 ~ "Médio",
                             VL_PATRIM_LIQ >= 250000000 &
                             VL_PATRIM_LIQ <  1000000000 ~ "Grande",
                             VL_PATRIM_LIQ >= 1000000000 ~ "Gigante",
                             T ~ "Micro")) |> 
  mutate(TAMANHO_CAP = case_when(CAPTC_LIQ >= 15000000   & CAPTC_LIQ <   50000000 ~ "Pequeno",
                                 CAPTC_LIQ <= -15000000  & CAPTC_LIQ >  -50000000 ~ "Pequeno",
                                 CAPTC_LIQ >= 50000000   & CAPTC_LIQ <   250000000 ~ "Médio",
                                 CAPTC_LIQ <= -50000000  & CAPTC_LIQ >  -250000000 ~ "Médio",
                                 CAPTC_LIQ >= 250000000  & CAPTC_LIQ <   1000000000 ~ "Grande",
                                 CAPTC_LIQ <= -250000000 & CAPTC_LIQ >  -1000000000 ~ "Grande",
                                 CAPTC_LIQ >= 1000000000 | CAPTC_LIQ <= -1000000000 ~ "Gigante",
                                 T ~ "Micro")) |> 
  collect() |> 
  ungroup() |> 
  mutate(TAMANHO = case_when(TAMANHO == "Grande" & TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & TAMANHO_CAP == "Gigante" ~ "Gigante",
                             TAMANHO == "Médio" & TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & TAMANHO_CAP == "Grande" ~ "Grande",
                             TAMANHO == "Pequeno" & TAMANHO_CAP == "Médio" ~ "Médio",
                             TAMANHO == "Micro" & TAMANHO_CAP != "Micro" ~ TAMANHO_CAP,
                             T ~ TAMANHO)) -> dados_painel_acoes_semestre2



