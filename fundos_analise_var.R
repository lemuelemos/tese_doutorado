#### Pareamento com a Base de Informes diários #### 
informe_diario_fundos_acoes
informe_diario_fundos_multimercado
informe_diario_fundos_rf



bind_rows(informe_diario_fundos_acoes,
          informe_diario_fundos_multimercado,
          informe_diario_fundos_rf) |> 
  lazy_dt() |> 
  group_by(DT_COMPTC,CLASSE) |> 
  summarise(RESG_DIA = sum(RESG_DIA),
            CAPTC_DIA = sum(CAPTC_DIA)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  select(-RESG_DIA,-CAPTC_DIA) |> 
  tidyr::pivot_wider(names_from = CLASSE,values_from = CAPTC_LIQ) |> 
  collect() |> 
  ungroup() |> 
  corrr::correlate(method = "spearman") |> 
  corrr::rearrange() %>% 
  corrr::shave()
  
bind_rows(informe_diario_fundos_acoes,
          informe_diario_fundos_multimercado,
          informe_diario_fundos_rf) |> 
  lazy_dt() |> 
  mutate(TRIMESTRE  = yearquarter(DT_COMPTC)) |> 
  group_by(TRIMESTRE ,CLASSE) |> 
  summarise(RESG_DIA = sum(RESG_DIA),
            CAPTC_DIA = sum(CAPTC_DIA)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |> 
  dplyr::select(-RESG_DIA,-CAPTC_DIA) |> 
  tidyr::pivot_wider(names_from = CLASSE,values_from = CAPTC_LIQ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(dplyr::select(variaveis_macro_trimestral,
                          -DataReferencia),
                   TRIMESTRE = yearquarter(TRIMESTRE))) |> 
  correlation::correlation(method = "spearman", 
                           p_adjust = "none") |> 
  as_tibble() |> 
  filter(Parameter1 %in% c("Fundo Multimercado",
                     "Fundo de Ações",
                     "Fundo de Renda Fixa")) |> 
  arrange(Parameter1,Parameter2) |> 
  print(n=75)



bind_rows(informe_diario_fundos_acoes,
          informe_diario_fundos_multimercado,
          informe_diario_fundos_rf) |> 
  lazy_dt() |> 
  mutate(TRIMESTRE  = yearquarter(DT_COMPTC)) |> 
  group_by(TRIMESTRE ,CLASSE) |> 
  summarise(RESG_DIA = sum(RESG_DIA),
            CAPTC_DIA = sum(CAPTC_DIA)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA) |>
  mutate(CAPTC_LIQ = CAPTC_LIQ/1000000) |>
  dplyr::select(-RESG_DIA,-CAPTC_DIA) |> 
  tidyr::pivot_wider(names_from = CLASSE,values_from = CAPTC_LIQ) |> 
  left_join(mutate(dplyr::select(variaveis_macro_trimestral,
                          -DataReferencia),
                   TRIMESTRE = yearquarter(TRIMESTRE))) |> 
  collect() -> dados_estimacao_var

dados_estimacao_var |> 
  dplyr::select(TRIMESTRE,
                `Fundo de Renda Fixa`,
                `Fundo de Ações`,
                `Fundo Multimercado`,
                IPCA,
                CDI,
                IBOV,
                USDBRL) |> 
  mutate(IPCA = c(NA_integer_,diff(log(IPCA))),
         CDI = c(NA_integer_,diff(log(CDI))),
         IBOV = c(NA_integer_,diff(log(IBOV))),
         USDBRL = c(NA_integer_,diff(log(USDBRL)))) |> 
  mutate(TRIMESTRE = zoo::as.yearqtr(as.character(TRIMESTRE))) |> 
  timetk::tk_xts() -> dados_estimacao_var

VARselect(dados_estimacao_var[,1:3], lag.max = 24)

VAR(dados_estimacao_var[,1:3],p = 12,exogen = dados_estimacao_var[,4:7]) |> 
  summary()
