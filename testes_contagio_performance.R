informe_diario_fundos_multimercado |> 
  filter(CNPJ_FUNDO == '00.888.897/0001-31') |> 
  select(DT_COMPTC,VL_QUOTA) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |> 
  group_by(Competencia) |> 
  arrange(DT_COMPTC) |> 
  summarise(DT_COMPTC = dplyr::last(DT_COMPTC),
            RETORNO = (dplyr::last(VL_QUOTA)/dplyr::first(VL_QUOTA))-1,
            VOLATILIDADE = sd(VL_QUOTA)) |> 
  select(-DT_COMPTC) |> 
  left_join(mutate(select(filter(taxa_performance_fundos,
                                 CNPJ_FUNDO == "00.888.897/0001-31"),
                          DT_COMPTC,
                          VL_SALDO_BALCTE),
                   Competencia = yearmonth(DT_COMPTC))) |> 
  na.omit() |> 
  mutate(VL_SALDO_BALCTE = (VL_SALDO_BALCTE-mean(VL_SALDO_BALCTE))/sd(VL_SALDO_BALCTE),
         VOLATILIDADE = (VOLATILIDADE-mean(VOLATILIDADE))/sd(VOLATILIDADE)) |> 
  ggplot(aes(VL_SALDO_BALCTE,VOLATILIDADE)) +
  geom_point() 


informe_diario_fundos_multimercado |>  
  filter(CONDOM == "Aberto",
         EXISTE_TAXA_PERFM == "S") |>
  lazy_dt() |> 
  filter(CNPJ_FUNDO %in% unique(taxa_performance_fundos$CNPJ_FUNDO)) |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_QUOTA) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |> 
  group_by(Competencia,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(DT_COMPTC = dplyr::last(DT_COMPTC),
            RETORNO = (dplyr::last(VL_QUOTA)/dplyr::first(VL_QUOTA))-1,
            VOLATILIDADE = sd(VL_QUOTA)) |> 
  ungroup() |> 
  left_join(mutate(select(taxa_performance_fundos,
                          DT_COMPTC,
                          CNPJ_FUNDO,
                          VL_SALDO_BALCTE),
                   Competencia = yearmonth(DT_COMPTC)),
            by = c("Competencia","CNPJ_FUNDO")) |>
  collect() |> 
  na.omit() |>
  group_by(CNPJ_FUNDO) |> 
  mutate(VL_SALDO_BALCTE = (VL_SALDO_BALCTE-mean(VL_SALDO_BALCTE))/sd(VL_SALDO_BALCTE),
         VOLATILIDADE = (VOLATILIDADE-mean(VOLATILIDADE))/sd(VOLATILIDADE)) |> 
  ggplot(aes(VL_SALDO_BALCTE,VOLATILIDADE)) +
  geom_point() 


informe_diario_fundos_acoes |> 
  filter(CONDOM == "Aberto",
         EXISTE_TAXA_PERFM == "S",
         CPF_CNPJ_GESTOR %in% BANCOES_VEREJO) |>
  lazy_dt() |> 
  filter(CNPJ_FUNDO %in% unique(taxa_performance_fundos$CNPJ_FUNDO)) |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_QUOTA) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |> 
  group_by(Competencia,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(DT_COMPTC = dplyr::last(DT_COMPTC),
            RETORNO = (dplyr::last(VL_QUOTA)/dplyr::first(VL_QUOTA))-1,
            VOLATILIDADE = sd(VL_QUOTA)) |> 
  ungroup() |> 
  left_join(mutate(select(taxa_performance_fundos,
                          DT_COMPTC,
                          CNPJ_FUNDO,
                          VL_SALDO_BALCTE),
                   Competencia = yearmonth(DT_COMPTC)),
            by = c("Competencia","CNPJ_FUNDO")) |>
  collect() |> 
  na.omit() |>
  group_by(CNPJ_FUNDO) |> 
  mutate(VL_SALDO_BALCTE = (VL_SALDO_BALCTE-mean(VL_SALDO_BALCTE))/sd(VL_SALDO_BALCTE),
         VOLATILIDADE = (VOLATILIDADE-mean(VOLATILIDADE))/sd(VOLATILIDADE)) |> 
  ggplot(aes(VL_SALDO_BALCTE,VOLATILIDADE)) +
  geom_point() 



informe_diario_fundos_rf |> 
  filter(CONDOM == "Aberto",
         EXISTE_TAXA_PERFM == "S",
         CPF_CNPJ_GESTOR %in% BANCOES_VEREJO) |> 
  lazy_dt() |> 
  filter(CNPJ_FUNDO %in% unique(taxa_performance_fundos$CNPJ_FUNDO)) |> 
  select(CNPJ_FUNDO,DT_COMPTC,VL_QUOTA) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |> 
  group_by(Competencia,CNPJ_FUNDO) |> 
  arrange(DT_COMPTC) |> 
  summarise(DT_COMPTC = dplyr::last(DT_COMPTC),
            RETORNO = (dplyr::last(VL_QUOTA)/dplyr::first(VL_QUOTA))-1,
            VOLATILIDADE = sd(VL_QUOTA)) |> 
  ungroup() |> 
  left_join(mutate(select(taxa_performance_fundos,
                          DT_COMPTC,
                          CNPJ_FUNDO,
                          VL_SALDO_BALCTE),
                   Competencia = yearmonth(DT_COMPTC)),
            by = c("Competencia","CNPJ_FUNDO")) |>
  collect() |> 
  na.omit() |>
  group_by(CNPJ_FUNDO) |> 
  mutate(VL_SALDO_BALCTE = (VL_SALDO_BALCTE-mean(VL_SALDO_BALCTE))/sd(VL_SALDO_BALCTE),
         VOLATILIDADE = (VOLATILIDADE-mean(VOLATILIDADE))/sd(VOLATILIDADE)) |> 
  ggplot(aes(VL_SALDO_BALCTE,VOLATILIDADE)) +
  geom_point() 


taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_multimercado_60_dias$CNPJ_FUNDO)) |> 
  lazy_dt() |> 
  group_by(CNPJ_FUNDO) |> 
  filter(!all(VL_SALDO_BALCTE == 0)) |> 
  mutate(Quartil1 = quantile(VL_SALDO_BALCTE,probs = 0.25),
         Quartil3 = quantile(VL_SALDO_BALCTE,probs = 0.75)) |> 
  mutate(Quartil1 = case_when(VL_SALDO_BALCTE <= Quartil1 ~ 1,
                              T ~ 0)) |> 
  mutate(Quartil3 = case_when(VL_SALDO_BALCTE >= Quartil3 ~ 1,
                              T ~ 0)) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  select(-DT_COMPTC) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_multimercado_60_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  filter(Quartil3 == 1) |> 
  group_by(Competencia) |> 
  summarise(`60 Dias Antes` = mean(`60 Dias Antes`),
            `60 Dias Depois` = mean(`60 Dias Depois`)) |> 
  mutate(`Diferen??a` = `60 Dias Antes`-`60 Dias Depois`)

taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_acoes_60_dias$CNPJ_FUNDO)) |> 
  lazy_dt() |> 
  group_by(CNPJ_FUNDO) |> 
  filter(!all(VL_SALDO_BALCTE == 0)) |> 
  mutate(Quartil1 = quantile(VL_SALDO_BALCTE,probs = 0.25),
         Quartil3 = quantile(VL_SALDO_BALCTE,probs = 0.75)) |> 
  mutate(Quartil1 = case_when(VL_SALDO_BALCTE <= Quartil1 ~ 1,
                              T ~ 0)) |> 
  mutate(Quartil3 = case_when(VL_SALDO_BALCTE >= Quartil3 ~ 1,
                              T ~ 0)) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  select(-DT_COMPTC) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_acoes_60_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |> 
  filter(!is.na(Quartil)) |> 
  mutate(M??s = factor(format(DT_COMPTC,"%B"))) |>
  group_by(M??s,Quartil) |> 
  summarise(`60 Dias Antes` = mean(`60 Dias Antes`,na.rm=T),
            `60 Dias Depois` = mean(`60 Dias Depois`,na.rm=T),
            Qtd = n()) |> 
  mutate(`Diferen??a` = `60 Dias Antes`-`60 Dias Depois`) |> View()




diferenca_vol_multimercado_60_dias |> 
  na.omit() |>
  mutate(M??s = factor(format(DT_COMPTC,"%B"))) |> 
  left_join(dados_fundos_multimercados,by = "CNPJ_FUNDO") |> 
  filter(CONDOM == "Aberto") |> 
  group_by(M??s,EXISTE_TAXA_PERFM)





taxa_performance_fundos |> 
  filter(CNPJ_FUNDO == "00.838.266/0001-08") |> 
  filter(!all(VL_SALDO_BALCTE == 0))

