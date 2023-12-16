dados_painel_acoes |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |> 
  select(-ANO,-SEMESTRE,-TRIMESTRE) |> 
  group_by(COMPETENCIA,CNPJ_FUNDO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            TRIB_LPRAZO = dplyr::last(TRIB_LPRAZO),
            PUBLICO_ALVO = dplyr::last(PUBLICO_ALVO),
            VL_QUOTA = dplyr::last(VL_QUOTA),
            GRUPO = dplyr::last(GRUPO)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA,
         PUBLICO_ALVO = as.factor(PUBLICO_ALVO)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(CAPTC_LIQ_ACUM = cumsum(CAPTC_LIQ),
         RETORNO = ((VL_QUOTA/dplyr::lag(VL_QUOTA))-1)*100) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         CAPTC_LIQ_ACUM = CAPTC_LIQ_ACUM/1000000) -> dados_fundos_teste_acoes


dados_painel_mm |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |> 
  select(-ANO,-SEMESTRE,-TRIMESTRE) |> 
  group_by(COMPETENCIA,CNPJ_FUNDO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            TRIB_LPRAZO = dplyr::last(TRIB_LPRAZO),
            PUBLICO_ALVO = dplyr::last(PUBLICO_ALVO),
            VL_QUOTA = dplyr::last(VL_QUOTA),
            GRUPO = dplyr::last(GRUPO)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA,
         PUBLICO_ALVO = as.factor(PUBLICO_ALVO)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(CAPTC_LIQ_ACUM = cumsum(CAPTC_LIQ),
         RETORNO = ((VL_QUOTA/dplyr::lag(VL_QUOTA))-1)*100) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         CAPTC_LIQ_ACUM = CAPTC_LIQ_ACUM/1000000) -> dados_fundos_teste_mm

dados_painel_rf |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |> 
  select(-ANO,-SEMESTRE,-TRIMESTRE) |> 
  group_by(COMPETENCIA,CNPJ_FUNDO) |> 
  summarise(CAPTC_DIA = sum(CAPTC_DIA),
            RESG_DIA = sum(RESG_DIA),
            TRIB_LPRAZO = dplyr::last(TRIB_LPRAZO),
            PUBLICO_ALVO = dplyr::last(PUBLICO_ALVO),
            VL_QUOTA = dplyr::last(VL_QUOTA),
            GRUPO = dplyr::last(GRUPO)) |> 
  mutate(CAPTC_LIQ = CAPTC_DIA-RESG_DIA,
         PUBLICO_ALVO = as.factor(PUBLICO_ALVO)) |> 
  group_by(CNPJ_FUNDO) |> 
  mutate(CAPTC_LIQ_ACUM = cumsum(CAPTC_LIQ),
         RETORNO = ((VL_QUOTA/dplyr::lag(VL_QUOTA))-1)*100) |> 
  ungroup() |> 
  mutate(CAPTC_DIA = CAPTC_DIA/1000000,
         RESG_DIA = RESG_DIA/1000000,
         CAPTC_LIQ = CAPTC_LIQ/1000000,
         CAPTC_LIQ_ACUM = CAPTC_LIQ_ACUM/1000000) -> dados_fundos_teste_rf



pin_read(board,"dolar") |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  group_by(COMPETENCIA) |> 
  arrange(DT_COMPTC) |> 
  summarise(`VAR MENSAL(%) Dólar` = (dplyr::last(USDBRL)/
                                           dplyr::first(USDBRL))-1,
            USDBRL = dplyr::last(USDBRL)) -> dolar_mensal

pin_read(board,"IRF_M") |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  group_by(COMPETENCIA) |> 
  summarise(`VAR MENSAL(%) IRF-M` = (dplyr::last(IRF_M)/
                                           dplyr::first(IRF_M))-1,
            IRF_M = dplyr::last(IRF_M)) -> IRF_M_mensal

pin_read(board,"IMA_S") |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  group_by(COMPETENCIA) |>  
  summarise(`VAR MENSAL(%) IMA-S` = (dplyr::last(IMA_S)/
                                           dplyr::first(IMA_S))-1,
            IMA_S = dplyr::last(IMA_S)) -> IMA_S_mensal

pin_read(board,"IMA_B") |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  group_by(COMPETENCIA) |> 
  summarise(`VAR MENSAL(%) IMA-B` = (dplyr::last(IMA_B)/
                                           dplyr::first(IMA_B))-1,
            IMA_B = dplyr::last(IMA_B)) -> IMA_B_mensal

pin_read(board,"IBOV") |> 
  mutate(COMPETENCIA = yearmonth(COMPETENCIA)) |>
  arrange(COMPETENCIA) |> 
  group_by(COMPETENCIA) |> 
  summarise(`VAR MENSAL(%) IBOV` = (dplyr::last(IBOV)/
                                          dplyr::first(IBOV))-1,
            IBOV = dplyr::last(IBOV)) -> ibov_mensal

pin_read(board,"IPCA") |> 
  filter(ANO > 2004) |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  group_by(COMPETENCIA) |>  
  arrange(DT_COMPTC) |> 
  summarise(`VAR MENSAL(%) IPCA` = (dplyr::last(IPCA)/
                                          dplyr::first(IPCA))-1,
            IPCA = dplyr::last(IPCA)) -> ipca_mensal


pin_read(board,"expectativa_inflacao_12M") |> 
  filter(ANO > 2004) |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  group_by(COMPETENCIA) |>  
  arrange(DT_COMPTC) |> 
  summarise(`VAR MENSAL(%) Expectativa IPCA` = (dplyr::last(Mediana)/dplyr::first(Mediana))-1,
            `Expectativa IPCA 12M` = dplyr::last(Mediana)) -> expectativa_ipca_12M_mensal

pin_read(board,"selic") |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  group_by(COMPETENCIA) |>  
  arrange(DT_COMPTC) |> 
  summarise(`VAR MENSAL(%) SELIC` = (dplyr::last(SELIC)/
                                           dplyr::first(SELIC))-1,
            `SELIC` = dplyr::last(SELIC)) -> selic_mensal

pin_read(board,"expectativa_selic") |> 
  filter(str_detect(Reuniao,"R8")) |> 
  mutate(COMPETENCIA = yearmonth(Data)) |>
  group_by(COMPETENCIA) |> 
  arrange(Data) |> 
  summarise_all(dplyr::last) |> 
  select(COMPETENCIA,Mediana) |> 
  mutate(`VAR MENSAL(%) Expec.SELIC` = (Mediana/
                                              dplyr::lag(Mediana))-1) |> 
  rename(`Expec.SELIC` = Mediana) -> expectativa_selic_mensal

pin_read(board,"expectativa_cambio") |> 
  filter(baseCalculo == 0) |> 
  filter(str_detect(DataReferencia,"12/")) |>
  mutate(COMPETENCIA = yearmonth(Data)) |>
  group_by(COMPETENCIA) |> 
  arrange(Data) |> 
  summarise_all(dplyr::last) |> 
  select(COMPETENCIA,Mediana) |> 
  mutate(`VAR MENSAL(%) Expec.CAMBIO` = (Mediana/
                                               dplyr::lag(Mediana))-1) |> 
  rename(`Expec.CAMBIO` = Mediana) -> expectativa_cambio_mensal

pin_read(board,"IBC_BR") |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  group_by(COMPETENCIA) |>  
  arrange(DT_COMPTC) |> 
  summarise(`VAR MENSAL(%) IBC-Br` = (dplyr::last(`IBC-Br`)/
                                            dplyr::first(`IBC-Br`))-1,
            `IBC-Br` = dplyr::last(`IBC-Br`)) -> ibc_br_mensal

pin_read(board,"CDI") |> 
  mutate(COMPETENCIA = yearmonth(DT_COMPTC)) |>
  # group_by(COMPETENCIA) |> 
  arrange(DT_COMPTC) |>  
  mutate(`VAR MENSAL(%) CDI` = ((CDI/dplyr::lag(CDI))-1)) |> 
  select(COMPETENCIA,CDI,`VAR MENSAL(%) CDI`) -> CDI_mensal

dolar_mensal |> 
  left_join(ibov_mensal) |> 
  left_join(ipca_mensal) |> 
  left_join(expectativa_ipca_12M_mensal) |> 
  left_join(selic_mensal) |> 
  left_join(expectativa_selic_mensal) |>
  left_join(expectativa_cambio_mensal) |> 
  left_join(ibc_br_mensal) |> 
  left_join(IRF_M_mensal) |>
  left_join(IMA_S_mensal) |> 
  left_join(IMA_B_mensal) |> 
  left_join(CDI_mensal) |>  
  mutate(across(starts_with("VAR"),~.x*100),
         IBOV = IBOV/1000) |> 
  rename(Expectativa_IPCA = `Expectativa IPCA 12M`,
         `IBC_Br` = `IBC-Br`) -> variaveis_macro_mensal

dados_fundos_teste_acoes |> 
  left_join(variaveis_macro_mensal) -> dados_fundos_teste_acoes

dados_fundos_teste_mm |> 
  left_join(variaveis_macro_mensal) -> dados_fundos_teste_mm

dados_fundos_teste_rf |> 
  left_join(variaveis_macro_mensal) -> dados_fundos_teste_rf

lm(CAPTC_LIQ_ACUM~
     lag(RETORNO,1)+
     lag(RETORNO,2)+
     PUBLICO_ALVO+
     Expec.SELIC+
     Expec.CAMBIO+
     Expectativa_IPCA+
     IBC_Br+
     IRF_M+
     IMA_B+
     IMA_S+
     USDBRL+
     IBOV+
     IPCA+
     CDI+
     GRUPO,data = dados_fundos_teste_acoes) -> agrupado_acoes_lin

plm(CAPTC_LIQ_ACUM~
      lag(RETORNO,1)+
      lag(RETORNO,2)+
      PUBLICO_ALVO+
      Expec.SELIC+
      Expec.CAMBIO+
      Expectativa_IPCA+
      IBC_Br+
      IRF_M+
      IMA_B+
      IMA_S+
      USDBRL+
      IBOV+
      IPCA+
      CDI+
      GRUPO,
    index = c("CNPJ_FUNDO","COMPETENCIA"),
    data=dados_fundos_teste_acoes, 
    model="random") -> plm_acoes_fixo

summary(plm_acoes_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

lmtest::coeftest(agrupado_acoes_lin,vcov = vcovHC, type = "HC0")


lm(CAPTC_LIQ_ACUM~
     lag(RETORNO,1)+
     lag(RETORNO,2)+
     PUBLICO_ALVO+
     Expec.SELIC+
     Expec.CAMBIO+
     Expectativa_IPCA+
     IBC_Br+
     IRF_M+
     IMA_B+
     IMA_S+
     USDBRL+
     IBOV+
     IPCA+
     CDI,data = dados_fundos_teste_mm) -> agrupado_mm_lin


plm(CAPTC_LIQ_ACUM~
      lag(RETORNO,1)+
      lag(RETORNO,2)+
      PUBLICO_ALVO+
      Expec.SELIC+
      Expec.CAMBIO+
      Expectativa_IPCA+
      IBC_Br+
      IRF_M+
      IMA_B+
      IMA_S+
      USDBRL+
      IBOV+
      IPCA+
      CDI,
    index = c("CNPJ_FUNDO","COMPETENCIA"),
    data=dados_fundos_teste_mm, 
    model="random") -> plm_mm_fixo

summary(plm_mm_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

lmtest::coeftest(agrupado_mm_lin,vcov = vcovHC, type = "HC1")


lm(CAPTC_LIQ_ACUM~
     lag(RETORNO,1)+
     lag(RETORNO,2)+
     PUBLICO_ALVO+
     lag(Expec.SELIC)+
     lag(Expec.CAMBIO)+
     lag(Expectativa_IPCA)+
     lag(IBC_Br)+
     lag(IRF_M)+
     lag(IMA_B)+
     lag(IMA_S)+
     lag(USDBRL)+
     lag(IBOV)+
     lag(IPCA)+
     lag(CDI),data = dados_fundos_teste_rf) -> agrupado_rf_lin


plm(CAPTC_LIQ_ACUM~
      lag(RETORNO,1)+
      lag(RETORNO,2)+
      PUBLICO_ALVO+
      lag(Expec.SELIC)+
      lag(Expec.CAMBIO)+
      lag(Expectativa_IPCA)+
      lag(IBC_Br)+
      lag(IRF_M)+
      lag(IMA_B)+
      lag(IMA_S)+
      lag(USDBRL)+
      lag(IBOV)+
      lag(IPCA)+
      lag(CDI)+
      GRUPO,
    index = c("CNPJ_FUNDO","COMPETENCIA"),
    data=dados_fundos_teste_rf, 
    model="within") -> plm_rf_fixo

plm(CAPTC_LIQ_ACUM~
      lag(RETORNO,1)+
      lag(RETORNO,2)+
      PUBLICO_ALVO+
      lag(Expec.SELIC)+
      lag(Expec.CAMBIO)+
      lag(Expectativa_IPCA)+
      lag(IBC_Br)+
      lag(IRF_M)+
      lag(IMA_B)+
      lag(IMA_S)+
      lag(USDBRL)+
      lag(IBOV)+
      lag(IPCA)+
      lag(CDI)+
      GRUPO,
    index = c("CNPJ_FUNDO","COMPETENCIA"),
    data=dados_fundos_teste_rf, 
    model="random") -> plm_rf_random

summary(plm_rf_random, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

lmtest::coeftest(agrupado_rf_lin,vcov = vcovHC, type = "HC1")


fundos_agrupados <- bind_rows(dados_fundos_teste_rf,
                              dados_fundos_teste_mm,
                              dados_fundos_teste_acoes)


lm(CAPTC_LIQ_ACUM~
     lag(RETORNO,1)+
     lag(RETORNO,2)+
     PUBLICO_ALVO+
     lag(Expec.SELIC)+
     lag(Expec.CAMBIO)+
     lag(Expectativa_IPCA)+
     lag(IBC_Br)+
     lag(IRF_M)+
     lag(IMA_B)+
     lag(IMA_S)+
     lag(USDBRL)+
     lag(IBOV)+
     lag(IPCA)+
     lag(CDI)+
     GRUPO,data = fundos_agrupados) -> agrupado_rf_lin


plm(CAPTC_LIQ_ACUM~
      lag(RETORNO,1)+
      lag(RETORNO,2)+
      PUBLICO_ALVO+
      lag(Expec.SELIC)+
      lag(Expec.CAMBIO)+
      lag(Expectativa_IPCA)+
      lag(IBC_Br)+
      lag(IRF_M)+
      lag(IMA_B)+
      lag(IMA_S)+
      lag(USDBRL)+
      lag(IBOV)+
      lag(IPCA)+
      lag(CDI)+
      GRUPO,
    index = c("CNPJ_FUNDO","COMPETENCIA"),
    data=fundos_agrupados, 
    model="random") -> plm_rf_fixo

summary(plm_rf_fixo, 
        vcov = function(x) vcovHC(x, method="arellano", 
                                  type="HC3"))

lmtest::coeftest(agrupado_rf_lin,vcov = vcovHC, type = "HC1")
summary(agrupado_rf_lin)
