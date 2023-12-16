library("dlookr")
library("ggstatsplot")
library("ggpubr")
library("patchwork")

# Carregamento de Dados ------------------------------------

diferenca_vol_rf_30_dias <- pin_read(board,"diferenca_vol_rf_30_dias")
diferenca_vol_rf_60_dias <- pin_read(board,"diferenca_vol_rf_60_dias")

diferenca_vol_multimercado_30_dias <- pin_read(board,"diferenca_vol_multimercado_30_dias")
diferenca_vol_multimercado_60_dias <- pin_read(board,"diferenca_vol_multimercado_60_dias")

diferenca_vol_acoes_30_dias <- pin_read(board,"diferenca_vol_acoes_30_dias")
diferenca_vol_acoes_60_dias <- pin_read(board,"diferenca_vol_acoes_60_dias")

select(informe_diario_fundos_multimercado,
       CNPJ_FUNDO,
       PUBLICO_ALVO,
       TP_PRAZO,
       POLIT_INVEST,
       DISTRIB,
       FUNDO_COTAS.x,
       CLASSE_ANBIMA,
       EXISTE_TAXA_PERFM,
       CPF_CNPJ_GESTOR,
       FUNDO_EXCLUSIVO,
       CONDOM) |> 
  distinct_all() |> 
  filter(POLIT_INVEST != "ENHANCED") -> dados_fundos_multimercados


select(informe_diario_fundos_rf,
       CNPJ_FUNDO,
       PUBLICO_ALVO,
       TP_PRAZO,
       POLIT_INVEST,
       DISTRIB,
       FUNDO_COTAS.x,
       CLASSE_ANBIMA,
       EXISTE_TAXA_PERFM,
       CPF_CNPJ_GESTOR,
       FUNDO_EXCLUSIVO,
       CONDOM) |> 
  distinct_all() |> 
  filter(POLIT_INVEST != "ENHANCED") -> dados_fundos_rf

select(informe_diario_fundos_acoes,
       CNPJ_FUNDO,
       PUBLICO_ALVO,
       TP_PRAZO,
       POLIT_INVEST,
       DISTRIB,
       FUNDO_COTAS.x,
       CLASSE_ANBIMA,
       EXISTE_TAXA_PERFM,
       CPF_CNPJ_GESTOR,
       FUNDO_EXCLUSIVO,
       CONDOM) |> 
  distinct_all() |> 
  filter(POLIT_INVEST != "ENHANCED") -> dados_fundos_acoes


# Renda Fixa ------------------------------------
## 30 Dias ------------------------------------

diferenca_vol_rf_30_dias |> 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(QT1 = quantile(Volatilidade,probs = 0.9)) %>% 
  mutate(Outlier = case_when(Volatilidade > QT1 ~ "Sim",
                             .default = "Não")) %>% 
  filter(Outlier == "Sim") %>% 
  distinct(CNPJ_FUNDO) -> fundos_outliers


diferenca_vol_rf_30_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>%
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  group_by(Mês) |> 
  summarise(Média = mean(`30 Dias Antes`,na.rm=T),
            Máx = max(`30 Dias Antes`,na.rm=T),
            Min = min(`30 Dias Antes`,na.rm=T))


diferenca_vol_rf_30_dias |> 
    # filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
    # na.omit() |>
    mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                          "fevereiro",
                                                          "março",
                                                          "abril",
                                                          "maio",
                                                          "junho",
                                                          "julho",
                                                          "agosto",
                                                          "setembro",
                                                          "outubro",
                                                          "novembro",
                                                          "dezembro"))) |> 
    tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                        names_to = "PERIODO",
                        values_to = "Volatilidade") |> 
    group_by(Mês,PERIODO) |>
    normality() %>% 
    mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                   .default = "Não")) %>% 
    print(n=30)


diferenca_vol_rf_30_dias |> 
  na.omit() |>
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(Volatilidade = Volatilidade*100) -> tbl_rank_test
  

purrr::map(c("janeiro",
             "fevereiro",
             "março",
             "abril",
             "maio",
             "junho",
             "julho",
             "agosto",
             "setembro",
             "outubro",
             "novembro",
             "dezembro"),
           function(x){
             ggbetweenstats(data = filter(tbl_rank_test,Mês == x),
                            x = PERIODO,
                            y = Volatilidade,
                            type = "nonparametric",
                            caption = x,
                            conf.level = 0.95,
                            title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                            k = 2L)
           }) -> plots

plots[[1]]+
  plots[[2]]+
  plots[[3]]+
  plots[[4]] + 
  plots[[5]] + 
  plots[[6]] +
  plot_layout(ncol = 2)

plots[[7]]+
  plots[[8]]+
  plots[[9]]+
  plots[[10]]+
  plots[[11]]+
  plots[[12]]+
  plot_layout(ncol = 2)


## 60 Dias ------------------------------------

diferenca_vol_rf_60_dias |> 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(QT1 = quantile(Volatilidade,probs = 0.9)) %>% 
  mutate(Outlier = case_when(Volatilidade > QT1 ~ "Sim",
                             .default = "Não")) %>% 
  filter(Outlier == "Sim") %>% 
  distinct(CNPJ_FUNDO) -> fundos_outliers


diferenca_vol_rf_60_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>%
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  group_by(Mês) |> 
  summarise(Média = mean(`60 Dias Antes`,na.rm=T),
            Máx = max(`60 Dias Antes`,na.rm=T),
            Min = min(`60 Dias Antes`,na.rm=T))


diferenca_vol_rf_60_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality() %>% 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=60)


diferenca_vol_rf_60_dias |> 
  na.omit() |>
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") -> tbl_rank_test


purrr::map(c("janeiro",
             "fevereiro",
             "março",
             "abril",
             "maio",
             "junho",
             "julho",
             "agosto",
             "setembro",
             "outubro",
             "novembro",
             "dezembro"),
           function(x){
             ggbetweenstats(data = filter(tbl_rank_test,Mês == x),
                            x = PERIODO,
                            y = Volatilidade,
                            type = "nonparametric",
                            caption = x,
                            conf.level = 0.95,
                            title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                            k = 2L)
           }) -> plots

plots[[1]]+
  plots[[2]]+
  plots[[3]]+
  plots[[4]] + 
  plots[[5]] + 
  plots[[6]] +
  plot_layout(ncol = 2)

plots[[7]]+
  plots[[8]]+
  plots[[9]]+
  plots[[10]]+
  plots[[11]]+
  plots[[12]]+
  plot_layout(ncol = 2)

# Multimercado ------------------------------------
## 30 Dias ------------------------------------

diferenca_vol_multimercado_30_dias |> 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(QT1 = quantile(Volatilidade,probs = 0.9)) %>% 
  mutate(Outlier = case_when(Volatilidade > QT1 ~ "Sim",
                             .default = "Não")) %>% 
  filter(Outlier == "Sim") %>% 
  distinct(CNPJ_FUNDO) -> fundos_outliers


diferenca_vol_multimercado_30_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>%
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  group_by(Mês) |> 
  summarise(Média = mean(`30 Dias Antes`,na.rm=T),
            Máx = max(`30 Dias Antes`,na.rm=T),
            Min = min(`30 Dias Antes`,na.rm=T))


diferenca_vol_multimercado_30_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality() %>% 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=30)


diferenca_vol_multimercado_30_dias |> 
  na.omit() |>
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(Volatilidade = Volatilidade*100) -> tbl_rank_test


purrr::map(c("janeiro",
             "fevereiro",
             "março",
             "abril",
             "maio",
             "junho",
             "julho",
             "agosto",
             "setembro",
             "outubro",
             "novembro",
             "dezembro"),
           function(x){
             ggbetweenstats(data = filter(tbl_rank_test,Mês == x),
                            x = PERIODO,
                            y = Volatilidade,
                            type = "nonparametric",
                            caption = x,
                            conf.level = 0.95,
                            title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                            k = 2L)
           }) -> plots

plots[[1]]+
  plots[[2]]+
  plots[[3]]+
  plots[[4]] + 
  plots[[5]] + 
  plots[[6]] +
  plot_layout(ncol = 2)

plots[[7]]+
  plots[[8]]+
  plots[[9]]+
  plots[[10]]+
  plots[[11]]+
  plots[[12]]+
  plot_layout(ncol = 2)

## 60 Dias ------------------------------------

diferenca_vol_multimercado_60_dias |> 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(QT1 = quantile(Volatilidade,probs = 0.9)) %>% 
  mutate(Outlier = case_when(Volatilidade > QT1 ~ "Sim",
                             .default = "Não")) %>% 
  filter(Outlier == "Sim") %>% 
  distinct(CNPJ_FUNDO) -> fundos_outliers


diferenca_vol_multimercado_60_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>%
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  group_by(Mês) |> 
  summarise(Média = mean(`60 Dias Antes`,na.rm=T),
            Máx = max(`60 Dias Antes`,na.rm=T),
            Min = min(`60 Dias Antes`,na.rm=T))


diferenca_vol_multimercado_60_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality() %>% 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=60)


diferenca_vol_multimercado_60_dias |> 
  na.omit() |>
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(Volatilidade = Volatilidade*100) -> tbl_rank_test


purrr::map(c("janeiro",
             "fevereiro",
             "março",
             "abril",
             "maio",
             "junho",
             "julho",
             "agosto",
             "setembro",
             "outubro",
             "novembro",
             "dezembro"),
           function(x){
             ggbetweenstats(data = filter(tbl_rank_test,Mês == x),
                            x = PERIODO,
                            y = Volatilidade,
                            type = "nonparametric",
                            caption = x,
                            conf.level = 0.95,
                            title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                            k = 2L)
           }) -> plots

plots[[1]]+
  plots[[2]]+
  plots[[3]]+
  plots[[4]] + 
  plots[[5]] + 
  plots[[6]] +
  plot_layout(ncol = 2)

plots[[7]]+
  plots[[8]]+
  plots[[9]]+
  plots[[10]]+
  plots[[11]]+
  plots[[12]]+
  plot_layout(ncol = 2)

# Ações ------------------------------------
## 30 Dias ------------------------------------

diferenca_vol_acoes_30_dias |> 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(QT1 = quantile(Volatilidade,probs = 1)) %>% 
  mutate(Outlier = case_when(Volatilidade > QT1 ~ "Sim",
                             .default = "Não")) %>% 
  filter(Outlier == "Sim") %>% 
  distinct(CNPJ_FUNDO) -> fundos_outliers


diferenca_vol_acoes_30_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>%
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  group_by(Mês) |> 
  summarise(Média = mean(`30 Dias Antes`,na.rm=T),
            Máx = max(`30 Dias Antes`,na.rm=T),
            Min = min(`30 Dias Antes`,na.rm=T))


diferenca_vol_acoes_30_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality() %>% 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=30)


diferenca_vol_acoes_30_dias |> 
  na.omit() |>
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(Volatilidade = Volatilidade*100) -> tbl_rank_test


purrr::map(c("janeiro",
             "fevereiro",
             "março",
             "abril",
             "maio",
             "junho",
             "julho",
             "agosto",
             "setembro",
             "outubro",
             "novembro",
             "dezembro"),
           function(x){
             ggbetweenstats(data = filter(tbl_rank_test,Mês == x),
                            x = PERIODO,
                            y = Volatilidade,
                            type = "nonparametric",
                            caption = x,
                            conf.level = 0.95,
                            title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                            k = 2L)
           }) -> plots

# plots[[1]]+
#   plots[[2]]+
#   plots[[3]]+
#   plots[[4]] + 
#   plots[[5]] + 
#   plots[[6]] +
#   plot_layout(ncol = 2)
# 
# plots[[7]]+
#   plots[[8]]+
#   plots[[9]]+
#   plots[[10]]+
#   plots[[11]]+
#   plots[[12]]+
#   plot_layout(ncol = 2)

lapply(plots, function(x){
  extract_stats(x)$subtitle_data
}) %>% 
  bind_rows() -> tbl_resumo_acoes_30_dias

dados_fundos_acoes %>% 
  filter(EXISTE_TAXA_PERFM == "S") %>% 
  distinct(CNPJ_FUNDO) -> fundos_performance

diferenca_vol_acoes_30_dias |> 
  na.omit() |>
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  filter(!CNPJ_FUNDO %in% fundos_performance$CNPJ_FUNDO) %>%
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(Volatilidade = Volatilidade*100) -> tbl_rank_test


purrr::map(c("janeiro",
             "fevereiro",
             "março",
             "abril",
             "maio",
             "junho",
             "julho",
             "agosto",
             "setembro",
             "outubro",
             "novembro",
             "dezembro"),
           function(x){
             ggbetweenstats(data = filter(tbl_rank_test,Mês == x),
                            x = PERIODO,
                            y = Volatilidade,
                            type = "nonparametric",
                            caption = x,
                            conf.level = 0.95,
                            title = paste0("Teste de diferença amostral mês c/ performance: ",str_to_title(x)), 
                            k = 2L)
           }) -> plots


# plots[[1]]+
#   plots[[2]]+
#   plots[[3]]+
#   plots[[4]] + 
#   plots[[5]] + 
#   plots[[6]] +
#   plot_layout(ncol = 2)
# 
# plots[[7]]+
#   plots[[8]]+
#   plots[[9]]+
#   plots[[10]]+
#   plots[[11]]+
#   plots[[12]]+
#   plot_layout(ncol = 2)

lapply(plots, function(x){
  extract_stats(x)$subtitle_data
}) %>% 
  bind_rows() -> tbl_resumo_acoes_30_dias_cperformance

dados_fundos_acoes %>% 
  filter(EXISTE_TAXA_PERFM == "N") %>% 
  distinct(CNPJ_FUNDO) -> fundos_performance

diferenca_vol_acoes_30_dias |> 
  na.omit() |>
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  filter(!CNPJ_FUNDO %in% fundos_performance$CNPJ_FUNDO) %>%
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(Volatilidade = Volatilidade*100) -> tbl_rank_test

purrr::map(c("janeiro",
             "fevereiro",
             "março",
             "abril",
             "maio",
             "junho",
             "julho",
             "agosto",
             "setembro",
             "outubro",
             "novembro",
             "dezembro"),
           function(x){
             ggbetweenstats(data = filter(tbl_rank_test,Mês == x),
                            x = PERIODO,
                            y = Volatilidade,
                            type = "nonparametric",
                            caption = x,
                            conf.level = 0.95,
                            title = paste0("Teste de diferença amostral mês s/ performance: ",str_to_title(x)), 
                            k = 2L)
           }) -> plots


# plots[[1]]+
#   plots[[2]]+
#   plots[[3]]+
#   plots[[4]] + 
#   plots[[5]] + 
#   plots[[6]] +
#   plot_layout(ncol = 2)
# 
# plots[[7]]+
#   plots[[8]]+
#   plots[[9]]+
#   plots[[10]]+
#   plots[[11]]+
#   plots[[12]]+
#   plot_layout(ncol = 2)


lapply(plots, function(x){
  extract_stats(x)$subtitle_data %>% 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = x$labels$caption,
           `Com performance?` = "Não") %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) %>% 
    relocate(Mês)
}) %>% 
  bind_rows() -> tbl_resumo_acoes_30_dias_sperformance

## 60 Dias ------------------------------------

diferenca_vol_acoes_60_dias |> 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(QT1 = quantile(Volatilidade,probs = 0.99)) %>% 
  mutate(Outlier = case_when(Volatilidade > QT1 ~ "Sim",
                             .default = "Não")) %>% 
  filter(Outlier == "Sim") %>% 
  distinct(CNPJ_FUNDO) -> fundos_outliers


diferenca_vol_acoes_60_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>%
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  group_by(Mês) |> 
  summarise(Média = mean(`60 Dias Antes`,na.rm=T),
            Máx = max(`60 Dias Antes`,na.rm=T),
            Min = min(`60 Dias Antes`,na.rm=T))


diferenca_vol_acoes_60_dias |> 
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  na.omit() |>
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality() %>% 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=60)


diferenca_vol_acoes_60_dias |> 
  na.omit() |>
  filter(!CNPJ_FUNDO %in% fundos_outliers$CNPJ_FUNDO) %>% 
  mutate(Mês = factor(format(DT_COMPTC,"%B"),levels = c("janeiro",
                                                        "fevereiro",
                                                        "março",
                                                        "abril",
                                                        "maio",
                                                        "junho",
                                                        "julho",
                                                        "agosto",
                                                        "setembro",
                                                        "outubro",
                                                        "novembro",
                                                        "dezembro"))) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  mutate(Volatilidade = Volatilidade*100) -> tbl_rank_test


purrr::map(c("janeiro",
             "fevereiro",
             "março",
             "abril",
             "maio",
             "junho",
             "julho",
             "agosto",
             "setembro",
             "outubro",
             "novembro",
             "dezembro"),
           function(x){
             ggbetweenstats(data = filter(tbl_rank_test,Mês == x),
                            x = PERIODO,
                            y = Volatilidade,
                            type = "nonparametric",
                            caption = x,
                            conf.level = 0.95,
                            title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                            k = 2L)
           }) -> plots

# plots[[1]]+
#   plots[[2]]+
#   plots[[3]]+
#   plots[[4]] + 
#   plots[[5]] + 
#   plots[[6]] +
#   plot_layout(ncol = 2)
# 
# plots[[7]]+
#   plots[[8]]+
#   plots[[9]]+
#   plots[[10]]+
#   plots[[11]]+
#   plots[[12]]+
#   plot_layout(ncol = 2)

lapply(plots, function(x){
  extract_stats(x)$subtitle_data %>% 
    select(-expression,
           -parameter1,
           -parameter2,
           -method,
           -alternative,
           -effectsize,
           -conf.method) %>% 
    mutate(Mês = x$labels$caption,
           `Com performance?` = "Ambos") %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) %>% 
    relocate(Mês)
}) %>% 
  bind_rows() -> tbl_resumo_acoes_60_dias



# Volatilidade Fundos Performance RF --------------------------------------------------
## Volatilidade 30 dias ----------------------------------------------------


taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_rf_30_dias$CNPJ_FUNDO)) |> 
  lazy_dt() |> 
  group_by(CNPJ_FUNDO) |> 
  # filter(!all(VL_SALDO_BALCTE == 0)) |>
  mutate(Quartil1 = quantile(VL_SALDO_BALCTE,probs = 0.25),
         Quartil3 = quantile(VL_SALDO_BALCTE,probs = 0.75)) |> 
  mutate(Quartil1 = case_when(VL_SALDO_BALCTE <= Quartil1 ~ 1,
                              T ~ 0)) |> 
  mutate(Quartil3 = case_when(VL_SALDO_BALCTE >= Quartil3 ~ 1,
                              T ~ 0)) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_rf_30_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |> 
  mutate(`Diferença` = `30 Dias Antes`-`30 Dias Depois`) |> 
  filter(!is.na(Quartil)) |> 
  mutate(Mês = factor(format(DT_COMPTC,"%B"))) |>
  left_join(dados_fundos_rf) |> 
  filter(!is.na(EXISTE_TAXA_PERFM)) |> 
  select(Quartil,
         `30 Dias Antes`,
         `30 Dias Depois`) %>% 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  filter(Volatilidade < 0.05) %>% 
  ggbetweenstats(x = PERIODO,
                 y = Volatilidade,
                 type = "nonparametric", 
                 k = 4L)



taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_rf_60_dias$CNPJ_FUNDO)) |> 
  lazy_dt() |> 
  group_by(CNPJ_FUNDO) |> 
  # filter(!all(VL_SALDO_BALCTE == 0)) |>
  mutate(Quartil1 = quantile(VL_SALDO_BALCTE,probs = 0.25),
         Quartil3 = quantile(VL_SALDO_BALCTE,probs = 0.75)) |> 
  mutate(Quartil1 = case_when(VL_SALDO_BALCTE <= Quartil1 ~ 1,
                              T ~ 0)) |> 
  mutate(Quartil3 = case_when(VL_SALDO_BALCTE >= Quartil3 ~ 1,
                              T ~ 0)) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_rf_60_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |> 
  mutate(`Diferença` = `60 Dias Antes`-`60 Dias Depois`) |> 
  filter(!is.na(Quartil)) |> 
  mutate(Mês = factor(format(DT_COMPTC,"%B"))) |>
  left_join(dados_fundos_rf) |> 
  filter(!is.na(EXISTE_TAXA_PERFM)) |> 
  select(Quartil,
         `60 Dias Antes`,
         `60 Dias Depois`) %>% 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  filter(Volatilidade < 0.05) %>% 
  ggbetweenstats(x = PERIODO,
                 y = Volatilidade,
                 type = "nonparametric", 
                 k = 4L)



taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_acoes_60_dias$CNPJ_FUNDO)) |> 
  lazy_dt() |> 
  group_by(CNPJ_FUNDO) |> 
  # filter(!all(VL_SALDO_BALCTE == 0)) |>
  mutate(Quartil1 = quantile(VL_SALDO_BALCTE,probs = 0.1),
         Quartil3 = quantile(VL_SALDO_BALCTE,probs = 0.9)) |> 
  mutate(Quartil1 = case_when(VL_SALDO_BALCTE <= Quartil1 ~ 1,
                              T ~ 0)) |> 
  mutate(Quartil3 = case_when(VL_SALDO_BALCTE >= Quartil3 ~ 1,
                              T ~ 0)) |> 
  mutate(Competencia = yearmonth(DT_COMPTC)) |>
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_acoes_60_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |> 
  mutate(`Diferença` = `60 Dias Antes`-`60 Dias Depois`) |> 
  filter(!is.na(Quartil)) |> 
  mutate(Mês = factor(format(DT_COMPTC,"%B"))) |>
  left_join(dados_fundos_acoes) |> 
  filter(
    Quartil == "Quartil1"
    # EXISTE_TAXA_PERFM == "S",
    # POLIT_INVEST == "ATIVA E REFERENCIADA"
         ) |> 
  select(Quartil,
         `60 Dias Antes`,
         `60 Dias Depois`) %>%
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") %>% 
  # filter(Volatilidade < 0.5) %>%
  ggbetweenstats(x = PERIODO,
                 y = Volatilidade,
                 type = "nonparametric", 
                 k = 4L,nboot = 1000L)
