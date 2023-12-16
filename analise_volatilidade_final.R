# Carregamento dos Dados --------------------------------------------------
source("global.R")
board <- board_folder("dados_tese/",versioned = T)
cad_fi <- pin_read(board,"cad_fi")
extrato_fi_atual <- pin_read(board,"extrato_fi_atual")
pin_read(board,"taxa_performance_fundos") -> taxa_performance_fundos
pin_read(board,"diferenca_vol_acoes_30_dias") -> diferenca_vol_acoes_30_dias
pin_read(board,"diferenca_vol_acoes_60_dias") -> diferenca_vol_acoes_60_dias
pin_read(board,"diferenca_vol_multimercado_30_dias") -> diferenca_vol_multimercado_30_dias
pin_read(board,"diferenca_vol_multimercado_60_dias") -> diferenca_vol_multimercado_60_dias
pin_read(board,"diferenca_vol_rf_30_dias") -> diferenca_vol_rf_30_dias
pin_read(board,"diferenca_vol_rf_60_dias") -> diferenca_vol_rf_60_dias

extrato_fi_atual |> 
  distinct_all() |> 
  select(CNPJ_FUNDO,
         CLASSE_ANBIMA,
         REG_ANBIMA,
         PUBLICO_ALVO,
         DISTRIB,
         POLIT_INVEST,
         FUNDO_COTAS,
         TAXA_PERFM,
         EXISTE_TAXA_PERFM,
         CALC_TAXA_PERFM) -> classe_anbima

cad_fi |> 
  filter(TP_FUNDO == "FI",
         SIT == "EM FUNCIONAMENTO NORMAL") |> 
  select(CNPJ_FUNDO,DT_FIM_EXERC) |> 
  mutate(`Fim de Exercicio` = factor(format(DT_FIM_EXERC,"%B"),levels = c("janeiro",
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
  count(`Fim de Exercicio`) |> 
  na.omit() |> 
  rename(Quantidade = n) |> 
  mutate(`Part(%)` = scales::percent(Quantidade/sum(Quantidade),
                                     big.mark = "\\.",
                                     decimal.mark = ",",
                                     scale = 100,
                                     accuracy = 0.01)) |> 
  kbl(caption = "Fundos de Renda Fixa - 30 Dias",
      label = "exercicio",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position")) |> 
  footnote(general = "Fonte: Dados do portal de dados aberto da CVM. Elaboração Própria.",
           threeparttable = T)



# Fundos de Renda Fixa ----------------------------------------------------
## Tratamento dos Dados ----------------------------------------------------

diferenca_vol_rf_30_dias |> 
  left_join(classe_anbima) |> 
  filter(str_detect(CLASSE_ANBIMA,"AÇÕES|MULTIMERCADO"),
         POLIT_INVEST != "ENHANCED") -> rf_acoes


diferenca_vol_rf_30_dias |> 
  filter(!CNPJ_FUNDO %in% rf_acoes$CNPJ_FUNDO) |>
  distinct_all() |> 
  left_join(classe_anbima) |> 
  filter(REG_ANBIMA == "S") |> 
  mutate(`30 Dias Antes` = round(`30 Dias Antes`*100,4),
         `30 Dias Depois` = round(`30 Dias Depois`*100,4)) |>
  filter(`30 Dias Antes` > 0,
         `30 Dias Depois` > 0) |> 
  filter(`30 Dias Depois` < 50,
         `30 Dias Antes` < 50) |>
  filter(`30 Dias Depois` > 0.01,
         `30 Dias Antes` > 0.01) |> 
  filter(!is.na(`30 Dias Depois`),
         !is.na(`30 Dias Antes`)) |> 
  mutate(Distorcao = `30 Dias Depois`/`30 Dias Antes`) |> 
  filter(Distorcao < 5) |> 
  mutate(Distorcao = `30 Dias Antes`/`30 Dias Depois`) |> 
  filter(Distorcao < 5) -> diferenca_vol_rf_30_dias


### Testes de Comparação de Médias 30 dias ------------------------------------------

diferenca_vol_rf_30_dias |> 
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality(Volatilidade) %>% 
  mutate(p_value = round(p_value,5)) |> 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=30)

diferenca_vol_rf_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 12) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_rf_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 12) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> dezembro

diferenca_vol_rf_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 9) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_rf_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 9) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> setembro


diferenca_vol_rf_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 6) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_rf_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 6) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> junho

  
diferenca_vol_rf_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 3) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_rf_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 3) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |> 
  bind_rows(junho) |> 
  bind_rows(setembro) |>
  bind_rows(dezembro) |> 
  filter(!(`Política de Investimentos` == "PASSIVA" & 
            `Cobra taxa de performance?` == "Sim")) |> 
  kbl(caption = "Fundos de Renda Fixa - 30 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down")) |> 
  pack_rows("Março", 1, 5) |> 
  pack_rows("Junho", 6, 10) |> 
  pack_rows("Setembro", 11, 15) |> 
  pack_rows("Dezembro", 16, 20) |> 
  footnote(general_title = "Fonte",
           general = "Dados do portal de dados aberto da CVM. Elaboração Própria.")


diferenca_vol_rf_30_dias |>  
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |>
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  left_join(
    select(extrato_fi_atual,
           CNPJ_FUNDO,
           EXISTE_TAXA_PERFM)
  ) -> tbl_rank_test

#### Teste de diferença todos os fundos --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_rf_teste_30dias_ambos


#### Teste de diferença fundos c/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,
                                         Mês == x,
                                         EXISTE_TAXA_PERFM == "S"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
           `Com performance?` = "Sim") %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) %>% 
    relocate(Mês)
}) %>% 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_rf_teste_30dias_cp



#### Teste de diferença fundos s/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "N"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_rf_teste_30dias_sp


### Testes de Comparação de Médias 60 dias ------------------------------------------

diferenca_vol_rf_60_dias |> 
  left_join(classe_anbima) |> 
  filter(str_detect(CLASSE_ANBIMA,"AÇÕES")) -> rf_acoes


diferenca_vol_rf_60_dias |> 
  filter(!CNPJ_FUNDO %in% rf_acoes$CNPJ_FUNDO) |> 
  left_join(classe_anbima) |> 
  filter(REG_ANBIMA == "S") |> 
  mutate(`60 Dias Antes` = round(`60 Dias Antes`*100,4),
         `60 Dias Depois` = round(`60 Dias Depois`*100,4)) |>
  filter(`60 Dias Antes` > 0,
         `60 Dias Depois` > 0) |> 
  filter(`60 Dias Depois` < 50,
         `60 Dias Antes` < 50) |>
  filter(`60 Dias Depois` > 0.01,
         `60 Dias Antes` > 0.01) |> 
  mutate(Distorcao = `60 Dias Depois`/`60 Dias Antes`) |> 
  filter(Distorcao < 5) |> 
  mutate(Distorcao = `60 Dias Antes`/`60 Dias Depois`) |> 
  filter(Distorcao < 5) -> diferenca_vol_rf_60_dias

diferenca_vol_rf_60_dias |>
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_rf_60_dias |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(Quantidade = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(Quantidade,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |>  
  kbl(caption = "Fundos de Renda Fixa - 60 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))


diferenca_vol_rf_60_dias |>
  filter(lubridate::month(DT_COMPTC) == 12) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_rf_60_dias |> 
      filter(lubridate::month(DT_COMPTC) == 12) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> dezembro

diferenca_vol_rf_60_dias |>
  filter(lubridate::month(DT_COMPTC) == 3) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_rf_60_dias |> 
      filter(lubridate::month(DT_COMPTC) == 12) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> marco

diferenca_vol_rf_60_dias |>
  filter(lubridate::month(DT_COMPTC) == 6) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_rf_60_dias |> 
      filter(lubridate::month(DT_COMPTC) == 6) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |> 
  bind_rows(dezembro) |> 
  kbl(caption = "Fundos Multimercados - 60 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down")) |> 
  pack_rows("Junho", 1, 6) |> 
  pack_rows("Dezembro", 7, 12)





diferenca_vol_rf_60_dias |>  
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |>
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  left_join(
    select(extrato_fi_atual,
           CNPJ_FUNDO,
           EXISTE_TAXA_PERFM)
  ) -> tbl_rank_test

#### Teste de diferença todos os fundos --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_rf_teste_60dias_ambos


  
#### Teste de diferença fundos c/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "S"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
           `Com performance?` = "Sim") %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) %>% 
    relocate(Mês)
}) %>% 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_rf_teste_60dias_cp



#### Teste de diferença fundos s/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "N"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_rf_teste_60dias_sp

# Fundos Multimercados ----------------------------------------------------
## Tratamento dos Dados ----------------------------------------------------

diferenca_vol_multimercado_30_dias |> 
  left_join(classe_anbima) |> 
  filter(str_detect(CLASSE_ANBIMA,"AÇÕES|RENDA FIXA")) -> mm_filtro


diferenca_vol_multimercado_30_dias |> 
  filter(!CNPJ_FUNDO %in% mm_filtro$CNPJ_FUNDO) |> 
  left_join(classe_anbima) |> 
  filter(REG_ANBIMA == "S") |> 
  mutate(`30 Dias Antes` = round(`30 Dias Antes`*100,4),
         `30 Dias Depois` = round(`30 Dias Depois`*100,4)) |>
  filter(`30 Dias Antes` > 0,
         `30 Dias Depois` > 0) |> 
  filter(`30 Dias Depois` < 80,
         `30 Dias Antes` < 80) |>
  filter(`30 Dias Depois` > 0.01,
         `30 Dias Antes` > 0.01) |> 
  filter(!is.na(`30 Dias Depois`),
         !is.na(`30 Dias Antes`)) |> 
  mutate(Distorcao = `30 Dias Depois`/`30 Dias Antes`) |>
  filter(Distorcao < 5) |> 
  mutate(Distorcao = `30 Dias Antes`/`30 Dias Depois`) |>
  filter(Distorcao < 5) -> diferenca_vol_multimercado_30_dias

diferenca_vol_multimercado_30_dias |>
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_multimercado_30_dias |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(Quantidade = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(Quantidade,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |> 
  kbl(caption = "Multimercado - 30 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))

### Testes de Comparação de Médias 30 dias ------------------------------------------

diferenca_vol_multimercado_30_dias |>
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
            Min = min(`30 Dias Antes`,na.rm=T),
            Qtd = n())

diferenca_vol_multimercado_30_dias |> 
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality(Volatilidade) %>% 
  mutate(p_value = round(p_value,5)) |> 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=30)


diferenca_vol_multimercado_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 12) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_multimercado_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 12) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> dezembro

diferenca_vol_multimercado_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 9) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_multimercado_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 9) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> setembro


diferenca_vol_multimercado_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 6) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_multimercado_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 6) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> junho


diferenca_vol_multimercado_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 3) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_multimercado_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 3) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |> 
  bind_rows(junho) |> 
  bind_rows(setembro) |>
  bind_rows(dezembro) |> 
  filter(!(`Política de Investimentos` == "PASSIVA" & 
             `Cobra taxa de performance?` == "Sim")) |> 
  kbl(caption = "Fundos de Renda Fixa - 30 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down")) |> 
  pack_rows("Março", 1, 5) |> 
  pack_rows("Junho", 6, 10) |> 
  pack_rows("Setembro", 11, 15) |> 
  pack_rows("Dezembro", 16, 20) |> 
  footnote(general_title = "Fonte",
           general = "Dados do portal de dados aberto da CVM. Elaboração Própria.")



diferenca_vol_multimercado_30_dias |>  
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |>
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  left_join(
    select(extrato_fi_atual,
           CNPJ_FUNDO,
           EXISTE_TAXA_PERFM)
  ) -> tbl_rank_test

#### Teste de diferença todos os fundos --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_mm_teste_30dias_ambos

#### Teste de diferença fundos c/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "S"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
           `Com performance?` = "Sim") %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) %>% 
    relocate(Mês)
}) %>% 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_mm_teste_30dias_cp

#### Teste de diferença fundos s/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "N"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_mm_teste_30dias_sp
### Testes de Comparação de Médias 60 dias ------------------------------------------

diferenca_vol_multimercado_60_dias |> 
  left_join(classe_anbima) |> 
  filter(str_detect(CLASSE_ANBIMA,"AÇÕES")) -> mm_filtro


diferenca_vol_multimercado_60_dias |> 
  filter(!CNPJ_FUNDO %in% mm_filtro$CNPJ_FUNDO) |> 
  left_join(classe_anbima) |> 
  filter(REG_ANBIMA == "S") |> 
  mutate(`60 Dias Antes` = round(`60 Dias Antes`*100,4),
         `60 Dias Depois` = round(`60 Dias Depois`*100,4)) |>
  filter(`60 Dias Antes` > 0,
         `60 Dias Depois` > 0) |> 
  filter(`60 Dias Depois` < 80,
         `60 Dias Antes` < 80) |>
  filter(`60 Dias Depois` > 0.01,
         `60 Dias Antes` > 0.01) |> 
  filter(!is.na(`60 Dias Depois`),
         !is.na(`60 Dias Antes`)) |> 
  mutate(Distorcao = `60 Dias Depois`/`60 Dias Antes`) |> 
  filter(Distorcao < 5) |> 
  mutate(Distorcao = `60 Dias Antes`/`60 Dias Depois`) |> 
  filter(Distorcao < 5) -> diferenca_vol_multimercado_60_dias


diferenca_vol_multimercado_60_dias |>
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_multimercado_60_dias |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(Quantidade = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(Quantidade,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |>  
  kbl(caption = "Fundos Multimercado - 60 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))



diferenca_vol_multimercado_60_dias |>
  filter(lubridate::month(DT_COMPTC) == 12) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_multimercado_60_dias |> 
      filter(lubridate::month(DT_COMPTC) == 12) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> dezembro


diferenca_vol_multimercado_60_dias |>
  filter(lubridate::month(DT_COMPTC) == 6) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_multimercado_60_dias |> 
      filter(lubridate::month(DT_COMPTC) == 6) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |> 
  bind_rows(dezembro) |> 
  kbl(caption = "Fundos Multimercados - 60 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down")) |> 
  pack_rows("Junho", 1, 6) |> 
  pack_rows("Dezembro", 7, 12)




diferenca_vol_multimercado_60_dias |> 
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality(Volatilidade) %>% 
  mutate(p_value = round(p_value,5)) |> 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=60)

diferenca_vol_multimercado_60_dias |>  
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |>
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  left_join(
    select(extrato_fi_atual,
           CNPJ_FUNDO,
           EXISTE_TAXA_PERFM)
  ) -> tbl_rank_test

#### Teste de diferença todos os fundos --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_mm_teste_60dias_ambos

#### Teste de diferença fundos c/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "S"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
           `Com performance?` = "Sim") %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) %>% 
    relocate(Mês)
}) %>% 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_mm_teste_60dias_cp

#### Teste de diferença fundos s/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "N"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_mm_teste_60dias_sp
# Fundos de Ações ----------------------------------------------------
## Tratamento dos Dados ----------------------------------------------------

diferenca_vol_acoes_30_dias |> 
  left_join(classe_anbima) |> 
  filter(str_detect(CLASSE_ANBIMA,"MULTIMERCADO|RENDA FIXA")) -> acoes_filtro


diferenca_vol_acoes_30_dias |> 
  filter(!CNPJ_FUNDO %in% acoes_filtro$CNPJ_FUNDO) |> 
  left_join(classe_anbima) |> 
  filter(REG_ANBIMA == "S") |> 
  mutate(`30 Dias Antes` = round(`30 Dias Antes`*100,4),
         `30 Dias Depois` = round(`30 Dias Depois`*100,4)) |> 
  filter(`30 Dias Antes` > 0,
         `30 Dias Depois` > 0) |> 
  filter(`30 Dias Depois` < 200,
         `30 Dias Antes` < 200) |>
  filter(`30 Dias Depois` > 0.01,
         `30 Dias Antes` > 0.01) |> 
  filter(!is.na(`30 Dias Depois`),
         !is.na(`30 Dias Antes`)) |> 
  mutate(Distorcao = `30 Dias Depois`/`30 Dias Antes`) |>
  filter(Distorcao < 5) |> 
  mutate(Distorcao = `30 Dias Antes`/`30 Dias Depois`) |>
  filter(Distorcao < 5) -> diferenca_vol_acoes_30_dias

diferenca_vol_acoes_30_dias |>
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  left_join(
    diferenca_vol_acoes_30_dias |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM,CALC_TAXA_PERFM) |> 
      summarise(Quantidade = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM",
                        "CALC_TAXA_PERFM"))
  ) |> 
  relocate(CALC_TAXA_PERFM:Quantidade,.after = EXISTE_TAXA_PERFM) |> 
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não"),
         CALC_TAXA_PERFM = case_when(is.na(CALC_TAXA_PERFM) ~ "NÂO INFORMA",
                                     .default = CALC_TAXA_PERFM)) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |> 
  kbl(caption = "Fundos de Ações - 30 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))

diferenca_vol_acoes_30_dias |>
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
            Min = min(`30 Dias Antes`,na.rm=T),
            Qtd = n())

diferenca_vol_acoes_30_dias |> 
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality(Volatilidade) %>% 
  mutate(p_value = round(p_value,5)) |> 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=30)

diferenca_vol_acoes_30_dias |>  
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |>
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  left_join(
    select(extrato_fi_atual,
           CNPJ_FUNDO,
           EXISTE_TAXA_PERFM)
  ) -> tbl_rank_test

### Testes de Comparação de Médias 30 dias ------------------------------------------

diferenca_vol_acoes_30_dias |>
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
            Min = min(`30 Dias Antes`,na.rm=T),
            Qtd = n())

diferenca_vol_acoes_30_dias |> 
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality(Volatilidade) %>% 
  mutate(p_value = round(p_value,5)) |> 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=30)


diferenca_vol_acoes_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 12) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_acoes_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 12) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> dezembro

diferenca_vol_acoes_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 9) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_acoes_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 9) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> setembro


diferenca_vol_acoes_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 6) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_acoes_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 6) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> junho


diferenca_vol_acoes_30_dias |>
  filter(lubridate::month(DT_COMPTC) == 3) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `30 Dias Antes`,
         `30 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`30 Dias Antes` = median(`30 Dias Antes`),
            `30 Dias Depois` = median(`30 Dias Depois`)) |>
  mutate(`Diferença` = `30 Dias Depois`-`30 Dias Antes`) |> 
  left_join(
    diferenca_vol_acoes_30_dias |> 
      filter(lubridate::month(DT_COMPTC) == 3) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`30 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |> 
  bind_rows(junho) |> 
  bind_rows(setembro) |>
  bind_rows(dezembro) |> 
  filter(!(`Política de Investimentos` == "PASSIVA" & 
             `Cobra taxa de performance?` == "Sim")) |> 
  kbl(caption = "Fundos de Renda Fixa - 30 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down")) |> 
  pack_rows("Março", 1, 5) |> 
  pack_rows("Junho", 6, 10) |> 
  pack_rows("Setembro", 11, 15) |> 
  pack_rows("Dezembro", 16, 20) |> 
  footnote(general_title = "Fonte",
           general = "Dados do portal de dados aberto da CVM. Elaboração Própria.")





diferenca_vol_acoes_30_dias |>  
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |>
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  left_join(
    select(extrato_fi_atual,
           CNPJ_FUNDO,
           EXISTE_TAXA_PERFM)
  ) -> tbl_rank_test


#### Teste de diferença todos os fundos --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso"))-> diferenca_acoes_teste_30dias_ambos

#### Teste de diferença fundos c/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "S"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
           `Com performance?` = "Sim") %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) %>% 
    relocate(Mês)
}) %>% 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso"))-> diferenca_acoes_teste_30dias_cp

#### Teste de diferença fundos s/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "N"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso")) -> diferenca_acoes_teste_30dias_sp



### Testes de Comparação de Médias 60 dias ------------------------------------------

diferenca_vol_acoes_60_dias |> 
  left_join(classe_anbima) |> 
  filter(str_detect(CLASSE_ANBIMA,"MULTIMERCADO|RENDA FIXA")) -> acoes_filtro


diferenca_vol_acoes_60_dias |> 
  filter(!CNPJ_FUNDO %in% acoes_filtro$CNPJ_FUNDO) |> 
  left_join(classe_anbima) |> 
  filter(REG_ANBIMA == "S") |> 
  mutate(`60 Dias Antes` = round(`60 Dias Antes`*100,4),
         `60 Dias Depois` = round(`60 Dias Depois`*100,4)) |>
  filter(`60 Dias Antes` > 0,
         `60 Dias Depois` > 0) |> 
  filter(`60 Dias Depois` < 200,
         `60 Dias Antes` < 200) |>
  filter(`60 Dias Depois` > 0.01,
         `60 Dias Antes` > 0.01) |> 
  filter(!is.na(`60 Dias Depois`),
         !is.na(`60 Dias Antes`)) |>  
  mutate(Distorcao = `60 Dias Depois`/`60 Dias Antes`) |> 
  filter(Distorcao < 5) |> 
  mutate(Distorcao = `60 Dias Antes`/`60 Dias Depois`) |> 
  filter(Distorcao < 5) -> diferenca_vol_acoes_60_dias


diferenca_vol_acoes_60_dias |>
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_acoes_60_dias |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(Quantidade = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(Quantidade,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |>  
  kbl(caption = "Fundos de Ações - 60 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down"))


diferenca_vol_acoes_60_dias |> 
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  group_by(Mês,PERIODO) |>
  normality(Volatilidade) %>% 
  mutate(p_value = round(p_value,5)) |> 
  mutate(Normalidade = case_when(p_value > 0.05 ~ "Sim",
                                 .default = "Não")) %>% 
  print(n=60)


diferenca_vol_acoes_60_dias |>
  filter(lubridate::month(DT_COMPTC) == 12) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_acoes_60_dias |> 
      filter(lubridate::month(DT_COMPTC) == 12) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) -> dezembro


diferenca_vol_acoes_60_dias |>
  filter(lubridate::month(DT_COMPTC) == 6) |> 
  select(CNPJ_FUNDO,
         POLIT_INVEST,
         PUBLICO_ALVO,
         EXISTE_TAXA_PERFM,
         `60 Dias Antes`,
         `60 Dias Depois`) |>
  filter(POLIT_INVEST != "ENHANCED") |>
  group_by(POLIT_INVEST,
           EXISTE_TAXA_PERFM) |>
  summarise(`60 Dias Antes` = median(`60 Dias Antes`),
            `60 Dias Depois` = median(`60 Dias Depois`)) |>
  mutate(`Diferença` = `60 Dias Depois`-`60 Dias Antes`) |> 
  left_join(
    diferenca_vol_acoes_60_dias |> 
      filter(lubridate::month(DT_COMPTC) == 6) |> 
      distinct(CNPJ_FUNDO,POLIT_INVEST,EXISTE_TAXA_PERFM) |> 
      summarise(`Quantidade de Fundos` = scales::number(n(),big.mark = "\\."),
                .by = c("POLIT_INVEST",
                        "EXISTE_TAXA_PERFM"))
  ) |> 
  relocate(`Quantidade de Fundos`,.after = EXISTE_TAXA_PERFM) |>
  mutate(across(`60 Dias Antes`:`Diferença`,~scales::percent(.x,
                                                             big.mark = "\\.",
                                                             decimal.mark = ",",
                                                             scale = 1,
                                                             accuracy = 0.01))) |>
  mutate(EXISTE_TAXA_PERFM = case_when(EXISTE_TAXA_PERFM == "S" ~ "Sim",
                                       T ~ "Não")) |> 
  rename(`Política de Investimentos` = POLIT_INVEST,
         `Cobra taxa de performance?` = EXISTE_TAXA_PERFM) |> 
  bind_rows(dezembro) |> 
  kbl(caption = "Fundos de Ações - 60 Dias",
      label = "rd_diferenca",
      format = "latex",
      booktabs = T) |> 
  kable_classic(full_width = F, 
                latex_options = c("hold_position","scale_down")) |> 
  pack_rows("Junho", 1, 6) |> 
  pack_rows("Dezembro", 7, 12)





diferenca_vol_acoes_60_dias |>  
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
  select(-Distorcao,-REG_ANBIMA,-CLASSE_ANBIMA,-DT_COMPTC) |>
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  left_join(
    select(extrato_fi_atual,
           CNPJ_FUNDO,
           EXISTE_TAXA_PERFM)
  ) -> tbl_rank_test

#### Teste de diferença todos os fundos --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso"))-> diferenca_acoes_teste_60dias_ambos

#### Teste de diferença fundos c/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "S"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
           `Com performance?` = "Sim") %>% 
    rename(`W_{Mann-Whitney}` = statistic,
           r_rank = estimate) %>% 
    relocate(Mês)
}) %>% 
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso"))-> diferenca_acoes_teste_60dias_cp

#### Teste de diferença fundos s/ Performance --------------------------------------

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
             ggwithinstats(data = filter(tbl_rank_test,Mês == x,EXISTE_TAXA_PERFM == "N"),
                           x = PERIODO,
                           y = Volatilidade,
                           type = "nonparametric",
                           caption = x,
                           conf.level = 0.95,
                           title = paste0("Teste de diferença amostral mês: ",str_to_title(x)), 
                           k = 2L)
           }) -> plots

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
  bind_rows() |> 
  mutate(across(where(is.numeric),~round(.x,5)),
         `Tamanho da Diferença` = interpret_rank_biserial(r_rank)) |> 
  rename(`Intervalo de Confiança` = conf.level,
         `Limite Superior` = conf.high,
         `Limite Inferior` = conf.low) |> 
  mutate(`Tamanho da Diferença` = case_when(`Tamanho da Diferença` == "tiny" ~ "Minúsculo",
                                            `Tamanho da Diferença` == "very small" ~ "Ínfimo",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "medium" ~ "Médio",
                                            `Tamanho da Diferença` == "small" ~ "Pequeno",
                                            `Tamanho da Diferença` == "large" ~ "Grande",
                                            `Tamanho da Diferença` == "very large" ~ "Imenso"))-> diferenca_acoes_teste_60dias_sp

# Calculo de Taxa de Performance ------------------------------------------

# Renda Fixa --------------------------------------------------------------

taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_rf_30_dias$CNPJ_FUNDO)) |> 
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
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_rf_30_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |>
  filter(!is.na(Quartil)) |> 
  select(CNPJ_FUNDO,
         Quartil,
         Mês,
         `30 Dias Antes`,
         `30 Dias Depois`) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") -> tbl_quartis
    
purrr::map(unique(as.character(tbl_quartis$Mês)), function(x){
  grouped_ggwithinstats(filter(tbl_quartis,Mês == x),
                        x = PERIODO,
                        y = Volatilidade,
                        grouping.var = Quartil,
                        type = "nonparametric",
                        conf.level = 0.95, 
                        k = 2L,
                        nboot = 1000L,
                        annotation.args = list(title = paste0("Mês de Comparação: ",x)))
}) -> plots_rf_30_quartil   


taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_rf_60_dias$CNPJ_FUNDO)) |> 
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
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_rf_60_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |>
  filter(!is.na(Quartil)) |> 
  select(CNPJ_FUNDO,
         Quartil,
         Mês,
         `60 Dias Antes`,
         `60 Dias Depois`) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") -> tbl_quartis

purrr::map(unique(as.character(tbl_quartis$Mês)), function(x){
  grouped_ggwithinstats(filter(tbl_quartis,Mês == x),
                        x = PERIODO,
                        y = Volatilidade,
                        grouping.var = Quartil,
                        type = "nonparametric",
                        conf.level = 0.95, 
                        k = 2L,
                        nboot = 1000L,
                        annotation.args = list(title = paste0("Mês de Comparação: ",x)))
}) -> plots_rf_60_quartil   


# Multimercado ------------------------------------------------------------

taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_multimercado_30_dias$CNPJ_FUNDO)) |> 
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
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_multimercado_30_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |>
  filter(!is.na(Quartil)) |> 
  select(CNPJ_FUNDO,
         Quartil,
         Mês,
         `30 Dias Antes`,
         `30 Dias Depois`) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") -> tbl_quartis

purrr::map(unique(as.character(tbl_quartis$Mês)), function(x){
  grouped_ggwithinstats(filter(tbl_quartis,Mês == x),
                        x = PERIODO,
                        y = Volatilidade,
                        grouping.var = Quartil,
                        type = "nonparametric",
                        conf.level = 0.95, 
                        k = 2L,
                        nboot = 1000L,
                        annotation.args = list(title = paste0("Mês de Comparação: ",x)))
}) -> plots_mm_30_quartil   


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
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_multimercado_60_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |>
  filter(!is.na(Quartil)) |> 
  select(CNPJ_FUNDO,
         Quartil,
         Mês,
         `60 Dias Antes`,
         `60 Dias Depois`) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") -> tbl_quartis

purrr::map(unique(as.character(tbl_quartis$Mês)), function(x){
  grouped_ggwithinstats(filter(tbl_quartis,Mês == x),
                        x = PERIODO,
                        y = Volatilidade,
                        grouping.var = Quartil,
                        type = "nonparametric",
                        conf.level = 0.95, 
                        k = 2L,
                        nboot = 1000L,
                        annotation.args = list(title = paste0("Mês de Comparação: ",x)))
}) -> plots_mm_60_quartil   


# Fundos de Ações ---------------------------------------------------------

taxa_performance_fundos |> 
  filter(CNPJ_FUNDO %in% unique(diferenca_vol_acoes_30_dias$CNPJ_FUNDO)) |> 
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
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_acoes_30_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |>
  filter(!is.na(Quartil)) |> 
  select(CNPJ_FUNDO,
         Quartil,
         Mês,
         `30 Dias Antes`,
         `30 Dias Depois`) |> 
  tidyr::pivot_longer(cols = c("30 Dias Antes","30 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") |> 
  left_join(classe_anbima) -> tbl_quartis

purrr::map(unique(as.character(tbl_quartis$Mês)), function(x){
  grouped_ggwithinstats(filter(tbl_quartis,Mês == x),
                        x = PERIODO,
                        y = Volatilidade,
                        grouping.var = Quartil,
                        type = "nonparametric",
                        conf.level = 0.95, 
                        k = 2L,
                        nboot = 1000L,
                        annotation.args = list(title = paste0("Mês de Comparação: ",x)))
}) -> plots_acoes_30_quartil   



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
  select(-DT_COMPTC,-PLANO_CONTA_BALCTE ) |> 
  collect() |> 
  ungroup() |> 
  left_join(mutate(diferenca_vol_acoes_60_dias,
                   Competencia = yearmonth(DT_COMPTC))) |> 
  mutate(Quartil = case_when(Quartil1 == 1 ~ "Quartil1",
                             Quartil3 == 1 ~ "Quartil3")) |>
  filter(!is.na(Quartil)) |> 
  select(CNPJ_FUNDO,
         Quartil,
         Mês,
         `60 Dias Antes`,
         `60 Dias Depois`) |> 
  tidyr::pivot_longer(cols = c("60 Dias Antes","60 Dias Depois"),
                      names_to = "PERIODO",
                      values_to = "Volatilidade") -> tbl_quartis

purrr::map(unique(as.character(tbl_quartis$Mês)), function(x){
  grouped_ggwithinstats(filter(tbl_quartis,Mês == x),
                        x = PERIODO,
                        y = Volatilidade,
                        grouping.var = Quartil,
                        type = "nonparametric",
                        conf.level = 0.95, 
                        k = 2L,
                        nboot = 1000L,
                        annotation.args = list(title = paste0("Mês de Comparação: ",x)))
}) -> plots_acoes_60_quartil  

