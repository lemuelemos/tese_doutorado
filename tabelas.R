library(kableExtra)

bind_rows(informe_diario_fundos_rf,
          informe_diario_fundos_multimercado,
          informe_diario_fundos_acoes) |> 
  select(CNPJ_FUNDO,CLASSE) |> 
  distinct_all() |> 
  group_by(CLASSE) |> 
  summarise(Quantidade = n()) |> 
  arrange(Quantidade) |> 
  mutate(`Participação Percentual(%)` = scales::percent(Quantidade/sum(Quantidade))) |> 
  rename(Classe = CLASSE) |> 
  kable(format = "latex",
        caption = "Quantidade e participação dos fundos por classe",
        booktabs = T,) |> 
  kable_styling(latex_options = c("hold_position"))




tibble(Variável = c("Captação Líquida",
                    "IBOV",
                    "IRF-M",
                    "IMA-B",
                    "IMA-S",
                    "IPCA",
                    "Expectativa de Inflação",
                    "Dólar",
                    "Expectativa Cambio",
                    "SELIC",
                    "Expectativa SELIC",
                    "IBC-Br",
                    "Tamanho Fundo",
                    "Público Geral",
                    "Investidor Qualificado",
                    "Grande Banco",
                    "Rentabilidade no Período",
                    "Rentabilidade Anual"),
       Descrição = c("Aportes menos resgates sofridos no fundo no perído",
                     "Índice Bovespa",
                     "Indice formado por formado por títulos públicos prefixados, que são as LTNs (Letras do Tesouro Nacional ou Tesouro Prefixado) e NTN-Fs (Notas do Tesouro Nacional – Série F ou Tesouro Prefixado com Juros Semestrais)",
                     "Indice formado por títulos públicos indexados à inflação medida pelo IPCA (Índice Nacional de Preços ao Consumidor Amplo), que são as NTN-Bs (Notas do Tesouro Nacional – Série B ou Tesouro IPCA+ com Juros Semestrais)",
                     "Indice formado por títulos pós-fixados atrelados à taxa básica de juros (Selic), que são as LFTs (Letras Financeira do Tesouro ou Tesouro Selic)",
                     "Indice de preços ao Consumido Amplo",
                     "Expectativa de inflação medida pela pesquisa focus para os próximos 12 meses",
                     "Dólar Comercial - Compra",
                     "Expectativa da taxa de câmbio medida pela pesquisa focus para os próximos 12 meses",
                     "A taxa de juros equivalente à taxa referencial do Sistema Especial de Liquidação e de Custódia (Selic) para títulos federais",
                     "Expectativa da taxa SELIC medida pela pesquisa focus para os próximos 12 meses",
                     "Indice de atividade econômica medido pelo banco central",
                     "Variável que representa o tamanho do fundo, estando divididos entre: gigante, grande e médio",
                     "Variável qualitativa indicando se o público do fundo é o público geral",
                     "Variável qualitativa indicando se o público do fundo é o investidor qualificado",
                     "Variável que indica se a gestão do fundo pertence a um grande banco como descrito na Tabela 2",
                     "Rentabilidade no Período",
                     "Rentabilidade nos últimos 12 meses"),
       `Representação` = c(
         "$CL$",
         "$IBOV$",
         "$IRF-M$",
         "$IMA-B$",
         "$IMA-S$",
         "$IPCA$",
         "$EXP_{IPCA-12M}$",
         "$Dol$",
         "$EXP_{CAMBIO}$",
         "$SELIC$",
         "$EXP_{SELIC}$",
         "$IBC-Br$",
         "$GIGANTE$, $GRANDE$, $MEDIO$",
         "$PG$",
         "$IQ$",
         "$GB$",
         "$Ret$",
         "$Ret_{12M}$"                  )
       ) |> 
  kable(format = "latex",
        caption = "Descrição de Variáveis",
        booktabs = T,
        longtable = T,
        escape = F) |> 
  kable_styling(latex_options = c("hold_position","repeat_header","striped")) |> 
  column_spec(1,latex_valign = "m",width = "8em") |> 
  column_spec(2, width = "20em",latex_valign = "m") |> 
  column_spec(3, width = "7em",latex_valign = "m") 


tibble(`Critério` = c("Patrimônio Líquido maior que 1 bilhão",
                      "Patrimônio Líquido menor que 1 bilhão e maior que 250 milhões",
                      "Patrimônio Líquido menor que 250 milhões e maior que 50 milhões",
                      "Patrimônio Líquido menor que 50 milhões"),
       `Classificação` = c("Gigante",
                           "Grande",
                           "Médio",
                           "Pequeno")) |> 
  kable(format = "latex",
        caption = "Descrição de Variáveis",
        booktabs = T) |> 
  kable_styling(latex_options = c("hold_position"))


bind_rows(mutate(dados_painel_rf_trimestre_reg,Classe = "Renda Fixa"),
          mutate(dados_painel_mm_trimestre_reg,Classe = "Multimercado"),
          mutate(dados_painel_acoes_trimestre_reg,Classe = "Ações")) |>  
  group_by(CNPJ_FUNDO) |> 
  summarise_all(dplyr::last) |> 
  group_by(Classe,GRANDES_BANCOS) |> 
  summarise(Quantidade = n(),
            PL = sum(VL_PATRIM_LIQ)/1000000) |> 
  group_by(CLASSE) |> 
  mutate(`Part(%) Quantidade` = (Quantidade/sum(Quantidade)),
         `Part(%) PL` = (PL/sum(PL))) |> 
  arrange(CLASSE,-`Part(%) Quantidade`,-`Part(%) PL`) |> 
  mutate(`Part(%) Quantidade` = scales::percent(`Part(%) Quantidade`),
         `Part(%) PL` = scales::percent(`Part(%) PL`),
         Quantidade = scales::dollar(Quantidade,
                                     prefix = "",
                                     big.mark = "\\.",
                                     decimal.mark = ","),
         PL = scales::dollar(PL,
                             prefix = "",
                             big.mark = "\\.",
                             decimal.mark = ",")) |>
  relocate(`Part(%) Quantidade`,.after = Quantidade) |>
  relocate(`Part(%) PL`,.after = PL) |> 
  rename(Classe = CLASSE,
         `Público Alvo` = PUBLICO_ALVO,
         `Política de Investiemnto` = POLIT_INVEST) |> 
  kable(format = "latex",
        caption = "Quantidade e participação dos fundos por classe",
        booktabs = T) |> 
  kable_styling(latex_options = c("hold_position","scale_down")) |> 
  column_spec(2,width = "2cm")


bind_rows(mutate(dados_painel_rf_trimestre_reg,Classe = "Renda Fixa"),
          mutate(dados_painel_mm_trimestre_reg,Classe = "Multimercado"),
          mutate(dados_painel_acoes_trimestre_reg,Classe = "Ações")) |>  
  group_by(CNPJ_FUNDO) |> 
  summarise_all(dplyr::last) |> 
  mutate(`Público Alvo` = case_when(PG == 1 ~ "Público Geral",
                                  IQ == 1 ~ "Investidor Qualificado",
                                  IP == 1 ~ "Investidor Profissional",
                                  T ~ "Demais Públicos")) |> 
  group_by(Classe,GRANDES_BANCOS,`Público Alvo`) |> 
  summarise(Quantidade = n(),
            PL = sum(VL_PATRIM_LIQ)/1000000) |>  
  mutate(GRANDES_BANCOS = case_when(GRANDES_BANCOS == 1 ~ "Sim",
                                    T ~ "Não")) |> 
  group_by(Classe) |> 
  mutate(`Part(%) Quantidade` = (Quantidade/sum(Quantidade)),
         `Part(%) PL` = (PL/sum(PL))) |> 
  arrange(Classe,GRANDES_BANCOS,-`Part(%) Quantidade`,-`Part(%) PL`) |> 
  mutate(`Part(%) Quantidade` = scales::percent(`Part(%) Quantidade`),
         `Part(%) PL` = scales::percent(`Part(%) PL`),
         Quantidade = scales::dollar(Quantidade,
                                     prefix = "",
                                     big.mark = "\\.",
                                     decimal.mark = ","),
         PL = scales::dollar(PL,
                             prefix = "",
                             big.mark = "\\.",
                             decimal.mark = ",")) |>
  relocate(`Part(%) Quantidade`,.after = Quantidade) |>
  relocate(`Part(%) PL`,.after = PL) |> 
  rename(`Grande Bancos` = GRANDES_BANCOS) |> 
  kable(format = "latex",
        caption = "Quantidade e participação dos fundos por classe",
        booktabs = T) |> 
  kable_styling(latex_options = c("hold_position","scale_down"))


bind_rows(mutate(dados_painel_rf_trimestre_reg,Classe = "Renda Fixa"),
          mutate(dados_painel_mm_trimestre_reg,Classe = "Multimercado"),
          mutate(dados_painel_acoes_trimestre_reg,Classe = "Ações")) |> 
  select(TRIMESTRE,
         Classe,
         CAPTC_LIQ,
         IBOV,
         USDBRL,
         IPCA,
         SELIC,
         CDI,
         IBC_Br,
         EXPEC_IPCA_12M,
         Expec.CAMBIO,
         Expec.SELIC,
         IRF_M,
         IMA_B,
         IMA_S,
         RENTABILIDADE,
         RENTABILIDADE_ANO) |> 
  group_by(Classe) |> 
  correlation::correlation(select = c("CAPTC_LIQ"),select2 = c("IBOV",
                                                               "USDBRL",
                                                               "IPCA",
                                                               "SELIC",
                                                               "CDI",
                                                               "IBC_Br",
                                                               "EXPEC_IPCA_12M",
                                                               "Expec.CAMBIO",
                                                               "Expec.SELIC",
                                                               "IRF_M",
                                                               "IMA_B",
                                                               "IMA_S",
                                                               "RENTABILIDADE",
                                                               "RENTABILIDADE_ANO"),
                           method = "spearman") |> 
  as_tibble() |> 
  mutate(across(rho:p,~scales::number(.x,
                                      big.mark = "\\.",
                                      decimal.mark = ",",
                                      accuracy = 0.0001))) |>
  select(-CI,-S,-Parameter1) |> 
  rename(`Captação Líquida` = Group,
         `Variáveis Macroeconomicas` = Parameter2,
         `Correlação` = rho,
         `NC Nível Inferior` = CI_low,
         `NC Nível Superior` = CI_high,
         `P Valor` = p,
         `Método` = Method,
         `Nº de Observações` = n_Obs) |> 
  mutate(`Método` = "Spearman") |> 
  kable(format = "latex",
        caption = "Quantidade e participação dos fundos por classe",
        booktabs = T) |> 
  kable_styling(latex_options = c("hold_position","scale_down"))
  

  
bind_rows(mutate(dados_painel_rf_trimestre_reg,Classe = "Renda Fixa"),
          mutate(dados_painel_mm_trimestre_reg,Classe = "Multimercado"),
          mutate(dados_painel_acoes_trimestre_reg,Classe = "Ações")) |> 
  select(TRIMESTRE,
         PG,
         IQ,
         CAPTC_LIQ,
         IBOV,
         USDBRL,
         IPCA,
         SELIC,
         CDI,
         IBC_Br,
         EXPEC_IPCA_12M,
         Expec.CAMBIO,
         Expec.SELIC,
         IRF_M,
         IMA_B,
         IMA_S,
         RENTABILIDADE,
         RENTABILIDADE_ANO) |> 
  mutate(`Público Alvo` = case_when(PG == 1 ~ "Público Geral",
                                    IQ == 1 ~ "Investidor Qualificado",
                                    T ~ "Demais Públicos")) |> 
  group_by(`Público Alvo`) |> 
  correlation::correlation(select = c("CAPTC_LIQ"),select2 = c("IBOV",
                                                               "USDBRL",
                                                               "IPCA",
                                                               "SELIC",
                                                               "CDI",
                                                               "IBC_Br",
                                                               "EXPEC_IPCA_12M",
                                                               "Expec.CAMBIO",
                                                               "Expec.SELIC",
                                                               "IRF_M",
                                                               "IMA_B",
                                                               "IMA_S",
                                                               "RENTABILIDADE",
                                                               "RENTABILIDADE_ANO"),
                           method = "spearman") |> 
  as_tibble() |> 
  mutate(across(rho:p,~scales::number(.x,
                                      big.mark = "\\.",
                                      decimal.mark = ",",
                                      accuracy = 0.0001))) |>
  select(-CI,-S,-Parameter1) |> 
  rename(`Captação Líquida` = Group,
         `Variáveis Macroeconomicas` = Parameter2,
         `Correlação` = rho,
         `NC Nível Inferior` = CI_low,
         `NC Nível Superior` = CI_high,
         `P Valor` = p,
         `Método` = Method,
         `Nº de Observações` = n_Obs) |> 
  mutate(`Método` = "Spearman") |> print(n=42)


bind_rows(mutate(dados_painel_rf_trimestre_reg,Classe = "Renda Fixa"),
          mutate(dados_painel_mm_trimestre_reg,Classe = "Multimercado"),
          mutate(dados_painel_acoes_trimestre_reg,Classe = "Ações")) |> 
  select(Classe,
         CAPTC_LIQ,
         IBOV,
         USDBRL,
         IPCA,
         SELIC,
         CDI,
         IBC_Br,
         EXPEC_IPCA_12M,
         Expec.CAMBIO,
         Expec.SELIC,
         IRF_M,
         IMA_B,
         IMA_S,
         RENTABILIDADE,
         RENTABILIDADE_ANO) |> 
  ggplot() +
  geom_histogram(aes(CAPTC_LIQ),bins = 5) +
  facet_wrap(vars(Classe))
