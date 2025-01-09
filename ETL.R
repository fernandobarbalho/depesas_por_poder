library(tidyverse)
library(deflateBR)
library(readxl)
library(colorspace)
library(ggrepel)


gera_dados_trabalho<- function(path_dados_orcamento, path_orgaos_orcamentarios= "orgao_orcamentario.csv"){
  
  despesas_primarais_totais_federal <- read_excel(path_dados_orcamento)
  
  
  despesas_primarais_totais_federal <- janitor::clean_names(despesas_primarais_totais_federal)
  
  despesas_primarais_totais_federal <- despesas_primarais_totais_federal[-1,]
  
  despesas_primarais_totais_federal <- despesas_primarais_totais_federal[-c(669:674),]
  
  
  
  orgao_orcamentario <- read_delim(path_orgaos_orcamentarios, 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  despesas_primarias_totais_federal_trabalho<-
    despesas_primarais_totais_federal %>%
    inner_join(orgao_orcamentario)
  

  despesas_primarias_totais_federal_trabalho
  
}

gera_dados_graficos<- function(.data, ano_ref =2010, mes_real= "11/2024"){
  
  dados_grafico<-
    .data %>%
    summarise(depesa_total = sum(pago),
              .by = c(ano, poder)) %>%
    mutate(data_nominal = as.Date(paste(ano,"12","01", sep = "-")),
           despesa_total_corrigida = ipca(depesa_total,data_nominal,mes_real )) %>%
    select(ano, poder, despesa_total_corrigida)
  
  dados_referencia<-
    dados_grafico %>%
    filter(ano==ano_ref) %>%
    rename(depesa_total_corrigida_ano_ref = despesa_total_corrigida) %>%
    select(poder, depesa_total_corrigida_ano_ref )
  
  dados_grafico<-
    dados_grafico %>%
    inner_join(dados_referencia) %>%
    mutate(evolucao =  (despesa_total_corrigida/depesa_total_corrigida_ano_ref)*100)
  
  dados_grafico
}


anos_sel<- c(2018,2020,2024)

dados_graficos_federal_total <-
  gera_dados_trabalho("despesas_primarais_totais_federal.xlsx") %>%
  gera_dados_graficos() %>%
  mutate(evolucao = evolucao/100)

ultimo_ano <-
  dados_graficos_federal_total %>%
  filter(ano == 2024)

dados_anos_sel<-
  dados_graficos_federal_total %>%
  filter(ano %in% anos_sel)

  


dados_graficos_federal_total %>%
  ggplot(aes(x = as.numeric(ano), y = evolucao)) + # Converte 'ano' para numérico
  geom_line(aes(color = poder, group = poder), size = 1, show.legend = FALSE) +
  geom_point(aes(color = poder), size = 2, show.legend = FALSE) +
  scale_color_discrete_qualitative(palette = "Dark 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.4)),
                     breaks = c(2010,2012,2014,2016,2018,2020,2022,2024)) + # Adiciona espaço à direita
  scale_y_continuous(labels = scales::percent) + # Formata o eixo Y como porcentagem
  geom_text(data = ultimo_ano, 
            aes(x = as.numeric(ano) + 0.2, y = evolucao, label = as.character(poder), color  = poder), 
            hjust = 0,
            show.legend = FALSE) + # Alinha texto à esquerda
  geom_label(data= dados_anos_sel,
                  aes(x= as.numeric(ano), y=evolucao + 0.05, 
                      label = paste0(as.character(round(evolucao*100),0) ,"%"), 
                      color = poder), 
             show.legend = FALSE,
             size = 3) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    y="",
    x=""
  )





ipca(100, as.Date("2023-12-01"), "11/2024")
