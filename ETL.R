library(tidyverse)
library(deflateBR)
library(readxl)
library(colorspace)
library(ggrepel)





gera_consolidado_sp<- function(path){
  
  # Define o caminho da pasta
  pasta <-  path 
  
  # Lista todos os arquivos na pasta
  arquivos <- list.files(path = pasta, full.names = TRUE)
  
  despesa_trabalho<-
  map_dfr(arquivos, function(arquivo){
    
    ano<- str_extract(arquivo,"\\d+")
    
    despesa <-  read_csv(arquivo, 
                         locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                         encoding = "latin1"))
    
    despesa<- janitor::clean_names(despesa)
    
    despesa$ano<- ano
    

    despesa
  })
  

}



gera_dados_trabalho<- function(path_dados_orcamento, path_orgaos_orcamentarios= "orgao_orcamentario.csv"){
  
  despesas_primarais_totais_federal <- read_excel(path_dados_orcamento)
  
  
  despesas_primarais_totais_federal <- janitor::clean_names(despesas_primarais_totais_federal)
  
  despesas_primarais_totais_federal <- despesas_primarais_totais_federal[-1,]
  
  despesas_primarais_totais_federal <-
    despesas_primarais_totais_federal %>%
    filter(!is.na(ano))
  

  orgao_orcamentario <- read_delim(path_orgaos_orcamentarios, 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  despesas_primarias_totais_federal_trabalho<-
    despesas_primarais_totais_federal %>%
    inner_join(orgao_orcamentario)
  

  despesas_primarias_totais_federal_trabalho
  
}

gera_dados_trabalho_sp<- function(.data, path_orgaos_orcamentarios= "orgaos_orcamento_sp.csv"){
  

  orgao_orcamentario <- read_delim(path_orgaos_orcamentarios, 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  .data<-
    .data %>%
    inner_join(orgao_orcamentario)
  
  
  .data
  
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


#####Despesas governo Federal


anos_sel<- c(2018,2020,2024)

dados_graficos_federal_total <-
  gera_dados_trabalho("despesas_primarais_totais_federal.xlsx") %>%
  gera_dados_graficos() %>%
  mutate(evolucao = evolucao/100)




saveRDS(dados_graficos_federal_total,"dados_graficos_federal_total.rds")

despesas_federal_fonte<-
  gera_dados_trabalho("despesas_primarias_federais_fontes.xlsx") %>%
  filter(as.numeric(substr(fonte,1,1))%in%c(1,3))


dados_graficos_federal_fonte_tesouro<-
despesas_federal_fonte %>%
  filter(between(as.numeric(ano),2010,2024)) %>%
  gera_dados_graficos()%>%
  mutate(evolucao = evolucao/100)


saveRDS(dados_graficos_federal_fonte_tesouro,"dados_graficos_federal_fonte_tesouro.rds")


dados_graficos_federal_pessoal_fontes <-
  gera_dados_trabalho("Despesas_pessoal_federal_todas_fontes.xlsx") %>%
  gera_dados_graficos() %>%
  mutate(evolucao = evolucao/100)


saveRDS(dados_graficos_federal_pessoal_fontes,"dados_graficos_federal_pessoal_fontes.rds")


despesas_pessoal_fonte<-
  gera_dados_trabalho("despesas_pessoal_federal_fontes.xlsx") %>%
  filter(as.numeric(substr(fonte,1,1))%in%c(1,3))


dados_graficos_federal_pessoal_fonte_tesouro<-
  despesas_pessoal_fonte %>%
  gera_dados_graficos()%>%
  mutate(evolucao = evolucao/100)

saveRDS(dados_graficos_federal_pessoal_fonte_tesouro,"dados_graficos_federal_pessoal_fonte_tesouro.rds")


###########Despesas governo São Paulo

despesa_pessoal_sp<- gera_consolidado_sp("despesas_pessoal_sp/")

despesa_pessoal_sp<-
  despesa_pessoal_sp %>%
  filter(!is.na(orgao))

outras_despesas_corrente_sp <- gera_consolidado_sp("despesas_odc_sp/")

outras_despesas_corrente_sp<-
  outras_despesas_corrente_sp %>%
  filter(!is.na(orgao))


despesas_funcao_sp<- gera_consolidado_sp("despesas_f_sp")

despesas_funcao_sp <-
  despesas_funcao_sp %>%
  filter(!is.na(orgao))

#Checa se as despesas de pessoal possuem apenas despesas de pessoal

despesa_pessoal_sp %>%
  filter(substr(despesa,1,2)!="31")


#Checa se as outras despesas correntes possuem apenas outras despesas correntes

outras_despesas_corrente_sp %>%
  filter(substr(despesa,1,2)!="33")



#checa os anos distintos de despesas de pessoal
unique(despesa_pessoal_sp$ano)

#checa os anos distintos de outras despesas correntes
unique(outras_despesas_corrente_sp$ano)

#Gera arquivo com os órgãos

# despesa_pessoal_sp %>%
#   distinct(orgao) %>%
#   readr::write_csv("orgaos_orcamento_sp.csv")

#GEra dados de trabalho de df de pessoal

despesa_pessoal_sp_trabalho<- 
despesa_pessoal_sp %>%
  gera_dados_trabalho_sp()


#GEra dados de trabalho de df de outras despesas correntes

outras_despesas_corrente_sp_trabalho<- 
  outras_despesas_corrente_sp %>%
  gera_dados_trabalho_sp()


#GEra dados de trabalho de df de despesa por função

despesas_funcao_sp_trabalho<- 
  despesas_funcao_sp %>%
  gera_dados_trabalho_sp()


#Gera dados de gráfico para SP depesas de pessoal todas fontes

dados_grafico_pessoal_todas_fontes_sp<-
  despesa_pessoal_sp_trabalho %>%
  mutate(pago = pago+ pago_restos) %>%
  filter(poder != unique(despesa_pessoal_sp_trabalho$poder)[5]) %>%
  filter(ano<=2016) %>%
  gera_dados_graficos()%>%
  mutate(evolucao = evolucao/100)



saveRDS(dados_grafico_pessoal_todas_fontes_sp, "dados_grafico_pessoal_todas_fontes_sp.rds")
