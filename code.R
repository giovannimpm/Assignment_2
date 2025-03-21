# Assignment 2
# Giovanni Machado
# Prof Ricardo Dahis

# Bibliot�cas
library(tidyverse)
library(ggplot2)
library(basedosdados) 
library(lubridate)

# Diret�rios
figura = "C:/Users/Giovanni Machado/OneDrive/Desktop/Pessoal/Mestrado/Ver�o/CIENCIA DE DADOS/Assignment 2"

# Tema para os gr�ficos
tema = theme(panel.background = element_rect(fill = "white"),
             panel.grid = element_line(color = "grey95"),
             legend.key = element_blank(),
             legend.position = "bottom",
             legend.text = element_text(size = 9),
             legend.title = element_blank())

# Importa��o dos dados - s�rie hist�rica
basedosdados::set_billing_id("aula1-341512")
query = "SELECT * FROM basedosdados.br_ibge_ipca.mes_brasil"
df = basedosdados::read_sql(query)

# Tratamento
IPCA_12m = df %>% 
  mutate(mes = ifelse(mes < 10, paste0("0",mes), paste0(mes)),
         data = dmy(paste0("01",mes,ano))) %>% 
  filter(ano >= 2015) %>% 
  select(data, variacao_doze_meses)


# Gr�fico
# evolu��o do IPCA

ggplot(IPCA_12m, aes(x = data, y = variacao_doze_meses))+
  geom_line()+ 
  labs(title = "�ndice de Pre�os ao Consumidor Amplo",
       subtitle = "Varia��o acumulada em 12 meses",
       y = "%", x = "data") + tema


# Salvando o gr�fico em pdf
ggsave("IPCA-12meses.pdf", path = figura, plot = last_plot())

# Importa��o dos dados - contribui��o por segmento
basedosdados::set_billing_id("aula1-341512")
query = "SELECT * FROM basedosdados.br_ibge_ipca.mes_categoria_brasil"
df = basedosdados::read_sql(query)

# Tratamento
contribuicao = df %>% 
  mutate(mes = ifelse(mes < 10, paste0("0",mes), paste0(mes)),
         data = dmy(paste0("01",mes,ano)),
         contribuicao_mensal = variacao_mensal*peso_mensal/100,
         agregacao = case_when(categoria == "Alimenta��o e bebidas" ~ "Alimenta��o no Domic�lio",
                               categoria == "Habita��o" ~ "Servi�os",
                               categoria == "Artigos de resid�ncia" ~ "Industriais",
                               categoria == "Vestu�rio" ~ "Industriais",
                               categoria == "Transportes" ~ "Administrados",
                               categoria == "Sa�de e cuidados pessoais" ~ "Servi�os",
                               categoria == "Despesas pessoais" ~ "Servi�os",
                               categoria == "Educa��o" ~ "Servi�os",
                               categoria == "Comunica��o" ~ "Servi�os")) %>% 
  filter(as.numeric(id_categoria) <= 9) %>% 
  select(data, agregacao, contribuicao_mensal)

# Gr�fico
# contribui��o de cada categoria p/ IPCA

ggplot(contribuicao, aes(x = data, y = contribuicao_mensal))+
  geom_col(aes(fill = agregacao)) + tema +
  labs(title = "�ndice de Pre�os ao Consumidor Amplo",
       subtitle = "Contribui��o de cada \ncategoria para varia��o mensal do �ndice",
       y = "%", x = NULL) + 
  scale_fill_manual(values = c("#ff8007", "#ffb404", "#05badd","#2b4871"),
                    guide = guide_legend(ncol = 2))
  
# Salvando o gr�fico em pdf
ggsave("IPCA-contribuicao.pdf", path = figura, plot = last_plot())
