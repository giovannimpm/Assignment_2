# Assignment 2
# Giovanni Machado
# Prof Ricardo Dahis

# Bibliotécas
library(tidyverse)
library(ggplot2)
library(basedosdados) 
library(lubridate)

# Diretórios
figura = "C:/Users/Giovanni Machado/OneDrive/Desktop/Pessoal/Mestrado/Verão/CIENCIA DE DADOS/Assignment 2"

# Tema para os gráficos
tema = theme(panel.background = element_rect(fill = "white"),
             panel.grid = element_line(color = "grey95"),
             legend.key = element_blank(),
             legend.position = "bottom",
             legend.text = element_text(size = 9),
             legend.title = element_blank())

# Importação dos dados - série histórica
basedosdados::set_billing_id("aula1-341512")
query = "SELECT * FROM basedosdados.br_ibge_ipca.mes_brasil"
df = basedosdados::read_sql(query)

# Tratamento
IPCA_12m = df %>% 
  mutate(mes = ifelse(mes < 10, paste0("0",mes), paste0(mes)),
         data = dmy(paste0("01",mes,ano))) %>% 
  filter(ano >= 2015) %>% 
  select(data, variacao_doze_meses)


# Gráfico
# evolução do IPCA

ggplot(IPCA_12m, aes(x = data, y = variacao_doze_meses))+
  geom_line()+ 
  labs(title = "Índice de Preços ao Consumidor Amplo",
       subtitle = "Variação acumulada em 12 meses",
       y = "%", x = "data") + tema


# Salvando o gráfico em pdf
ggsave("IPCA-12meses.pdf", path = figura, plot = last_plot())

# Importação dos dados - contribuição por segmento
basedosdados::set_billing_id("aula1-341512")
query = "SELECT * FROM basedosdados.br_ibge_ipca.mes_categoria_brasil"
df = basedosdados::read_sql(query)

# Tratamento
contribuicao = df %>% 
  mutate(mes = ifelse(mes < 10, paste0("0",mes), paste0(mes)),
         data = dmy(paste0("01",mes,ano)),
         contribuicao_mensal = variacao_mensal*peso_mensal/100,
         agregacao = case_when(categoria == "Alimentação e bebidas" ~ "Alimentação no Domicílio",
                               categoria == "Habitação" ~ "Serviços",
                               categoria == "Artigos de residência" ~ "Industriais",
                               categoria == "Vestuário" ~ "Industriais",
                               categoria == "Transportes" ~ "Administrados",
                               categoria == "Saúde e cuidados pessoais" ~ "Serviços",
                               categoria == "Despesas pessoais" ~ "Serviços",
                               categoria == "Educação" ~ "Serviços",
                               categoria == "Comunicação" ~ "Serviços")) %>% 
  filter(as.numeric(id_categoria) <= 9) %>% 
  select(data, agregacao, contribuicao_mensal)

# Gráfico
# contribuição de cada categoria p/ IPCA

ggplot(contribuicao, aes(x = data, y = contribuicao_mensal))+
  geom_col(aes(fill = agregacao)) + tema +
  labs(title = "Índice de Preços ao Consumidor Amplo",
       subtitle = "Contribuição de cada \ncategoria para variação mensal do índice",
       y = "%", x = NULL) + 
  scale_fill_manual(values = c("#ff8007", "#ffb404", "#05badd","#2b4871"),
                    guide = guide_legend(ncol = 2))
  
# Salvando o gráfico em pdf
ggsave("IPCA-contribuicao.pdf", path = figura, plot = last_plot())
