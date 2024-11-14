##########################################################################################################
#----------------------- TUTORIA - ECONOMETRIA I - PROF. SUSAN SCHOMMER ------------------------
##########################################################################################################

## Tutoria 4: Multicolinearidade 

## Script elaborado a partir de: https://edisciplinas.usp.br/course/view.php?id=103117

#####################################################################################################################

# Carregando os pacotes necessários: 

library(lmtest) # testes de heterocedasticidade (tutoria 3)
library(car) # teste VIF de multicolinearidade
library(ggplot2)
library(stargazer)

# Abrindo a base de dados:

dados_1 <- read.csv("Dados/base_1.csv") #read.csv é uma funcao do R base para abrir arquivos .csv

# Variaveis genericas:
# x1 e x2: variaveis explicativas
# c1 e c2: controles da regressao
# y1: variavel dependente

##_____________________________Exemplo 1:_________________________________________


# Vamos rodar quatro regressoes, adicionando as variaveis progressivamente:

reg1_1 <- lm(y1~x1, data = dados_1) # Só explicativa 1
summary(reg1_1)

reg2_1 <- lm (y1~x2, data = dados_1) # Só explicativa 2
summary(reg2_1)

reg3_1 <- lm(y1~x1+x2,data = dados_1) # Duas explicativas
summary(reg3_1)

reg4_1 <- lm(y1~x1+x2+c1, data = dados_1) # Duas explicativas e o controle 1
summary(reg4_1)

# Vamos criar uma tabela que contém os summaries de todas as 4 regressoes para compara-las:

stargazer(reg1_1, reg2_1, reg3_1, reg4_1,
          type ="text",
          column.labels=c("x1", "x2", "x1 + x2", "x1 + x2 + c1"),
          omit.stat=c("f"),
          header=FALSE)

# O coeficiente de x1 nao muda quando adicionamos x2, mas se torna menor quando adicionamos c1, além de se tornar significante somente ao nível de 10%
# O coeficiente de x2 se mantem constante em todas as especificacoes do modelo, inclusive em termos de significancia estatistica

#_______________________________________________________________________________

# Agora, vamos rodar uma regressao com as duas explicativas e os dois controles
# Vamos comparar esta especificao do modelo com a reg4:

reg5_1 <- lm(y1~x1+x2+c1+c2, data = dados_1)

# Criando uma tabela com o summary da reg4_1 e o da reg5_1 para compara-las:

stargazer(reg4_1, reg5_1,
          type ="text",
          column.labels=c("x1 + x2 + c1", "x1 + x2 + c1 + c2"),
          omit.stat=c("f"),
          header=FALSE)

# Com a inclusão do controle 2, o coeficiente de x1 passa a ser negativo (unico caso em que isso ocorreu)
# e deixa de ser significante (erro-padrao aumentou)
# Possivel causa para isso: a inclusao do c2 gerou multicolinearidade entre as variaveis
# Relembrando da aula teorica:
# a multicolinearidade amplia o intervalo de confianca do coeficiente, facilitando a nao-rejeicao da H0 

# Vamos executar o teste de multicolinearidade para checar esta hipotese

#__________________________________Teste VIF____________________________________

# VIF (Variance Inflation Factor): velocidade com que as variancias dos estimadores aumentam com a presenca de multicolinearidade

# Testando presenca de multicolinearidade na reg4_1 (y1~x1+x2+c1):

vif(reg4_1) 

  ## Valores de VIF muito proximos a 1: não ha indicios de multicolinearidade

# Testando presenca de multicolinearidade na reg5_1(y1~x1+x2+c1+c2):

vif(reg5_1)

  ## Valores de VIF maiores que 10 para x1 e c2: estes coeficientes estao com variancia inflacionada
  ## Ha evidencias maiores de multicolinearidade na reg5_1, com a adicao de c2, como ja tinhamos suspeitado anteriormente

# Checando a correlacao entre x1 e c2: 

cor(dados_1$x1,dados_1$c2) # As variaveis sao muito fortemente correlacionadas

# Matriz de correlacao entre as variaveis da dados_1:

cor(dados_1) # Somente x1 e c2 têm correlacao muito forte: temos que retirar c2 do modelo 

# Solucao para multicolinearidade: retirar c2 da regressao 

#_________________________ Relembrando heterocedasticidade______________________

# Vamos aproveitar para testar se tambem ha violacao de outra hipotese de MQO: homocedasticidade

# Grafico residuos vs. variavel x1 para a reg4:


resid_reg4_1_plot <- ggplot(dados_1, aes(fitted(reg4_1), reg4_1$residuals))+
  geom_point(alpha=0.3)+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  stat_smooth(method="lm")+
  labs(x = "Fitted values", y = "Residuals Reg4_1")

# Grafico residuos vs. variavel x1 para a reg5:

resid_reg5_1_plot <- ggplot(dados_1, aes(fitted(reg5_1), reg5_1$residuals))+
  geom_point(alpha=0.3)+
  theme_minimal()+
  theme(panel.grid = element_blank())+
  stat_smooth(method="lm")+
  labs(x = "Fitted values", y = "Residuals Reg5_1")

ggsave("Graficos/resid_reg4_1_plot.pdf", resid_reg4_1_plot)
ggsave("Graficos/resid_reg5_1_plot.pdf", resid_reg5_1_plot)


  ## Nas duas regressoes, nao ha indicios de presenca de heterocedasticidade
  ## A dispersao dos residuos nao aumenta/diminui conforme o valor predito aumenta/diminui

# Mas vamos rodar o teste de Breusch-Pagan:

# Para a reg4 (y1~x1+x2+c1):

bptest(reg4_1)
  # p-valor: 2.2e-16 < 0.05

# Para a reg5 (especificacao completa: y1~x1+x2+c1+c2):

bptest(reg5_1)
  # p-valor: 2.2e-16 < 0.05

#H0: Homocedasticidade
#H1: Heterocedasticidade
  # Paras as duas regressoes, rejeitamos H0: indicio de heterocedasticidade

#_____________________________Exemplo 2:_________________________________________

# Com outra base de dados, novamente vamos rodar quatro regressoes, adicionando as variaveis progressivamente:

# Vamos refazer os mesmos procedimentos para multicolinearidade e heterocedasticidade
# tendo agora como especificacao do modelo: y1=x1+ln(x2)+c1+c2

dados_2 <- read.csv("Dados/base_2.csv")

# Variaveis genericas:
# x1 e x2: variaveis explicativas
# c1 e c2: controles da regressao
# y1 e y2: duas variaveis dependentes

reg1_2 <- lm(y1~x1, data = dados_2) # Só explicativa 1
summary(reg1_2)

reg2_2 <- lm (y1~x2, data = dados_2) # Só explicativa 2
summary(reg2_2)

reg3_2 <- lm(y1~x1+x2,data = dados_2) # Duas explicativas
summary(reg3_2)

reg4_2 <- lm(y1~x1+x2+c1+c2, data = dados_2) # Duas explicativas e dois controles
summary(reg4_2)

stargazer(reg1_2, reg2_2,reg3_2,reg4_2,
          type ="text",
          column.labels=c("x1", "x2", "x1 + x2", "x1 + x2 + c1 + c2"),
          omit.stat=c("f"),
          header=FALSE)

# Com a inclusao dos dois controles, o coeficiente de x2 deixa de ser estatisticamehte sginificante 
# Suspeita de presença de multicolinearidade após à adicao dos controles: vamos fazer o teste VIF

# Teste de multicolinearidade:

vif(reg4_2)

# VIF baixo: b2 nao parece estar com variancia inflacionada
# Logo, nao ha indicios de multicolinearidade

# Matriz de correlacao entre as variaveis da dados_2:

cor(dados_2) # Nenhuma variavel apresenta forte correlacao com alguma outra variavel
             # Nao precisamos remover do modelo nenhum dos dois controles  

#####################################################################################################################

#_________________________ R2 e R2 ajustado:____________________________________

# Vamos analisar de novo a primeira tabela de regressoes:

# Do exemplo 1: 

stargazer(reg1_1, reg2_1, reg3_1, reg4_1,
          type ="text",
          column.labels=c("x1", "x2", "x1 + x2", "x1 + x2 + c1"),
          omit.stat=c("f"),
          header=FALSE)

stargazer(reg4_1, reg5_1,
          type ="text",
          column.labels=c("x1 + x2 + c1", "x1 + x2 + c1 + c2"),
          omit.stat=c("f"),
          header=FALSE)

# Do exemplo 2: 

stargazer(reg1_2, reg2_2,reg3_2,reg4_2,
          type ="text",
          column.labels=c("x1", "x2", "x1 + x2", "x1 + x2 + c1 + c2"),
          omit.stat=c("f"),
          header=FALSE)

# Observamos que, conforme adicionamos variaveis, o R2 da regressao aumentou 
# O que ja era esperado, pois o R2 nunca diminui quando acrescentamos mais variaveis
# Por isso, vamos analisar o R2 ajustado, que contorna a inflacao do R2 com adicao de explicativas
# usando um fator de correcao que se baseia no numero de parametros do modelo 

  ## Observamos que o R2 ajustado tambem foi aumentando com a inclusao de variaveis, mas se manteve menor do que o R2
  ## Vale destacar que a diferenca entre os dois é pequena porque o o numero de parametros nao é tao grande (4)

# Cuidados com a interpretacao do R2: 

  ## O R2 e o R2 ajustados nos dizem somente o quanto da variacao em y é explicada pelas variaveis explicativas
  ## Mas nao é uma medida indicativa de quais variaveis devem ou nao estar no modelo 
  ## Teste de significancia individual do coeficiente e teste F de significancia conjunta que devem indicar isso
  ## E tambem nao nos diz nada sobre causalidade (se x causa efeito em y): eletiva de microeconometria

####################################################################################################################