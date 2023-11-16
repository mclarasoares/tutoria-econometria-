############################### Econometria I

### Prof. Susan Schommer

# Tutoria Serie de tempo (heterocedasticidade e autocorrelacao)

# Adaptado a partir de script elaborado por Ana Coelho 

#####################################################################################################################


# Carregando os pacotes que vamos usar: 

install.packages("lmtest") # Caso nunca tenha instalado
library(readxl)
library(ggplot2)
library(tidyverse)
library(lmtest)


# Abrindo os dados:

dadospetro <- read_excel("Dados/dadospetro.xlsx")

  # Na tutoria passada: cross-section (526 observacoes de individuos em 1976)
  # Nesta tutoria: serie temporal (acompanha o pib de um mesmo país mes a mes ao longo de 20 anos)


# A primeira coluna está sem o nome da variável; vamos consertar isso

# Duas formas de fazer:

  # Pelo R base:

  colnames(dadospetro)[1]<-"Time"

  # Usando o pacote tidyverse (melhor opção):

  dadospetro <- dadospetro %>%
                  rename(periodo = ...1)
  
# Variáveis: periodo (ano-mes); variacao mensal do PIB; volatilidade mensal do preco do petroleo; 
# dummy para medir o impacto da crise de 2008; variacao mensal do IGP-DI (indice de precos)
  
#####################################################################################################################
  
# Plotando a série histórica do PIB:

# De novo, duas formas de plotar:
  
  # R base (grafico mais simples):
  
plot (dadospetro$time, dadospetro$var_PIB ,type="l")

  # Usando o pacote ggplot (grafico mais elaborado):

ggplot(data = dadospetro) + 
  geom_line(aes(x= periodo, y = var_PIB))

#_________________________Modelo __________________________________

# Modelo: var_PIB = b0 + b1*volpetro+ b2*var_IGPDI + b3*dummy

reg_1 <- lm(var_PIB ~ volpetro + var_IGPDI + dummy, data= dadospetro)

summary(reg_1)

# 

#####################################################################################################################


#_________________________ Heterocedasticidade_________________________________

# Modelo MQO trabalha com a hipotese de homocedasticidade, isto é, variancia dos erros é constante.

# Se a hipotese de homocedasticidade é violada, entao:

## coeficientes continuam nao-tendenciosos e consistentes, mas são ineficientes 
## erros-padrao dos coeficientes estimados serao viesados -> intefere nos testes de significancia dos estimadores
## por ex, se o erro-padrao do coeficiente esta viesado para baixo, sendo muito menor do que deveria ser,
## a estatistica t aumenta (t=b/ep), facilitando a rejeicao de H0 (de que o coeficiente estimado nao é significante)
## assim, podemos estar considerando como significante um coeficiente que, na verdade, nao é

# Quando aparece a heterocedasticidade? Quando existem outliers ou omissao de variavel relevante.

# Testes para detectar heterocedasticidade:

# Plotando a distribuicao dos residuos em relacao aos valores ajustados:

ggplot(data = dadospetro, aes(x= fitted(reg_1), y = resid(reg_1))) + 
  geom_point()+
  geom_smooth(method = "loess", color = "red", fill = "transparent") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs. Fitted Values")+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")


# Grafico 1 - Residual vs Fitted: 

# Se a hip de homocedasticidade nao é violada, entao, espera-se encontrar uma linha "reta" vermelha horizontal em 0 (MQO: media dos residuos nula).
# Nos dados, observamos que a linha vermelha esta muito proxima da reta pontilhada, entao
# por uma observacao rapida do grafico, a hip de homocedasticidade nao parece ter sido violada 
# a dispersao dos residuos nao parece crescer ao longo dos valores preditos
# eles parecem estar distribuidos homogeneamente ao longo da media zero (hipotese do MQO: E[u]=0)
# Mas, para realmente ter certeza, precisamos fazer os testes de heterocedasticidade:


#_________________________ Testes de Heterocedasticidade_________________________________

  # H0: Variancia dos residuos constante (homocedasticidade)
  # H1: Não H0

# Tipos de teste: teste LM/Breusch-Pagan; teste White; teste Goldfel-Quandt

library(lmtest) # Carrega o pacote para os testes de hetericedasticidade

# Teste Breusch-Pagan no R:

bptest(reg_1)

  # p-value = 0.8463 > 0.05 -> nao rejeita a hipotese nula: nao ha presenca de homocedasticidade



#_________________________ Inferencia robusta à heterocedasticidade_________________________________


# Na presenca de heterocedasticidade, os erros-padrao dos coeficientes estimados estao errados 
# isso prejudica os testes de significancia dos estimadores
# Por isso, precisamos ajustar esses erros-padrao. Fazemos isso com a funcao coeftest, do pacote lmtest

library(sandwich) # Carrega o pacote sandwich, necessario para usar a funcao vcovHC

# A funcao coeftest corrige os erros-padrao e refaz as estatisticas t e p-valores 

coeftest(reg_1,vcov = vcovHC, type="HC1")

# Comparando com os erros-padrao de MQO: 

summary(reg_1)

  # Como neste caso há homocedasticidade, a significancia dos coeficientes nao mudou tanto quando usamos o erro-padrao robusto 
  # Diferentemente do exemplo da aula, em que o estimador passa a ser significante após a correcao com o erro-padrao robusto

#####################################################################################################################


#_________________________ Autocorrelacao dos erros______________________________________________________


# Modelo MQO trabalha com a hipotese de nao haver autocorrelacao serial entre os residuos, ie, E(ei,ej)=0
# Se existe autocorrelacao serial, entao os elementos fora da diagonal principal da matriz
# variancia e covariancia dos erros nao sao todos nulos

# Se existe autocorrelacao entre os erros, entao, os coeficientes estimados de MQO ainda sao nao-viesados 
# mas nao sao eficientes


# O que causa Autocorrelacao? 

  ## Inercia (series de tempo costumam apresentar ciclos. Quando isso se reflete nos residuos, é comum que mudancas na tendencia ocorram lentamente)

  ## Falhas de especificacao (omissao de variaveis relevantes ou modelo incorreto)

  ## Defasagens (as decisoes de um periodo podem afetar os demais)

  ## Transformacoes nos dados (ex: transformacao de dados mensais em trimestrais)


# Plotando a distribuicao dos residuos ao longo dos periodos da amostra:

ggplot(data = dadospetro, aes(x=periodo, y = resid(reg_1))) + 
  geom_line()+
  labs(x = "tempo", y = "resíduos") +
  ggtitle("Resíduos vs. Tempo")+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")


  # Observamos ciclos nos residuos ao longo dos anos. Logo, espera-se que exista autocorrelacao serial.
  # Mesmo assim, da mesma forma que fizemos com heterocedasticidade, só observar o grafico não é suficiente
  # Precisamos realizar testes para saber se há ou nao presenca de autocorrelacao serial entre os erros

#_________________________ Testes de autocorrelacao____________________________________

# H0: Nao há autocorrelacao
# H1: presenca de autocorrelacao

# Tipos de teste: teste LM/Breusch-Godfrey; teste Ljung-Box; teste Durbin-Watson (nao vamos usar por ser um teste com limitacoes)
# De novo, vamos usar as funcoes do pacote lmtest (ja carregado anteriormente)

# Teste Breusch-Godfrey no R:

bgtest(reg_1)

  # p-value < 2.2e-16 < 0,05 -> rejeitamos H0: existe autocorrelacao entre os erros


# Teste Ljung-Box no R:

Box.test(resid(reg_1),type = "Ljung-Box")

  # p-value < 2.2e-16 < 0,05 -> rejeitamos H0: existe autocorrelacao entre os erros



#_________________________ Corrigindo autocorrelacao____________________________________


# Uma forma de corrigir a autocorrelacao é acrescentar a variaveis defasadas (tanto y quanto x)


# Os graficos acf e pcf ajudam a identificar quantos lags sao necessarios

# ACF : mostra como a correlacao serial muda no tempo

# PACF : mostra a correlacao parcial de uma serie com seus proprios valores defasados


acf(resid(reg_1)) 

pacf(resid(reg_1))

# Interpretacao: 

# olhando para o grafico PACF, vamos tentar corrigir a autocorrelacao adicionando variaveis defasadas a 1 e 2 periodos.


# Neste exemplo, vamos incorporar a variavel dependente com uma defasagem:

## Teste de Correcao 1:


reg_d1<-lm(var_PIB ~ lag(var_PIB,1) + volpetro + var_IGPDI + dummy, data= dadospetro)

summary(reg_d1)

# Vamos fazer o teste Breusch-Godfrey com essa nova regressao para verificar se ainda ha autocorrelacao: 

bgtest(reg_d1) 

# Queremos testar se o residuo é correlacionado com essa variavel

  # p-value = 1.854e-05 < 0.05 -> Rejeito H0: problema de autocorrelacao continua


# Teste Ljung-Box com essa nova regressao:

Box.test(resid(reg_d1),type = "Ljung-Box")

  # p-value = 0.0001664 < 0.05 -> Rejeito H0: problema de autocorrelacao continua


pacf(resid(reg_d1))


## Teste de Correcao 2:

# Incorporando a segunda defasagem do PIB

reg_d2<-lm(var_PIB ~ lag(var_PIB,1) + lag(var_PIB,2) + volpetro + var_IGPDI + dummy, data= dadospetro)

summary(reg_d2)

# Teste Breusch-Godfrey para verificar se ainda ha autocorrelacao com as duas defasagens do PIB: 

bgtest(reg_d2)

  # p-value = 0.003292 < 0.05 -> Rejeito H0: problema de autocorrelacao continua

# Teste Ljung-Box:

Box.test(resid(reg_d2),type = "Ljung-Box")

  # p-value = 0.365 > 0.05 -> Nao rejeito H0: problema de autocorrelacao resolvido

pacf(resid(reg_d2))

# Como o teste Breusch-Godfrey apontou que existe autocorrelacao, vamos colocar mais uma defasagem do PIB.


## Teste de Correcao 3:


reg_d3<-lm(var_PIB ~ lag(var_PIB,1) + lag(var_PIB,2) + lag(var_PIB,3) + volpetro + var_IGPDI + dummy, data= dadospetro)

summary(reg_d3)

# Teste Breusch-Godfrey para verificar se ainda ha autocorrelacao com as tres defasagens do PIB: 

bgtest(reg_d3)

  # p-value = 0.06283 > 0.05 -> Nao rejeito H0: problema de autocorrelacao corrigido

# Teste Ljung-Box:

Box.test(resid(reg_d3),type = "Ljung-Box")

  # p-value = 0.6179 > 0.05 -> Nao rejeito H0: problema de autocorrelacao corrigido


pacf(resid(reg_d3))


# Como os testes Ljung-Box indicam que o problema foi resolvido, poderiamos parar aqui.
# Contudo, podemos tambem adcicionar as variaveis independentes defasadas:

## Teste de Correcao 4:

# Incluindo na regressao a primeira defasagem das variaveis volatilidade do petroleo e IGPDI e retirando a terceira defsagem do PIB:

reg_d4<-lm(var_PIB ~ lag(var_PIB,1) + lag(var_PIB,2) + volpetro + lag(volpetro,1) + var_IGPDI + lag(var_IGPDI,1) + dummy, data= dadospetro)

summary(reg_d4)

# Teste Breusch-Godfrey para verificar se ha presenca de autocorrelacao com essas variaveis defasadas: 

bgtest(reg_d4)

  # p-value = 0.001334 < 0.05 -> Rejeito H0: presenca de autocorrelacao

# Teste Ljung-Box:

Box.test(resid(reg_d4),type = "Ljung-Box")

  # p-value = 0.3285 > 0.05 -> Nao rejeito H0: problema de autocorrelacao corrigido

pacf(resid(reg_d4))


## Teste de Correcao 5: 

# Incluindo na regressao a segunda defasagem das variaveis volatilidade do petroleo e IGPDI:


reg_d5<-lm(var_PIB ~ lag(var_PIB,1) + lag(var_PIB,2) + volpetro + lag(volpetro,1) + lag(volpetro,2) + var_IGPDI + lag(var_IGPDI,1) + lag(var_IGPDI,2)+dummy, data= dadospetro)

summary(reg_d5)

# Teste Breusch-Godfrey para verificar se ha presenca de autocorrelacao com a segunda defasagem das variaveis: 

bgtest(reg_d5)

  # p-value = 0.04419 < 0.05 -> Rejeito H0: presenca de autocorrelacao

# Teste Ljung-Box:

Box.test(resid(reg_d5),type = "Ljung-Box")

  # p-value = 0.506 > 0.05 -> Nao rejeito H0: problema de autocorrelacao corrigido

pacf(resid(reg_d5))


## Teste de Correcao 6: 

# Incluindo na regressao a terceira defasagem do PIB e mantendo as variaveis da regressao anterior:

reg_d6<-lm(var_PIB ~ lag(var_PIB,1) + lag(var_PIB,2) + lag(var_PIB,3) + volpetro + lag(volpetro,1) + lag(volpetro,2) + var_IGPDI + lag(var_IGPDI,1) + lag(var_IGPDI,2)+dummy, data= dadospetro)

summary(reg_d6)

# Teste Breusch-Godfrey para verificar se ha presenca de autocorrelacao: 

bgtest(reg_d6)

  # p-value = 0.7637 > 0.05 -> Nao rejeito H0: problema de autocorrelacao corrigido

# Teste Ljung-Box:

Box.test(resid(reg_d6),type = "Ljung-Box") 
  
  # p-value = 0.9312 > 0.05  -> Nao rejeito H0: problema de autocorrelacao corrigido

pacf(resid(reg_d6))


# Pelo comportamento da variavel, a solucao da autocorrelacao dos erros no modelo passa pela incorporacao da variavel dependente defasada ate 3 periodos.

# 1 defasagem do PIB: autocorrelacao
# 2 defasagens do PIB: so Ljung-Box
# 3 defasagens do PIB: autocorrelacao corrigida

# Testando com 1 defasagem do PIB, como no primeiro caso, mas agora também com defasagens das variaveis explicativas: 

reg_d7<-lm(var_PIB ~ lag(var_PIB,1) + volpetro + lag(volpetro,1) + var_IGPDI + lag(var_IGPDI,1) + dummy, data= dadospetro)

summary(reg_d7)

# Teste Breusch-Godfrey para verificar se ha presenca de autocorrelacao: 

bgtest(reg_d7) 
  
  # 2.107e-05 < 0.05 -> Rejeito H0: presenca de autocorrelacao 

# Teste Ljung-Box:


Box.test(resid(reg_d7),type = "Ljung-Box") 

  # 0.0003003 < 0.05 -> Rejeito H0: presenca de autocorrelacao 

#_________________________ Inferencia robusta à heterocedasticidade e à autocorrelacao_________________________________

# Assim como na heterocedasticidade, a autocorrelacao torna os estimadores ineficientes, o que interefere nos erros-padrao, prejudicando teste de hipotese

# Novamente, podemos usar a funcao coeftest corrige os erros-padrao e refaz as estatisticas t e p-valores 
# Assim como foi com a funcao a funcao vcovHC, precisamos do pacote sandwich para usar a funcao vcovHAC (mas ele ja foi carregado anteriormente, entao podemos usá-lo)

coeftest(reg_d2,vcov. = vcovHAC, type="HAC")

# "HAC": Heteroskedasticity and Autocorrelation Consistency 
# Essa funcao corrige os erros-padrao e estatistica t quando temos suspeita de haver no modelo tanto heterocedasticidade quanto autocorrelacao



####################################################################################################################
