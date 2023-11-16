##########################################################################################################
#----------------------- TUTORIA Regressao Multipla - ECONOMETRIA I - PROF. SUSAN SCHOMMER ------------------------
##########################################################################################################

install.packages("readxl") # Instala o pacote readxl (só precisa rodar esta linha caso ele ainda não tenha sido baixado no computador)
library(readxl)   # Carrega o pacote readxl, que possui funcionalidades para ler arquivos Excel

# Quando não usamos Projeto:

setwd("/Users/mclarasoares/Documents/Tutoria Econometria/Dados") # O caminho para o arquivo com os dados varia para cada computador, dificultando o compartilhamento de scripts

# Podemos criar projetos, agrupando dados, scripts, plots etc numa mesma pasta compartilhável 

# Dentro do projeto Tutoria Econometria há uma subpasta destinada aos scripts e outra destinada aos arquivos de dados (essa organização é uma boa prática de programação)

# Lendo os dados a partir de um arquivo Excel:

Dados_Auto <- read_xlsx("Dados/auto.xlsx") # Agora, só precisamos indicar o caminho dentro do projeto, que não varia de acordo com cada computador 
View(Dados_Auto) # Visualisando a base de dados para verificar o número de observações e quais são suas variáveis 

####################################################################################################
# Exemplo do livro Causal Inference: The Mixtape (Scott Cunningham)

# Queremos estimar o efeito da variação no comprimento do carro (em polegadas) sobre a variação do seu preço.

#----------------------------------------------------------------------------------------------------
# Vamos começar com o modelo de regressão simples: price = B0 + B1*length+u

# Qual sinal você espera que o coeficiente B1 tenha? + comprimento -> + preço ou + comprimento -> - preço?

# Estimando o modelo de regressão simples:

modelo1<- lm(price~length, data = Dados_Auto)
summary(modelo1)

# Uma polegada a mais de comprimento do carro faz com que, em média, o preço seja $57,20 mais caro. 
# Relembrando: B1^ é a inclinação da reta de regressão, que indica o quanto a variável Y muda dada uma variação de uma unidade em X.
# Entretanto, o comprimento não parece ser a única característica que tem efeito sobre o preço de um carro. Precisamos controlar por outras variáveis explicativas. 

#----------------------------------------------------------------------------------------------------

# Agora, vamos estimar um modelo de regressão múltipla: price = B0+B1*length+B2*weight+B3*headroom+B4*MPG+u

# Quando controlamos por outras variáveis, você espera que B1 tenha qual sinal?

modelo2 <- lm(price ~ length + weight + headroom + mpg, data = Dados_Auto)
summary(modelo2)

# Ao incluir outras variáveis explicativas na regressão, o coeficiente B1 passa a ter sinal negativo
# Ou seja, aumentar o comprimento do carro em uma polegada faz com que, em média, o preço seja $94,50 mais barato.
# Repare também que não só o sinal mudou, mas também a magnitude do efeito do comprimento sobre o preço se tornou bem maior quando controlamos por outras variáveis explicativas. 
# Essa mudança na estimação de B1 indica que o efeito do comprimento estava sendo confundido por várias outras variáveis.
# Weight, Headroom e Miles per gallon são correlacionados tanto com Length quanto com Price e, portanto, precisamos incluí-las no nosso modelo de regressão.

plot(Dados_Auto$weight,Dados_Auto$length) # Gráfico mostrando que weight e length são variáveis positivamente correlacionadas 

#----------------------------------------------------------------------------------------------------

# E se quisermos converter length de inches para cm?

length_cm <- Dados_Auto$length*2.54 # Cria uma nova variável a partir da variável inches da base de dados

# Para selecionar uma variável da base de dados: NomedaBase$NomedaVariável

modelo3 <- lm(price ~ length_cm+ weight + headroom + mpg, data = Dados_Auto)
summary(modelo3)

# O coeficiente B1^ é tal que -94.497/2.54
# Quando multiplicamos a variável inches por 2.54, ao invés de termos uma variação de uma unidade, temos uma variação de a unidades em X. 
# Isso faz com que o efeito de X em Y também esteja multiplicado por a.
# Para que o coeficiente B1 continue representando o efeito da variação de uma unidade de X em Y, é preciso que o valor estimado do coeficiente seja dividido por a. 

#----------------------------------------------------------------------------------------------------

# Vamos analisar os resíduos do modelo de regressão múltipla:

residuos_modelo1 <- resid(modelo1) # Extrai os resíduos da regressão  
mean(residuos_modelo1) # Calcula a média dos resíduos 

# Uma das propriedades algébricas do MQO é a de que a média amostral resíduos é igual a zero.
# Vamos visualizar esta hipótese também graficamente, plotando a distribuição dos resíduos ao redor da média:

plot(residuos_modelo1,type = "p", col = "blue")
abline(h = mean(residuos_modelo1)) # Adiciona uma linha horizontal no ponto do eixo y equivalente à média dos resíduos

plot(Dados_Auto$weight,Dados_Auto$length)

