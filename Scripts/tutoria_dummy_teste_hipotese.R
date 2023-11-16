### Econometria I - Professora Susan Schommer

### Tutoria 2




######################### Regressao multipla, dummys e testes de significancia 

#####################################################################################################################
#####################################################################################################################

#_________________________Preparando ambiente __________________________________


  # Definindo diretorio:


setwd("C:/Users/.../Documents/.../Tutoria - Econometria/Tutoria 2")

# Relembrando a tutoria passada: quando usamos um projeto, não precisamos nos preocupar em definir diretório 
#_______________________________________________________________________________

# Utilizaremos os seguintes pacotes: wooldridge, ggplot2

# Hoje vamos usar a base do Wooldridge 

# ggplot2: pacote para fazer gráficos mais elaborados do que aqueles com o comando plot do R-base

# Caso ainda não tenha estes pacotes instalados no computador, o primeiro passo é: 

install.packages("wooldridge")
install.packages("ggplot2")



# No inicio de qualquer script, carregar os pacotes: 

library(wooldridge)
library(ggplot2)



#_________________________________ Dados________________________________________
  
# Do pacote wooldridge, utilizaremos os dados chamados 'wage1' (atribuindo os dados ao dataframe 'dados')

dados <- wage1 # Dados da força de trabalho dos EUA em 1976

# Armazeno os dados a serem utilizados num objeto chamado "dados"

View(dados) # Visualisando a base de dados para verificar o número de observações e quais são suas variáveis 

# Mais variáveis e mais observações em comparação com a base de dados da tutoria passada

# Cada observacao representa uma pessoa da nossa amostra de pessoas da forca de trabalho dos EUA em 1976

# Analisando média e desvio-padrão de algumas das variáveis que vamos usar:

mean(dados$wage)
sd(dados$wage)
median(dados$wage)

mean(dados$educ)
sd(dados$educ)
median(dados$educ)

####################################################################################################################
####################################################################################################################

# Queremos estimar o segunte modelo: salario = B0 + B1*educ + BiXi + u, Xi: outras variaveis explicativas

#_______________________ Revisao________________________________________________
  
  
# Regressao simples: wage = B0+B1*educ+u

# Visualizando graficamente a reta de regressao:

# "A construção de gráficos com o ggplot2 é como construir uma escultura de Lego, no sentido de que o gráfico a ser construído é composto de 'blocos' que são empilhados"
# "+" indica que haverá uma continuação nas instruções

reta_reg <- ggplot(data = dados,aes(x=educ, y=wage)) + # em data indicamos quais dados queremos usar no gráfico e aes indica os argumentos dos eixos x e y
    geom_point()+ # geom_point indica o tipo de gráfico que queremos plotar (nesse caso, gráfico de ponto)  
    geom_smooth (method = 'lm',se = F) # geom_smooth adiciona mais uma camada ao gráfico, com a linha de regressao da regressão simples wage~educ


reta_reg # visualizando o gráfico 

# para cada nível de escolaridade na amostra, temos diversos valores de salários: não é uma relaçao deterministica

# a reta de regressão me diz qual seria o valor esperado do salario por anos de escolaridade,

# isto é, dado n anos de educacao, espera-se ganhar um salário de $y. Note que este valor,
    
# é em MEDIA: para um nivel de escolaridade x, na realidade existem pessoas com diferentes salarios.

# temos a média do salário como uma função de escolaridade e o salário médio varia positivamente em relaçao aos anos de escolaridade

# Se para cada x, a dispersao é muito grande (ou existem outliers), entao nossas estimativas podem 
    
# ser viesadas para cima ou para baixo.



reg_simples <- lm(wage~educ, data = dados)
summary(reg_simples)
    
# Interpretação do b1?

# b1= 0.54136

# para 1 ano a mais de escolaridade é previsto um aumento no salário-hora de, em média, $0.54.

#__________________ Unidades de medida: (wooldridge, cap 2, secao 2.4)__________



# E se o salario estivesse em log?

# Pela regressão linear anterior, $0.54 é o aumento tanto para o primeiro ano de educação quanto para o vigésimo ano de educação

# isso não parece ser factível

# Uma caracterização melhor de como o salário muda com o nível de escolaridade é que cada ano de educação aumenta o salário em uma porcentagem constante

# Vamos usar agora como y o log do salário-hora (já presente da base de dados)

reg_simples_lny <- lm(lwage~educ, data = dados)
summary(reg_simples_lny)

# b1 = 0.082744

# variacao % wage = 100*b1

# Interpretacao: 1 ano a mais de educacao aumenta, em média, em 8% o valor do salario-hora. 
    

# Relacoes com ln:

#### y        -   x       -> Ay = b * Ax        (1 und a mais em x, gera uma variacao de b und em y)
#### y        -   log(x)  -> Ay = (b/100) * Ax  (1% a mais de x, gera b/100 variacao de b und em y)
#### log(y)   -   x       -> Ay = (b*100) * Ax  (1 und a mais de x, gera uma variacao de b*100 % em y)
#### log(y)   -   log(x)  -> Ay = b Ax          (1 % a mais de x, gera uma variacao de b% em y)





####################################################################################################################
####################################################################################################################


#______________________________________ DUMMIES_________________________________
    
# O que é uma dummy?

# Dummies sao variaveis binarias criadas para incorporar no modelo variaveis categoricas (aspecto qualitativo)

# Exemplos: Genero, Nivel de escolaridade, regiao, estado civil,...



#_______________Trabalhando com dummies: (wooldridge, cap 7, secao 7.2)_________


# Sera que homens e mulheres ganham salarios iguais? 

# Estrutura da variável dummy: não podemos denominar a variável como "genero", pois o valor de 1 ou 0 nao nos informaria nada

# 2 formas de identificar se existe ou nao diferenca:


# Forma 1- Media dos salarios de homens e mulheres:

aggregate(wage ~ female, data = dados, FUN = "mean") # FUN indica qual estatística dos dados voce quer obter (nesse caso, queremos a média)

# Comparar com a média da amostra total que obtivemos anteriormente

 
# Calculando a diferenca da media salarial entre homens e mulheres

4.587659 - 7.099489 # diferenca entre os salarios femininos e masculinos

# -2.51183

# Mulheres ganham, em média, $ 2.51 a menos do que os homens na amostra. 

#_______________________________________________________________________________

# Forma 2- Usando Regressao:

# Lembre que b0 é coef linear da regressao. No caso de uma variavel dummy, b1 assumira tambem esse papel,
# isto é, se b1 é significativo, para a categoria = 1, entao o coeficiente linear sera dado por b0+b1.


# Modelo: wage = b0 + b1 female + u

reg_genero <- lm(wage ~ female, data = dados) 

summary(reg_genero)

# Se female = 0, entao b0 = salario medio dos homens 

# Se female = 1, entao b0 + b1 = salario medio das mulheres (ou salario esperado)

# OBS: Essa igualdade entre os metodos SOMENTE E VALIDA SE A REGRESSAO SO CONSIDERA COMO VARIAVEL EXPLICATIVA A DUMMY

# estamos apenas calculando o valor esperado do salário para as mulheres

# Qual o salario esperado de uma mulher no metodo da regressao?

# Fazendo a conta: b0 + b1 = (7.0995) + (-2.5118) = 4.5877 -> salario esperado das mulheres do metodo 1.


#________________________ Regressao multipla:___________________________________


# Vamos voltar para a regressao de salario em educaçao: salario ~ dummy_genero + educacao

# Inspecao Grafica:

ggplot(data = dados,aes(x = educ, y = wage,colour = factor(female))) + # indicamos que queremos que o gráfico tenha cores diferentes para as duas categorias da dummy
      geom_point() 
     

# O que acontece com a regressão de salário em educação se incluirmos a dummy de genero?

reg_educ_genero <- lm(wage ~ female+educ, data = dados)

summary(reg_educ_genero)

# B1 é interpretado como a diferença no salário-hora entre homens e mulheres, dado o mesmo nível de educacao

# B1 indica se existe diferença salarial de genero

# B1 = E(wage|female=1,educ)-E(wage|female=0,educ)= -2.27

# Interpretacao: 1 ano a mais de educ aumenta em media o salario em 0.5 dolares.

# Mulheres ganham, em media, 2.27 dolares a menos do que homens, dado o mesmo grau de educacao.



# Note que, graficamente, a dummie representa um deslocamento vertical da reta da regressao simples 
# (para baixo, no caso):
# A diferenca entre os salarios nao depende da escolaridade: as retas de regressão sao paralelas, pois trata-se de um deslocamento do intercepto

dados$wage_predict_1 <- predict(reg_educ_genero)

ggplot(data = dados) + 
    geom_line(aes(x = educ, y = wage_predict_1, colour = factor(female)))

# Qual o salario esperado dos homens e das mulheres nesse caso?

# Homens: salario_h = b0 + b2 educ

# Mulheres: salario_m = b0 + b1 female + b2 educ


#_______________________________________________________________________________
    
# Poderiamos repetir o exercicio acima para os subgrupos de pessoas casadas nao-casadas na nossa amostra:

ggplot(data = dados) + 
  geom_point(aes(x = educ, y = wage, colour = factor(married)))

reg_married <- lm(wage ~ married + educ, data = dados)
summary(reg_married)

# Dado o mesmo nível de escolaridade, pessoas casadas ganham, em média, $1.52 a mais de salário-hora.

# E(wage|married=1,educ)-E(wage|married=0,educ) = 1.52

dados$wage_predict_2 <- predict(reg_married)

ggplot(data = dados) + 
    geom_line(aes(x = educ, y = wage_predict_2, colour = factor(married)))

# Ser casado desloca para cima a reta de regressão (aumento do intercepto)

# Em média, os salarios sao mais altos para o subgrupo de casados da amostra, controlado pela escolaridade
    

### Criando uma variavel dummy a partir de uma variavel numerica:

# E se quisermos criar uma dummy para anos de escolaridade igual ou maior/menor a um certo valor? 

# Ex: Dummy para saber se a pessoa tem o ensimo medio - 12 anos ou mais de escolaridade?

dados$dummy_educ <- ifelse(dados$educ >= 12, 1, 0) # crio uma variavel dummy 

# com dados$dummy_educ esta dummy é adicionada como uma nova variável no conjunto de dados

# Se educacao>=12, retorna 1, se nao 0

table(dados$dummy_educ) # Visualizando quantas observacoes têm 12< anos de educacao e quantas têm 12>= 

# Princípio geral para a inclusão de dummies que indicam grupos diferentes:

# Se temos g grupos/categorias, precisamos incluir g-1 dummies no modelo

# Exemplo: Se criamos dummies para 5 anos de educacao, 10 anos de educacao e de 12 anos ou mais, não podemos incluir todas as dummies

# O intercepto do grupo-base (grupo cuja dummy está omitida) é o intercepto global do modelo e o coeficiente da dummy de cada grupo representa a diferença estimada entre este grupo e o grupo-base

####################################################################################################################
####################################################################################################################


#____________________________TESTE DE SIGNIFICANCIA_____________________________

# Testando significancia dos coeficientes:


# Significancia de cada variavel individualmente:

# Queremos testar se os coeficientes sao significantemente diferentes de zero

reg_w_educ_gen <- lm(wage ~ female + educ, data = dados)
summary(reg_w_educ_gen)


# O teste que fazemos é:
    
    # H0: b = 0 (o coeficiente b não é significantemente diferente de zero)
    # H1: b != 0
    
    # Estatistica t apropriada é:
    # t = (b^ - 0)/sd(b^)
    
# Para a variavel educ:

  # (0.50645 - 0) / 0.05039 = 10.05061 (estatistica t na coluna 3)
  # Rejeitamos H0 se |t|> c
  # Regra de bolso: significante a 5% se |t|> 2.
  # No caso, rejeitamos h0 (b=0) e educacao tem significancia para a explicacao dos salarios ao nivel de significancia de 5%.
    
# Outra forma: olhar estatistica t na tabela de regressão ou p-valor.
    # P-valor: Menor nivel de significância com que se rejeitaria a hipotese nula 
    # é a probabilidade de rejeitar H0 quando ela é verdadeira (assumir que o coeficiente é significante, quando nao é)
    # Logo, para p-valores muito baixos rejeitamos h0. 
    
# Para a dummy female:

    # (-2.27336 - 0)/ 0.27904 = -8.147076
    # |t| > 2, rejeitamos h0 ao nivel de significancia de 5% 
    # O efeito de female sobre wage é significantemente diferente de zero. 
    
#_______________________________________________________________________________

# Significancia Conjunta (de mais de 1 variavel conjuntamente):
    
# Queremos testar se os coeficientes de um grupo de variáveis são conjuntamente diferentes de zero:
    
    # H0: b0 = b1 = b2 =... = bk = 0
    # H1: pelo menos um bi != 0
    
# Voltando ao modelo: salario ~ genero + educ + u

summary(reg_w_educ_gen)

summary(reg_w_educ_gen)$fstatistic

# F=91.315

    # Podemos testar a significancia geral da regressão ao olhar para a estatistica F ou para o P-valor
    
#Encontrando o valor crítico para nível de significancia de 5%:

qf(p=0.05,df1=2,df2=523,lower.tail = FALSE)

#3.012957

# estatistica F > valor critico: rejeitamos H0 e as variaveis sao estatisticamente significantes conjuntamente ao nível de 5%

# 5% é o nivel de significancia padrao utilizado em pesquisa aplicada

# Mas note que, para niveis menores de significancia (1%), o valor critico é maior

qf(p=0.01,df1=2,df2=523,lower.tail = FALSE)

# 4.645959
    
# Outro modelo
    
# E se quisermos testar em um modelo mais completo, se alguns coeficientes sao zero?
    
    reg_mult <- lm(wage ~ educ + exper + female + nonwhite + married, data = dados)
    summary(reg_mult)


# Primeiro, vamos testar se female, nonwhite e married são estatisticamente significantes conjuntamente:

reg_reduced_1 <- lm(wage ~ educ + exper, data = dados)
summary(reg_reduced_1)

testef_1 <- anova(reg_reduced_1, reg_mult, test = "F") 
View(testef_1) # Rejeito H0 (p-valor muito baixo e menor que todos os niveis de significancia- 10%, 5% e 1%)

# female, nonwhite e married são estatisticamente significantes conjuntamente a todos os niveis de significancia

# Agora, vamos testar se apenas as variaveis nonwhite e married são estatisticamente significantes conjuntamente:

reg_reduced_2 <- lm(wage ~ educ + exper+female, data = dados)

testef_2 <- anova(reg_reduced_2, reg_mult, test = "F") 
View(testef_2) # Rejeito H0 somente ao nivel de significancia de 10% (p-valor = 0.08555107)

######################################################################################################################
######################################################################################################################

    # Trabalho 1 de Econometria - Como filtrar a base 
    

library(readr) # Pacote para leitura de arquivos .csv

dados_trabalho <- read.csv("Dados/Trabalho1Econometria.csv")


#_________________Selecionando sua amostra para o trabalho______________________________________
    
    
table(dados_trabalho$numero_aluno) # Vemos quantas observacoes existem para cada numero_aluno
    
    # Exemplo: filtro = 77 
    # Minha amostra deve ter 827 observacoes


# Filtro com numero 77 usando o pacote tidyverse:

# tidyverse é um pacote de manipulacao de dados que oferece solucoes mais simples do que o R base
    
install.packages("tidyverse") # Caso ainda nao tenha instalado  
library(tidyverse)

# Coseguimos modificar os dados e filtrar somente os que precisamos com 2 linhas de codigo:

  dados_trabalho <- dados_trabalho %>%
    filter(numero_aluno==77) # Este comando diz: filtre os dados da coluna numero_aluno cujo valor seja 77 

# Obs: o operador pipe % esta recebendo o objeto dados_trabalho (que armazena nossa base dados) para depois aplicar a funçao filter() nos dados
  
    # Como esperado, ficamos com 827 observacoes 
  
    # Mas vamos verificar se realmente selecionamos a amostra corretamente:
  
   dados_trabalho %>%
    filter(numero_aluno!=77) # Nao existe nenhuma observacao cujo numero-aluno seja diferente de 77
    
    
##############################################################################################################
##############################################################################################################

