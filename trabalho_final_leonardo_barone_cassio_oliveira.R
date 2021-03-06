### nome <- "Cassio Santos Pinto de Oliveira"
### programa <- "Mestrado em Ciencia Politica"
### n_usp <- 6912700
### data_entrega: "31/07/2017"

# Introdu��o � Programa��o e Ferramentas Computacionais para as Ci�ncias Sociais
# Prof. Dr. Leonardo Sangali Barone
# 2017
# Trabalho Final

#------- OBJETIVOS -------#

# O trabalho a seguir est� dividido em duas etapas, visando a sistematiza��o
# de informa��es sobre o STF atrav�s de diversas ferramentas computacionais
# desenvolvidas durante o curso. Embora haja uma clara continuidade entre cada uma
# das etapas, de fato os bancos que utilizamos tiveram tamb�m algumas informa��es
# coletadas manualmente e de outras fontes.
# Assim, nosso objetivo central aqui � mais avan�ar para exemplificar as diferentes
# etapas para a sistematiza��o e an�lise de dados, utilizando pesquisa de mestrado
# em andamento do autor, que obter um script �nico que rode todas as etapas
# do come�o ao fim. A vantagem dessa abordagem est� no fato de que, atrav�s dos
# exemplos e coment�rios, podemos adaptar o c�digo para diferentes necessidades
# futuras.

# Segue cada uma das etapas em quest�o:

# 1.) WEBSCRAPPING

# Coleta de alguns dados em ADIs no site do STF, atrav�s de consulta
# a formul�rios em p�ginas de acompanhamento processual

# 2.) COMBINA��ES DE DADOS

# Montagem de banco de dados a partir dos julgamentos, combinando dados de tr�s fontes
# diferentes e unindo-o com outros bancos de dados relacionados �s leis questionadas no STF


# ------------------------------------------------------ #

# 1.) WEBSCRAPPING

# Ao entrar na p�gina do STF, notamos que os endere�os das p�ginas de andamentos
# de a��es diretas de inconstitucionalidade (ADIs) consultadas n�o tem um padr�o,
# sendo indexadas por um par�metro chamado "iincidente", cujo padr�o n�o conseguimos
# identificar. Assim, primeiro devemos obter essas informa��es a fim de prosseguir.

# O script abaixo nos ajuda com essa primeira tarefa.

# Limpando ambiente de trabalho
# rm(list = ls())

# 1.1) Busca de incidentes

# Chamando bibliotecas
# install.packages ("tidyverse")
library(rvest)
# install.packages("purrr")
library(purrr)

# Fun��o para busca de incidentes

stf_incidente <- function(tipo, numero){ # Par�metros: tipo de processo, dentre
# as diferentes classes processuais (cuja lista pode ser consultada aqui:
# < http://www.stf.jus.br/portal/processo/listarClasse.asp?numero= > ) e n�mero
# do processo.
  
  # Abrindo sess�o na qual o formul�rio deve ser preenchido para acesso �s p�ginas
  # de acompanhamento processual
  stf_session <- html_session("http://www.stf.jus.br/portal/processo/listarProcesso.asp")
  # Estabelecendo url na sess�o
  stf_session$url <- "http://www.stf.jus.br/portal/processo/listarProcesso.asp"
  # O formul�rio a ser preenchido � o terceiro da lista (pois h� outros dois na p�gina)
  stf_form <- html_form(read_html(stf_session))[[3]]
  # Configurando o preenchimento do formul�rio, onde o par�metro "numero", que
  # aparece no formul�rio do pr�prio site � preenchido com o valor colocado
  # para enumerar o processo no argumento da fun��o 
  stf_form <- set_values(stf_form, 
                         numero = numero)
  # Submetendo o formul�rio na se��o
  stf_submission <- submit_form(stf_session, stf_form)
  
  # Agora, come�amos a extrair informa��es dos processos com o rvest.
  # Come�amos pesquisando todos os processos encontrados com o n�mero pesquisado
  processos <- stf_submission %>% 
    html_nodes("td a") %>%  # Extraindo nodes
    html_text() # Extraindo texto
  
  # Para pegar os links de cada processo (nos  quais constam os valores dos
  # "incidentes"), utilizamos o "href"
  incidentes <- stf_submission %>% 
    html_nodes("td a") %>% 
    html_attr("href")
  
  # Por fim, utilizamos a fun��o substring para extrair apenas o valor dos
  # incidentes dos links
  incidentes <- as.numeric(
    substr(incidentes, 43, nchar(incidentes))
  )
  
  # Como queremos apenas o incidente correspondente � classe processual
  # pesquisada, utilizamos a fun��o grep para filtr�-lo pelo argumento
  # "tipo", requerido pela fun��o.
  incidentes[grep(tipo, processos)]
}

# Testando a fun��o com dez ADIs aleat�rias. Configuramos uma seed para o RNG
# a fim de facilitar a replicabilidade
classe <- "ADI"

set.seed(201701)
vetor <- sample(1:5000, 10)

bd_incidentes <- map_dbl(vetor, function(x) {stf_incidente(classe, x)})

# Dessa forma, a partir da elabora��o dessa lista, podemos incluir o n�mero de
# incidente nas p�ginas de andamento processual, substituindo "INCIDENTE"
# por esse valor no link abaixo:
# "http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?incidente=INCIDENTE"
# Outra p�gina que tamb�m oferece o mesmo padr�o � a p�gina de detalhes de processos,
# que tamb�m utilizaremos na pr�xima etapa.
# "http://www.stf.jus.br/portal/processo/verProcessoDetalhe.asp?incidente=INCIDENTE"

# O que faremos a seguir s�o dois exerc�cios de coleta de dados, um para cada p�gina
# elencada (andamentos e detalhes do processo): o primeiro com express�es regulares e
# outro com o pacote rvest. A finalidade de ambos � a raspagem de datas relacionadas
# ao in�cio do andamento da a��o, mas poderiam ser utilizadas para coletar
# as mais diversas informa��es.
# Nos dois casos, utilizaremos as ADIs aleat�rias coletadas como exerc�cio,
# mas podem ser utilizadas todas as ADIs.
# ANDAMENTOS: data de primeira distribui��o (express�es regulares)
# DETALHES: data de entrada no STF (frequentemente diverge da data de primeira
# distribui��o) (rvest).

# Criando urls que servir�o de base para substitui��es nos loops
andamentos <- "http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?incidente=INCIDENTE"
detalhes <- "http://www.stf.jus.br/portal/processo/verProcessoDetalhe.asp?incidente=INCIDENTE"

k<-0
trechodatas<-NULL
datas<-NULL
Distribuicoes<-NULL

for (i in bd_incidentes){
  print(i)
  # Substituindo n�meros de p�ginas
  url <- gsub("INCIDENTE", i, andamentos)
  # Lendo e parseando p�ginas contidas nas urls
  pagina <- readLines(url)
  # Pegando datas de distribui��o atrav�s das express�es regulares nas linhas. Tamb�m
  # poderia ser feito utilizando o rvest e localizando o XPath correspondente.
  # Os colchetes d�o conta de dois cen�rios poss�veis: um no qual a palavra "Distribu�do"
  # vem acentuada e outro no qual ela n�o ve com acento.
  Distribuicoes <- grep("<td><span>Distribu[�,i]do", pagina, ignore.case = TRUE)
  # Pegando a primeira data de distribui��o (�ltima do vetor), j� que um processo
  # pode ser distribu�do mais de uma vez
  Distribuicoes <- Distribuicoes[length(Distribuicoes)]
  # Inclu�mos o "if" para lidar com poss�veis exce��es (caso ADI n�o encontre data
  # de distribui��o claramente estabelecida)
  if(length(Distribuicoes) != 0)
  {
    # Essa nova vari�vel permite incrementar o vetor ignorando os casos nos quais n�o
    # h� data de distribui��o estabelecida no andamento dos processos
    k <- k+1
    # Os trechos onde est�o as datas est�o sempre uma linha acima da linha encontrada
    # em Distribuicoes
    trechodatas[k] <- pagina[Distribuicoes-1]
    # O substring pega apenas a data correspondente
    datas[k] <- substr(trechodatas[k], 11, 20)
  }
  
}

# Loop para coletar datas de entrada nas p�ginas de detalhes:
k <- 0
data_entrada <- NULL
for (i in bd_incidentes){
  print(i)
  # Substituindo n�meros de p�ginas
  url <- gsub("INCIDENTE", i, detalhes)
  # Lendo e parseando p�ginas contidas nas urls
  pagina <- read_html(url)

  # Inspecionando os elementos, encontramos que as datas de entrada est�o no padr�o
  "td b"
  entrada <- pagina %>%
    html_nodes("td b") %>%
    html_text()
  k <- k+1
  # As datas de entrada s�o as �ltimas da lista
  data_entrada[k] <- entrada[length(entrada)]
}

# FIM DA SE��O 1



# As se��o 2 � a adapta��o de script que foi feito a partir dos conhecimentos adquiridos
# na disciplina para a elabora��o do banco de dados para a utiliza��o na disserta��o
# de mestrado do discente. Ela utiliza bancos de dados que podem ser disponibilizados
# a partir de contato com o autor.

# Script para cria��o de banco de dados que une dados de ADI a dados de 
# normas questionadas a partir de recorte pr�-definido.

#install.packages("tidyr")
#install.packages("data.table")
library(dplyr)
library(data.table)
library(readr)

# Diret�rio de trabalho
wordir <- "C:/Users/Cassio/Dropbox/Mestrado/Dados - Mestrado"
setwd(wordir)
rm(list=ls())

# Carregando primeiro banco, elaborado pelo autor:
# BDSTF
# Lendo banco
bdstf <- read.csv2("ADIs_1988-2014.csv", stringsAsFactors = FALSE)
# Estrutura do banco
str(bdstf)

# Realizando modifica��es necess�rias
bdstf <- bdstf %>%
  # Filtrando apenas ADIs com julgamento final, removendo medidas cautelares
  # (coluna "ADI...MCs" recebendo NA)
  filter(is.na(ADI...MCs) == TRUE) %>%
  # selecionando vari�veis de interesse
  select(NumCaso, Status, Data.de.Entrada, DataDecisao, tEntradaDecisao,
         CompInicial, CompCorte, Requerente1, Requerente2, Autor1, Autor2) %>%
  # Renomeando vari�veis
  rename(adi = NumCaso, resultado = Status, entrada = Data.de.Entrada, 
         decisao = DataDecisao, tdecisao = tEntradaDecisao,
         compinicial = CompInicial, compfinal = CompCorte, req1 = Requerente1,
         req2 = Requerente2, autor1 = Autor1, autor2 = Autor2) %>%
  # Convertendo n�mero de ADIs em inteiro
  mutate(adi = as.integer(adi))

head(bdstf)

# Segundo banco, de J�lio Canello - dispon�vel publicamente no Dataverse em
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/8WV3A4
# e utilizado para verificar as leis questionadas em cada ADI.
# CANELLO
bdcan <- read.csv2("acoes_canello_recorte_pesquisa.csv")
head(bdcan)

bdcan <- bdcan %>%
  # Selecionando vari�veis de interesse
  select(num, relator.resultado, vencido, diploma.legal, ambito, tipo.norma) %>%
  # Renomeando
  rename(adi = num, relator = relator.resultado, norma = diploma.legal,
         competencia = ambito, tiponorma = tipo.norma) %>%
  # Modifica��es: vari�vel "norma" recebe a sigla "LF" para leis federais;
  # vari�vel vencido (que categoriza relator em vencido ou n�o) � transformada em dummy
  mutate(norma = gsub("Lei ", "LF ", norma),
         vencido = ifelse(vencido == "vencido", 1, 0))

# Banco de dados do NECI, com informa��es sobre normas
# NECI
bdnec <- read.csv2("Dados - NECI.csv")

bdnec <- bdnec %>%
  # Selecionando vari�veis de interesse
  select(TIPO_PROJETO, NUM_LEI_FORMATADO, ORIGEM, AUTOR, PARTIDO, DATAORIG, 
         DATAULT, COALIZAO, PROJETO_ORIGINAL, ID_coalizao, GOVERNO) %>%
  # Renomeando-as
  rename(tipoproj = TIPO_PROJETO, numlei = NUM_LEI_FORMATADO, iniciativa = ORIGEM,
         proponente = AUTOR, partidoprop = PARTIDO, dataprop = DATAORIG,
         dataaprov = DATAULT, coalizao = COALIZAO, alterado = PROJETO_ORIGINAL,
         idcoalizao = ID_coalizao, governo = GOVERNO) %>%
  # Recodificando os tipos de norma para facilitar a combina��o dos bancos, recriando tamb�m
  # a vari�vel "norma" com o tipo de norma e seu n�mero, como no banco anterior
  mutate(tiponorma = recode(tipoproj, "PL." = "LF", "PLN" = "LF", "PLP" = "LC", "PEC" = "EC"),
         numlei = as.character(numlei),
         numlei = gsub("^0", "", numlei),
         norma = paste0(tiponorma, " ", substr(numlei, 1, nchar(numlei)-4),
                        substr(numlei, nchar(numlei)-1, nchar(numlei)))) %>%
  # Selecionando vari�veis nocamente ap�s altera��es
  select(norma, tiponorma, tipoproj, iniciativa, proponente, partidoprop, dataprop, dataaprov,
         coalizao, alterado, idcoalizao, governo)



# Juntando os bancos de dados
# BDSTF e CANELLO

# O primeiro "join" � pelo n�mero das ADIs
comb1 <- bdstf %>%
  right_join(bdcan, by = "adi") %>%
  filter(is.na(adi) == FALSE)

# O segundo "join" � pela vari�vel "norma"
# COMB1 e NECI

comb2 <- comb1 %>%
  left_join(bdnec, by = "norma", suffix = c("_canello", "_neci"))

View(comb2)

# Banco final
write.csv2(comb2, "bd_adinorma.csv", row.names = F)