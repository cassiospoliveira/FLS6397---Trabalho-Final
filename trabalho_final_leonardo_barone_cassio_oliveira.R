### nome <- "Cassio Santos Pinto de Oliveira"
### programa <- "Mestrado em Ciencia Politica"
### n_usp <- 6912700
### data_entrega: "31/07/2017"

# Introdução à Programação e Ferramentas Computacionais para as Ciências Sociais
# Prof. Dr. Leonardo Sangali Barone
# 2017
# Trabalho Final

#------- OBJETIVOS -------#

# O trabalho a seguir está dividido em duas etapas, visando a sistematização
# de informações sobre o STF através de diversas ferramentas computacionais
# desenvolvidas durante o curso. Embora haja uma clara continuidade entre cada uma
# das etapas, de fato os bancos que utilizamos tiveram também algumas informações
# coletadas manualmente e de outras fontes.
# Assim, nosso objetivo central aqui é mais avançar para exemplificar as diferentes
# etapas para a sistematização e análise de dados, utilizando pesquisa de mestrado
# em andamento do autor, que obter um script único que rode todas as etapas
# do começo ao fim. A vantagem dessa abordagem está no fato de que, através dos
# exemplos e comentários, podemos adaptar o código para diferentes necessidades
# futuras.

# Segue cada uma das etapas em questão:

# 1.) WEBSCRAPPING

# Coleta de alguns dados em ADIs no site do STF, através de consulta
# a formulários em páginas de acompanhamento processual

# 2.) COMBINAÇÕES DE DADOS

# Montagem de banco de dados a partir dos julgamentos, combinando dados de três fontes
# diferentes e unindo-o com outros bancos de dados relacionados às leis questionadas no STF


# ------------------------------------------------------ #

# 1.) WEBSCRAPPING

# Ao entrar na página do STF, notamos que os endereços das páginas de andamentos
# de ações diretas de inconstitucionalidade (ADIs) consultadas não tem um padrão,
# sendo indexadas por um parâmetro chamado "iincidente", cujo padrão não conseguimos
# identificar. Assim, primeiro devemos obter essas informações a fim de prosseguir.

# O script abaixo nos ajuda com essa primeira tarefa.

# Limpando ambiente de trabalho
# rm(list = ls())

# 1.1) Busca de incidentes

# Chamando bibliotecas
# install.packages ("tidyverse")
library(rvest)
# install.packages("purrr")
library(purrr)

# Função para busca de incidentes

stf_incidente <- function(tipo, numero){ # Parâmetros: tipo de processo, dentre
# as diferentes classes processuais (cuja lista pode ser consultada aqui:
# < http://www.stf.jus.br/portal/processo/listarClasse.asp?numero= > ) e número
# do processo.
  
  # Abrindo sessão na qual o formulário deve ser preenchido para acesso às páginas
  # de acompanhamento processual
  stf_session <- html_session("http://www.stf.jus.br/portal/processo/listarProcesso.asp")
  # Estabelecendo url na sessão
  stf_session$url <- "http://www.stf.jus.br/portal/processo/listarProcesso.asp"
  # O formulário a ser preenchido é o terceiro da lista (pois há outros dois na página)
  stf_form <- html_form(read_html(stf_session))[[3]]
  # Configurando o preenchimento do formulário, onde o parâmetro "numero", que
  # aparece no formulário do próprio site é preenchido com o valor colocado
  # para enumerar o processo no argumento da função 
  stf_form <- set_values(stf_form, 
                         numero = numero)
  # Submetendo o formulário na seção
  stf_submission <- submit_form(stf_session, stf_form)
  
  # Agora, começamos a extrair informações dos processos com o rvest.
  # Começamos pesquisando todos os processos encontrados com o número pesquisado
  processos <- stf_submission %>% 
    html_nodes("td a") %>%  # Extraindo nodes
    html_text() # Extraindo texto
  
  # Para pegar os links de cada processo (nos  quais constam os valores dos
  # "incidentes"), utilizamos o "href"
  incidentes <- stf_submission %>% 
    html_nodes("td a") %>% 
    html_attr("href")
  
  # Por fim, utilizamos a função substring para extrair apenas o valor dos
  # incidentes dos links
  incidentes <- as.numeric(
    substr(incidentes, 43, nchar(incidentes))
  )
  
  # Como queremos apenas o incidente correspondente à classe processual
  # pesquisada, utilizamos a função grep para filtrá-lo pelo argumento
  # "tipo", requerido pela função.
  incidentes[grep(tipo, processos)]
}

# Testando a função com dez ADIs aleatórias. Configuramos uma seed para o RNG
# a fim de facilitar a replicabilidade
classe <- "ADI"

set.seed(201701)
vetor <- sample(1:5000, 10)

bd_incidentes <- map_dbl(vetor, function(x) {stf_incidente(classe, x)})

# Dessa forma, a partir da elaboração dessa lista, podemos incluir o número de
# incidente nas páginas de andamento processual, substituindo "INCIDENTE"
# por esse valor no link abaixo:
# "http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?incidente=INCIDENTE"
# Outra página que também oferece o mesmo padrão é a página de detalhes de processos,
# que também utilizaremos na próxima etapa.
# "http://www.stf.jus.br/portal/processo/verProcessoDetalhe.asp?incidente=INCIDENTE"

# O que faremos a seguir são dois exercícios de coleta de dados, um para cada página
# elencada (andamentos e detalhes do processo): o primeiro com expressões regulares e
# outro com o pacote rvest. A finalidade de ambos é a raspagem de datas relacionadas
# ao início do andamento da ação, mas poderiam ser utilizadas para coletar
# as mais diversas informações.
# Nos dois casos, utilizaremos as ADIs aleatórias coletadas como exercício,
# mas podem ser utilizadas todas as ADIs.
# ANDAMENTOS: data de primeira distribuição (expressões regulares)
# DETALHES: data de entrada no STF (frequentemente diverge da data de primeira
# distribuição) (rvest).

# Criando urls que servirão de base para substituições nos loops
andamentos <- "http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?incidente=INCIDENTE"
detalhes <- "http://www.stf.jus.br/portal/processo/verProcessoDetalhe.asp?incidente=INCIDENTE"

k<-0
trechodatas<-NULL
datas<-NULL
Distribuicoes<-NULL

for (i in bd_incidentes){
  print(i)
  # Substituindo números de páginas
  url <- gsub("INCIDENTE", i, andamentos)
  # Lendo e parseando páginas contidas nas urls
  pagina <- readLines(url)
  # Pegando datas de distribuição através das expressões regulares nas linhas. Também
  # poderia ser feito utilizando o rvest e localizando o XPath correspondente.
  # Os colchetes dão conta de dois cenários possíveis: um no qual a palavra "Distribuído"
  # vem acentuada e outro no qual ela não ve com acento.
  Distribuicoes <- grep("<td><span>Distribu[í,i]do", pagina, ignore.case = TRUE)
  # Pegando a primeira data de distribuição (última do vetor), já que um processo
  # pode ser distribuído mais de uma vez
  Distribuicoes <- Distribuicoes[length(Distribuicoes)]
  # Incluímos o "if" para lidar com possíveis exceções (caso ADI não encontre data
  # de distribuição claramente estabelecida)
  if(length(Distribuicoes) != 0)
  {
    # Essa nova variável permite incrementar o vetor ignorando os casos nos quais não
    # há data de distribuição estabelecida no andamento dos processos
    k <- k+1
    # Os trechos onde estão as datas estão sempre uma linha acima da linha encontrada
    # em Distribuicoes
    trechodatas[k] <- pagina[Distribuicoes-1]
    # O substring pega apenas a data correspondente
    datas[k] <- substr(trechodatas[k], 11, 20)
  }
  
}

# Loop para coletar datas de entrada nas páginas de detalhes:
k <- 0
data_entrada <- NULL
for (i in bd_incidentes){
  print(i)
  # Substituindo números de páginas
  url <- gsub("INCIDENTE", i, detalhes)
  # Lendo e parseando páginas contidas nas urls
  pagina <- read_html(url)

  # Inspecionando os elementos, encontramos que as datas de entrada estão no padrão
  "td b"
  entrada <- pagina %>%
    html_nodes("td b") %>%
    html_text()
  k <- k+1
  # As datas de entrada são as últimas da lista
  data_entrada[k] <- entrada[length(entrada)]
}

# FIM DA SEÇÃO 1



# As seção 2 é a adaptação de script que foi feito a partir dos conhecimentos adquiridos
# na disciplina para a elaboração do banco de dados para a utilização na dissertação
# de mestrado do discente. Ela utiliza bancos de dados que podem ser disponibilizados
# a partir de contato com o autor.

# Script para criação de banco de dados que une dados de ADI a dados de 
# normas questionadas a partir de recorte pré-definido.

#install.packages("tidyr")
#install.packages("data.table")
library(dplyr)
library(data.table)
library(readr)

# Diretório de trabalho
wordir <- "C:/Users/Cassio/Dropbox/Mestrado/Dados - Mestrado"
setwd(wordir)
rm(list=ls())

# Carregando primeiro banco, elaborado pelo autor:
# BDSTF
# Lendo banco
bdstf <- read.csv2("ADIs_1988-2014.csv", stringsAsFactors = FALSE)
# Estrutura do banco
str(bdstf)

# Realizando modificações necessárias
bdstf <- bdstf %>%
  # Filtrando apenas ADIs com julgamento final, removendo medidas cautelares
  # (coluna "ADI...MCs" recebendo NA)
  filter(is.na(ADI...MCs) == TRUE) %>%
  # selecionando variáveis de interesse
  select(NumCaso, Status, Data.de.Entrada, DataDecisao, tEntradaDecisao,
         CompInicial, CompCorte, Requerente1, Requerente2, Autor1, Autor2) %>%
  # Renomeando variáveis
  rename(adi = NumCaso, resultado = Status, entrada = Data.de.Entrada, 
         decisao = DataDecisao, tdecisao = tEntradaDecisao,
         compinicial = CompInicial, compfinal = CompCorte, req1 = Requerente1,
         req2 = Requerente2, autor1 = Autor1, autor2 = Autor2) %>%
  # Convertendo número de ADIs em inteiro
  mutate(adi = as.integer(adi))

head(bdstf)

# Segundo banco, de Júlio Canello - disponível publicamente no Dataverse em
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/8WV3A4
# e utilizado para verificar as leis questionadas em cada ADI.
# CANELLO
bdcan <- read.csv2("acoes_canello_recorte_pesquisa.csv")
head(bdcan)

bdcan <- bdcan %>%
  # Selecionando variáveis de interesse
  select(num, relator.resultado, vencido, diploma.legal, ambito, tipo.norma) %>%
  # Renomeando
  rename(adi = num, relator = relator.resultado, norma = diploma.legal,
         competencia = ambito, tiponorma = tipo.norma) %>%
  # Modificações: variável "norma" recebe a sigla "LF" para leis federais;
  # variável vencido (que categoriza relator em vencido ou não) é transformada em dummy
  mutate(norma = gsub("Lei ", "LF ", norma),
         vencido = ifelse(vencido == "vencido", 1, 0))

# Banco de dados do NECI, com informações sobre normas
# NECI
bdnec <- read.csv2("Dados - NECI.csv")

bdnec <- bdnec %>%
  # Selecionando variáveis de interesse
  select(TIPO_PROJETO, NUM_LEI_FORMATADO, ORIGEM, AUTOR, PARTIDO, DATAORIG, 
         DATAULT, COALIZAO, PROJETO_ORIGINAL, ID_coalizao, GOVERNO) %>%
  # Renomeando-as
  rename(tipoproj = TIPO_PROJETO, numlei = NUM_LEI_FORMATADO, iniciativa = ORIGEM,
         proponente = AUTOR, partidoprop = PARTIDO, dataprop = DATAORIG,
         dataaprov = DATAULT, coalizao = COALIZAO, alterado = PROJETO_ORIGINAL,
         idcoalizao = ID_coalizao, governo = GOVERNO) %>%
  # Recodificando os tipos de norma para facilitar a combinação dos bancos, recriando também
  # a variável "norma" com o tipo de norma e seu número, como no banco anterior
  mutate(tiponorma = recode(tipoproj, "PL." = "LF", "PLN" = "LF", "PLP" = "LC", "PEC" = "EC"),
         numlei = as.character(numlei),
         numlei = gsub("^0", "", numlei),
         norma = paste0(tiponorma, " ", substr(numlei, 1, nchar(numlei)-4),
                        substr(numlei, nchar(numlei)-1, nchar(numlei)))) %>%
  # Selecionando variáveis nocamente após alterações
  select(norma, tiponorma, tipoproj, iniciativa, proponente, partidoprop, dataprop, dataaprov,
         coalizao, alterado, idcoalizao, governo)



# Juntando os bancos de dados
# BDSTF e CANELLO

# O primeiro "join" é pelo número das ADIs
comb1 <- bdstf %>%
  right_join(bdcan, by = "adi") %>%
  filter(is.na(adi) == FALSE)

# O segundo "join" é pela variável "norma"
# COMB1 e NECI

comb2 <- comb1 %>%
  left_join(bdnec, by = "norma", suffix = c("_canello", "_neci"))

View(comb2)

# Banco final
write.csv2(comb2, "bd_adinorma.csv", row.names = F)