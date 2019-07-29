#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(plotly)
library(shiny)
source("Analise_texto.R")
source("sentimentscript.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
   # Application title
   titlePanel("Text Sentiment Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Insira o arquivo PDF a ser analisado",
                  accept = c(
                    "text/pdf",".pdf") ),
        fileInput("file2", "Insira o arquivo PDF a ser analisado",
                  accept = c(
                    "text/pdf",".pdf") ),
       #  sliderInput("Palavras",
        #             "Palavras de frequencia igual ou menor a serem rejeitadas da analise",
         #            min = 0,
          #           max = 50,
           #          value = 10),
        selectInput(inputId = "linguagens",label = "Lingua do PDF",choices = c("en","pt")),
	      numericInput(inputId="Bloco",label="Numero de palavras por ponto",min=1,max=500,value=100),
       uiOutput("Intervalos")
       #uiOutput("Intervalo2")
       
        #selectInput("escolhas",label = "Métrica",choices = c("Media","Desvio Padrao","Ambos")),
        #tabsetPanel(
        #  tabPanel("Analysys",selectInput("selecionador",choices = 1:13,label="Cluster"))
          #tabPanel("Analysys",uiOutput("selecionador"))
        #)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="Referenciador",
         tabPanel("LocalSentiment",plotlyOutput("distPlot")),
         tabPanel("LocalSentiment2",plotlyOutput("distPlot2")),
         tabPanel("Local Sentiment by sentiment",plotlyOutput("distPlotSenti")),
         tabPanel("LocalSentiment2 by sentiment",plotlyOutput("distPlot2Senti")),
         tabPanel("CumulativeSentiment",plotlyOutput("Saida")),
         tabPanel("CumulativeSentiment2",plotlyOutput("Saida2")),
         tabPanel("CumulativeSentimentbySentiment",plotlyOutput("distPlotSentiCum")),
         tabPanel("CumulativeSentimentbySentiment2",plotlyOutput("distPlotSentiCum2")),
         tabPanel("WordAnalysis",verbatimTextOutput("Informacoes"),tableOutput('TabelaSentimentos')),
         tabPanel("WordAnalysis2",verbatimTextOutput("Informacoes2"),tableOutput('TabelaSentimentos2'))
       #  tabPanel("CompareLocalSentiment",plotlyOutput("LocalComparative")),
        # tabPanel("CompareCumulativeSentiment",plotlyOutput("CumulativeComparative"))
         
         
         
         
        ),
       h3("Aplicativo desenvolvido por Rafael Silva Pereira!\n\n"),
       h4("Em caso de duvidas ou problemas favor entrar em contato\n\n"),
       h4("Insira um arquivo PDF para comecar sua analise\n\n"),
       h4("Email de contato: r.s.p.models@gmail.com")
    
       
      )
   )
) 

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000*1024^2)
  
  
  ProcArquivo<-reactive({
    if(!is.null(input$file1)){
     a=leitura(input$file1$datapath)
     b=Palavras(a,input$linguagens)
     return(b)
    }
    
  })
  
  ProcArquivo2<-reactive({
    if(!is.null(input$file2)){
      a=leitura(input$file2$datapath)
      b=Palavras(a,input$linguagens)
      return(b)
    }
    
  })
  
  
  
  #Matriz=Calculo()
  
  output$Intervalos=renderUI({
   if(input$Referenciador=="WordAnalysis"|input$Referenciador=="WordAnalysis2"){
      a=ProcArquivo()
      if(input$Referenciador=="WordAnalysis2")
        a=ProcArquivo2()
      #a=1:100
      print(length(a))
      sliderInput("Intervalo","Bloco de texto selecionado",min=0,max=as.integer(length(a)/input$Bloco),value=1,step=1)
    }
    
  })
  
  output$Intervalo2<-renderUI({
    if(input$Referenciador=="WordAnalysis2"){
      a=ProcArquivo2()
      sliderInput("Intervalo2","Bloco de texto selecionado",min=0,max=as.integer(length(a)/input$Bloco),step=1)
    }
    
  })
  
  
  
  output$Saida<-renderPlotly({
    if(!is.null(input$file1)){
      # caminho=input$file1$datapath
      # reject=input$Palavras
      # a=leitura(caminho)
      # a=Palavras(a)
      # b=ConversorMatriz(unlist(a),reject)
      # b=MatrizDistanciasPalavras(b,method)
      a=ProcArquivo()
      texterCum(a,input$Bloco,input$linguagens)  
      #if(input$escolhas=="Media")
      #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
      # else if(input$escolhas=="Desvio Padrao")
      #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
      # else if(input$escolhas=="Ambos")
      #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
      # ggplotly(grafico)
    }
  })
  
  output$Saida2<-renderPlotly({
    if(!is.null(input$file2)){
      # caminho=input$file1$datapath
      # reject=input$Palavras
      # a=leitura(caminho)
      # a=Palavras(a)
      # b=ConversorMatriz(unlist(a),reject)
      # b=MatrizDistanciasPalavras(b,method)
      a=ProcArquivo2()
      texterCum(a,input$Bloco,input$linguagens)  
      #if(input$escolhas=="Media")
      #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
      # else if(input$escolhas=="Desvio Padrao")
      #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
      # else if(input$escolhas=="Ambos")
      #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
      # ggplotly(grafico)
    }
  })
  

  
   output$distPlot <- renderPlotly({
     if(!is.null(input$file1)){
      # caminho=input$file1$datapath
      # reject=input$Palavras
      # a=leitura(caminho)
      # a=Palavras(a)
      # b=ConversorMatriz(unlist(a),reject)
      # b=MatrizDistanciasPalavras(b,method)
       a=ProcArquivo()
       texterLocal(a,input$Bloco,input$linguagens)  
     #if(input$escolhas=="Media")
    #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
    # else if(input$escolhas=="Desvio Padrao")
    #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
    # else if(input$escolhas=="Ambos")
    #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
    # ggplotly(grafico)
     }
   })
   
   output$distPlot2 <- renderPlotly({
     if(!is.null(input$file2)){
       # caminho=input$file1$datapath
       # reject=input$Palavras
       # a=leitura(caminho)
       # a=Palavras(a)
       # b=ConversorMatriz(unlist(a),reject)
       # b=MatrizDistanciasPalavras(b,method)
       a=ProcArquivo2()
       texterLocal(a,input$Bloco,input$linguagens)  
       #if(input$escolhas=="Media")
       #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
       # else if(input$escolhas=="Desvio Padrao")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
       # else if(input$escolhas=="Ambos")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
       # ggplotly(grafico)
     }
   })
   
   output$distPlotSenti <- renderPlotly({
     if(!is.null(input$file1)){
       # caminho=input$file1$datapath
       # reject=input$Palavras
       # a=leitura(caminho)
       # a=Palavras(a)
       # b=ConversorMatriz(unlist(a),reject)
       # b=MatrizDistanciasPalavras(b,method)
       a=ProcArquivo()
       texterLocalEmotions(a,input$Bloco,input$linguagens)  
       #if(input$escolhas=="Media")
       #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
       # else if(input$escolhas=="Desvio Padrao")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
       # else if(input$escolhas=="Ambos")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
       # ggplotly(grafico)
     }
   })
   
   output$distPlotSentiCum <- renderPlotly({
     if(!is.null(input$file1)){
       # caminho=input$file1$datapath
       # reject=input$Palavras
       # a=leitura(caminho)
       # a=Palavras(a)
       # b=ConversorMatriz(unlist(a),reject)
       # b=MatrizDistanciasPalavras(b,method)
       a=ProcArquivo()
       texterCumulativeEmotions(a,input$Bloco,input$linguagens)  
       #if(input$escolhas=="Media")
       #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
       # else if(input$escolhas=="Desvio Padrao")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
       # else if(input$escolhas=="Ambos")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
       # ggplotly(grafico)
     }
   })
   
   output$distPlotSentiCum2 <- renderPlotly({
     if(!is.null(input$file2)){
       # caminho=input$file1$datapath
       # reject=input$Palavras
       # a=leitura(caminho)
       # a=Palavras(a)
       # b=ConversorMatriz(unlist(a),reject)
       # b=MatrizDistanciasPalavras(b,method)
       a=ProcArquivo2()
       texterCumulativeEmotions(a,input$Bloco,input$linguagens)  
       #if(input$escolhas=="Media")
       #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
       # else if(input$escolhas=="Desvio Padrao")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
       # else if(input$escolhas=="Ambos")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
       # ggplotly(grafico)
     }
   })
   
   output$distPlot2Senti <- renderPlotly({
     if(!is.null(input$file2)){
       # caminho=input$file1$datapath
       # reject=input$Palavras
       # a=leitura(caminho)
       # a=Palavras(a)
       # b=ConversorMatriz(unlist(a),reject)
       # b=MatrizDistanciasPalavras(b,method)
       a=ProcArquivo2()
       texterLocalEmotions(a,input$Bloco,input$linguagens)  
       #if(input$escolhas=="Media")
       #   grafico= ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="media",linguagem=input$linguagens)
       # else if(input$escolhas=="Desvio Padrao")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="desvio",input$linguagens)
       # else if(input$escolhas=="Ambos")
       #   grafico=ProcessoShiny(caminho=input$file1$datapath,reject=input$Palavras,graph="gg",method="ambos",input$linguagens)
       # ggplotly(grafico)
     }
   })
   
   output$Informacoes<-renderPrint({
     if(!is.null(input$file1)){
     #  if(input$escolhas=="Media")
    #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
    #   else if(input$escolhas=="Desvio Padrao")
    #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       #a=leitura(input$file1$datapath)
       #a=Palavras(a,input$linguagens)
       a=ProcArquivo()
       ini=input$Bloco*input$Intervalo
       fim=(input$Bloco*(input$Intervalo+1) -1)
      # print(ini)
      # print(fim)
       b=a[ini:fim]
       print(b)
       #z=Clusterizacao()
       #vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       #vec=names(vec)
       
       #print(vec)
     
   }})
   
   output$TabelaSentimentos<-renderTable({
     if(!is.null(input$file1)){
       #  if(input$escolhas=="Media")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       #   else if(input$escolhas=="Desvio Padrao")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       #a=leitura(input$file1$datapath)
       #a=Palavras(a,input$linguagens)
       a=ProcArquivo()
       ini=input$Bloco*input$Intervalo
       fim=(input$Bloco*(input$Intervalo+1) -1)
       # print(ini)
       # print(fim)
       b=a[ini:fim]
       #print(b)
       library(dplyr)
       library(stringr)
       library(tidytext)
       
       tidy_books <-data.frame(b,b,1:length(b))
       names(tidy_books)=c("word","book","linenumber")
       linguagem=input$linguagens
       block=input$Bloco
       library(tidyr)
       if(linguagem=="en")
         Lexicon=get_sentiments("nrc")
       else if(linguagem=="pt")
         Lexicon=read.csv("~/SentimentAnalysis/LexiconPortugues.csv",header=TRUE)
       jane_austen_sentiment <- tidy_books %>%
         inner_join(Lexicon) %>%
         count(word, index = linenumber %/% block, sentiment) %>%
         spread(sentiment, n, fill = 0)
       return(jane_austen_sentiment)
       #z=Clusterizacao()
       #vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       #vec=names(vec)
       
       #print(vec)
       
     }})
   
   output$TabelaSentimentos2<-renderTable({
     if(!is.null(input$file2)){
       #  if(input$escolhas=="Media")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       #   else if(input$escolhas=="Desvio Padrao")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       #a=leitura(input$file1$datapath)
       #a=Palavras(a,input$linguagens)
       a=ProcArquivo2()
       ini=input$Bloco*input$Intervalo
       fim=(input$Bloco*(input$Intervalo+1) -1)
       # print(ini)
       # print(fim)
       b=a[ini:fim]
       #print(b)
       library(dplyr)
       library(stringr)
       library(tidytext)
       
       tidy_books <-data.frame(b,b,1:length(b))
       names(tidy_books)=c("word","book","linenumber")
       linguagem=input$linguagens
       block=input$Bloco
       library(tidyr)
       if(linguagem=="en")
         Lexicon=get_sentiments("nrc")
       else if(linguagem=="pt")
         Lexicon=read.csv("~/SentimentAnalysis/LexiconPortugues.csv",header=TRUE)
       jane_austen_sentiment <- tidy_books %>%
         inner_join(Lexicon) %>%
         count(word, index = linenumber %/% block, sentiment) %>%
         spread(sentiment, n, fill = 0)
       return(jane_austen_sentiment)
       #z=Clusterizacao()
       #vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       #vec=names(vec)
       
       #print(vec)
       
     }})
   output$Informacoes2<-renderPrint({
     if(!is.null(input$file2)){
       #  if(input$escolhas=="Media")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="media",linguagem=input$linguagens)
       #   else if(input$escolhas=="Desvio Padrao")
       #     d=DistanceMatrix(caminho=input$file1$datapath,reject=input$Palavras,method="desvio",linguagem=input$linguagens)
       #z=BestCluster(Calculo(),20)
       #a=leitura(input$file1$datapath)
       #a=Palavras(a,input$linguagens)
       a=ProcArquivo2()
       ini=input$Bloco*input$Intervalo
       fim=(input$Bloco*(input$Intervalo+1) -1)
       # print(ini)
       # print(fim)
       b=a[ini:fim]
       print(b)
       #z=Clusterizacao()
       #vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       #vec=names(vec)
       
       #print(vec)
       
     }})
   
   
   ### copy for second file
   
   
   
   #Matriz=Calculo()
   output$LocalComparative<-renderPlotly({
     if(!is.null(input$file2) & !is.null(input$file1) ){
          print("passei aqui")
          a=ProcArquivo()
          b=ProcArquivo2()
          textLocalComp(a,b,input$Blocos,input$linguagens)
       }
      
     
   })
   
   
   output$CumulativeComparative<-renderPlotly({
     if(!is.null(input$file2) & !is.null(input$file1) ){
       
         print("Passei aqui")
         a=ProcArquivo()
         b=ProcArquivo2()
         textCumComp(a,b,input$Blocos,input$linguagens)
       }
       
     
   })
   
   
   

   
   
   
  
   
  
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

