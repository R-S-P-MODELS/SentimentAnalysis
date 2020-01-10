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
source("WordCloud.R")
source("Analise_texto.R")
source("sentimentscript.R")
# Define UI for application that draws a histogram
#source("UIOriginal.R") 
source("UiDashBoard.R")
# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=1000*1024^2)
  
  output$Pdfs<-renderUI({
    if(!is.null(input$file1)){
      selectInput('OpcoesPdf','File',choices=input$file1$name,multiple=TRUE)
    }
  })
  output$EscolhaFilme<-renderUI({
    if(!is.null(input$file1)){
      w<-ProcArquivo()
      selectInput("textoescolhido",'Which file do you want to inspect',choices=names(w))
    }
  })
  output$EscolhaFilme1<-renderUI({
    if(!is.null(input$file1)){
      w<-ProcArquivo()
      selectInput("textoescolhido1",'Which file do you want to inspect',choices=names(w))
    }
  })
  
  
  output$FiltrarSentimento<-renderUI({
    if(!is.null(input$file1)){
     if(input$Referenciador %in% c('LocalSentimentbysentiment','CumulativeSentimentbySentiment'))
       selectInput('FiltroSentimento','Which sentiment types do you want to vizualize',choices=c('Anger','Anticipation','Disgust','Fear','Joy','Sadness','Surprise','Trust'),multiple = TRUE,selected = 'Anger' )
    }
  })
  
  ProcArquivo<-reactive({
    if(!is.null(input$file1)){
      index<-c()
      a<-list()
      b<-list()
      for(i in 1:length(input$OpcoesPdf))
      {
        index[i]<-which(input$OpcoesPdf[i]==input$file1$name)
        a[[i]]=leitura(input$file1$datapath[index[i]])
        b[[i]]=Palavras(a[[i]],input$linguagens)
      }
      names(b)<-input$OpcoesPdf
     return(b)
    }
    
  })
  
  
  
  
  #Matriz=Calculo()
  
  output$Intervalos=renderUI({
   if(input$Referenciador=="WordAnalysis"|input$Referenciador=="WordAnalysis2"){
      a=ProcArquivo()
      a<-a[[input$textoescolhido]]
      
            #a=1:100
      print(length(a))
      sliderInput("Intervalo","Text Block Selected",min=0,max=as.integer(length(a)/input$Bloco),value=1,step=1)
    }
    
  })
  
  #output$Intervalo2<-renderUI({
  #  if(input$Referenciador=="WordAnalysis2"){
   #   a=ProcArquivo2()
  #    sliderInput("Intervalo2","Bloco de texto selecionado",min=0,max=as.integer(length(a)/input$Bloco),step=1)
 #   }
    
#  })
  
  
  
  output$Saida<-renderPlotly({
    if(!is.null(input$file1)){
      # caminho=input$file1$datapath
      # reject=input$Palavras
      # a=leitura(caminho)
      # a=Palavras(a)
      # b=ConversorMatriz(unlist(a),reject)
      # b=MatrizDistanciasPalavras(b,method)
      a=ProcArquivo()
      #print(a)
      require(ggplot2)
      require(plotly)
      #p<-ggplot()
      data=list()
      data[[1]]<-CumulativeSentimentPart(a[[1]],input$Bloco,input$linguagens,names(a)[1])
      if(length(a)>1)
        for(i in 2:length(a)){
          #data<-rbind(data,CumulativeSentimentPart(a[[i]],input$Bloco,input$linguagens,paste("Livro",i) ) )
          data[[i]]<-CumulativeSentimentPart(a[[i]],input$Bloco,input$linguagens,names(a)[i]) 
        }
      data<-Reduce(rbind,data)
      p1=ggplot(data, aes(index, sentiment, col = book)) +
         geom_line()
      
      ggplotly(p1)
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
       #print(a)
       require(ggplot2)
       require(plotly)
       #p<-ggplot()
       data=list()
       data[[1]]<-LocalSentimentPart(a[[1]],input$Bloco,input$linguagens,names(a)[1])
       if(length(a)>1)
         for(i in 2:length(a)){
           #data<-rbind(data,CumulativeSentimentPart(a[[i]],input$Bloco,input$linguagens,paste("Livro",i) ) )
           data[[i]]<-LocalSentimentPart(a[[i]],input$Bloco,input$linguagens,names(a)[i]) 
         }
       data<-Reduce(rbind,data)       
       p1=ggplot(data, aes(index, sentiment, fill = book)) +
        geom_col(show.legend = FALSE)
        
       ggplotly(p1)
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
       #print(a)
       require(ggplot2)
       require(plotly)
       #p<-ggplot()
       data=list()
       data[[1]]<-LocalEmotionsPart(a[[1]],input$Bloco,input$linguagens,names(a)[1])
       if(length(a)>1)
         for(i in 2:length(a)){
           #data<-rbind(data,CumulativeSentimentPart(a[[i]],input$Bloco,input$linguagens,paste("Livro",i) ) )
           data[[i]]<-LocalEmotionsPart(a[[i]],input$Bloco,input$linguagens,names(a)[i]) 
         }
       data<-Reduce(rbind,data)  
     #  print(input$FiltroSentimento)
       Aceitaveis<-input$FiltroSentimento
       #for(i in 1:length(Aceitaveis))
        #Aceitaveis<-paste(Aceitaveis,"_",sep="")
        print(length(Aceitaveis))
        Filtrados<-1
        for(i in 1:length(Aceitaveis))
        {
         #print(Aceitaveis[i])
         #print(as.character(data$Name[1:5]))
         filtro<-which(grepl(Aceitaveis[i],as.character(data$Name) ))
         Filtrados<-c(Filtrados,filtro)
         print(length(Filtrados))
       }
       Filtrados<-Filtrados[-1]
       print(length(Filtrados))
       data=data[Filtrados,]
       p1=ggplot(data,aes(x=Index,y=Emotion,fill=Name)) + geom_col(show.legend = FALSE)
       ggplotly(p1)
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
       #print(a)
       require(ggplot2)
       require(plotly)
       #p<-ggplot()
       data=list()
       data[[1]]<-CumulativeEmotionsPart(a[[1]],input$Bloco,input$linguagens,names(a)[1])
         
       if(length(a)>1)
       for(i in 2:length(a)){
           #data<-rbind(data,CumulativeSentimentPart(a[[i]],input$Bloco,input$linguagens,paste("Livro",i) ) )
           data[[i]]<-CumulativeEmotionsPart(a[[i]],input$Bloco,input$linguagens,names(a)[i]) 
          
       }
       data<-Reduce(rbind,data) 
       
       Aceitaveis<-input$FiltroSentimento
       #for(i in 1:length(Aceitaveis))
       #Aceitaveis<-paste(Aceitaveis,"_",sep="")
       #print(length(Aceitaveis))
       Filtrados<-1
       for(i in 1:length(Aceitaveis))
       {
         #print(Aceitaveis[i])
         #print(as.character(data$Name[1:5]))
         filtro<-which(grepl(Aceitaveis[i],as.character(data$Name) ))
         Filtrados<-c(Filtrados,filtro)
         print(length(Filtrados))
       }
       Filtrados<-Filtrados[-1]
      # print(length(Filtrados))
       data=data[Filtrados,]
       
       p1=ggplot(data,aes(x=Index,y=Emotion,col=Name)) + geom_line(show.legend = FALSE)
       ggplotly(p1)
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
       #print(a[[1]])
       #print(names(a))
       #print(input$textoescolhido)
       a<-a[[input$textoescolhido]]
       #print(a)
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
       a<-a[[input$textoescolhido]]
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
       jane_austen_sentiment=jane_austen_sentiment[,-2]
       return(jane_austen_sentiment)
       #z=Clusterizacao()
       #vec=z$cluster[which(as.numeric(z$cluster)==as.numeric(input$selecionador))]
       #vec=names(vec)
       
       #print(vec)
       
     }})
  
   
   output$Palavras<-renderPlot({
     #  Pergunta<-1
     #   Pergunta<-GetTwitter()
     if(!is.null(input$file1) ){
       Pergunta=ProcArquivo()
       Pergunta<-Pergunta[[input$textoescolhido1]]
       
       NuvemSentimentos(Palavras(tolower(Pergunta),input$linguagens),input$linguagens)
       
       
       
     }
   }
   )
   

   
   
   
   
   
   

   
   
   
  
   
  
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

