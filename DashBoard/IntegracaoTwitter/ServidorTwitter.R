require(shiny) 
QueriesExecutadas<-reactiveValues(Queries=c(),Resultados=list())
  
  TokenReativo<-reactive({
    token<-CriarToken()
    return(token)
  })
  
  

  GetTwitter<-eventReactive(input$Botao,{
    token<-TokenReativo()
    #Pergunta<-Query(input$String,input$Linguagem,input$Numero)
    Pergunta<- search_tweets(q = input$String,n = input$Numero,include_rts = FALSE)
    #Pergunta$text
    Pergunta<-Pergunta$text[Pergunta$lang=="en"]
    

    QueriesExecutadas$Queries[length(QueriesExecutadas$Queries)+1]=input$String
    QueriesExecutadas$Resultados[[length(QueriesExecutadas$Resultados)+1]]=Pergunta
    names(QueriesExecutadas$Resultados)[length(QueriesExecutadas$Resultados)]=QueriesExecutadas$Queries[length(QueriesExecutadas$Resultados)]
    print( QueriesExecutadas$Resultados )
    return(Pergunta)
  })

