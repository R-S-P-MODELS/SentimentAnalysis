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
        fileInput("file1", "Insert PDF files here",
                  accept = c(
                    "text/pdf",".pdf"),multiple = TRUE ),
       # fileInput("file2", "Insira o arquivo PDF a ser analisado",
        #          accept = c(
         #           "text/pdf",".pdf") ),
       #  sliderInput("Palavras",
        #             "Palavras de frequencia igual ou menor a serem rejeitadas da analise",
         #            min = 0,
          #           max = 50,
           #          value = 10),
       uiOutput('Pdfs'),
        selectInput(inputId = "linguagens",label = "PDF Language",choices = c("en","pt")),
	      numericInput(inputId="Bloco",label="Number of words to calculate",min=1,max=500,value=100),
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
       #  tabPanel("LocalSentiment2",plotlyOutput("distPlot2")),
         tabPanel("Local Sentiment by sentiment",plotlyOutput("distPlotSenti")),
        # tabPanel("LocalSentiment2 by sentiment",plotlyOutput("distPlot2Senti")),
         tabPanel("CumulativeSentiment",plotlyOutput("Saida")),
         #tabPanel("CumulativeSentiment2",plotlyOutput("Saida2")),
         tabPanel("CumulativeSentimentbySentiment",plotlyOutput("distPlotSentiCum")),
         #tabPanel("CumulativeSentimentbySentiment2",plotlyOutput("distPlotSentiCum2")),
         tabPanel("WordAnalysis",uiOutput('EscolhaFilme'),verbatimTextOutput("Informacoes"),tableOutput('TabelaSentimentos'))
         #tabPanel("WordAnalysis2",verbatimTextOutput("Informacoes2"),tableOutput('TabelaSentimentos2'))
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
