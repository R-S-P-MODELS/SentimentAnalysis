library(shiny)
 library(shinydashboard)
library(shinydashboardPlus)

require(plotly)

titulo<-dashboardHeaderPlus(title = "Text Sentiment Analysis",titleWidth = 450,enable_rightsidebar = TRUE, rightSidebarIcon = "gears",disable = TRUE)



menu<- dashboardSidebar(width = 250,
 tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
    selectInput('DataSource','Select your Data Source',choices=c('Local','Twitter')),
    sidebarMenu(id="Referenciador",
      menuItem("Local Sentiment", tabName = "LocalSentiment", icon = icon("thumbs-up")),
      menuItem("Local Sentiment by sentiment", tabName = "LocalSentimentbysentiment", icon = icon("heart")),
      menuItem("Cumulative Sentiment",tabName="CumulativeSentiment",icon=icon("thumbs-down")),
menuItem("Cumulative Sentiment by Sentiment", tabName = "CumulativeSentimentbySentiment", icon = icon("heart")),
menuItem("WordAnalysis", tabName = "WordAnalysis", icon = icon("comment")),
menuItem("Word Cloud",tabName="WordCloud",icon = icon("thumbs-up"))
    ),

conditionalPanel("input.DataSource == 'Local'",
        selectInput(inputId = "linguagens",label = "PDF Language",choices = c("en","pt"))
),
	      numericInput(inputId="Bloco",label="Number of words to calculate",min=1,max=500,value=100),
       uiOutput("Intervalos"),
conditionalPanel(
  condition = "input.DataSource == 'Twitter'",
# uiOutput('Historico'),
textInput(inputId = "String",label = "Write your twitter query here",placeholder = "Esperando Texto"),
         
         numericInput(inputId = "Numero",label = "Number of tweets",value=100),
         #selectInput("Escolha",label = "Graph Type",choices = c('Total','Por Sentimento')),
         selectInput("Linguagem",label = "Query Language",choices = c('en','pt')),
         actionButton(inputId = "Botao",label = "Start Query")
),
       uiOutput("FiltrarSentimento"),
	uiOutput('Pdfs')
       
)


corpo<- dashboardBody(
   
     
   tags$head(tags$script("$(function(){
                          $('.sidebar-toggle').on('click',function(){Shiny.onInputChange('sidebarStatus',$('aside.main-sidebar.shiny-bound-input').attr('data-collapsed'))});
                          });
                          ")),
conditionalPanel("input.DataSource == 'Local'",
 fileInput("file1", "Insert PDF files here",
                  accept = c(
                    "text/pdf",".pdf"),multiple = TRUE )
),
uiOutput('Historico'),
  #textOutput("res"),

    tabItems(
      # First tab content
      tabItem(tabName = "LocalSentiment",
        fluidRow(
          plotlyOutput("distPlot")

          
        )
      ),

tabItem(tabName = "LocalSentimentbysentiment",
        fluidRow(
          plotlyOutput("distPlotSenti")

          
        )
      ),

tabItem(tabName = "CumulativeSentiment",
        fluidRow(
	plotlyOutput("Saida")

          
        )
      ),



tabItem(tabName = "CumulativeSentimentbySentiment",
        fluidRow(
          plotlyOutput("distPlotSentiCum")
	  #plotOutput('GrafoCorrelacaov1')

          
        )
      ),

tabItem(tabName = "WordAnalysis",
        fluidRow(
		uiOutput('EscolhaFilme'),
	  	verbatimTextOutput("Informacoes"),
		tableOutput('TabelaSentimentos')

          
        )
      ),
tabItem(tabName = "WordCloud",
        fluidRow(
          uiOutput('EscolhaFilme1'),
          plotOutput("Palavras")
          
          
          
        )
)



      
    ),


         h3("Application developed by Rafael Silva Pereira!\n\n"),
         h4("If you have any questions or problems, please contact us.\n\n"),
	 h4("Insert PDF files to begin analysis\n\n"),
	 h4("Contact: r.s.p.models@gmail.com")


  )


ui <- dashboardPagePlus(titulo,menu,corpo,skin="blue",title = "Text Sentiment Analysis" )


