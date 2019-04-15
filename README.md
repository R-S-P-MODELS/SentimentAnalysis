# SentimentAnalysis
A shinyApp with a sentimentAnalysis tool for PDF files

This is a app that receives a PDF and lets the user do a process of Sentiment Analysis on its content

The user may define how many words are to be used to calculate each point and the following analitical processes are:

Positive vs Negative local : The program will analyze the file and for every  set of n words defined by the user define a score for how positive or negative this section was

Positive vs Negative Cumulative: Draws a continous line graph of the positive vs negative, you can see how your file evolves its content from positive to negative

By sentiment: Instead of classifying by positive vs negative the code uses a codex with several sentiments and classifies each section between then all

Word Analysis: the user may see which words were used to calculate a certain point in these graphs.

to run this app the user may use the following command in your R session:

shiny::runGitHub(repo="SentimentAnalysis",username="R-S-P-MODELS")

If you want to run as a docker container you can clone this repository and use docker build .

The program will be run on port 3838
