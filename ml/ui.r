library('shiny')
library('shinythemes')
library('tidyverse')
library('mlr3')
library('mlr3verse')
library("mlr3viz")
library('reshape2')
library('DT')
library('ggplot2')

# pearson heat map
pearson_heat <- function(df, corm = -1){
  # df = features
  defaultW <- getOption("warn") 
  options(warn = -1) 
  if(corm == -1){
    corm = cor(df)
  }
  options(warn = defaultW)
  res <- melt(corm) %>%
    ggplot(aes(Var1,Var2,fill = value)) + 
    geom_tile(color = "black",alpha = 0.8) + 
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 0.5, vjust = 0.5)) + 
    scale_fill_gradient2() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),) + 
    xlab(NULL) + 
    ylab(NULL)
  return(res)
}
# feature selecting by pearson correlation coefficient
fea_slc_bycor <- function(df, corr = 0.7){
  # df = features
  # corr = 0.7
  corm <- cor(df)
  name_of_features <- colnames(df)
  name_of_features_d <- name_of_features
  origin_fea_length <- length(name_of_features)
  for(q in 1:(origin_fea_length - 1)){
    # q = 1
    fea_t <- name_of_features_d[q]
    other_fea_t <- name_of_features_d[(q+1):length(name_of_features_d)]
    de_fea <- names(corm[fea_t, other_fea_t][abs(corm[fea_t, other_fea_t]) >= corr])
    name_of_features <- name_of_features[!(name_of_features %in% de_fea)]
  }
  res <- df[, colnames(df) %in% name_of_features]
  return(res)
}
st <- read.csv(file = 'www/稳定性.csv')

shinyUI(fluidPage(
    theme = shinytheme('sandstone'),
    titlePanel('Machine Learning V0.0'),
    navbarPage(
      "Let's get started",
      tabPanel(
        icon("home"),
        fluidRow(
          h3('Please input the file'),
          hr(),
          sidebarLayout(
            sidebarPanel(
              fileInput("file1", label = h3("File input"))
            ),
            mainPanel(
              h4("only .csv is accepted"),
              h4("必须有列名，不需要行名，列名不能有斜杠或反斜杠"),
              h4("没有数据的单元格不能有东西")
            )
          ),
          hr(),
          column(DT::dataTableOutput("RawData"),width = 12),
          hr(),
          column(DT::dataTableOutput("fea_new"),width = 12),
          hr(),
          p(em("Developed by"),br("Zheng Pan"),style="text-align:center; font-family: times")
        )           
      ),
      tabPanel(
        'feature engineering',
        h3('Pearson heat map for origin data'),
        plotOutput("originfeas_p",width = 800,height =  800),
        hr(),
        h3('Pearson heat map for data after selection'),
        plotOutput("feas_after_p",width = 600,height = 600),
        hr(),
        h3('Features selected: '),
        verbatimTextOutput(outputId = 'feas_after')
        ),
      tabPanel(
        'Machine Learning models',
        hr(),
        plotOutput("models_p",width = 800,height = 600),
        hr(),
        column(DT::dataTableOutput("models_prob"),width = 12)
        )
      # tabPanel(title = a(href="http://www.antioquiadatos.gov.co/index.php/anuario-estadistico-de-antioquia-2016","Here",target="_blank"))
    )
  )
)