library(shiny)
library(ggplot2)
library(reshape2)
sta <- read.csv(file = "data/稳定性.csv")
ui <- navbarPage("Pearson Correlation heat",
                tabPanel("Example", 
                         h4("输入数据为"),
                         DT::dataTableOutput("sta"),
                         h4("输出图为"),
                         plotOutput("pearson_demo")
                         ),
                tabPanel("Application", 
                         titlePanel("Pearson heat map"),
                         sidebarLayout(
                           sidebarPanel(
                             fileInput("file1", label = h3("File input")),
                             h4("only .csv is accepted"),
                             h4("必须有列名，尽量不要有行名,行名不能有斜杠或反斜杠"),
                             h4("没有数据的单元格不能有东西")
                           ),
                           mainPanel(
                             plotOutput("pearson")
                           )
                         )
                       )
  
)
server <- function(input, output) {
  output$pearson <- renderPlot(width = 800,height =  800, 
  {
    file <- input$file1
    ipd <- read.csv(file$datapath, header = T)
    if(!is.numeric(ipd[,1])){
      ipd <- ipd[,-1]
    }
    pcm <- cor(ipd)
    dadf <- melt(pcm)
    q <- ggplot(dadf,aes(Var1,Var2,fill = value)) +
      geom_tile(color = "black",alpha = 0.8) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 0.5, vjust = 0.5)) +
      scale_fill_gradientn(colors = c("black","blue",
                                      "darkgreen","yellow")) +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor =element_blank(),
            axis.line =
              element_line(colour = "black"),) +
      xlab(NULL) +
      ylab(NULL) +
      theme(text = element_text(family = "STXihei")) +
      guides(fill=guide_legend(title=NULL)) +
      labs(title = "Person heat map")
      # theme(text = element_text( size = 20),legend.position = "top")
    q
  })
  output$sta <- DT::renderDataTable({sta})
  output$pearson_demo <- renderPlot(width = 800,height =  800, 
   {
     ipd <- sta
     if(!is.numeric(ipd[,1])){
       ipd <- ipd[,-1]
     }
     pcm <- cor(ipd)
     dadf <- melt(pcm)
     q <- ggplot(dadf,aes(Var1,Var2,fill = value)) +
       geom_tile(color = "black",alpha = 0.8) +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 90,
                                        hjust = 0.5, vjust = 0.5)) +
       scale_fill_gradientn(colors = c("black","blue",
                                       "darkgreen","yellow")) +
       theme(panel.border = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor =element_blank(),
             axis.line =
               element_line(colour = "black"),) +
       xlab(NULL) +
       ylab(NULL) +
       theme(text = element_text(family = "STXihei")) +
       guides(fill=guide_legend(title=NULL)) +
       labs(title = "Person heat map")
     # theme(text = element_text( size = 20),legend.position = "top")
     q
   })
}
shinyApp(ui = ui, server = server)