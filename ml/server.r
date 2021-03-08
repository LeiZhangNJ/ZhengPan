library('shiny')
library('shinythemes')
library('tidyverse')
library('mlr3')
library('mlr3verse')
library("mlr3viz")
library('reshape2')
library('DT')
library('ggplot2')

# rm(list = ls())
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
fea_slc_bycor <- function(df, corr = 0.8){
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
cl <- mlr_learners$keys()[1:15]




shinyServer(
  function(input, output){
    output$RawData <- DT::renderDataTable({
      file <- input$file1
      st <- tryCatch(expr = {read.csv(file$datapath, header = T)},error = function(e){st})
      # st <- read.csv(file$datapath, header = T)
      if(!is.numeric(st[,1])){
        st <- st[,-1]
      }
      DT::datatable({
        st
      },
      options = list(lengthMenu = list(c(5,15,20),c('5','15','20')),pageLength=10,
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     columnDefs=list(list(className='dt-center',targets="_all"))
      ),
      filter = "top",
      selection = 'multiple',
      class = 'cell-border stripe',
      rownames = FALSE
      )
    })
    output$fea_new <- DT::renderDataTable({
      file <- input$file1
      st <- tryCatch(expr = {read.csv(file$datapath, header = T)},error = function(e){st})
      # st <- read.csv(file$datapath, header = T)
      if(!is.numeric(st[,1])){
        st <- st[,-1]
      }
      features <- st[, -ncol(st)]
      DT::datatable({
          fea_slc_bycor(features, corr = 0.8)
        },
        options = list(lengthMenu = list(c(5,15,20),c('5','15','20')),pageLength=10,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                         "}"),
                       columnDefs=list(list(className='dt-center',targets="_all"))
        ),
        filter = "top",
        selection = 'multiple',
        class = 'cell-border stripe',
        rownames = FALSE
        )
      }
    )
    output$originfeas_p <- renderPlot(width = 800,height =  800,
      expr = {
        file <- input$file1
        st <- tryCatch(expr = {read.csv(file$datapath, header = T)},error = function(e){st})
        # st <- read.csv(file$datapath, header = T)
        if(!is.numeric(st[,1])){
          st <- st[,-1]
        }
        features <- st[, -ncol(st)]
        pearson_heat(features, corm = cor(features))
      }
    )
    output$feas_after <- renderPrint(
      expr = {
        file <- input$file1
        st <- tryCatch(expr = {read.csv(file$datapath, header = T)},error = function(e){st})
        # st <- read.csv(file$datapath, header = T)
        if(!is.numeric(st[,1])){
          st <- st[,-1]
        }
        features <- st[, -ncol(st)]
        fea_new <- fea_slc_bycor(features, corr = 0.8)
        feas_name <- colnames(fea_new)
        feas_name
      }
    )
    output$feas_after_p <- renderPlot(
      width = 600,height = 600,
      expr = {
        file <- input$file1
        st <- tryCatch(expr = {read.csv(file$datapath, header = T)},error = function(e){st})
        # st <- read.csv(file$datapath, header = T)
        if(!is.numeric(st[,1])){
          st <- st[,-1]
        }
        features <- st[, -ncol(st)]
        fea_new <- fea_slc_bycor(features, corr = 0.8)
        pearson_heat(fea_new)
      }
    )
    output$models_p <- renderPlot(
      width = 800,height = 600,
      expr = {
        file <- input$file1
        st <- tryCatch(expr = {read.csv(file$datapath, header = T)},error = function(e){st})
        # st <- read.csv(file$datapath, header = T)
        if(!is.numeric(st[,1])){
          st <- st[,-1]
        }
        features <- st[, -ncol(st)]
        fea_new <- fea_slc_bycor(features, corr = 0.8)
        tsk_df <- cbind(fea_new, st = st[, ncol(st)])
        tsk_df <- as.data.frame(tsk_df)
        tsk_df$st <- as.factor(tsk_df$st)
        task <- TaskClassif$new(id = "task", backend = tsk_df, target = "st")
        set.seed(12345)
        tt1 <- lapply(1:200, function(s){
          temp <- lapply(1:15, function(q){
            # print(q)
            # q = 2 
            learner <- mlr_learners$get(cl[q])
            train_set <- sample(task$nrow, 0.7 * task$nrow)
            test_set <- setdiff(seq_len(task$nrow), train_set)
            
            er <- tryCatch(learner$train(task, row_ids = train_set), error = function(e){return(1)})
            if(is.numeric(er)){
              return(NULL)
            }
            prediction <- learner$predict(task, row_ids = test_set)
            
            prediction$confusion
            
            measure = msr("classif.acc")
            res <- prediction$score(measure)
            names(res) <- cl[q]
            return(res)
          })
          unlist(temp)
        })
        tt2 <- do.call(rbind,tt1)
        tt2_melt <- melt(tt2)
        ggplot(tt2_melt, aes(x = value, color = Var2, fill = Var2)) +
          geom_point(stat = 'count') +
          geom_line(stat = 'count') +
          geom_density(stat = 'count', alpha = 0.1) +
          scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1)) +
          theme_bw()
      }
    )
    output$models_prob <- DT::renderDataTable({
      file <- input$file1
      st <- tryCatch(expr = {read.csv(file$datapath, header = T)},error = function(e){st})
      # st <- read.csv(file$datapath, header = T)
      if(!is.numeric(st[,1])){
        st <- st[,-1]
      }
      features <- st[, -ncol(st)]
      features <- st[, -ncol(st)]
      fea_new <- fea_slc_bycor(features, corr = 0.8)
      tsk_df <- cbind(fea_new, st = st[, ncol(st)])
      tsk_df <- as.data.frame(tsk_df)
      tsk_df$st <- as.factor(tsk_df$st)
      task <- TaskClassif$new(id = "task", backend = tsk_df, target = "st")
      set.seed(12345)
      tt1 <- lapply(1:200, function(s){
        temp <- lapply(1:15, function(q){
          # print(q)
          # q = 2 
          learner <- mlr_learners$get(cl[q])
          train_set <- sample(task$nrow, 0.7 * task$nrow)
          test_set <- setdiff(seq_len(task$nrow), train_set)
          
          er <- tryCatch(learner$train(task, row_ids = train_set), error = function(e){return(1)})
          if(is.numeric(er)){
            return(NULL)
          }
          prediction <- learner$predict(task, row_ids = test_set)
          
          prediction$confusion
          
          measure = msr("classif.acc")
          res <- prediction$score(measure)
          names(res) <- cl[q]
          return(res)
        })
        unlist(temp)
      })
      tt2 <- do.call(rbind,tt1)
      tt2_melt <- melt(tt2)
      p <- ggplot(tt2_melt, aes(x = value, color = Var2, fill = Var2)) +
        geom_point(stat = 'count') +
        geom_line(stat = 'count') +
        geom_density(stat = 'count', alpha = 0.1) +
        scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1)) +
        theme_bw()
      pb <- ggplot_build(p)$plot$data
      tt3 <- pb %>%
        group_by(Var2,value) %>%
        count()
      tt3 <- as.data.frame(tt3)
      tt4 <- cbind(as.character(unique(tt3$Var2)), sapply(unique(tt3$Var2), function(q){
        df <- tt3[tt3[,1] == q,]
        df[which.max(df$n), 2]
      }))
      tt4 <- as.data.frame(tt4)
      tt4[,2] <- round(as.numeric(tt4[,2]), 3)
      colnames(tt4) <- c('model', 'prob')
      DT::datatable({
        tt4
      },
      options = list(pageLength=10,
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                       "}"),
                     columnDefs=list(list(className='dt-center',targets="_all"))
      ),
      filter = "top",
      selection = 'multiple',
      class = 'cell-border stripe',
      rownames = FALSE
      )
    }
    )
  }
)