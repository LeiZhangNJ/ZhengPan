# football demo
library("shiny")
library("tidyverse")
library(GGally)
library(ggplot2)
library(ranger)
# library("ggraph")
# library("tidygraph")
# library("ggsoccer")
library("mlr3")
library("mlr3verse")
# library("mlr3viz")
library(e1071)
library("readr")
# library("dplyr")
# library("igraph")
# library("ggraph")
library("reshape2")
# library("scales")
library("DT")
matches <- read.csv(file = "data/matches.csv")
passingevents <- read.csv(file = "data/passingevents.csv")
fullevents <- read.csv(file = "data/fullevents.csv")
#### number of division: show_number() ####
show_number <- function(df =  fullevents){ # number of division 
  temp <- unique(df$OriginPlayerID)[sapply(1:length(strsplit(unique(df$OriginPlayerID), split = '_')), function(q){
    strsplit(unique(df$OriginPlayerID), split = '_')[[q]][1]
  }) == "Huskies"]
  res <- t(sapply(c('G', 'D', 'M', 'F'), function(p){
    sum(factor(sapply(1:length(temp), function(q){
      strsplit(strsplit(temp, split = '_')[[q]][2], split = '')[[1]][1]
    })) == p)  
  }))
  res <- as.data.frame(res)
  return(res)
}
#### team formation: get_team_formation() ####
get_team_formation <- function(match = 1){
  df <- fullevents[fullevents$MatchID == match & fullevents$TeamID == "Huskies", ]
  index_of_sub <- which(df$EventType == "Substitution")
  total_formation <- show_number(df)
  switch <- df[index_of_sub,]
  osp <- strsplit(switch$OriginPlayerID, split = '_')
  dsp <- strsplit(switch$DestinationPlayerID, split = '_')
  orole <- sapply(1:length(osp), function(q){
    res <- strsplit(osp[[q]][2], split = '')[[1]][1]
    return(res)
  })
  drole <- sapply(1:length(dsp), function(q){
    res <- strsplit(dsp[[q]][2], split = '')[[1]][1]
    return(res)
  })
  origin_formation <- total_formation
  switch1 <- switch[orole == drole, ]
  switch2 <- switch[orole != drole, ]
  origin_formation$D <- origin_formation$D - sum(drole == "D" )
  origin_formation$M <- origin_formation$M - sum(drole == "M" )
  origin_formation$`F` <- origin_formation$`F` - sum(drole == "F")
  if(sum(orole != drole) > 0){
    temp <- t(sapply(1:sum(orole != drole), function(q){
      switch_formation <- origin_formation
      df1 <- switch2[1:q, ]
      osp1 <- strsplit(df1$OriginPlayerID, split = '_')
      dsp1 <- strsplit(df1$DestinationPlayerID, split = '_')
      orole1 <- orole[orole != drole][1:q]
      drole1 <- drole[orole != drole][1:q]
      switch_formation$D <- switch_formation$D - sum(orole1 == "D")
      switch_formation$M <- switch_formation$M - sum(orole1 == "M")
      switch_formation$`F` <- switch_formation$`F` - sum(orole1 == "F")
      switch_formation$D <- switch_formation$D + sum(drole1 == "D")
      switch_formation$M <- switch_formation$M + sum(drole1 == "M")
      switch_formation$`F` <- switch_formation$`F` + sum(drole1 == "F")
      # if(q == nrow(df) ){
      #   if(df$MatchPeriod == '1H'){
      #     duration <- df$EventTime
      #   }else{
      #     duration <- df$EventTime + fullevents[fullevents$MatchID == 1 & fullevents$MatchPeriod == '1H', ][nrow(fullevents[fullevents$MatchID == 1 & fullevents$MatchPeriod == '1H', ]), ]$EventTime
      #   }
      # }
      switch_formation$time <- df1[q,]$EventTime
      switch_formation$period <- df1[q,]$MatchPeriod
      res <- switch_formation
      return(res)
    }))
    temp <- rbind(cbind(origin_formation, time = 0, period = "1H"), temp, cbind(as.data.frame(temp[nrow(temp),1:4]), time = fullevents[fullevents$MatchID == match, ]$EventTime[length(fullevents[fullevents$MatchID == match, ]$EventTime)], period = '2H'))
    nas <- colnames(temp)
    temp <- sapply(1:ncol(temp), function(q){
      temp[,q] <- unlist(temp[,q])
    })
    colnames(temp) <- nas
    temp <- as.data.frame(temp)
    temp$time <- as.numeric(temp$time)
    temp$time[temp$period == '2H'] <- temp$time[temp$period == '2H'] + fullevents[fullevents$MatchID == match & fullevents$MatchPeriod == '1H', ][nrow(fullevents[fullevents$MatchID == match & fullevents$MatchPeriod == '1H', ]), ]$EventTime
    all_formation_types <- cbind(temp[-nrow(temp),1:4], duration = temp$time[2:length(temp$time)] - temp$time[1:(length(temp$time) - 1)])
    res <- all_formation_types[which.max(all_formation_types$duration), 1:4]
  }else{
    res <- origin_formation
  }
  return(res)
}
#### passing paths: show_passing_events(rcd = passingevents, save = F, match, sub = '') #### 
show_passing_events <- function(rcd = passingevents, match, sub = ''){
  # rcd = passingevents[passingevents$MatchPeriod == "2H" & passingevents$MatchID ==1 , ]
  # rcd = data.frame(passingevents[passingevents$MatchPeriod == "2H" & passingevents$MatchID == 1 & passingevents$Distance > 120, ])
  if(nrow(rcd) == 0){
    return(paste("no such event you are looking for"))
  }
  rcd <- as.data.frame(rcd)
  attach(rcd)
  pass_path <- do.call(rbind, lapply(1:nrow(rcd), function(q){
    res <- as.data.frame(rbind(c(EventOrigin_x[q], EventOrigin_y[q],q,MatchID[q]),  c(EventDestination_x[q], EventDestination_y[q],q,MatchID[q])))
    # res <- cbind(res, EventTime[q], MatchPeriod[q])
    names(res) <- c("xs", "ys","id", "matchid")
    return(res)
  }))
  detach(rcd)
  pl <- pass_path %>%
    ggplot(aes(x  = xs, y = ys)) +
    annotate_pitch(colour = "white",
                   fill   = "springgreen4",
                   limits = FALSE) +
    # theme_pitch() +
    geom_path(aes(group = id), arrow = grid::arrow(type = 'closed', ends = 'last', length = unit(0.1, "inches"))) +
    # geom_line(aes(group = id)) +  
    geom_point(fill = "yellow",
               shape = 21,
               size = 1) +
    labs(title = paste("passing events of match",match, sep = " "),
         subtitle = sub) +
    # theme_void() + 
    theme(panel.background = element_rect(fill = "springgreen4"))
  pl
}
#### getting team member id: get_member_id() ####
get_member_id <- function(){
  tn <- show_number()
  member_id <- unlist(sapply(1:ncol(tn), function(q){
    paste('Huskies_', colnames(tn)[q], 1:tn[,q], sep = '')
  }))
  return(member_id)
}
#### get_name_matrix()####
get_name_matrix <- function(){
  a <- matrix("a", nc = 30, nr = 30)
  b <- get_member_id()
  for (i in 1:30) {
    for (j in 1:30) {
      a[i,j] <- paste(b[i], b[j], sep = " vs ")
    }
  }
  return(a)
}
#### calculate distance: calculate_distance(df = passingevents) ####
calculate_distance <- function(df = passingevents){
  sqrt((df$EventOrigin_x - df$EventDestination_x)^2 + (df$EventOrigin_y - df$EventDestination_y)^2)
}
#### distance categorization: categorize_distance(df = passingevents) ####
categorize_distance <- function(df = passingevents){
  df$CDistance <- rep(0, length(df$Distance))
  df$CDistance[df$Distance <= 15] <- 0.1
  df$CDistance[df$Distance <= 25 & df$Distance > 15] <- 0.25
  df$CDistance[df$Distance <= 60 & df$Distance > 25] <- 0.35
  df$CDistance[df$Distance > 60] <- 0.3
  return(df$CDistance)
}
#### Adjacency Matrix directed: Adjacency_matrix_directed(df = passingevents) ####
Adjacency_matrix_directed <- function(df = passingevents){
  hpe <- df[df$TeamID == "Huskies", ] # Huskies passing events
  member_id <- get_member_id()
  ad <- matrix(0, nr = 30, nc = 30)
  for(i in 1:nrow(ad)){
    for(j in 1:ncol(ad)){
      if(sum(hpe$OriginPlayerID == member_id[i] & hpe$DestinationPlayerID == member_id[j]) != 0){
        selected_row <- hpe[hpe$OriginPlayerID == member_id[i] & hpe$DestinationPlayerID == member_id[j], ]
        ad[i,j] <- sum(selected_row$CDistance)
      }
    }
  }
  ad <- as.data.frame(ad)
  dimnames(ad) <- list(member_id, member_id)
  return(ad)
}
#### coordination: coordination(matrix) ####
coordination <- function(matrix){
  res <- matrix(0,nc = ncol(matrix), nr = nrow(matrix))
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if(i == j){
        res[i,j] <- 0
      }else{
        res[i,j] <- sum(matrix[i,j],matrix[j,i])
      }
    }
  }
  dimnames(res) <- dimnames(matrix)
  return(res)
}
#### team contribution: get_team_contribution()####
get_team_contribution <- function(match = 1){
  df <- fullevents[fullevents$MatchID == match, ]
  df_H <- df[df$TeamID == 'Huskies', ]
  a <- matches$OwnScore[match]/sum(df_H$EventType == 'Shot')
  b <- sum(df_H$EventType == 'Duel')/(matches$OpponentScore + 1)[match]
  res <- a*b
  return(res)
}
#### dyadic: dyadic_rank(df = passingevents) ####
dyadic_rank <- function(df = passingevents, first = 5, name = 'ALL'){
  namematrix <- get_name_matrix()
  amd <- Adjacency_matrix_directed(df)
  crd <- coordination(amd)
  tcrd <- crd
  tcrd[lower.tri(tcrd)] <- 0
  upp_c <- as.vector(tcrd[upper.tri(tcrd)])
  names(upp_c) <- namematrix[upper.tri(tcrd)]
  upp_c <- sort(upp_c, decreasing = T)
  # plot((upp_c[1:(length(upp_c) - 1)]/upp_c[2:(length(upp_c))])[1:10])
  # plot(upp_c[upp_c != 0])
  # dev.off()
  res <- upp_c
  return(res)
}
# dyadic_rank(passingevents[passingevents$MatchID == 10, ])
#### triadic: triadic_rank(df = passingevents) ####
triadic_rank <- function(df = passingevents, name = 'ALL'){
  # df = passingevents[passingevents$MatchID ==1, ]
  # name_matrix <- get_name_matrix()
  amd <- Adjacency_matrix_directed(df)
  crd <- coordination(amd)
  member_id <- get_member_id()
  res1 <- sapply(1:(nrow(df) - 1), function(q){
    df1 <- df[q:(q+1), ]
    # dyadic_rank_pair(df1)
    uni <- unique(c(df1$OriginPlayerID, df1$DestinationPlayerID))
    if(sum(!(uni %in% member_id)) == 0 & length(uni) == 3){
      res <- sum(crd[uni[1], uni[2]], crd[uni[1], uni[3]], crd[uni[3], uni[2]])
    }else{
      return()
    }
    names(res) <- paste(uni[1], uni[2], uni[3], sep = ', ')
    return(res)
  })
  rk <- sort(unlist(res1), decreasing = TRUE)
  rk <- rk[unique(names(rk))]
  sp <- do.call(rbind,strsplit(names(rk), split = ', '))
  bindname <- sapply(1:length(unique(rk)), function(q){
    res <- paste(sort(sp[q,])[1], sort(sp[q,])[2], sort(sp[q,])[3], sep = ' ')
    return(res)
  })
  ind <- sapply(1:length(unique(bindname)), function(q){
    which(bindname == unique(bindname)[q])[1]
  })
  rk <- rk[ind]
  # plot(rk)
  # dev.off()
  res <- rk
  return(res)
}
# triadic_rank(df = passingevents[passingevents$MatchID == 1, ])
#### possion rate: get_possession_rate() ####
get_possession_rate <- function(match_number = 1){
  df <- fullevents[fullevents$MatchID == match_number, ]
  p_time <- sapply(1:(nrow(df) - 1), function(q){
    df1 <- df[q:(q+1), ]
    if(length(unique(df1$TeamID)) == 1 & df1$TeamID[1] == "Huskies" & df1$EventTime[1] - df1$EventTime[2] < 0){
      res <- df1$EventTime[2] - df1$EventTime[1]
    }else{
      res <- 0
    }
    return(res)
  })
  p_time <- sum(p_time)
  if(df[1,]$TeamID == "Huskies"){
    p_time <- p_time + df$EventTime[1]
  }
  rate <- p_time/df[nrow(df),]$EventTime
  return(rate)
}
#### get_sucess_passing_rate(df = fullevents) ####
get_sucess_passing_rate <- function(df = fullevents){
  df <- df[df$TeamID == 'Huskies', ]
  res <- sum(df$EventType == 'Pass' & df$DestinationPlayerID != '')/sum(df$EventType == 'Pass')
  return(res)
}
#### plot function of q2: plot_col_q2(df, yl) ####
plot_col_q2 <- function(df, yl){
  daf <- data.frame(df = df, match_id = 1:38)
  qq <- ggplot(daf, aes(x = match_id, y = df)) +
    geom_col(fill = "blue", color = 'purple') +
    theme_bw() +
    ylab(yl) +
    xlab('match id')
  return(qq)
}
#### ml demo ####
possession_rate <- round(sapply(1:38, function(q){
  get_possession_rate(q)
}), 3)
## side
side <- matches$Side
## team formation
team_formation <- sapply(1:38, function(q){
  formation <- get_team_formation(match = q)
  if(!is.null(ncol(formation))){
    res <- paste(formation$D, formation$M, formation$`F`, sep = "-")  
  }else{
    res <- formation
  }
  return(res)
})
## pressing
pressing <- round(sapply(1:38, function(q){
  res <- mean(fullevents[fullevents$MatchID == q & fullevents$TeamID == 'Huskies', ]$EventDestination_x, na.rm = TRUE)
  return(res)
}), 3)
## flexibility
flexibility <- round(sapply(1:38, function(q){
  ybar <- mean(fullevents[fullevents$MatchID == q & fullevents$TeamID == 'Huskies', ]$EventDestination_y, na.rm = TRUE)
  res <- mean(abs(fullevents[fullevents$MatchID == q & fullevents$TeamID == 'Huskies', ]$EventDestination_y - ybar), na.rm = TRUE)
  return(res)
}), 3)
## couch
couch <- matches$CoachID
## team contribution
team_contribution <- round(sapply(1:38, function(q){
  res <- get_team_contribution(q)
  return(res)
}), 3)
## match outcome
outcome <- matches$Outcome
## ml
tsk_df <- data.frame(possession_rate, side, team_formation, pressing, flexibility, couch, team_contribution, outcome)
tsk_df$side <- as.factor(tsk_df$side)
tsk_df$team_formation <- as.factor(tsk_df$team_formation)
tsk_df$couch <- as.factor(tsk_df$couch)
tsk_df$outcome <- as.factor(tsk_df$outcome)
tsk_q2 <- TaskClassif$new(id = "Q2", backend = tsk_df, target = "outcome")
tsk_plot_paris <- autoplot(tsk_q2, type = "pairs")
tsk_duo <- autoplot(tsk_q2, type = "duo")

## filter
lrn9 = lrn("classif.ranger", importance = "impurity")
filter9 = flt("importance", learner = lrn9)
filter9$calculate(tsk_q2)
fil <- as.data.frame(as.data.table(filter9))


## learner 9
learner_names <- mlr_learners$keys()[1:14]
learner9 <- lrn(learner_names[9])
test_set9 <- c(12,15,21,22,30,35)
train_set9 <- setdiff(seq_len(tsk_q2$nrow), test_set9)
test_set9_t <- tsk_df[test_set9, ]
train_set9_t <- tsk_df[train_set9, ]
## train
learner9$train(tsk_q2, row_ids = train_set9)
model9 <- learner9$model

## prediction
prediction9 <- learner9$predict(tsk_q2, row_ids = test_set9)
## performance measure
measure = msr("classif.acc")
# prediction9$score(measure)
# autoplot(prediction9)

dadf9 <- melt(prediction9$confusion)
confusion_matrix9 <- ggplot(dadf9,aes(response,truth,fill = value, label = value, fill = value)) + 
  geom_tile(color = "black",alpha = 0.8) + 
  geom_text(size = 3, color = 'black') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 0.5, vjust = 0.5)) + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor =element_blank(),
        axis.line = element_line(colour = "black"))

## learner 11
learner11 <- lrn(learner_names[11])
test_set11 <- c(2,13,14,27,35,38)
train_set11 <- setdiff(seq_len(tsk_q2$nrow), test_set11)
## train
learner11$train(tsk_q2, row_ids = train_set11)

## prediction
prediction11 <- learner11$predict(tsk_q2, row_ids = test_set11)
# prediction11$confusion
# prediction11$score(measure)
# autoplot(prediction11)
dadf11 <- melt(prediction11$confusion)
confusion_matrix11 <- ggplot(dadf11,aes(response,truth,fill = value, label = value, fill = value)) + 
  geom_tile(color = "black",alpha = 0.8) + 
  geom_text(size = 3, color = 'black') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 0.5, vjust = 0.5)) + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor =element_blank(),
        axis.line = element_line(colour = "black"))

model11 <- learner11$model
test_set11_t <- tsk_df[test_set11, ]
train_set11_t <- tsk_df[train_set11, ]

## learner 12
learner12 <- lrn(learner_names[12])
test_set12 <- c(5,6,11,13,26,29)
train_set12 <- setdiff(seq_len(tsk_q2$nrow), test_set12)
## train
learner12$train(tsk_q2, row_ids = train_set12)
## prediction
prediction12 <- learner12$predict(tsk_q2, row_ids = test_set12)
# prediction12$score(measure)
# autoplot(prediction12)

dadf12 <- melt(prediction12$confusion)
confusion_matrix12 <- ggplot(dadf12,aes(response,truth,fill = value, label = value, fill = value)) + 
  geom_tile(color = "black",alpha = 0.8) + 
  geom_text(size = 3, color = 'black') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 0.5, vjust = 0.5)) + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor =element_blank(),
        axis.line = element_line(colour = "black"))

model12 <- learner12$model
test_set12_t <- tsk_df[test_set12, ]
train_set12_t <- tsk_df[train_set12, ]


#### app ####
ui <- fluidPage(
  titlePanel("Demo on ML Based on Study of Football Matches"),
  sidebarLayout(
    sidebarPanel(width = 0
      # fileInput("file1", h3("File input"))
    ),
    mainPanel(
      tabsetPanel(tabPanel("Matches", DT::dataTableOutput("matches")),
                  tabPanel("Passing events", DT::dataTableOutput("passing")),
                  tabPanel("Full events", DT::dataTableOutput("full")),
                  tabPanel("Task table", DT::dataTableOutput("tsk_df")),
                  tabPanel("ML demo", navlistPanel(
                    "Feature engineering",
                    tabPanel("importance of variables", DT::dataTableOutput("fil")),
                    tabPanel("person correlation map", plotOutput("pearson")),
                    "Naive Bayes",
                    tabPanel("Parameters of model", verbatimTextOutput(outputId = 'bp')),
                    tabPanel("training set", DT::dataTableOutput("ta9")),
                    tabPanel("test set", DT::dataTableOutput("te9")),
                    tabPanel("autoplot prediction",plotOutput("p9")),
                    tabPanel("confusion matrix", plotOutput("c9")),
                    "Random Forest",
                    tabPanel("Parameters of model", verbatimTextOutput(outputId = 'bp11')),
                    tabPanel("training set", DT::dataTableOutput("ta11")),
                    tabPanel("test set", DT::dataTableOutput("te11")),
                    tabPanel("autoplot prediction",plotOutput("p11")),
                    tabPanel("confusion matrix", plotOutput("c11")),
                    "RPART",
                    tabPanel("Parameters of model", verbatimTextOutput(outputId = 'bp12')),
                    tabPanel("training set", DT::dataTableOutput("ta12")),
                    tabPanel("test set", DT::dataTableOutput("te12")),
                    tabPanel("autoplot prediction",plotOutput("p12")),
                    tabPanel("confusion matrix", plotOutput("c12"))
          )
        )
      )
    )
  )
)
server <- function(input, output) {
  output$matches <- DT::renderDataTable({
    matches})
  output$passing <- DT::renderDataTable({
    passingevents})
  output$full <- DT::renderDataTable({
    fullevents})
  output$tsk_df <- DT::renderDataTable({
    tsk_df})
  output$fil <- DT::renderDataTable({
    fil})
  output$pearson <- renderPlot(width = 800,height =  800, 
   {
     tsk_df <- data.frame(possession_rate, side, team_formation, pressing, flexibility, couch, team_contribution, outcome)
     us <- unique(tsk_df$side)
     ut <- unique(tsk_df$team_formation)
     uc <- unique(tsk_df$couch)
     uo <- unique(tsk_df$outcome)
     tsk_df$side[tsk_df$side == us[1]] <- 1
     tsk_df$side[tsk_df$side == us[2]] <- 2
     tsk_df$team_formation[tsk_df$team_formation == ut[1]] <- 1
     tsk_df$team_formation[tsk_df$team_formation == ut[2]] <- 2
     tsk_df$team_formation[tsk_df$team_formation == ut[3]] <- 3
     tsk_df$team_formation[tsk_df$team_formation == ut[4]] <- 4
     tsk_df$team_formation[tsk_df$team_formation == ut[5]] <- 5
     tsk_df$team_formation[tsk_df$team_formation == ut[6]] <- 6
     tsk_df$couch[tsk_df$couch == uc[1]] <- 1
     tsk_df$couch[tsk_df$couch == uc[2]] <- 2
     tsk_df$couch[tsk_df$couch == uc[3]] <- 3
     tsk_df$outcome[tsk_df$outcome == uo[1]] <- 1
     tsk_df$outcome[tsk_df$outcome == uo[2]] <- 2
     tsk_df$outcome[tsk_df$outcome == uo[3]] <- 3
     ipd <- tsk_df
     na <- names(ipd)
     ipd <- do.call(cbind, ipd)
     colnames(ipd) <- na
     ipd <- as.data.frame(ipd)
     for (i in 1:ncol(ipd)) {
       ipd[,i] <- as.numeric(ipd[,i])
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
  output$bp <- renderPrint(model9)
  output$ta9 <- DT::renderDataTable({
    test_set9_t})
  output$te9 <- DT::renderDataTable({
    train_set9_t})
  output$p9 <- renderPlot(width = 800,height =  800,autoplot(prediction9))
  output$c9 <- renderPlot(width = 800,height =  800,confusion_matrix9)
  output$bp11 <- renderPrint(model11)
  output$ta11 <- DT::renderDataTable({
    test_set11_t})
  output$te11 <- DT::renderDataTable({
    train_set11_t})
  output$p11 <- renderPlot(width = 800,height =  800,autoplot(prediction11))
  output$c11 <- renderPlot(width = 800,height =  800,confusion_matrix11)
  output$bp12 <- renderPrint(model12)
  output$ta12 <- DT::renderDataTable({
    test_set12_t})
  output$te12 <- DT::renderDataTable({
    train_set12_t})
  output$p12 <- renderPlot(width = 800,height =  800,autoplot(prediction12))
  output$c12 <- renderPlot(width = 800,height =  800,confusion_matrix12)
}
shinyApp(ui = ui ,server = server) 