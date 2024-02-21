#install.packages('ggplot2') 
#install.packages('dplyr') 
#install.packages('readxl')
#install.packages('qdap')
#install.packages('stringr')
#install.packages('writexl')
#install.packages('rebus')
#install.packages('tictoc')
#install.packages('parallel')
#install.packages('wordcloud')
#install.packages('shiny')
#install.packages('shinythemes')
#install.packages('DT')


library('ggplot2') 
library('dplyr') 
library('readxl')
library('qdap')
library('stringr')
library('writexl')
library('rebus')
library('tictoc')
library('parallel')
library('wordcloud')
library('shiny')


alp_rmv <- function(chr_lst) {
  
  ## receives a set of string and removes one-length parts of each string's words
  
  chr_lst_new <- vector('list', length(chr_lst))
  
  for(i in 1:length(chr_lst)){
    list <- unlist(chr_lst[i])
    list <- list[nchar(list)>1]
    chr_lst_new[i] <- list(list)
  }
  
  return(chr_lst_new)
}



chr_stp <- function(chr_lst = chr_lst, stp_chr = stp_chr, core_num=detectCores()) {
  
  ## receives a character set, removes the characters in stp_chr set from each member of the character set, and returns the result
  
  cs_p <- function(i, chr_lst, stp_chr) {
    for(j in 1:length(stp_chr)) {
      chr_lst[i] <- str_remove_all(chr_lst[i], as.character(stp_chr[j]))
    }
    
    return(chr_lst[i])
  }
  
  cl <- makeCluster(core_num)
  clusterEvalQ(cl, library('stringr'))
  
  chr_lst <- parSapply(cl, 1:length(chr_lst), cs_p, chr_lst, stp_chr)
  
  stopCluster(cl)
  
  return(chr_lst)
  
}



integ <- function(chr_lst = chr_lst) {
  
  ## receives a character set, replaces:
  ## "ي" and "ئ" with "ی"
  ## "ك" with "ک" 
  ## "ؤ" with "و"
  ## "¬" with " " 
  ##and returns the integrated character set
  
  int <- vector('character', length(chr_lst))
  
  for(i in 1:length(chr_lst)) {
    str <- str_replace_all(chr_lst[i], "ي", "ی")
    str <- str_replace_all(str, "ئ", "ی")
    str <- str_replace_all(str, "ك", "ک")
    str <- str_replace_all(str, "ؤ", "و")
    str <- str_replace_all(str, "¬", " ")
    int[i] <- str
  }
  
  return(int)
  
}



plrel_to_sng <- function(chr_lst, plrel_exc, core_num=detectCores()) {
  
  ## receives a set of string and removes 'plural and relative' and 'plural' parts of each string's words if they have meaning (regarding words in the set)
  
  pl <- c('ها', 'ان', 'ات')
  plrel <- pl %R% optional('ی') %R% optional('ی') %R% optional('ی') %R% END    
  wrds <- unlist(chr_lst)
  
  chr_lst_new <- vector('list', length(chr_lst))
  
  plrel_f <- function(i, chr_lst, plrel_exc) {
    list <- unlist(chr_lst[i])
    for(j in 1:length(list)) {
      for(k in 1:length(plrel)) {
        if(str_detect(list[j], plrel[k]) == TRUE) {
          if(str_remove(list[j], plrel[k]) %in% wrds) {
            if(!(list[j] %in% plrel_exc)) {
              list[j] <- str_remove(list[j], plrel[k])
            }
          }
          
        }
      }
    }
    chr_lst_new <- list(list)
    
    return(chr_lst_new)
    
  }
  
  cl <- makeCluster(core_num)
  
  clusterEvalQ(cl, library('stringr'))
  
  chr_lst_new <- parSapply(cl, 1:length(chr_lst), plrel_f, chr_lst, plrel_exc)
  
  stopCluster(cl)
  
  return(chr_lst_new)
}



sim_mat <- function(chr_lst=chr_lst, stp_wrd=stp_wrd, stp_chr=stp_chr, plrel_exc=plrel_exc, core_num=detectCores()) {
  
  ## receives a set of lists of words, and returns the similarity score based on the n(int)/n(uni)
  ## -1: comparison of a string with itself
  
  chr_lst <- sm_wrd_lst(chr_lst=chr_lst, stp_wrd=stp_wrd, stp_chr=stp_chr, plrel_exc=plrel_exc, core_num=core_num)
  
  sim_scr <- matrix(0, length(chr_lst), length(chr_lst))
  for(i in 1:length(chr_lst)) {
    sim_scr[i, i] <- -1
  }
  
  for(i in 1:(length(chr_lst)-1)) {
    for(j in (i+1):(length(chr_lst))) {
      sim_scr[i, j] <- (length(intersect(unlist(chr_lst[i]), unlist(chr_lst[j]))))/(length(union(unlist(chr_lst[i]), unlist(chr_lst[j]))))
    }
  }
  
  for(i in 2:length(chr_lst)) {
    for(j in 1:i-1) {
      sim_scr[i, j] <- sim_scr[j, i]
    }
  }
  
  return(sim_scr)
}



sim_scr <- function(sim_mat, min, max=1) {
  
  ## receives the similarity matrix, and returns the indices and the similarity in the desired range
  
  ind <- which((sim_mat>=min & sim_mat<=max), arr.ind=TRUE)
  scr <- vector('numeric', nrow(ind))
  for(i in 1:nrow(ind)) {
    scr[i] <- sim_mat[ind[i,1], ind[i,2]]
  }
  res <- cbind(ind, scr)
  
  return(res[order(-res[, 3]), ])
}



sm_clst <- function(chr_lst=chr_lst, stp_wrd=stp_wrd, stp_chr=stp_chr, plrel_exc=plrel_exc, core_num=detectCores(), sim_lim=.3, out_name="str_clst") {
  
  ## receives the similarity matrix, and returns the resultant clustered data frame based on the similarity limit that user provides
  
  cls <- vector('list', 0)
  sim_mat <- sim_mat(chr_lst=chr_lst, stp_wrd=stp_wrd, stp_chr=stp_chr, plrel_exc=plrel_exc, core_num=core_num)
  ind <- seq(1, nrow(sim_mat))
  
  rep <- 1
  
  while(max(sim_mat) > sim_lim) {
    
    omt <- vector('numeric', 0)
    
    x <- which(sim_mat==max(sim_mat), arr.ind=TRUE)[1, 1]
    y <- which(sim_mat==max(sim_mat), arr.ind=TRUE)[1, 2]
    
    omt[length(omt)+1] <- x
    omt[length(omt)+1] <- y
    
    cls[rep] <- list(c(ind[x], ind[y]))
    
    for(k in 1:nrow(sim_mat)) {
      
      sum <- 0
      
      for(l in 1:length(omt)) {
        sum <- sum + (sim_mat[k, omt[l]] >= sim_lim)
      }
      
      if(sum==length(omt)) {
        cls[rep] <- list(c(unlist(cls[rep]), ind[k]))
        omt[length(omt)+1] <- k
      }
      
    }
    
    sim_mat <- sim_mat[-omt, -omt]
    ind <- ind[-omt]
    
    rep <- rep+1
    
  }
  
  cls <- c(cls, as.list(ind))
  
  
  txt <- chr_lst[unlist(cls)]
  
  rep <- vector('numeric', length(cls))
  for(i in 1:length(rep)) {
    rep[i] <- length(unlist(cls[i]))
  }
  num <- vector('numeric', 0)
  for(i in 1:length(rep)) {
    num <- c(num, rep(i, rep[i]))
  }
  
  
  pat <- one_or_more(DGT)
  rnum <- as.numeric(str_extract(rownames(data.frame(txt)), pat))
  
  table <- data.frame(txt, num)
  colnames(table) <- c("Text", "ClunsterNum")
  
  write_xlsx(table, paste0(out_name, '.xlsx'))
  
  return(table)
  
}



sm_wrd_cnt <- function(chr_lst=chr_lst, stp_wrd=stp_wrd, stp_chr=stp_chr, plrel_exc=plrel_exc, out_name='wrd_frq', max.words=150, core_num=detectCores()) {
  
  ## receives a string set and returns the word counts as well as the wordcloud
  
  wrd_cnt <- unlist(sm_wrd_lst(chr_lst=chr_lst, stp_wrd=stp_wrd, stp_chr=stp_chr, plrel_exc=plrel_exc, core_num))
  table <- data.frame(sort(table(wrd_cnt), decreasing=TRUE))
  
  write_xlsx(table, paste0(out_name, '.xlsx'))
  
  words <- table$wrd_cnt
  freq <- table$Freq
  
  wordcloud(words, freq, max.words=max.words, colors=c('black', 'orange', 'red','green', 'blue'))
  
  cat(" \n\nDONE!\n")
  
}


sm_wrd_lst <- function(chr_lst=chr_lst, stp_wrd=stp_wrd, stp_chr=stp_chr, plrel_exc=plrel_exc, core_num=detectCores()) {
  
  tic()
  chr_lst <- integ(chr_lst)
  cat(" \n\nIntegration process complete ..\n")
  toc()
  
  tic()
  chr_lst <- str_remove_all(chr_lst, '[:emoji:]')
  cat(" \n\nEmoji removal process complete ..\n")
  toc()
  
  tic()
  chr_lst <- chr_stp(chr_lst, stp_chr, core_num)
  cat(" \n\nExtra characters removel process complete ..\n")
  toc()
  
  tic()
  chr_lst <- str_replace_all(chr_lst, SPC %R% one_or_more(SPC), " ")
  cat(" \n\nWhite-space correction process complete ..\n")
  toc()
  
  tic()
  chr_lst <- str_to_wrd(chr_lst, stp_wrd)
  cat(" \n\nSentence-to-word conversion process complete ..\n")
  toc()
  
  tic()
  chr_lst <- alp_rmv(chr_lst)
  cat(" \n\nOne-character phrase removal process complete ..\n")
  toc()
  
  tic()
  chr_lst <- plrel_to_sng(chr_lst, plrel_exc, core_num)
  cat(" \n\nPlural-Relative appendix removal process complete ..\n")
  toc()
  
  return(chr_lst)
  
  cat(" \n\nDONE!\n")
}



str_to_wrd <- function(chr_lst = chr_lst, stp_wrd = stp_wrd) {
  
  ## receives a character set and returns their separated words list
  
  chr_lst_new <- vector('list', length(chr_lst))
  for (i in 1:length(chr_lst)) {
    chr_lst_new[i] <- rm_stopwords(chr_lst[i], stp_wrd)
  }
  
  return(chr_lst_new)
}





ui <- fluidPage(
  
  theme = shinythemes::shinytheme("cosmo"),
  
  titlePanel("Social Media Text Clustering"), 
  
  sidebarLayout(
    sidebarPanel(
      fileInput("chr_lst", "Text file:"), 
      textOutput("chr_lst_cnt"),
      
      fileInput("stp_wrd", "Stop words:"), 
      textOutput("stp_wrd_cnt"),
      
      fileInput("stp_chr", "Stop characters:"), 
      textOutput("stp_chr_cnt"),
      
      fileInput("plrel_exc", "Parallel Exceptions:"), 
      textOutput("plrel_exc_cnt"),
      
      textInput("str_clst", "Name of the text cluster output file:", value="txt_clst"), 
      
      numericInput("mx_wrd", "Number of words in words cloud:", 100), 
      
      sliderInput("core_num", "Number of threads to be used:", min=1, max=detectCores(), value=detectCores()-1, step=1, ticks=FALSE),
      
      textInput("wrd_cnt", "Name of the word count output file:", value="wrd_frq"), 
      
      sliderInput("sim_lim", "Similarity Limit:", 0, 1, .3, .025), 
      
      actionButton("run1", "Word Cloud"),
      actionButton("run2", "Clustering")
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", 
                 plotOutput("wrd_cld")), 
        
        tabPanel("clusters", 
                 DT::DTOutput("wrd_clst"))
      )
    )
  )
)


server <- server <- function(input, output, session) {
  
  cnt_wrd <- eventReactive(
    input$run1, {
      
      sm_wrd_cnt(
        chr_lst=unlist(read_xlsx(input$chr_lst$datapath)), 
        stp_wrd=unlist(read_xlsx(input$stp_wrd$datapath)), 
        stp_chr=unlist(read_xlsx(input$stp_chr$datapath)), 
        plrel_exc=unlist(read_xlsx(input$plrel_exc$datapath)), 
        out_name=input$wrd_cnt, 
        max.words=input$mx_wrd, 
        core_num=input$core_num
      )
    }
  )
  
  output$wrd_cld <- renderPlot({
    cnt_wrd()
  })
  
  clst <- eventReactive(
    input$run2, {
      
      DT::datatable(
        sm_clst(
          chr_lst=unlist(read_xlsx(input$chr_lst$datapath)),
          stp_wrd=unlist(read_xlsx(input$stp_wrd$datapath)),
          stp_chr=unlist(read_xlsx(input$stp_chr$datapath)),
          plrel_exc=unlist(read_xlsx(input$plrel_exc$datapath)), 
          core_num=input$core_num, 
          sim_lim=input$sim_lim, 
          out_name=input$str_clst
        )
      )
    }
  )
  
  output$wrd_clst <- DT::renderDT({
    clst()
  })
  
  
  output$chr_lst_cnt <- renderText({
    
    req(input$chr_lst)
    
    cnt <- read_excel(input$chr_lst$datapath)
    cnt <- nrow(cnt)
    paste0(cnt)
  })
  
  output$stp_wrd_cnt <- renderText({
    
    req(input$stp_wrd)
    
    cnt <- read_excel(input$stp_wrd$datapath)
    cnt <- nrow(cnt)
    paste0(cnt)
  })
  
  output$stp_chr_cnt <- renderText({
    
    req(input$stp_chr)
    
    cnt <- read_excel(input$stp_chr$datapath)
    cnt <- nrow(cnt)
    paste0(cnt)
  })
  
  output$plrel_exc_cnt <- renderText({
    
    req(input$plrel_exc)
    
    cnt <- read_excel(input$plrel_exc$datapath)
    cnt <- nrow(cnt)
    paste0(cnt)
  })
  
}


shinyApp(ui, server)
