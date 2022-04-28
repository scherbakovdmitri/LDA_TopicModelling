library(shiny)
library(shinyjs)
library(tidyverse)
library(quanteda)
library(text2vec)

texts=readRDS("texts.RDS")
df=readRDS("df.4.RDS")


ui <- fluidPage(
    useShinyjs(),
    titlePanel("Topic modelling UI"),
    sidebarLayout(
            sidebarPanel(width=2,
            #          radioButtons("dataset",
            #               "Select dataset:",
            #               c("Upload a new dataset (zipped PDFs format)" = "custom",
            #                 "Use suffering dataset" = "suffering"))),
            # hidden(fileInput("file","Upload zip file with PDFs")),
            sliderInput("topics",
                        "Number of topics:",
                        min = 1,
                        max = 200,
                        value = 20),
            (radioButtons("verbatim", "Use full article or verbatim quotes:",
                         c("Full article (141 articles)" = "full",
                           "Verbatim quotes (110 articles)" = "verbatim"))),
            (radioButtons("full", "When using full article use full text or word window aroundterm suffer*:",
                         c("Full article" = "full",
                           "Word window" = "window"))),
            # hidden(textInput("token","Enter search term for which to build word window","suffer*")),
            hidden(checkboxGroupInput("columns", "Which columns to use?",
                                      df %>% select_if(is.character) %>% names,
                                      selected=(df %>% select(c(20,23,26,29,44,47,53,56,50)) %>% names))),
            disabled(sliderInput("window",
                        "Word window:",
                        min = 1,
                        max = 100,
                        value = 5)),
            br(),
            actionButton("go","Go"),
            htmltools::a("Show articles used",href="article_list.html"),
            hidden(sliderInput("topwords",
                               "Number of top words to display:",
                               min = 1,
                               max = 50,
                               value = 10)),
            hidden(actionButton("show.advanced","Show advanced settings")),
            hidden(h4(id='advanced.label',"Set lambda and hypertuning parameters:")),
            hidden(sliderInput("lambda",
                               "Set lambda (refreshes table in real-time):",
                               min = 0,
                               max = 1,
                               step=0.1,
                               value = 0.8)),
            hidden(sliderInput("alpha",
                               "Topics per document (Alpha prior):",
                               min = 0,
                               max = 50,
                               step=0.01,
                               value=50/20
                               )),
            hidden(sliderInput("eta",
                               "Words topic (Eta prior):",
                               min = 0,
                               step=0.01,
                               max = 10,
                               value=1/20)),
            hidden(h6(id='param.label',"Higher alpha assumes that each document is likely to have several topics, higher eta assumes that topics have more shared words between each other. 
                      And vice versa. Default values can be used for start. Lambda filters the output words, so that with lower lambda more frequent/common words are penalized and removed from table")),
            hidden(actionButton("update","Apply hyperparameters"))
            ),
            
            

        # Show a plot of the generated distribution
        mainPanel(
          hidden(h5(id='wait',"Please wait for the calculations to finish, the table will be displayed below... This can take about a minute.")),
          tableOutput('lda')
        )
    )
)


server <- function(input, output,session) {

    lda_model=reactiveValues()
    crp=reactiveValues()
    
    observe({
      #if(input$full=="window") 
        toggleState("window")
    }) %>% 
      bindEvent(input$full,ignoreInit = T)
    
    observe({
      show("wait")}) %>% 
      bindEvent(input$go,ignoreInit=T)
    
    observe({
      output$lda=renderTable(data.frame(message="Please wait for calculations to finish..."))
      lda_model$lda_model=LDA$new(n_topics=input$topics,doc_topic_prior=input$alpha,topic_word_prior=input$eta)
      if(input$verbatim=="full")
        crp$crp=(texts %>% unlist %>% when(input$full=="window"~(quanteda::kwic(.,pattern="suffer*",window=input$window) %>% 
                                                                   mutate(text=paste0(pre,' ',keyword,' ',post)) %>% pull(text)),input$full!="window"~.)) %>% corpus
      else             
        crp$crp=(df %>%  
                   rowwise() %>% 
                   mutate(tokens=paste0(collapse=" ",
                                        tokenize_fastestword(c_across(input$columns)))) %>%
                   pull(tokens)) %>% 
          corpus
      fit=
        crp$crp %>% 
        tokens(
          remove_punct = T,
          remove_symbols = T,
          remove_numbers = T,
          remove_url = T,
          remove_separators = TRUE,
          split_hyphens = F) %>%  
        tokens_tolower() %>% 
        tokens_remove(pattern = c("can","may",stopwords("english")), padding = FALSE)  %>% 
        tokens_remove(min_nchar=3) %>% 
        #quanteda::tokens_ngrams(1:2) %>% 
        dfm %>% 
        lda_model$lda_model$fit_transform()
      #lda_model_full$plot()
     # lda_model$get_top_words(lambda=1)
      
        output$lda=renderTable(lda_model$lda_model$get_top_words(lambda=input$lambda,input$topwords))
        hide("wait")
      }) %>% bindEvent(input$go,input$update,ignoreInit=T)
  #  observe({output$lda=renderTable()}) %>% bindEvent(input$update,ignoreInit=F)
    
    observe({
      if (input$verbatim=="full") {show("full");show("window");hide("columns");}
      else {hide("window");hide("full");show("columns");}
    }) %>% bindEvent(input$verbatim,ignoreInit=T)
    
    
    # observe({
    #   if(input$verbatim=="full")
    #     crp$crp=(texts %>% unlist %>% when(input$full=="window"~(quanteda::kwic(.,pattern="suffer*",window=input$window) %>% 
    #                     mutate(text=paste0(pre,' ',keyword,' ',post)) %>% pull(text)),input$full!="window"~.)) %>% corpus
    #   else             
    #   crp$crp=(df %>%  
    #            rowwise() %>% 
    #            mutate(tokens=paste0(collapse=" ",
    #                                 tokenize_fastestword(c_across(input$columns)))) %>%
    #            pull(tokens)) %>% 
    #   corpus
    # }) %>% 
    #   bindEvent(input$full,input$verbatim,input$columns,input$window,ignoreInit=F)
    
    
    
    observe({show("topwords")
             show('show.advanced')
             }) %>% 
      bindEvent(input$go,ignoreInit=T)
    
    observe({
      toggle("advanced.label")
      toggle('lambda')
      toggle('alpha')
      toggle('eta')
      toggle('param.label')
      toggle('update')
    }) %>% 
      bindEvent(input$show.advanced,ignoreInit=T)
    
    observe({
      output$lda=renderTable(lda_model$lda_model$get_top_words(lambda=input$lambda,input$topwords))
    }) %>% 
      bindEvent(input$topwords,ignoreInit = T)
    
    observe({
      updateSliderInput(session,'alpha',value=50/input$topics,max=round(500/input$topics,2))
      updateSliderInput(session,'eta',value=1/input$topics,max=round(10/input$topics,2))
    }) %>% 
      bindEvent(input$topics,ignoreInit = T)
    

}

# Run the application 
shinyApp(ui = ui, server = server)
