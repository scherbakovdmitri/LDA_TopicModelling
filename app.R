library(shiny)
library(shinyjs)
library(tidyverse)
#library(corpus)
library(quanteda)
library(text2vec)
#library(doParallel)
#quanteda::quanteda_options(verbose=F,threads=1)
library(foreach)
library(shinyalert)

df=readRDS("df.4.RDS")

options(shiny.maxRequestSize = 512*1024^2)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  titlePanel("Topic modelling UI"),
  h6("A tool to analyze latent topics in a given corpus. After first run additional advanced settings will become visible"),
  h6("Currently only english texts are supported in the format of zipped txt files. You can use a converter below to convert zipped PDFs to zipped TXTs. The converter is slow, so please start with a small archive or already parsed suffering corpus to test the tool. Please note: the converter will store the files in my gmail as it is used to send the files"),
  sidebarLayout(
    sidebarPanel(width=3,
                 
                 radioButtons("corpus",
                              "Select corpus:",
                              c("Upload a new corpus" = "custom",
                                "Old suffering corpus" = "suffering",
                                "New suffering corpus" = "suffering_new"),
                              selected='suffering_new'
                 ),
                 hidden(h5(id='step1',"Step 1. Convert zipped PDFs to TXTs")),
                 hidden(fileInput("file","Upload zip file with PDFs",accept='.zip')),
                 hidden(textInput("email","Your email")),
                 hidden(actionButton("convert","Start conversion",class = "btn-info")),
                 br(),br(),br(),
                 hidden(h5(id='step2',"Step 2. Wait for email and load TXTs")),
                 hidden(fileInput("file2","Upload zip file with TXTs",accept='.zip')),
                 sliderInput("topics",
                             "Number of topics:",
                             min = 1,
                             max = 200,
                             value = 20),
                 hidden(radioButtons("verbatim", "Use full article or verbatim quotes:",
                                     c("Full article (141 articles)" = "full",
                                       "Verbatim quotes (110 articles)" = "verbatim"))),
                 hidden(a(id='articlesused',"Show articles used",href="article_list.html")),
                 (radioButtons("full", "When using full article use full text or word window around specified term:",
                               c("Full article" = "full",
                                 "Word window" = "window"))),
                 hidden(checkboxGroupInput("columns", "Which columns to use?",
                                           df %>% select_if(is.character) %>% names,
                                           selected=(df %>% select(c(20,23,26,29,44,47,53,56,50)) %>% names))),
                 disabled(sliderInput("window",
                                      "Word window:",
                                      min = 1,
                                      max = 100,
                                      value = 5)),
                 (hidden(textInput("token","Enter search term for which to build word window","suffer*"))),
                 br(),
                 actionButton("go","Go",class = "btn-primary btn-lg"),
                 br(),br(),
                 hidden(sliderInput("topwords",
                                    "Number of top words to display (refreshes table in real-time):",
                                    min = 1,
                                    max = 50,
                                    value = 7)),
                 hidden(actionButton("show.advanced","Show advanced settings")),
                 hidden(h4(id='advanced.label',"Set lambda and hypertuning parameters:")),
                 hidden(sliderInput("ngrams",
                                    "Analyze singe words (default), word-pair or word-triplet combinations",
                                    min = 1,
                                    max = 3,
                                    step=1,
                                    value = 1)),
                 hidden(checkboxInput("stem",
                                    "Reduce words endings to improve LDA results")),
                 hidden(textInput("exclude","Exclude some unwanted tokens from corpus (comma-separated list)")),      
                 hidden(sliderInput("lambda",
                                    "Set lambda (refreshes table in real-time):",
                                    min = 0,
                                    max = 1,
                                    step=0.1,
                                    value = 0.8)),
                 hidden(checkboxInput("coherence",
                                    "Display topic coherence (bottom row)"
                                    )),
                 hidden(sliderInput("alpha",
                                    "Topics per document (Alpha prior):",
                                    min = 0,
                                    max = 50,
                                    step=0.05,
                                    value=50/20
                 )),
                 hidden(sliderInput("eta",
                                    "Words topic (Eta prior):",
                                    min = 0,
                                    step=0.05,
                                    max = 10,
                                    value=1/20)),
                 hidden(h6(id='param.label',"Higher alpha assumes that each document is likely to have several topics, higher eta assumes that topics have more shared words between each other. 
                      And vice versa. Default values can be used for start. Lambda filters the output words, so that with lower lambda more frequent/common words are penalized and removed from table")),
                 hidden(actionButton("update","Apply hyperparameters"))
    ),
    
    
    mainPanel(
      hidden(h5(id='wait',"Please wait for the calculations to finish, the table will be displayed below... This can take about a minute.")),
      textOutput('warnings'),
      tableOutput('lda')
    )
  )
)


server <- function(input, output,session) {
  
  lda_model=reactiveValues()
  crp=reactiveValues()
  
  observe({
    if(input$full=="window") {enable("window");show("token");}
    else {disable("window");hide("token");}
  }) %>% 
    bindEvent(input$full,ignoreInit = T)
  
  observe({
    
    output$warnings=renderText({
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    print(ext)
    validate(need(ext == "zip", "Please upload a zip file with PDFs or choose suffering example corpus"))
    show("email")
    show("convert")
    })
  }) %>% bindEvent(input$file)
  
  observe({
    output$warnings=renderText({
      validate(need(str_detect(input$email,"@"),"Enter your email address"))
      hide('email')
      hide('convert')
      system(wait=F,paste0('Rscript script.R ',input$file$datapath,' ',input$email))
      mes="Processing has started... This can take from 10 minutes (for ~10 pages) up to a day (for 1000+ pages) or more.
      You will recieve an email when OCR is done. This instance of the app will now close, but you can open a new session to run analysis in parallel."
      mes
      shinyalert("Submitted!",mes, type = "info")
      # Sys.sleep(10)
      # js$closeWindow()
      # stopApp()
    })
  }) %>% bindEvent(input$convert)
  
  
  observe({
    
    if(input$corpus=="custom"&&!length(crp$texts)) 
    {
      output$warnings=renderText({
        file <- input$file2
        ext <- tools::file_ext(file$datapath)
        validate(need(ext == "zip", "Please upload a zip file with TXTs or choose suffering example corpus"))
        dir=tempfile()
        unzip(file$datapath,exdir = dir)
        texts=list()
        #print(getwd())
        proj_path=getwd()
        setwd(dir)
        texts=foreach (i=1:length(list.files(dir,".txt"))) %do%
           {
             readLines(list.files(dir,".txt",full.names=T)[i])
           }
        
        setwd(proj_path)
        unlink(dir,force=T,recursive = T)
        crp$texts=texts[which(texts %>% sapply(class)!="try-error")]
        print(paste0("Text files loaded: ",length(crp$texts),". You can now press Go butoon" ))


      })
      file <- input$file2
      ext <- tools::file_ext(file$datapath)
      validate(need(ext == "zip", "Please upload a zip file with TXTs or choose suffering example corpus"))
      
    }
  }) %>% bindEvent(input$file2)
  
  observe({
    show("wait")
    show("topwords")
    show('show.advanced')
    html("wait","Please wait for the calculations to finish, the table will be displayed below... This can take about a minute.")
    lda_model$lda_model=LDA$new(n_topics=input$topics,doc_topic_prior=input$alpha,topic_word_prior=input$eta)
    if(input$verbatim=="full"|input$corpus=="custom")
    {
      print("START")
      if(input$full=="window")
      {
      ind=crp$texts %>% sapply(.,paste0,collapse=' ') %>% str_match_all(gsub("\\*\\\\b$","",gsub("^\\\\b\\*","",paste0("\\b",input$token,"\\b")))) %>% 
        sapply(length) %>% tibble %>% 
        mutate(cumsum=(cumsum(.) %/% 1000 + 1))  
      
      kw=tibble()
      # cl <- parallel::makePSOCKcluster(2)
      # doParallel::registerDoParallel()
      token=input$token
      window=input$window
      #kw=foreach(i=(unique(ind$cumsum)),.combine = bind_rows) %do%
      for (i in unique(ind$cumsum))  
        {
        html("wait",paste0("Finding word window matches... ",(i-1)*1000,"/",sum(ind$.)))
        kw=bind_rows(kw,crp$texts[which(ind$cumsum==i)] %>% sapply(.,paste0,collapse=' ') %>% 
                       kwic(pattern=input$token,window=input$window,valuetype = "glob"))
        }
      crp$crp=kw %>% mutate(text=paste0(pre,' ',keyword,' ',post)) %>% pull(text) %>% corpus
      # parallel::stopCluster(cl)
      print(dim(kw))
      }
      else
        crp$crp=crp$texts %>% sapply(.,paste0,collapse=' ') %>% corpus
      # purrr::when(input$full=="window"~(
      # # #   text_locate(.,terms=input$token)  %>% 
      # # #     mutate_if(is_corpus_text,as.character) %>% 
      # # #     #rowwise() %>% 
      # # #     mutate(after=as.character(text_sub(after,1,input$window))) %>% 
      # # #     mutate(before=as.character(text_sub(before,length(unlist(text_tokens(before)))-input$window,length(unlist(text_tokens(before)))))) %>% 
      # # #   #  ungroup %>% 
      # quanteda::kwic(.,pattern=input$token,window=input$window,valuetype = "glob") %>% 
      # mutate(text=paste0(pre,' ',keyword,' ',post)) %>% pull(text)),input$full!="window"~.) %>% 
      #quanteda::kwic(pattern=input$token,window=input$window,valuetype = "glob") %>% head(100) %>% 
      #corpus
      html("wait","Please wait for the calculations to finish, the table will be displayed below... This can take about a minute.")
      
    }
    else             
      crp$crp=(df %>%  
                 rowwise() %>% 
                 mutate(tokens=paste0(collapse=" ",
                                      tokenize_fastestword(c_across(input$columns)))) %>%
                 pull(tokens) %>% 
      quanteda::corpus())
      print("OK")
      print(class(crp$crp))
      #crp$crp %>% str
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
      tokens_remove(pattern = c("can","may",quanteda::stopwords("english")), padding = FALSE)  %>% 
      tokens_remove(min_nchar=3) %>% 
      {if (str_length(input$exclude)>1) tokens_remove(.,gsub(" ","",str_split(input$exclude,'[,;]',simplify = F) %>% unlist)) else .}%>%   
      {if (input$stem) tokens_wordstem(.,language="english") else .}%>%   
      tokens_ngrams(1:input$ngrams) %>% 
      dfm %>% 
      lda_model$lda_model$fit_transform()
    #lda_model_full$plot()
    # lda_model$get_top_words(lambda=1)
      output$lda=renderTable({
        tw=as.data.frame(lda_model$lda_model$get_top_words(lambda=input$lambda,input$topwords))
        if (input$coherence){
        coherence=parallel::mclapply(lapply(tw,paste0,collapse='%20'),
                                     function (x) httr::content(httr::GET(paste0("http://palmetto.aksw.org/palmetto-webapp/service/cp?words=",x,"text"))))
        tw %>% add_case(mutate_all(round(as.data.frame(coherence),2),as.character))
        }
        else tw
        })
      
    hide("wait")
  }) %>% bindEvent(input$go,input$update,ignoreInit=T)
  #  observe({output$lda=renderTable()}) %>% bindEvent(input$update,ignoreInit=F)

    
  observe({
    if (input$verbatim=="full") {show("full");show("window");show("token");hide("columns");}
    else {hide("window");hide("full");show("columns");hide("token");}
  }) %>% bindEvent(input$verbatim,ignoreInit=T)
  
  observe({
    if (input$corpus=="custom")
    {
      hide("verbatim");show("file");hide('articlesused');show('file2')
      crp$texts=NULL
      show('convert');show('step1');show('step2')
    }
    else
    { 
      if (input$corpus=="suffering") {
        crp$texts=readRDS("texts.RDS")%>% gsub("\\\\n"," ",.)
        show("verbatim")
        }
      else {
        crp$texts=readRDS("texts_new")%>% gsub("\\\\n"," ",.)
        hide("verbatim")
      }
      hide("file");show('articlesused');hide("file2")
      hide('convert');hide('step1');hide('step2')
    }

  }) %>% bindEvent(input$corpus)
  
  
  
  observe({
    toggle("advanced.label")
    toggle("ngrams")
    toggle("exclude")
    toggle("stem")
    toggle('lambda')
    toggle('coherence')
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
    updateSliderInput(session,'alpha',value=50/input$topics)
    updateSliderInput(session,'eta',value=1/input$topics)
  }) %>% 
    bindEvent(input$topics,ignoreInit = T)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
