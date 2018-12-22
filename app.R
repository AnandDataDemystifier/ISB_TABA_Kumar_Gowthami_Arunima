#---------------------------------------------------------------------#
#              TABA Assignment Term 1 ISB CBA                         #

#Group Project: Members Details	
# 1. Kumar Anand	11810023
# 2. Gowthami Matta	11810034
# 3. Arunima Sinha	11810073

#---------------------------------------------------------------------#


# Define ui function
ui <- shinyUI(

  fluidPage(
   theme = shinytheme("sandstone"),
   tags$head(
     tags$style(HTML("
                     @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');

                     h2 {
                     font-family: 'Lobster';
                     font-weight: 500;
                     line-height: 1.1;
                     color: #48ca3b;
                     }

                     "))),
   includeCSS("style.css"),

   headerPanel("TABA Assignment"),
    
    titlePanel("Shiny App around the UDPipe NLP workflow"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        
        checkboxInput("OtherLanguage", h4("By default this shiny app works for English. Select this check box for any other Language (Non English).Please ensure to upload same language
                      text and udpipe file.If you are testing for English, please don't check this check box")),
        #checkboxInput("Other", "Other"),
        
        conditionalPanel( # XPOS English
          condition = "input.OtherLanguage != true",
          checkboxGroupInput(inputId = 'xposcheckgroup', label = h2("Part-of-speech tags English (XPOS) "), 
                             choices = list("Adjective (JJ)" = "JJ", "Noun(NN)" = "NN",
                                            "Proper Noun (NNP)"="NNP","Adverb (RB)"="RB","Verb (VB)" ="VB"),
                             selected = list("JJ","NN","NNP"))
          
        ),
        conditionalPanel(# UPOS Other Languages
          condition = "input.OtherLanguage == true",
          checkboxGroupInput(inputId = 'uposcheckgroup', label = h2("Part-of-speech tags Non English (UPOS) "), 
                             choices = list("Adjective (JJ)" = "ADJ", "Noun(NN)" = "NOUN",
                                            "Proper Noun (NNP)"="PROPN","Adverb (RB)"="ADV","Verb (VB)" ="VERB"),
                             selected = list("ADJ","NOUN","PROPN"))
          
        ),
        fileInput("file", label= h2("Upload text file")),
        fileInput("file1", label = h2("Upload  upload trained udpipe model for different languages"))
       ),   # end of sidebar panel
      
      
      mainPanel(
        img(src = "parts_of_speech_icon.png", height = 200, width = 200),
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             h4(p("Data input")),
                             p("This app supports two files: txt file and udpipe trained model file.",align="justify"),
                             p("Please refer to the link below for sample file of english udpipe trained model"),
                             a(href="https://github.com/AnandDataDemystifier/ShinyAppTest/blob/master/english-ud-2.0-170801.udpipe?raw=true"
                               ,"Sample udpipe file"),  
                             br(),
                             p("Please refer to the link below for sample file of other udpipe trained models"),
                             a(href="https://github.com/bnosac/udpipe.models.ud/tree/master/models"
                               ,"Other udpipe files"),  
                             br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload the text file")),
                               'and also upload the udpipe data'),
                    
          
                    fluidPage(

                    h3("Help text"),
                           br(),
                    helpText("Click on the different tabs to see the analysis of your texts")),
                    helpText("For English,please do not check the top check box on left. Please upload the corresponding txt and udpipe file"),
                    helpText("For non English, please select check the top check box on left (such as Hindi or Spanish). Please upload the corresponding txt and udpipe file.")       
                    
                    ), ## fluid page end
                  

                    tabPanel("Example dataset", h4(p("Download Sample text file")),
                              downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                              p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                              img(src = "example1.png")),
                     
                    tabPanel("Annotated Documents ",
                             dataTableOutput('datatableOutput'),
                             downloadButton("downloadData", "Download Annotated Data")),

                    tabPanel("Co-Occurrences plots",
                             h3("Co-occurrence plots"),
                             plotOutput('cooccurenceplot')),

                    
                     tabPanel("POS frequency plots",
                              h3("POS frequency plots"),
                              plotOutput('posfrequencyplot')),

                    tabPanel("Word Clouds",
                             h3("Word Cloud of Adjectives"),
                             plotOutput('wordcloudplot1'),
                             h3("Word Cloud of Nouns"),
                             plotOutput('wordcloudplot2'),
                             h3("Word Cloud of Proper Noun"),
                             plotOutput('wordcloudplot3'),
                             h3("Word Cloud of Adverbs"),
                             plotOutput('wordcloudplot4'),
                             h3("Word Cloud of Verbs"),
                             plotOutput('wordcloudplot5')
                          
                             ),
                    tabPanel("Most Occuring POS Elements",
                            h3("Most Occuring Adjective"),
                            plotOutput('mostOccuringAdjective'),
                            h3("Most Occuring Noun"),
                            plotOutput('mostOccuringNoun'),
                            h3("Most Occuring Proper Noun"),
                            plotOutput('mostOccuringProperNoun'),
                            h3("Most Occuring Adverb"),
                            plotOutput('mostOccuringAdverb'),
                            h3("Most Occuring Verb"),
                            plotOutput('mostOccuringVerb')
                            )
                
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



# Define Server function
server <- shinyServer(function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  #windowsFonts(devanew=windowsFont("Devanagari new normal"))
  set.seed=2092014 
  Dataset <- reactive({
    
    if (is.null(input$file)) {
      return(NULL) } 
    else{
      Data <- str_replace_all(readLines(input$file$datapath,encoding = "UTF-8"), "<.*?>", "")
      Data = Data[Data!= ""]
      str(Data)
      return(Data)
    }
  })
  
  annotated_Allxpos<- reactive({
    x <- udpipe_annotate(udpipe_load_model(input$file1$datapath),x = Dataset())
    x <- as.data.frame(x)
    return(x)
  }  
  )
  

  
  annotated<- reactive({
    #x <- udpipe_annotate(udpipe_load_model(input$file1$datapath),x = Dataset())
    #x <- as.data.frame(x)
    #return(x)
    print(class(input$OtherLanguage))
    print(input$OtherLanguage)
    print(input$uposcheckgroup)
    print(input$xposcheckgroup)
    if(isTRUE(input$OtherLanguage)){ ## Other lang
      x <- subset(annotated_Allxpos(), upos %in% input$uposcheckgroup)
      x <- as.data.frame(x)
      print(input$OtherLanguage)
     return(x) 
      
    }
    else{ ## English
    x <- subset(annotated_Allxpos(), xpos %in% input$xposcheckgroup)
    x <- as.data.frame(x)
    print(input$OtherLanguage)
    return(x)
    }
    #return(x)
  }
    
  )
  
  ## Annotated data download 
  output$downloadData <- downloadHandler(
    filename = function(){
      "annotated_data.csv"
    },
    content = function(file){
      write.csv(annotated()[,-4],file,row.names = FALSE)
    }
  )
  
  # Annotated data
  output$datatableOutput = renderDataTable({
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    if(is.null(input$file)){return(NULL)}
    else{
      out = annotated()[,-4]
      return(out)
    }
  })

  
  ## 1. Word Cloud of Adjectives
  output$wordcloudplot1 = renderPlot({
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    if(is.null(input$file)){
      return(NULL)
      }
    else{
      if(isTRUE(input$OtherLanguage)){ ## Other Language UPOS
      all_adjectives = annotated() %>% subset(., upos %in% "ADJ")  ## NOUN
      top_adjectives = txt_freq(all_adjectives$lemma) 
      }
      else{ # English XPOS
        all_adjectives = annotated() %>% subset(., xpos %in% "JJ")  ## NOUN
        top_adjectives = txt_freq(all_adjectives$lemma)  
        
      }
      wordcloud(words= top_adjectives$key,freq= top_adjectives$freq, min.freq = 2,max.words = 100,
                random.order = FALSE,colors = brewer.pal(6, "Dark2"))
    }
  })

  
  
  ## 2. Word Cloud of Nouns
  output$wordcloudplot2 = renderPlot({
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){ ## UPOS Other Language
      all_nouns = annotated() %>% subset(., upos %in% "NOUN")  
      top_nouns = txt_freq(all_nouns$lemma) 
      }
      else{ ## XPOS English
        all_nouns = annotated() %>% subset(., xpos %in% "NN")  
        top_nouns = txt_freq(all_nouns$lemma) 
        
      }
      wordcloud(words= top_nouns$key,freq= top_nouns$freq, min.freq = 2,max.words = 100,
                random.order = FALSE,colors = brewer.pal(6, "Dark2"))
    }
  })
  
  ##3.  Word Cloud of Proper Nouns
  output$wordcloudplot3 = renderPlot({
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){ ## UPOS Other Language
      all_propernouns = annotated() %>% subset(., upos %in% "PROPN")  
      top_propernouns = txt_freq(all_propernouns$lemma) 
      }
      else{ ## XPOS English Language
          all_propernouns = annotated() %>% subset(., xpos %in% "NNP")  
          top_propernouns = txt_freq(all_propernouns$lemma) 
        
      }
      wordcloud(words= top_propernouns$key,freq= top_propernouns$freq, min.freq = 2,max.words = 100,
                random.order = FALSE,colors = brewer.pal(6, "Dark2"))
    }
  })
  
  ##4.  Word Cloud of Adverbs
  output$wordcloudplot4 = renderPlot({
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){ ## UPOS Other Language
      all_adverbs = annotated() %>% subset(., upos %in% "ADV")  
      top_adverbs = txt_freq(all_adverbs$lemma) 
      }
      else{ # XPOS English
      all_adverbs = annotated() %>% subset(., xpos %in% "RB")  
      top_adverbs = txt_freq(all_adverbs$lemma)  
        
      }
      
      wordcloud(words= top_adverbs$key,freq= top_adverbs$freq, min.freq = 2,max.words = 100,
                random.order = FALSE,colors = brewer.pal(6, "Dark2"))
    }
  })
  
  ## 5. Word Cloud of Verbs
  output$wordcloudplot5 = renderPlot({
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){ ## UPOS Other Language
        #x = subset(annotated(), xpos %in% input$upos)
        all_verbs = annotated() %>% subset(., upos %in% "VERB")  
        top_verbs = txt_freq(all_verbs$lemma) 
      }
      else{# XPOS English
        #x = subset(annotated(), xpos %in% input$xpos)
        all_verbs = annotated() %>% subset(., xpos %in% "VB")  
        top_verbs = txt_freq(all_verbs$lemma) 
        
      }
        wordcloud(words= top_verbs$key,freq= top_verbs$freq, min.freq = 2,max.words = 100,
                  random.order = FALSE,colors = brewer.pal(6, "Dark2"))
      
    }
  })
  
  
  ## POS frequency plot
  ## reference: https://bnosac.github.io/udpipe/docs/doc5.html
  output$posfrequencyplot = renderPlot({
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){ ## UPOS Other Language
      poselements = txt_freq(annotated()$upos)
      }
      else{ ## XPOS Eng Language
        poselements = txt_freq(annotated()$xpos) 
      }
      poselements$key <- factor(poselements$key, levels = rev(poselements$key))
      barchart(key ~ freq, data = poselements, col = "yellow", 
               main = "xpos (Universal Parts of Speech)\n frequency of occurrence", 
               xlab = "Freq")
      
     
    }
  })
  
  
  # ## Most Occuring Adjective
  output$mostOccuringAdjective = renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){# UPOS Other Language
      all_adj = head(annotated() %>% subset(., upos %in% "ADJ"),20)
      }
      else{ ## XPOS English
      all_adj = head(annotated() %>% subset(., xpos %in% "JJ"),20) 
      }
      top_adj = txt_freq(all_adj$lemma)
      barchart(top_adj$key ~ top_adj$freq, col = "cadetblue",
               main = "Most occurring adjectives", xlab = "Freq")
      }
    
      })
  
  # ## Most Occuring NOun
  output$mostOccuringNoun = renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){# UPOS Other Language
      all_verbs = head(annotated() %>% subset(., upos %in% "NOUN"),20)
      }
      else{# XPOS Eng
      all_verbs = head(annotated() %>% subset(., xpos %in% "NN"),20) 
      }
      top_verbs = txt_freq(all_verbs$lemma)
      barchart(top_verbs$key ~ top_verbs$freq, col = "cadetblue",
               main = "Most occurring nouns", xlab = "Freq")
    }
  })
  
  # ## Most Occuring Proper Noun
  output$mostOccuringProperNoun = renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){# UPOS Other Language
      all_verbs = head(annotated() %>% subset(., upos %in% "PROPN"),20)
      }
      else{# XPOS Eng
      all_verbs = head(annotated() %>% subset(., xpos %in% "NNP"),20)
      }
      top_verbs = txt_freq(all_verbs$lemma)
      barchart(top_verbs$key ~ top_verbs$freq, col = "cadetblue",
               main = "Most occurring proper nouns", xlab = "Freq")
     
  
    }
  })
  
  # ## Most Occuring Adverbs
  output$mostOccuringAdverb = renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){# UPOS Other Language
      all_verbs = head(annotated() %>% subset(., upos %in% "ADV"),20)
      }
      else{ # Eng XPOS
      all_verbs = head(annotated() %>% subset(., xpos %in% "RB"),20)  
      }
      top_verbs = txt_freq(all_verbs$lemma)
      barchart(top_verbs$key ~ top_verbs$freq, col = "cadetblue",
               main = "Most occurring adverbs", xlab = "Freq")
      
    }
  })
  
  
  
  # ## Most Occuring Verb
  output$mostOccuringVerb = renderPlot({
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){# UPOS Other Language
      all_verbs = head(annotated() %>% subset(., upos %in% "VERB"),20)  
      }
      else{ # XPOS Eng
      all_verbs = head(annotated() %>% subset(., xpos %in% "VB"),20)
      }
      top_verbs = txt_freq(all_verbs$lemma)
      barchart(top_verbs$key ~ top_verbs$freq, col = "cadetblue",
                             main = "Most occurring nouns", xlab = "Freq")
     
    }
  })
  

  
 
  
  output$cooccurenceplot = renderPlot({
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    if(is.null(input$file)){return(NULL)}
    else{
      if(isTRUE(input$OtherLanguage)){# UPOS Other Language
      nokia_cooc <- cooccurrence(
          x = subset(annotated_Allxpos(), upos %in% input$uposcheckgroup),
          term = "lemma", 
          group = c("doc_id", "paragraph_id", "sentence_id"))
        }
        else{ # ENG XPOS
          nokia_cooc <- cooccurrence(
          x = subset(annotated_Allxpos(), xpos %in% input$xposcheckgroup),
          term = "lemma", 
          group = c("doc_id", "paragraph_id", "sentence_id"))
        }
      
      
      wordnetwork <- head(nokia_cooc, 50)
      wordnetwork <- igraph::graph_from_data_frame(wordnetwork) 
      
      ggraph(wordnetwork, layout = "fr") +  
        
        geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
        geom_node_text(aes(label = name), col = "darkgreen", size = 6) +
        
        theme_graph(base_family = "Arial") +  
        theme(legend.position = "none") +
        
        labs(title = "Cooccurrences within 3 words distance", subtitle = "Select single/multiple option on check box")
    }
  })
  
 
})
 

shinyApp(ui = ui, server = server)