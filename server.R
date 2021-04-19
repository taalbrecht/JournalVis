library(igraph)
library(rCharts)
library(reshape2)
library(rentrez)
library(RISmed)
library(rplos)
library(aRxiv)
library(shiny)
library(rcytoscapejs)
library(DT)
library(XML)
library(htmlwidgets)
library(shinyjs)
library(collapsibleTree)
library(RColorBrewer)
#library(taucharts)

##############################
#Shiny server options
##############################
#Use line below for error tracer. To reset to default, use options(shiny.error = NULL)
#options(shiny.error=browser)

#Variable to use quanteda or not for debugging purposes until tm replacement verified for stm topic modeling
usequanteda = TRUE

#Set maximum upload file size from fileInput to 500 mb
options(shiny.maxRequestSize=500*1024^2) 


shinyServer(function(input, output, session) {
  
  ######################
  ############Temporary local file directory selection. Eventually replace with fileInput to allow for server side use
  ########################
  
  localfilePath <- eventReactive(input$local_filepath, {
    #file.choose()
    choose.dir()
  })
  
  output$chosenlocalPath <- renderText({
    localfilePath()
  })
  
  ###################End temporary local file directory selection
  
  
  #Observer to enter debug mode
  observe ({  
    
    if(input$debug_button > 0){
      
      browser()
      
    }
  })
  
  #Reactive value to contain flag stating whether DT objects have been initialized
  tableinitialized <- reactiveValues()
  tableinitialized$DocumentDT = FALSE
  tableinitialized$SentenceDT = FALSE
  
  #Reactive value to contain clicks from bar graphs of key terms to highlight on graph
  groupclicks <- reactiveValues()
  
  #Reactive value to contain clicks from topic grouping graph
  topicclicks <- reactiveValues()
  
  #Reactive value to contain articles that have been selected by filtering
  filterinclude <- reactiveValues()
  
  #Observer to capture topic clicks from topic graph
  observe ({  
    
    if(is.null(input$topicClicked) == FALSE){
      
      if(isolate(input$topicClicked %in% topicclicks$selected == TRUE)){
        
        isolate(topicclicks$selected <- topicclicks$selected[!(topicclicks$selected == input$topicClicked)])
        
      }else{
        
        isolate(topicclicks$selected[length(topicclicks$selected) + 1] <- input$topicClicked)
        
      }
    }
  })
  
  #Observer to clear topicclicks
  observe ({  
    
    if(input$cleartopics > 0 | input$summary_search > 0 | input$link_search > 0 | input$detailed_search > 0){
      
      isolate(topicclicks$selected <- list())
      
    }
  })
  
  #Observer to update groupclicks reactive value on click event
  observe ({  
    
    if(is.null(input$click$category) == FALSE){
      
      if(isolate(input$click$category %in% groupclicks[[input$click$chart]] == TRUE)){
        
        isolate(groupclicks[[input$click$chart]] <- groupclicks[[input$click$chart]][!(groupclicks[[input$click$chart]] == input$click$category)])
        
      }else{
        
        isolate(groupclicks[[input$click$chart]][length(groupclicks[[input$click$chart]]) + 1] <- input$click$category)
        
      }
    }
  })
  
  #Observer to clear groupclicks
  observe ({  
    
    if(input$cleargroups > 0 | input$summary_search > 0 | input$link_search > 0 | input$detailed_search > 0){
      
      isolate(groupclicks$keyword <- list())
      isolate(groupclicks$articletype <- list())
      
    }
  })
  
  #Observer to apply groupclicks selections to generate a filtered list
  observe ({  
    
    if(input$filtergroups > 0 & length(DBSwitch()[["Fetch"]]) > 0){
      
      
      
      isolate(entfetch <- DBSwitch()[["Fetch"]])
      
      #Implement as an OR filter for now. IDpass is list of articles that match filter requirements
      IDpass <- c()
      
      if(length(isolate(groupclicks$articletype)) > 0){
        
        #Get articles that match the selected article types
        #       articletypelist <- sapply(entfetch@PublicationType, FUN = "[")
        #       articletypelist <- sapply(articletypelist, FUN = tolower)
        articletypelist <- entfetch$articletype
        
        for (i in 1:length(isolate(groupclicks$articletype))){
          
          matchvect <- c()
          #Construct matching vector
          matchvect <- lapply(articletypelist, FUN = "%in%", isolate(groupclicks$articletype[i]))
          matchvect <- lapply(matchvect, FUN = "!")
          matchvect <- lapply(matchvect, FUN = prod)
          IDpass <- unique(c(IDpass, entfetch$PMID[which(matchvect == 0)]))
          
        }
      }
      
      if(length(isolate(groupclicks$keyword)) > 0){
        
        
        #       articlekeywords <- sapply(entfetch@Mesh, FUN = "[", "Heading")
        #       articlekeywords <- sapply(articlekeywords, FUN = tolower)
        articlekeywords <- entfetch$keywords
        
        for (i in 1:length(isolate(groupclicks$keyword))){
          
          matchvect <- c()
          #Construct matching vector
          matchvect <- lapply(articlekeywords, FUN = "%in%", isolate(groupclicks$keyword[i]))
          matchvect <- lapply(matchvect, FUN = "!")
          matchvect <- lapply(matchvect, FUN = prod)
          IDpass <- unique(c(IDpass, entfetch$PMID[which(matchvect == 0)]))
          
        }
      }
      
      #Filter out IDpass list to only include IDs that are also included in filter list (allows for multiple filter applications)
      
      isolate(filterinclude$ids <- intersect(filterinclude$ids, IDpass))
      
    }
    
  })
  
  #Observer to clear filtered results and expand to include all results again
  observe ({  
    
    #if(input$summary_search > 0 | input$link_search > 0 | input$detailed_search > 0| input$clear_filters > 0 | !is.null(LoadModel())){
    if(input$summary_search > 0 | input$link_search > 0 | input$detailed_search > 0| input$clear_filters > 0 | input$database == "Load Model"){
      
      if(length(DBSwitch()[["Fetch"]]) > 0){
        
        entfetch <- DBSwitch()[["Fetch"]]
        
        isolate(filterinclude$ids <- entfetch$PMID)
        
      }
    }
  })
  
  #Reactive value to contain augmentation data
  reactaugment <- reactiveValues()
  
  #Observer to collect augmented data
  observe ({  
    
    if(input$augment_button > 0){
      
      isolate({
        
        augmentframe = augmentdat()
        
        #Check to make sure file input, id column selector, and new column name are not blank
        if(!is.null(augmentframe)){
          
          # if(isolate(input$topicClicked %in% topicclicks$selected == TRUE)){
          #   
          #   isolate(topicclicks$selected <- topicclicks$selected[!(topicclicks$selected == input$topicClicked)])
          #   
          # }else{
          #   
          #   isolate(topicclicks$selected[length(topicclicks$selected) + 1] <- input$topicClicked)
          #   
          # }
          AugmentNewDat = augmentframe[[input$augmentnew]]
          names(AugmentNewDat) = augmentframe[[input$augmentkey]]
          
          reactaugment$newdat[[length(reactaugment$newdat) + 1]] = list("DBKey" = input$keymatch,
                                                                        #"AugmentKeyDat" = augmentframe[[input$augmentkey]],
                                                                        #"AugmentNewDat" = augmentframe[[input$augmentnew]],
                                                                        "AugmentNewDat" = AugmentNewDat,
                                                                        "AugmentDatName" = input$augmentnew)
          
        }
        
      })
      
    }
  })
  
  ####################### Dynamic UI selectors
  
  augmentdat = reactive({
    
    #Read file if it exists
    if(!is.null(input$augment_input)){
      output <- read.csv(input$augment_input$datapath, header = TRUE, stringsAsFactors = FALSE)
    }else{
      output = NULL
    }
    
    return(output)
    
  })
  
  output$ui_augmentkey <- renderUI({
    
    # If a file has not yet been uploaded, don't display the element
    if (is.null(augmentdat())){
      
      return(NULL)
      
    }else{
    
    #Import default model values and get length of formulalist
    choicevals <- c("", colnames(augmentdat()))
    
    selectInput(inputId = "augmentkey", label = "Key to Match In Augmentation Data",
                choices = choicevals, selected = "")
    }
    
  })
  
  output$ui_augmentnew <- renderUI({
    
    # If a file has not yet been uploaded, don't display the element
    if (is.null(augmentdat())){
      
      return(NULL)
      
    }else{
    
    #Get column names from augmentation dataset
    choicevals <- c("", colnames(augmentdat()))
    
    selectInput(inputId = "augmentnew", label = "Data to Merge From Augmentation Data",
                choices = choicevals, selected = "")
    }
    
  })
  
  output$ui_dbkey <- renderUI({
    
    # If a file has not yet been uploaded, don't display the element
    if (is.null(augmentdat())){
      
      return(NULL)
      
    }else{
    
    dbvals = DBSwitch()[["Fetch"]]
    
    #Import default model values and get length of formulalist
    choicevals <- c("", names(dbvals))
    
    selectInput(inputId = "keymatch", label = "Key to Match in Existing Data",
                choices = choicevals, selected = "")
    }
    
  })
  
  output$ui_graphlayout <- renderUI({
    
    #Import default model values and get length of formulalist
    choicevals <- c("cose", "drl", "preset")
    
    
    selectInput(inputId = "graphlayout", label = "Select layout for graph",
                choices = choicevals, selected = "preset")
    
  })
  
  #Dropdown for selecting preloaded models
  output$ui_preloadmodlist <- renderUI({
    
    #Get a list of all .RData files
    choicevals <- list.files(pattern = ".RData")
    
    #Append blank value to start of list to prevent long loading time of unwanted model when app is opened
    choicevals = c("", choicevals)
    selected = choicevals[1]
    
    #Determine which model should be selected based on URL query if provided
    modelURLquery = getQueryString()$model
    if(!is.null(modelURLquery)){
      selected = choicevals[choicevals == modelURLquery][1]
    }
    
    selectInput(inputId = "preloadmodsel", label = "Select dataset",
                choices = choicevals, selected = selected)
    
  })
  
  #Output UI for determining if any options on the UI should be displayed:
  output$showUI <- reactive({
    showUI = TRUE
    if(!is.null(getQueryString()$showUI) && getQueryString()$showUI == "FALSE"){
      showUI = FALSE
    }
    showUI
  })
  outputOptions(output, 'showUI', suspendWhenHidden = FALSE)
  
  #Output UI for enabling Admin features:
  output$adminUI <- reactive({
    adminUI = FALSE
    if(!is.null(getQueryString()$admin) && getQueryString()$admin == "TRUE"){
      adminUI = TRUE
    }
    adminUI
  })
  outputOptions(output, 'adminUI', suspendWhenHidden = FALSE)
  
  
  ####################################################################
  ######## Reactive Function Section
  ####################################################################
  
  LoadModel <- reactive({
    
    #File loader for pre-loaded user modes
    
    #If there is no file, return required blank structure
    if(input$usermode == "Pre-Loaded"){
      
      if (is.null(input$preloadmodsel) || input$preloadmodsel == ""){
        
        return(list())
      }
      
      #Load the model file and return
      loadeddat = new.env()
      load(input$preloadmodsel, envir = loadeddat)
    }
    
    #File loader for basic and advanced user modes
    if(input$usermode != "Pre-Loaded"){
      
      inFile <- input$saved_model_file
      #If there is no file, return blank lists
      if (is.null(inFile)){
        #DBSwitchLoad = list(Summary = list(), Fetch = list(), NodeEdge= list())
        
        return(list())
      }
      
      #Load the model file and return 
      loadeddat = new.env()
      load(inFile$datapath, envir = loadeddat)
    }
    
    #Return list of loaded data
    return(list(DBSwitchLoad = loadeddat$savedmodel$DBSwitchReact, TopicModelLoad = loadeddat$savedmodel$TopicReact))
  })
  ####PUBMED FETCH FUNCTIONS#####
  
  #Perform summary search to get results count
  EntrezSummary<-reactive({
    
    
    
    if(input$summary_search==0 | nchar(isolate(input$search_text)) == 0){
      
      count <- "No Search Terms Entered"
      ids <- list()
      
    }
    else{
      #  
      isolate(searchsummary <- entrez_search(db = input$database, term = input$search_text, retmax = 10000))
      
      count <- searchsummary$count
      ids <- searchsummary$ids
    }
    
    
    return(structure(list("count" = count, "ids" = ids)))
  })
  
  #Pass-through to add referenced or cited articles if button is pressed
  EntrezNodeEdge<-reactive({
    
    if(input$link_search==0 | is.null(isolate(EntrezFetch()[["PMID"]]))){
      
      CitationFrame <- list()
      ReferenceList <- list()
      TimesCited <- list()
      
      #relations <- list()
      #relations$CitationLinks <- NULL
    }
    else{
      
      isolate(relations <- GetPubmedCitationLinks(EntrezFetch()[["PMID"]]))
      CitationFrame <- relations$CitationFrame
      ReferenceList <- relations$ReferenceList
      TimesCited <- relations$TimesCited
    }
    return(structure(list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited)))
  })
  
  #Fetch entrez articles and format responses
  EntrezFetch<-reactive({
    
    if(input$detailed_search==0 | (length(isolate(EntrezSummary()[["ids"]])) == 0)){
      
      return(entfetch <- list())
    }
    else{
      #
      isolate(entfetch <- EUtilsGet(EntrezSummary()[["ids"]], db = input$database))
    }
    
    
    
    if(length(entfetch) > 0){
      
      #Get type of article (journal, clinical, etc)
      PMID <- entfetch@PMID
      
      title <- entfetch@ArticleTitle
      
      articletype <- entfetch@PublicationType
      articletype <- sapply(articletype, FUN = "[")
      articletype <- sapply(articletype, FUN = tolower)
      
      journal <- entfetch@Title
      
      #Assign minimum year out of all year fields as year article published to preserve reference-year hierarchy as much as possible
      yearpubframe <- data.frame(as.numeric(entfetch@YearReceived), as.numeric(entfetch@YearAccepted), as.numeric(entfetch@YearEpublish), as.numeric(entfetch@YearPpublish), as.numeric(entfetch@YearPmc), as.numeric(entfetch@YearPubmed))
      year <- apply(yearpubframe, 1, FUN = min, na.rm = TRUE)
      
      #If no year specified, set to 2015 to allow for proper topic modeling
      year[which(year == Inf)] <- 2015
      
      DOI <- entfetch@ELocationID
      abstract <- entfetch@AbstractText
      
      keywords <- entfetch@Mesh
      keywords <- sapply(keywords, FUN = "[", "Heading")
      keywords <- sapply(keywords, FUN = tolower)
      
      authors <- sapply(entfetch@Author , function(x) paste0(x$LastName, ", ", x$ForeName))
      
      #Construct hyperlink to pubmed database
      hyperlink <- paste0("http://www.ncbi.nlm.nih.gov/pubmed/", PMID)
      
      #Construct data to show on hover
      hovertip <- paste0("<b>Title: </b>", title,
                         "<br /><b>Year: </b>", year,
                         "<a href=", '"http://www.ncbi.nlm.nih.gov/pubmed/', PMID, '"target = "_blank"><br /><b>Pubmed ID: </b>', PMID, "</a>")
      
      #Construct data to show on click
      clicktip <- paste0("<b>Title: </b>", title,
                         "<br /><b>Year: </b>", year,
                         "<a href=", '"http://www.ncbi.nlm.nih.gov/pubmed/', PMID, '"target = "_blank"><br /><b>Pubmed ID: </b>', PMID, "</a>",
                         "<br /><b>Authors: </b>", authors,
                         "<br /><b>Abstract: </b>", abstract)
      
      
      #####Get article linkage structure#####
      
      relations <- GetPubmedCitationLinks(PMID)
      CitationFrame <- relations$CitationFrame
      ReferenceList <- relations$ReferenceList
      OutsideCitationFrame <- relations$OutsideCitationFrame
      TimesCited <- relations$TimesCited
      
      
      return(structure(list(details = list("title" = title, "articletype" = articletype,
                                           "journal" = journal, "year" = year,
                                           "DOI" = DOI, "abstract" = abstract,
                                           "keywords" = keywords, "authors" = authors,
                                           "PMID" = PMID, "hyperlink" = hyperlink,
                                           "HoverTip" = hovertip, "ClickTip" = clicktip),
                            refstructure = list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "OutsideCitationFrame" = OutsideCitationFrame, "TimesCited" = TimesCited))))
      
    }else{
      return(entfetch <- list())}
    
  })
  
  
  ####PLOS FETCH FUNCTIONS###
  
  #Perform summary search to get results count
  PLOSSummary<-reactive({
    
    if(input$summary_search==0 | nchar(isolate(input$search_text)) == 0){
      
      count <- "No Search Terms Entered"
      ids <- list()
      
    }
    else{
      #
      isolate(searchsummary <- searchplos(q = input$search_text, limit = 0))
      
      #ids set to dummy variable of 1 if search performed since PLOS search doesn't need ids to fetch details
      count <- searchsummary$meta$numFound
      ids <- 1
    }
    
    
    return(structure(list("count" = count, "ids" = ids)))
  })
  
  #Fetch PLOS articles
  PLOSFetch<-reactive({
    
    if(input$detailed_search==0 | (length(isolate(PLOSSummary()[["ids"]])) == 0)){
      
      return(plosfetch <- list())
    }
    else{
      #
      #Get details. See dataset plosfields in package rplos for available fields for "fl"
      isolate(plosfetch <- searchplos(q = input$search_text, limit = 1000,
                                      fl = c("id", "title", "abstract", "article_type", "author", "publication_date", "received_date", "accepted_date", "subject", "journal")))
      
      if(length(plosfetch) > 0){
        
        #Get type of article (journal, clinical, etc)
        PMID <- plosfetch$data$id
        
        title <- plosfetch$data$title
        
        articletype <- plosfetch$data$article_type
        articletype <- sapply(articletype, FUN = tolower)
        
        journal <- plosfetch$data$journal
        
        #Assign minimum year out of all year fields as year article published to preserve reference-year hierarchy as much as possible
        yearpubframe <- data.frame(as.numeric(format(as.Date(plosfetch$data$received_date), format = "%Y")),
                                   as.numeric(format(as.Date(plosfetch$data$accepted_date), format = "%Y")),
                                   as.numeric(format(as.Date(plosfetch$data$publication_date), format = "%Y")))
        
        
        year <- apply(yearpubframe, 1, FUN = min, na.rm = TRUE)
        
        DOI <- plosfetch$data$id
        abstract <- plosfetch$data$abstract
        
        keywords <- strsplit(plosfetch$data$subject, split = "/", fixed = TRUE)
        keywords <- sapply(keywords, FUN = tolower)
        
        authors <- strsplit(plosfetch$data$author, split = ";", fixed = TRUE)
        
        #Construct data to show on hover
        hovertip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<a href=", '"http://journals.plos.org/plosone/article?id=', PMID, '"target = "_blank"><br /><b>PLOS DOI: </b>', PMID, "</a>")
        
        #Construct data to show on click
        clicktip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<a href=", '"http://journals.plos.org/plosone/article?id=', PMID, '"target = "_blank"><br /><b>PLOS DOI: </b>', PMID, "</a>",
                           "<br /><b>Authors: </b>", authors,
                           "<br /><b>Abstract: </b>", abstract)
        
        #No linkage functions exist yet for PLOS
        CitationFrame <- data.frame(c(""), c(""))
        ReferenceList <- list()
        TimesCited <- list()
        
        return(structure(list(details = list("title" = title, "articletype" = articletype, "journal" = journal, "year" = year, "DOI" = DOI, "abstract" = abstract, "keywords" = keywords, "authors" = authors, "PMID" = PMID, "HoverTip" = hovertip, "ClickTip" = clicktip),
                              refstructure = list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited))))
        
      }else{
        return(plosfetch <- list())}
      
    }
    
  })
  
  #Perform linkage search to get specific nodes and connections
  PLOSNodeEdge<-reactive({
    
    if(input$link_search==0 | is.null(isolate(PLOSFetch()[["PMID"]]))){
      
      CitationFrame <- list()
      ReferenceList <- list()
      TimesCited <- list()
      
      #relations <- list()
      #relations$CitationLinks <- NULL
    }
    else{
      #
      #Not yet implemented. Add link code using "reference" facet of general PLOS search
      #isolate(relations <- GetPubmedCitationLinks(EntrezFetch()[["PMID"]]))
      CitationFrame <- data.frame(c(""), c(""))
      ReferenceList <- list()
      TimesCited <- list()
    }
    return(structure(list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited)))
  })
  
  ####arxiv FETCH FUNCTIONS###
  
  #Perform summary search to get results count
  ARXIVSummary<-reactive({
    
    if(input$summary_search==0 | nchar(isolate(input$search_text)) == 0){
      
      count <- "No Search Terms Entered"
      ids <- list()
      
    }
    else{
      
      query <- isolate(strsplit(input$search_text, split = " ", fixed = TRUE))
      
      query <- query[which(length(query) > 0)]
      
      searchsummary <- arxiv_count(query = query[[1]])
      
      #ids set to dummy variable of 1 if search performed since PLOS search doesn't need ids to fetch details
      count <- searchsummary
      ids <- 1
    }
    
    
    return(structure(list("count" = count, "ids" = ids)))
  })
  
  #Fetch Arxiv articles
  ARXIVFetch<-reactive({
    
    if(input$detailed_search==0 | (length(isolate(ARXIVSummary()[["ids"]])) == 0)){
      
      return(arxivfetch <- list())
    }
    else{
      #
      #Split query into vector of words to default to AND searching
      query <- isolate(strsplit(input$search_text, split = " ", fixed = TRUE))
      query <- query[which(length(query) > 0)]
      
      #Get result count for limit setting
      isolate(articlecount <- ARXIVSummary()[["count"]])
      
      #Get details. See dataset plosfields in package rplos for available fields for "fl"
      arxivfetch <- arxiv_search(query = query[[1]], batchsize = 100, limit = as.integer(articlecount))
      
      if(length(arxivfetch) > 0){
        
        #Get type of article (journal, clinical, etc)
        PMID <- arxivfetch$id
        
        title <- arxivfetch$title
        
        articletype <- arxivfetch$primary_category
        articletype <- sapply(articletype, FUN = tolower)
        
        journal <- arxivfetch$journal_ref
        
        #Assign minimum year out of all year fields as year article published to preserve reference-year hierarchy as much as possible
        year <- as.numeric(format(as.Date(arxivfetch$submitted), format = "%Y"))
        
        DOI <- arxivfetch$doi
        abstract <- arxivfetch$abstract
        
        keywords <- strsplit(arxivfetch$categories, split = "|", fixed = TRUE)
        keywords <- sapply(keywords, FUN = tolower)
        
        authors <- strsplit(arxivfetch$authors, split = "|", fixed = TRUE)
        
        #Construct data to show on hover
        hovertip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<a href=", '"http://arxiv.org/abs/', PMID, '"target = "_blank"><br /><b>aRxiv ID: </b>', PMID, "</a>")
        
        #Construct data to show on click
        clicktip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<a href=", '"http://arxiv.org/abs/', PMID, '"target = "_blank"><br /><b>aRxiv ID: </b>', PMID, "</a>",
                           "<br /><b>Authors: </b>", authors,
                           "<br /><b>Abstract: </b>", abstract)
        
        #Not yet implemented.
        CitationFrame <- data.frame(c(""), c(""))
        ReferenceList <- list()
        TimesCited <- list()
        
        return(structure(list(details = list("title" = title, "articletype" = articletype, "journal" = journal, "year" = year, "DOI" = DOI, "abstract" = abstract, "keywords" = keywords, "authors" = authors, "PMID" = PMID, "HoverTip" = hovertip, "ClickTip" = clicktip),
                              refstructure = list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited))))
        
      }else{
        return(arxivfetch <- list())}
      
    }
    
  })
  
  #Perform linkage search to get specific nodes and connections
  ARXIVNodeEdge<-reactive({
    
    if(input$link_search==0 | is.null(isolate(ARXIVFetch()[["PMID"]]))){
      
      CitationFrame <- list()
      ReferenceList <- list()
      TimesCited <- list()
      
      #relations <- list()
      #relations$CitationLinks <- NULL
    }
    else{
      #
      #Not yet implemented. Add link code using "reference" facet of general PLOS search
      #isolate(relations <- GetPubmedCitationLinks(EntrezFetch()[["PMID"]]))
      CitationFrame <- data.frame(c(""), c(""))
      ReferenceList <- list()
      TimesCited <- list()
    }
    return(structure(list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited)))
  })
  
  
  #################################################################Local document fetch functions
  
  #Perform summary search to get results count. Only works on TXT files for now.
  LocalSummary<-reactive({
    
    if(input$summary_search==0 | nchar(isolate(input$search_text)) == 0 | is.na(localfilePath())){
      
      count <- "No Search Terms Entered"
      ids <- list()
      
    }
    else{
      
      
      query <- isolate(strsplit(input$search_text, split = " ", fixed = TRUE))
      
      query <- query[which(length(query) > 0)]
      query <- unlist(query)
      
      #searchsummary <- lapply(query, searchdirtxt, sourcefolder = localfilePath(), includesubdirs = FALSE, searchperc = c(0,1))
      searchsummary <- lapply(query, searchdirtxt, sourcefolder = localfilePath(), includesubdirs = (input$localrecursive == "Yes"), searchperc = c(0,1))
      
      #Start with list of all possible elements
      if(length(searchsummary) > 0){
        filepathresults <- unique(unlist(sapply(searchsummary, "[", "Filepaths")))
        
        #Locate filepaths in each returned result for AND search
        for (i in 1:length(searchsummary)){
          
          filepathresults <- filepathresults[filepathresults %in% searchsummary[[1]]$Filepaths]
          
          
        }
        
      }
      
      ##Temporary code to take all files in path for now. Replace with functioning search later
      filepathresults <- list.files(localfilePath(), full.names = TRUE, recursive = (input$localrecursive == "Yes"))
      #####
      
      count <- length(filepathresults)
      ids <- filepathresults
    }
    
    
    return(structure(list("count" = count, "ids" = ids)))
  })
  
  #Fetch Local articles
  LocalFetch<-reactive({
    
    if(input$detailed_search==0 | (length(isolate(LocalSummary()[["ids"]])) == 0)){
      
      return(localfetch <- list())
    }
    else{
      
      #Get result count for limit setting
      isolate(articlecount <- LocalSummary()[["count"]])
      
      #Get details.
      isolate(localfetch <- loadlocalfilelist(LocalSummary()[["ids"]]))
      
      if(length(localfetch) > 0){
        
        #Get type of article (journal, clinical, etc)
        PMID <- localfetch$Filepaths
        
        title <- localfetch$Filenames
        
        ##No parallel for these right now. Use single placeholder
        articletype <- localfetch$FileExtension
        #articletype <- arxivfetch$primary_category
        #articletype <- sapply(articletype, FUN = tolower)
        
        journal <- rep(list("No Journal"), times = length(PMID))
        #journal <- arxivfetch$journal_ref
        
        keywords <- rep(list("No Keywords"), times = length(PMID))
        #keywords <- strsplit(arxivfetch$categories, split = "|", fixed = TRUE)
        #keywords <- sapply(keywords, FUN = tolower)
        
        authors <- rep(list("No Authors"), times = length(PMID))
        #authors <- strsplit(arxivfetch$authors, split = "|", fixed = TRUE)
        
        #Assign minimum year out of all year fields as year article published to preserve reference-year hierarchy as much as possible
        yearpubframe <- data.frame(localfetch$CreateDate,
                                   localfetch$ModifyDate)
        year <- apply(yearpubframe, 1, FUN = min, na.rm = TRUE)
        
        DOI <- localfetch$Filepaths
        abstract <- localfetch$Abstract
        
        #Construct data to show on hover
        hovertip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<a href=", '"', PMID, '"target = "_blank"><br /><b>File Name: </b>', title, "</a>")
        
        #Construct data to show on click
        clicktip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<a href=", '"', PMID, '"target = "_blank"><br /><b>File Name: </b>', title, "</a>",
                           "<br /><b>Abstract: </b>", sapply(abstract, substr, start = 1, stop = 3000))
        
        #####Get article linkage structure#####
        #Link by title:
        
        #If linking by title
        browser()
        if(input$locallinkby == "Filename"){
          titlelinktab = documentnamelinkage(docids = PMID, doctext = as.character(abstract), searchids = title)
          CitationFrame = titlelinktab$CitationFrame
          TimesCited = titlelinktab$TimesCited
        }else{
          #Use no citation
          CitationFrame <- data.frame(c(""), c(""))
          TimesCited <- list()
        }
        
        ReferenceList <- list()
        
        return(structure(list(details = list("title" = title, "articletype" = articletype, "journal" = journal, "year" = year, "DOI" = DOI, "abstract" = abstract, "keywords" = keywords, "authors" = authors, "PMID" = PMID, "HoverTip" = hovertip, "ClickTip" = clicktip),
                              refstructure = list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited))))
        
      }else{
        return(localfetch <- list())}
      
    }
    
  })
  
  #Perform linkage search to get specific nodes and connections
  LocalNodeEdge<-reactive({
    
    if(input$link_search==0 | is.null(isolate(LocalFetch()[["PMID"]]))){
      
      CitationFrame <- list()
      ReferenceList <- list()
      TimesCited <- list()
      
      #relations <- list()
      #relations$CitationLinks <- NULL
    }
    else{
      #
      #Not yet implemented.
      #isolate(relations <- GetPubmedCitationLinks(EntrezFetch()[["PMID"]]))
      CitationFrame <- data.frame(c(""), c(""))
      ReferenceList <- list()
      TimesCited <- list()
    }
    return(structure(list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited)))
  })
  
  #############################################################################End New Section
  
  
  
  #Dynamic input generators for CSV file input below:
  
  #Dropdown for selecting raw text column
  output$ui_CSVID <- renderUI({
    
    #Import default model values and get length of formulalist
    choicevals <- CSVSummary()[["colnames"]]
    
    
    selectInput(inputId = "CSVID", label = "Select column to use for unique article ID",
                choices = choicevals, selected = NULL)
    
  })
  
  #Dropdown for selecting raw text column
  output$ui_CSVtext <- renderUI({
    
    #Import default model values and get length of formulalist
    choicevals <- CSVSummary()[["colnames"]]
    
    
    selectInput(inputId = "CSVtext", label = "Select column to use for article text",
                choices = choicevals, selected = NULL)
    
  })
  
  #Dropdown for selecting article type column
  output$ui_CSVarticletype <- renderUI({
    
    #Import default model values and get length of formulalist
    choicevals <- CSVSummary()[["colnames"]]
    
    
    selectInput(inputId = "CSVarticletype", label = "Select column to use for article type",
                choices = choicevals, selected = NULL)
    
  })
  
  output$ui_CSVkeyword <- renderUI({
    
    #Import default model values and get length of formulalist
    choicevals <- CSVSummary()[["colnames"]]
    
    
    selectInput(inputId = "CSVkeyword", label = "Select column to use for article keyword",
                choices = choicevals, selected = NULL)
    
  })
  
  #################################################################CSV import functions
  
  #Perform summary search to get results count. Only works on TXT files for now.
  CSVSummary<-reactive({
    
    if(is.null(input$csv_input)){
      
      count <- "No File Selected"
      ids <- list()
      
    }
    else{
      
      ##Temporary code to take all rows in table for now. Replace with functioning search later
      #filepathresults <- list.files(localfilePath(), full.names = TRUE)
      filepathresults <- read.csv(input$csv_input$datapath, header = TRUE, stringsAsFactors = FALSE)
      
      #Add search function for sub-selecting rows that contain desired text
      query <- isolate(strsplit(input$search_text, split = " ", fixed = TRUE))
      
      query <- query[which(length(query) > 0)]
      query <- unlist(query)
      
      searchsummary <- lapply(query, searchdirtxt, sourcefolder = localfilePath(), includesubdirs = FALSE, searchperc = c(0,1))
      
      #Start with list of all possible elements
      if(length(searchsummary) > 0){
        filepathresults <- unique(unlist(sapply(searchsummary, "[", "Filepaths")))
        
        #Locate filepaths in each returned result for AND search
        for (i in 1:length(searchsummary)){
          
          filepathresults <- filepathresults[filepathresults %in% searchsummary[[1]]$Filepaths]
          
          
        }
        
      }
      
      #ids set to all row numbers for now. Replace with search result row numbers later
      ids <- c(1:nrow(filepathresults))
      count <- length(ids)
      colnames <- colnames(filepathresults)
      fulltable <- filepathresults
    }
    
    
    return(structure(list("count" = count, "ids" = ids, "colnames" = colnames, "fulltable" = fulltable)))
  })
  
  #Fetch Local articles
  CSVFetch<-reactive({
    
    if(input$detailed_search==0 | (length(isolate(CSVSummary()[["ids"]])) == 0)){
      
      return(localfetch <- list())
    }
    else{
      
      #Get result count for limit setting
      isolate(articlecount <- CSVSummary()[["count"]])
      
      #Get details and filter down to rows returned by search
      isolate(localfetch <- CSVSummary()[["fulltable"]][CSVSummary()[["ids"]],])
      
      if(nrow(localfetch) > 0){
        
        #browser()
        
        #Get type of article (journal, clinical, etc)
        #PMID <- rownames(localfetch)
        PMID <- localfetch[[input$CSVID]]
        
        #Use first 50 characters of text for title for now. Consider adding separate column later if desired
        #title <- localfetch$Filenames
        title <- sapply(localfetch[input$CSVtext], function(x) strtrim(as.character(x), width = 50))
        #strtrim(localfetch[input$CSVtext], width = rep(50, times = length(PMID)))
        
        ##No parallel for these right now. Use single placeholder
        #articletype <- as.list(localfetch[[input$CSVarticletype]])
        articletype <- localfetch[[input$CSVarticletype]]
        #articletype <- localfetch$FileExtension
        #articletype <- arxivfetch$primary_category
        #articletype <- sapply(articletype, FUN = tolower)
        
        journal <- rep(list("No Journal"), times = length(PMID))
        #journal <- arxivfetch$journal_ref
        
        #Consider adding a free text field to let users define the splitting character
        keywords <- localfetch[[input$CSVkeyword]]
        keywords <- sapply(keywords, FUN = function(x) strsplit(as.character(tolower(x)), split = "/", fixed = TRUE))
        
        authors <- rep(list("No Authors"), times = length(PMID))
        #authors <- strsplit(arxivfetch$authors, split = "|", fixed = TRUE)
        
        #Assign minimum year out of all year fields as year article published to preserve reference-year hierarchy as much as possible
        # yearpubframe <- data.frame(localfetch$CreateDate,
        #                            localfetch$ModifyDate)
        # year <- apply(yearpubframe, 1, FUN = min, na.rm = TRUE)
        
        #Set year as null. Consider adding options for this later
        year <- rep(1, times = length(PMID))
        
        DOI <- rownames(localfetch)
        abstract <- localfetch[[input$CSVtext]]
        
        #Loop through all articles to combine duplicate entries - simply remove for now, later loop through to combine
        
        PMIDuniques <- !duplicated(PMID)
        PMID <- PMID[PMIDuniques]
        title <- title[PMIDuniques]
        articletype <- articletype[PMIDuniques]
        journal <- journal[PMIDuniques]
        keywords <- keywords[PMIDuniques]
        authors <- authors[PMIDuniques]
        DOI <- DOI[PMIDuniques]
        abstract <- abstract[PMIDuniques]
        
        # for(i in unique(PMID)){
        #   
        #   
        #   
        # }
        
        #Construct data to show on hover
        hovertip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<a href=", '"', PMID, '"target = "_blank"><br /><b>File Name: </b>', title, "</a>")
        
        #Construct data to show on click
        clicktip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<a href=", '"', PMID, '"target = "_blank"><br /><b>File Name: </b>', title, "</a>",
                           "<br /><b>Abstract: </b>", sapply(abstract, substr, start = 1, stop = 3000))
        
        #Not yet implemented.
        CitationFrame <- data.frame(c(""), c(""))
        ReferenceList <- list()
        TimesCited <- list()
        
        return(structure(list(details = list("title" = title, "articletype" = articletype, "journal" = journal, "year" = year, "DOI" = DOI, "abstract" = abstract, "keywords" = keywords, "authors" = authors, "PMID" = PMID, "HoverTip" = hovertip, "ClickTip" = clicktip),
                              refstructure = list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited))))
        
      }else{
        return(localfetch <- list())}
      
    }
    
  })
  
  #Perform linkage search to get specific nodes and connections
  CSVNodeEdge<-reactive({
    
    if(input$link_search==0 | is.null(isolate(CSVFetch()[["PMID"]]))){
      
      CitationFrame <- list()
      ReferenceList <- list()
      TimesCited <- list()
      
      #relations <- list()
      #relations$CitationLinks <- NULL
    }
    else{
      #
      #Not yet implemented.
      #isolate(relations <- GetPubmedCitationLinks(EntrezFetch()[["PMID"]]))
      CitationFrame <- data.frame(c(""), c(""))
      ReferenceList <- list()
      TimesCited <- list()
    }
    return(structure(list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited)))
  })
  
  #############################################################################End New Section
  
  
  ####OPS (open patent) FETCH FUNCTIONS###
  
  #Perform summary search to get results count
  OPSSummary<-reactive({
    
    if(input$summary_search==0 | nchar(isolate(input$search_text)) == 0){
      
      count <- "No Search Terms Entered"
      ids <- list()
      
    }
    else{
      #
      
      searchsummary <- isolate(OPSSearch(query = input$search_text, getall = FALSE))
      
      #ids set to dummy variable of 1 if search performed since PLOS search doesn't need ids to fetch details
      count <- searchsummary$ResultCount
      ids <- 1
    }
    
    
    return(structure(list("count" = count, "ids" = ids)))
  })
  
  #Fetch PLOS articles
  OPSFetch<-reactive({
    
    if(input$detailed_search==0 | (length(isolate(OPSSummary()[["count"]])) == 0)){
      
      return(opsfetch <- list())
    }
    else{
      
      #Get article IDs
      idsearch <- isolate(OPSSearch(query = input$search_text, getall = TRUE))
      
      idlist <- idsearch$ResultList
      
      #Get details. See dataset plosfields in package rplos for available fields for "fl"
      opsfetch <- OPSAbsBib(idlist)
      
      if(length(opsfetch) > 0){
        
        #Get patent number and title
        PMID <- unlist(opsfetch$ID)
        
        title <- sapply(opsfetch$Title, FUN = paste, collapse = "|")
        
        #         #For now, type will be country code
        #         articletype <- sapply(PMID, FUN = substr, start = 0, stop = 2)
        #         articletype <- sapply(articletype, FUN = tolower)
        
        #Use patent classification as article type
        articletype <- opsfetch$PatentClassifications
        articletype <- sapply(articletype, FUN = gsub, pattern = "[^[:alnum:]///' ]", replacement = "")
        
        #For now, journal will be inventor
        journal <- opsfetch$Inventors
        
        #Assign minimum year out of all priority date fields as patent year to preserve reference-year hierarchy as much as possible
        year <- sapply(opsfetch$PriorityYears, FUN = as.Date, format = "%Y%m%d")
        year <- sapply(year, FUN = format, format = "%Y")
        year <- as.numeric(sapply(year, FUN = min))
        #For applications that do not yet have a year, apply the current year
        year[is.na(year)] <- as.numeric(format(Sys.Date(), "%Y"))
        
        DOI <- opsfetch$PatentID
        AppID <- opsfetch$ApplicationID
        abstract <- sapply(opsfetch$Abstract, FUN = paste, collapse = "|")
        
        #for now, keyword = assignee
        keywords <- opsfetch$Assignee
        keywords <- sapply(keywords, FUN = tolower)
        keywords <- sapply(keywords, FUN = iconv, to = "ASCII//TRANSLIT")
        keywords <- sapply(keywords, FUN = gsub, pattern = "[^[:alnum:]///' ]", replacement = "")
        
        
        authors <- opsfetch$Inventors
        
        #Get citation info
        CitationFrame <- opsfetch$CitationFrame
        ReferenceList <- opsfetch$ReferenceList
        TimesCited <- opsfetch$TimesCited
        
        patlinks <- lapply(DOI, function(x) paste0("<a href=", '"http://www.google.com/search?q=', x, '"target = "_blank"><br /><b>Patent ID: </b>', x, "</a>", collapse = "<b>, </b>"))
        
        #Construct data to show on hover
        hovertip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<br /><b>Patent Family: </b>", PMID,
                           patlinks)
        
        #Construct data to show on click
        clicktip <- paste0("<b>Title: </b>", title,
                           "<br /><b>Year: </b>", year,
                           "<br /><b>Patent Family: </b>", PMID,
                           patlinks,
                           "<br /><b>Assignee: </b>", keywords,
                           "<br /><b>Abstract: </b>", abstract)
        
        return(structure(list(details = list("title" = title, "articletype" = articletype, "journal" = journal, "year" = year, "DOI" = DOI, "abstract" = abstract, "keywords" = keywords, "authors" = authors, "PMID" = PMID,  "HoverTip" = hovertip, "ClickTip" = clicktip),
                              refstructure = list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited))))
        
      }else{
        return(opsfetch <- list())}
      
    }
    
  })
  
  #Perform linkage search to get specific nodes and connections
  OPSNodeEdge<-reactive({
    
    
    
    if(input$link_search==0 | is.null(isolate(OPSFetch()[["PMID"]]))){
      
      CitationFrame <- list()
      ReferenceList <- list()
      TimesCited <- list()
      
      #relations <- list()
      #relations$CitationLinks <- NULL
    }
    else{
      
      
      
      #CitationFrame <- data.frame(c(""), c(""))
      #ReferenceList <- list()
      #TimesCited <- list()
      
      isolate(CitationFrame <- OPSFetch()[["CitationFrame"]])
      isolate(ReferenceList <- OPSFetch()[["ReferenceList"]])
      isolate(TimesCited <- OPSFetch()[["TimesCited"]])
      
      if(is.null(CitationFrame) == TRUE){
        
        CitationFrame <- data.frame(c(""), c(""))
        
      }
    }
    return(structure(list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited)))
  })
  
  #Function to add data from related excel or R files to information extracted from databases
  DBAdd <- reactive({
    
    #Method to use here:
    # 1. Make this an observer that uses a reactive variable initialized outside of observer
    # 2. Each time the "add to data" button is pressed, the program checks for the loaded augment csv
    # 3. The three inputs specifying columns are read and used to determine: 1. the key column to use for matching in the augmentation data. 2: the new data column in the augmentation data to add. 3. the key column in the existing dataset to match with key 1. 4. The name to use for the new data. Will overwrite data in existing data if name is the same
    # 4. The data structure according to 3 above is added as an item in the reactive variable list
    # 5. When DBSwitch runs, the last thing it should do is iterate through every item in this reactive variable to perform appropriate replacements per the augmented data
    
    return(structure(list("Summary" = Summary, "Fetch" = Fetch, "NodeEdge" = NodeEdge)))
    
    
  })
  
  #Function to swap fetch functions defined above based on database radio button selected
  DBSwitch <- reactive({
    
    #Initialize return structutre with null elements for crash prevention when no data has yet been loaded or when switching databases
    Summary = NULL
    Fetch = NULL
    NodeEdge = NULL
    
    
    #disable database polling search buttons while loading
    disable("summary_search")
    disable("detailed_search")
    disable("link_search")
    
    
    # When a new model is uploaded, check to see if it should be rendered
    if(isolate(input$database == "Load Model")){
      
      #browser()
      #Load existing model
      loaddat = LoadModel()
      
      Summary = loaddat$DBSwitchLoad$Summary
      Fetch = loaddat$DBSwitchLoad$Fetch
      NodeEdge = loaddat$DBSwitchLoad$NodeEdge
      
    }
    
    
    # Only update the online databases when one of the search buttons has been pressed:
    
    if (input$summary_search > 0 | input$detailed_search > 0 | input$link_search > 0){
      
      isolate({
        
        if(input$database == "pubmed"){
          
          Summary <- EntrezSummary()
          Fetch <- EntrezFetch()[["details"]]
          NodeEdge <- EntrezFetch()[["refstructure"]]
          
        }
        
        if(input$database == "PLOS"){
          
          Summary <- PLOSSummary()
          Fetch <- PLOSFetch()[["details"]]
          NodeEdge <- PLOSFetch()[["refstructure"]]
          
        }
        
        if(input$database == "aRxiv"){
          
          Summary <- ARXIVSummary()
          Fetch <- ARXIVFetch()[["details"]]
          NodeEdge <- ARXIVFetch()[["refstructure"]]
          
        }
        
        if(input$database == "OPS patents"){
          
          Summary <- OPSSummary()
          Fetch <- OPSFetch()[["details"]]
          NodeEdge <- OPSFetch()[["refstructure"]]
          
        }
        
        if(input$database == "Local Files"){
          
          Summary <- LocalSummary()
          Fetch <- LocalFetch()[["details"]]
          NodeEdge <- LocalFetch()[["refstructure"]]
          
        }
        
        if(input$database == "CSV File"){
          
          Summary <- CSVSummary()
          Fetch <- CSVFetch()[["details"]]
          NodeEdge <- CSVFetch()[["refstructure"]]
          
        }
        
        
      })
    }
    
    #Loop through reactive variable for augmentation data here
    if(length(reactaugment$newdat) > 0){
      
      for(i in 1:length(reactaugment$newdat)){
        # THIS MAY BE AN ISSUE USING NAME MATCHING SINCE NEWLY ADDED VARIABLES MIGHT NOT WORK WELL
        Fetch[[reactaugment$newdat[[i]]$AugmentDatName]] = reactaugment$newdat[[i]]$AugmentNewDat[Fetch[[reactaugment$newdat[[i]]$DBKey]]]
        
      }
      
    }

    #enable database polling search buttons after loading is complete
    enable("summary_search")
    enable("detailed_search")
    enable("link_search")
    
        
    return(structure(list("Summary" = Summary, "Fetch" = Fetch, "NodeEdge" = NodeEdge)))
    
  })
  
  #Get important information from detail search ##Current filtering problem### Investigate
  FilterDetail<-reactive({
    
    fetch <- DBSwitch()[["Fetch"]]
    
    if(length(fetch) > 0){
      
      #Get raw article ID list
      PMID <- fetch$PMID
      
      #Construct filtered set
      filtered <- which(PMID %in% filterinclude$ids)
      
      #apply filters to all element lists in DBSwitch()[["Fetch"]]
      
      output <- lapply(fetch, FUN = "[", filtered)
      
      #       PMID <- fetch$PMID[filtered]
      #       title <- fetch$title[filtered]
      #       articletype <- fetch$articletype[filtered]
      #       journal <- fetch$journal[filtered]
      #       year <- fetch$year[filtered]
      #       DOI <- fetch$DOI[filtered]
      #       abstract <- fetch$abstract[filtered]
      #       keywords <- fetch$keywords[filtered]
      #       authors <- fetch$authors[filtered]
      #       
      #       return(structure(list("title" = title, "articletype" = articletype, "journal" = journal, "year" = year, "DOI" = DOI, "abstract" = abstract, "keywords" = keywords, "authors" = authors, "PMID" = PMID)))
      
      return(output)
      
    }else{
      return(fetch <- list())}
    
  })
  
  #Create keyword and article type lists
  CategoryCount <- reactive({
    
    keywordfreq <- list()
    articletypefreq <- list()
    
    if(is.null(FilterDetail()[["articletype"]]) == FALSE){
      
      articletypefreq <- list()
      
      articletypefreq <- FilterDetail()[["articletype"]]
      #articletypefreq <- summary.factor(tolower(unlist(sapply(articletypefreq, FUN = as.character))))
      articletypefreq <- summary.factor(unlist(sapply(articletypefreq, FUN = as.character)))
      
      articletypefreq <- data.frame(names(articletypefreq), articletypefreq, stringsAsFactors = FALSE)
      colnames(articletypefreq) <- c("name", "count")
      
      #Sort highest to lowest
      articletypefreq <- articletypefreq[order(articletypefreq["count"], decreasing = TRUE),]
      
      #Add default color
      articletypefreq$color <- "#888888"
      
      
      
    }
    
    if(is.null(FilterDetail()[["keywords"]]) == FALSE){
      
      keywordfreq <- list()
      
      keywordfreq <- FilterDetail()[["keywords"]]
      #keywordfreq <- sapply(sapply(keywordfreq, FUN = as.character), FUN = tolower)
      keywordfreq <- sapply(keywordfreq, FUN = as.character)
      keywordfreq <- summary.factor(unlist(sapply(keywordfreq, FUN = unique)))
      
      keywordfreq <- data.frame(names(keywordfreq), keywordfreq, stringsAsFactors = FALSE)
      colnames(keywordfreq) <- c("name", "count")
      
      #Sort highest to lowest
      keywordfreq <- keywordfreq[order(keywordfreq["count"], decreasing = TRUE),]
      
      #Add default color
      keywordfreq$color <- "#888888"
      
      
      
    }
    
    return(structure(list("articletypefreq" = articletypefreq, "keywordfreq" = keywordfreq)))
    
  })
  
  
  #Create proper network layout once the article parameters (including year) have been identified
  NetworkLayout <- reactive({
    
    #Default to cose layout if no linkages are found
    layout <- "cose"
    nodeData <- list()
    nodeData$x <- 1
    nodeData$y <- 1
    
    #Get node data
    NodeEdge <- DBSwitch()[["NodeEdge"]]
    edgeList <- NodeEdge[["CitationFrame"]]
    startnodes <- FilterDetail()[["PMID"]]
    
    if(ncol(edgeList) == 2){
      
      #Filter edgeList by node IDs
      edgeList <- edgeList[intersect(which(edgeList[,1] %in% startnodes), which(edgeList[,2] %in% startnodes)),]
      colnames(edgeList) <- c("source", "target")
      
    }
    
    nodes <- startnodes
    
    id <- nodes
    name <- nodes
    nodeData <- data.frame(id, name, stringsAsFactors=FALSE)
    nodeData$x <- 1
    nodeData$y <- 1
    
    #drl layout (x and y just set based on minimizing crossings)
    if(input$graphlayout == "drl"){
      #Build network and remove redundant connections (self and duplicate connections)
      net <- graph.data.frame(edgeList, nodeData, directed = TRUE)
      net = simplify(net)
      
      #Apply drl method to generate layout and extract x and y positions
      sublayout <-  layout.fruchterman.reingold(net)
      #sublayout <- layout.drl(net)
      
      #Translate sublayout results for x into nodeData network
      nodeData$x <- sublayout[,1]
      nodeData$y <- sublayout[,2]
      
      layout <- "preset"
      
    }
    
    #If there are connections, create a sugiyama network of connected nodes and fill orphans in around it
    if(input$graphlayout == "preset"){
      if(is.null(FilterDetail()[["year"]]) == FALSE){
        
        # NodeEdge <- DBSwitch()[["NodeEdge"]]
        # 
        # edgeList <- NodeEdge[["CitationFrame"]]
        # 
        # startnodes <- FilterDetail()[["PMID"]]
        # 
        # if(ncol(edgeList) == 2){
        # 
        # #Filter edgeList by node IDs
        # edgeList <- edgeList[intersect(which(edgeList[,1] %in% startnodes), which(edgeList[,2] %in% startnodes)),]
        # 
        # colnames(edgeList) <- c("source", "target")
        # 
        # }
        
        layer <- FilterDetail()[["year"]]
        
        #nodes <- startnodes
        
        #id <- nodes
        #name <- nodes
        #nodeData <- data.frame(id, name, stringsAsFactors=FALSE)
        nodeData$x <- as.integer(NA)
        
        #Translate year into layer number and store as y variable
        nodeData$y <- as.integer(abs(layer - max(layer) - 1))
        
        #     #Set default center position
        #     centerspan <- c(0,1)
        
        #Set default start position for unconnected nodes
        startpos <- 0
        
        #If there are edges, perform minimum crossing layout for edges
        if(nrow(edgeList) > 0){
          
          #Find the row index of the connected nodes
          connectindex <- which(nodeData[["id"]] %in% c(edgeList[,1], edgeList[,2]))
          
          #Build network and remove redundant connections (self and duplicate connections)
          net <- graph.data.frame(edgeList, nodeData[connectindex,], directed = TRUE)
          net = simplify(net)
          sublayer <- layer[connectindex]
          #Apply sugiyama method to nodes with connections only
          sublayout <- layout.sugiyama(net, sublayer)
          
          #Translate sublayout results for x into nodeData network
          nodeData$x[connectindex] <- sublayout$layout[,1]
          
          #       #Clear center space of graph for connected nodes
          #       centerspan <- c(min(nodeData$x, na.rm = TRUE)- 1, max(nodeData$x, na.rm = TRUE) + 1)
          
          #Find rightmost position of set and define as starting position for unconnected nodes
          startpos <- max(nodeData$x, na.rm = TRUE) + 1
          
        }
        
        #Randomly space remaining nodes to the right of the connected nodes for each layer in increments of 1
        for (i in min(nodeData$y):max(nodeData$y)){
          
          #Only execute code for layers that have nodes in them where the position of at least one node has not yet been set
          if(sum(nodeData$y == i) > 0 & sum(is.na(nodeData$x[nodeData$y == i])) > 0){
            
            xvalloop <- nodeData$x[nodeData$y == i]
            
            #position remaining nodes to the right of the network
            for (j in 1:sum(is.na(xvalloop))){
              
              nodeData$x[which(is.na(nodeData$x) & nodeData$y == i)][1] <- j - 1 + startpos
              
            }
            
            #         #position remaining nodes around center for the current layer
            #         switchflip <- 1
            #         for (j in 1:sum(is.na(xvalloop))){
            #           
            #           
            #           nodeData$x[which(is.na(nodeData$x) & nodeData$y == i)][1] <- centerspanloop[switchflip]
            #           
            #           centerspanloop[switchflip] <- centerspanloop[switchflip] + (-1)^switchflip
            #           switchflip <- switchflip - (-1)^switchflip
            #           
            #         }
            
            
          }
          
        }
        #Change to preset layout if this section of code was executed
        layout <- "preset"
        
      }
    }
    
    return(structure(list("layout" = layout, "nodeData" = nodeData)))
    
  })
  
  #Create node edge matrix from journal search results
  CreateNetwork <- reactive({
    
    #Initialize outputs
    edgeData <- list()
    nodeData <- list()
    articletypefreq <- list()
    keywordfreq <- list()
    
    NodeEdge <- DBSwitch()[["NodeEdge"]]
    
    edgeList <- NodeEdge[["CitationFrame"]]
    
    #Initialize detail processing if the details of the articles have been captured
    startnodes <- FilterDetail()[["PMID"]]
    details <- FilterDetail()
    citationcount <- NodeEdge[["TimesCited"]]
    
    
    if(ncol(edgeList) == 2){
      #Filter edgeList by node subset
      
      
      edgeList <- edgeList[intersect(which(edgeList[,1] %in% startnodes), which(edgeList[,2] %in% startnodes)),]
      
      colnames(edgeList) <- c("source", "target")
    }
    
    #If there are no edges, create blank row so that the graph can still be rendered
    if(nrow(edgeList) == 0){
      
      edgeList[1,] <- c("","")
      
    }
    
    #Use this line later when node list has option to be expanded to include referenced nodes
    #nodes <- unique(c(startnodes, edgeList$source, edgeList$target))
    
    nodes <- startnodes
    
    id <- nodes
    name <- nodes
    nodeData <- data.frame(id, name, stringsAsFactors=FALSE)
    
    
    #Create tooltip info for network graph
    if(length(details$title) == nrow(nodeData)){
      
      nodeData$href <- details[["HoverTip"]]
      nodeData$hrefclick <- details[["ClickTip"]]
      
      #       nodeData$href <- paste0("<b>Title: </b>", details[["title"]],
      #                               "<br /><b>Year: </b>", details[["year"]],
      #                               "<a href=", '"http://www.ncbi.nlm.nih.gov/pubmed/', details[["PMID"]], '"target = "_blank"><br /><b>Pubmed ID: </b>', details[["PMID"]], "</a>")
      #       nodeData$hrefclick <- paste0("<b>Title: </b>", details[["title"]],
      #                                    "<br /><b>Year: </b>", details[["year"]],
      #                                    "<a href=", '"http://www.ncbi.nlm.nih.gov/pubmed/', details[["PMID"]], '"target = "_blank"><br /><b>Pubmed ID: </b>', details[["PMID"]], "</a>",
      #                                    "<br /><b>Abstract: </b>", details[["abstract"]])
      
    }
    
    #Size nodes based on number of citations - perhaps change this to logrithmic scaling? add input option for this
    if(length(citationcount) > 0 && (max(citationcount) != min(citationcount))){
      
      
      
      maxpix <- 100
      minpix <- 20
      
      nodeData$height <- 0
      for(i in 1:length(citationcount)){
        
        if(names(citationcount[i]) %in% nodeData[["id"]]){
          
          nodeData[which(nodeData[["id"]] %in% names(citationcount[i])),"height"] <- citationcount[i]
          
        }
      }
      
      if(max(nodeData$height) == min(nodeData$height)){
        #Set to minimum pixel size if no citations found
        nodeData$height <- minpix
        
      }else{
        
        #Linear scaling of node size based on number of citations
        slope <- (maxpix - minpix)/(max(nodeData$height) - min(nodeData$height))
        nodeData$height <- slope*(nodeData$height - min(nodeData$height)) + minpix
        
        #Apply log scaling to node size based on number of citations
        nodeData$height <- (maxpix - minpix)*(log(nodeData$height) - log(min(nodeData$height)))/(log(max(nodeData$height)) - log(min(nodeData$height))) + minpix
        
      }
      
      nodeData$width <- nodeData$height
      
    }else{
      #If there are no citations at all, set node width and height to default value of 20
      nodeData$width <- 20
      nodeData$height <- 20
      
    }
    
    #Article type colorizer
    if(length(details$articletype) == nrow(nodeData)){
      
      #Base node color to be set per below
      nodeData$color <- "#888888"
      
      articletypefreq <- CategoryCount()[["articletypefreq"]]
      
      #Execute different color application if user has selected an input
      if(length(groupclicks$articletype) > 0){
        
        #Color based on selected inputs and drop last two characters as those are for transparency
        randcolor <- substr(rainbow(length(groupclicks$articletype)), 1,7)
        
        for (i in 1:length(groupclicks$articletype)){
          
          matchvect <- c()
          #Construct matching vector
          matchvect <- lapply(details$articletype, FUN = "%in%", groupclicks$articletype[i])
          matchvect <- lapply(matchvect, FUN = "!")
          matchvect <- lapply(matchvect, FUN = prod)
          nodeData$color[which(matchvect == 0)] <- randcolor[i]
          
          
          #Apply color to article frquency list
          articletypefreq$color[which(articletypefreq$name == groupclicks$articletype[i])] <- randcolor[i]
          
        }
      }
      
    }
    
    #Article keyword colorizer
    if(length(details$keywords) == nrow(nodeData)){
      
      #Base node overlay color to be set to match graph background color  
      nodeData$bordercolor <- "#FF00FF"
      
      #Default overlay pixel width to be zero on unselected nodes
      nodeData$borderwidth = "0"
      
      #Default overlay opacity to be zero (hidden) on unselected nodes
      nodeData$borderopacity <- "0"
      
      
      keywordfreq <- CategoryCount()[["keywordfreq"]]
      
      #Execute different color application if user has selected an input
      if(length(groupclicks$keyword) > 0){
        
        #Color based on selected inputs and drop last two characters as those are for transparency
        
        
        randcolor <- substr(rainbow(length(groupclicks$keyword)), 1,7)
        
        
        for (i in 1:length(groupclicks$keyword)){
          
          matchvect <- c()
          #Construct matching vector
          matchvect <- lapply(details$keywords, FUN = "%in%", groupclicks$keyword[i])
          matchvect <- lapply(matchvect, FUN = "!")
          matchvect <- lapply(matchvect, FUN = prod)
          nodeData$bordercolor[which(matchvect == 0)] <- randcolor[i]
          
          #Set overlay opacity to 10% for selected node groups
          nodeData$borderopacity[which(matchvect == 0)] <- 0.25
          
          #Set overlay padding to 10 pixels for selected node groups
          nodeData$borderwidth[which(matchvect == 0)] <- nodeData$width[which(matchvect == 0)]*1
          
          #Apply color to article frquency list
          keywordfreq$color[which(keywordfreq$name == groupclicks$keyword[i])] <- randcolor[i]
          
        }
      }
      
    }
    #Set edgeList to edgeData prior to calculating opacity. Opacity values will be added back to edgeData after calculations below
    edgeData <- edgeList
    
    #Article topic opacity colorizer
    if(length(topicclicks$selected) > 0 ){
      
      topicprob <- CreateTopicModel()[["TopicProb"]]
      
      #Base node and edge opacity to be set to zero  
      nodeData$opacity <- 0
      edgeList$targetopacity <- 0
      edgeList$sourceopacity <- 0
      #Set opacity for each node as the product of the selected topics for that document
      #Set edge opacity equal 
      
      for(i in 1:nrow(topicprob)){
        
        #Calculate probability 
        nodeData$opacity[which(nodeData[["id"]] %in% rownames(topicprob)[[i]])] <- prod(topicprob[i,c(paste("Topic", topicclicks$selected))])
        edgeList$sourceopacity[which(edgeList[["source"]] %in% rownames(topicprob)[[i]])] <- prod(topicprob[i,c(paste("Topic", topicclicks$selected))])
        edgeList$targetopacity[which(edgeList[["target"]] %in% rownames(topicprob)[[i]])] <- prod(topicprob[i,c(paste("Topic", topicclicks$selected))])
        
        
      }
      
      #Store maximum edge opacity as edgeData$opacity
      edgeData$opacity <- apply(edgeList[c("sourceopacity", "targetopacity")], MARGIN = 1, FUN = max)
      
      #Normalize opacity using maximum caluclated opacity value
      nodeData$opacity <- nodeData$opacity/max(nodeData$opacity)
      edgeData$opacity <- edgeData$opacity/max(nodeData$opacity)
      
      #Alternative opacity by comparison to absolute possible best
      #nodeData$opacity <- nodeData$opacity/((1/length(topicclicks$selected))^(length(topicclicks$selected)))
      #edgeData$opacity <- edgeData$opacity/((1/length(topicclicks$selected))^(length(topicclicks$selected)))
      
    }
    
    #     addLinks <- TRUE
    
    #     if(addLinks) {
    #       href <- paste0("https://www.google.com/search?q=", nodes)
    #       nodeData <- data.frame(id, name, href, stringsAsFactors=FALSE)
    #     } else {
    #       nodeData <- data.frame(id, name, stringsAsFactors=FALSE)
    #     }
    
    #     nodeData$color <- rep("#888888", nrow(nodeData))
    #     nodeData$color[which(grepl("[a-z]", nodes))] <- "#FF0000"
    #     
    #     nodeData$shape <- rep("ellipse", nrow(nodeData))
    #     nodeData$shape[which(grepl("[a-z]", nodes))] <- "octagon"
    
    return(structure(list("edgeData" = edgeData, "nodeData" = nodeData, "articletypefreq" = articletypefreq, "keywordfreq" = keywordfreq)))
  })
  
  #   #Perform text cluster grouping of articles
  CreateTopicModel <- reactive({
    
    ##Second option using stm (can expand to model topics by year or other meta data later, also much faster. Must set working directory to C directory for "Spectral" method to work)
    library(qdap)
    library(stm)
    library(huge)
    library(LDAvis)
    library(tm)
    library(quanteda)
    
    #If the generate model button is pressed, create a new topic model
    if(input$gentopicmodelbutton > 0){
    #if(is.null(LoadModel()[["TopicModelLoad"]]) | (input$database != "Load Model")){
    
      #Isolate internal code to prevent updates where the button has not been pressed
      isolate({
      
        #Get document details
        details <- FilterDetail()
        
    #Process abstract text to stm text object. Includes control objects, including number removal, punctuation removal, lowercase, and stemming
    #metadata should have unique document ID that is used in other charts as first column
    meta <- data.frame(details$PMID, details$year)
    colnames(meta) <- c("PMID", "year")
    
    
    #Aggressively strip the article text, removing all non-letter items due to bugs in stm and tm (based on likely bug in removePunctuation function in tm package) not properly removing punctuation for trademark symbol anymore
    details$abstractonlyletters <- lapply(details$abstract, function(x) gsub("[^a-zA-Z ]"," ", x))
    
    #disable database polling search buttons using shinyjs before intense computation steps below
    disable("summary_search")
    disable("detailed_search")
    disable("link_search")
    
    
    #####################################################
#     temp <- textProcessor(documents = details$abstract, metadata = meta,  stem = FALSE)
#     
#     #Process abstract details into format for stm document preparation
#     meta<-temp$meta
#     vocab<-temp$vocab
#     docs<-temp$documents
#     
#     #Prep document for stm modeling. Removes infrequent and too frequent terms and sparse documents
#     out <- prepDocuments(docs, vocab, meta)
#     docs<-out$documents
#     vocab<-out$vocab
#     meta <-out$meta
#     
#     #Create vector of document numbers that were not removed during text processing:
#     usedocs <- as.integer(names(docs))
#     
#     #Alternate loop to implement variable length phrases using ToPMine
#     library(tm)
#     
#     #Strip carriage return characters and replace with end of sentence. Write file to directory containing ToPMine
#     writeLines(gsub("\n", ". ", details$abstract[usedocs]), "C:/PDFtoText/ToPMine/topicalPhrases/rawFiles/mod.txt")
#     
#     currentwd <- getwd()
#     
#     setwd("C:/PDFtoText/ToPMine/topicalPhrases/")
#     system("C:/PDFtoText/ToPMine/topicalPhrases/win_run.bat", wait = TRUE)
#     
#     setwd(currentwd)
#     
#     multiword <- ToPMinetoDTM("C:/PDFtoText/ToPMine/topicalPhrases/TopicalPhrases/input_dataset_output/input_wordTraining.txt")
#     
#     multiword <- readCorpus(multiword, type = "dtm")
#     docs <- multiword$documents
#     vocab <- multiword$vocab
#     
#     #Set working directory to C: temp to ensure write permission is allowed for init.type "Spectral" in stm function below
#     setwd("C:/temp")
    
#Look for TopMine in installation directory for JournalVis and make sure multiword use is selected
    
     
if((file.exists(paste0(getwd(),"/ToPMine/topicalPhrases/win_run.bat")) == TRUE) & (input$usemultiword == "Yes")){
  
  currentwd <- getwd()
  
    #Create subfolder to write raw data to for ToPMine if it does not exist
    if (file.exists(paste0(currentwd,"/ToPMine/topicalPhrases/rawFiles/"))){
    } else {
      dir.create(file.path(paste0(currentwd,"/ToPMine/topicalPhrases/rawFiles/")))
      
    }
    
    #Write raw text for ToPMine phrase identification
    writeLines(gsub("\n", ". ", details$abstract), paste0(currentwd,"/ToPMine/topicalPhrases/rawFiles/mod.txt"))
    ##Had to switch to abstract with all non-letters stripped due to how tm package removePunctuation was broken by update
    #writeLines(gsub("\n", ". ", details$abstractonlyletters), paste0(currentwd,"/ToPMine/topicalPhrases/rawFiles/mod.txt"))
    
    #Run ToPMine phrase identification
    setwd(paste0(currentwd,"/ToPMine/topicalPhrases/"))
    system(paste0(currentwd,"/ToPMine/topicalPhrases/win_run.bat"), wait = TRUE)
    
    
    ##Read ToPMine files back into R - OLD method to get entire corpus
    #multiword <- ToPMinetoDTM(paste0(currentwd,"/ToPMine/topicalPhrases/TopicalPhrases/input_dataset_output/input_wordTraining.txt"))
    #multiword <- readCorpus(multiword, type = "dtm")
    ##Identify all multi-word phrases identified by TopMine and sort by number of characters (large to small) and create dataframe with replacement token
    #phrases <- multiword$vocab[grep(" ", multiword$vocab, fixed = TRUE)]
    
    #Extract only multiword phrases produced by topmine
    phrases = ToPMineExtractMultiword(paste0(currentwd,"/ToPMine/topicalPhrases/TopicalPhrases/input_dataset_output/input_wordTraining.txt"))
    
    #Delete temporary directories used by ToPMine
    unlink(file.path(paste0(currentwd,"/ToPMine/topicalPhrases/rawFiles/")), recursive = TRUE)
    unlink(file.path(paste0(currentwd,"/ToPMine/topicalPhrases/TopicalPhrases/input_dataset/")), recursive = TRUE)
    unlink(file.path(paste0(currentwd,"/ToPMine/topicalPhrases/TopicalPhrases/input_dataset_output/")), recursive = TRUE)
    
    #Set directory back to JournalVis working directory
    setwd(currentwd)
    
    #Sort phrases from largest to smallest
    phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
    
    # # ##Moved to TM specific use below
    # #Define null vector to start in case no phrases identified
    # placeholder <- c()
    # 
    # if(length(phrases) > 0){
    # 
    # placeholder <-  apply(expand.grid(lapply(1:max(2,ceiling(log(length(phrases), base = 26))), function(i) letters))[1:length(phrases),], MARGIN = 1, paste0, collapse = "")
    # placeholder <- paste0("phrasefindqqxzqcvx", placeholder,"phrasefind")
    # 
    # }
    # 
    # #Replace multiword phrase instances. Add leading and trailing space to make sure they don't get combined with other words
    # #multdef <- mgsub(as.character(phrases), as.character(placeholder), details$abstract, ignore.case = TRUE, leadspace = TRUE, trailspace = TRUE)
    # multdef <- mgsub(as.character(phrases), as.character(placeholder), details$abstractonlyletters, leadspace = TRUE, trailspace = TRUE)
    # 
    # ##Moved to TM specific use below
    # #Get integer position of remaining non-zero length documents
    # usedocs <- which(nchar(multdef) > 0)
    # 
    # #Process resultant text in preparation for STM/LDA modeling
    # temp <- textProcessor(documents = multdef[usedocs], metadata = meta[usedocs,],  stem = FALSE)
    

}else{
  
  ##Moved to TM specific use section below
  # #If TopMine is not installed, process corpus as single word tokens:
  # 
  # #Get integer position of remaining non-zero length documents
  # usedocs <- which(sapply(details$abstractonlyletters, nchar) > 0)
  # 
  # #Had to switch to abstract with all non-letters stripped due to how tm package removePunctuation was broken by update
  # #temp <- textProcessor(documents = details$abstract, metadata = meta,  stem = FALSE)
  # #Process resultant text in preparation for STM/LDA modeling
  # temp <- textProcessor(documents = details$abstractonlyletters[usedocs], metadata = meta[usedocs,],  stem = FALSE)
}
    
    #Determine if older tm package or quanteda package should be used for preprocessing.
    #Note: tm package use soon to be deprecated since it is inferior
    if(usequanteda){
      #Use quanteda package processing (doesn't support multiword tokens yet)
      #MIGHT NOT BE NECESSARY USING QUANTEDA. Get integer position of remaining non-zero length documents
      #usedocs <- which(sapply(details$abstractonlyletters, nchar) > 0)
      
      #########################################################
      #Quanteda prototype section
      
      #Create corpus
      quantcorp = corpus(as.character(details$abstract), docnames = c(1:length(details$abstract)), docvars = meta)
      
      #If multi-word phrases exist, tokenize using them
      if(exists("phrases")){
        
        #Tokenize without removing anything to allow for multiword phrase finding
        quanttok = tokens(x = quantcorp)
        
        #Convert multi-word tokens to underscore joined tokens
        #NOTE: Investigate ways to up since this is fairly slow (takes approx 30 minutes on SOP dataset, but still faster than older way shown below in tm specific implementation)
        quanttok = tokens_compound(quanttok, pattern = phrase(as.character(phrases)))
        
        #Remove extraneous items from tokens
        quanttok = tokens(x = quanttok,
                          remove_numbers = input$removenumbers,
                          remove_punct = TRUE,
                          remove_symbols = TRUE,
                          remove_separators = TRUE)
        
        
      }else{
      
      #Tokenize while removing many items without multiword phrases
      quanttok = tokens(x = quantcorp,
                        remove_numbers = input$removenumbers,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_separators = TRUE)
      }
      
      #Remove stopwords
      quanttok = tokens_remove(quanttok, stopwords())
      
      #Remove tokens that are less than 3 characters
      quanttok = tokens_remove(quanttok, min_nchar = 3, max_nchar = 100000)
      
      ##Define token equivalence (define synonyms). need to do more research on how to use
      #tokens_lookup(...)
      
      #Create dfm (much quicker with tokens vs. corpus)
      quantdfm = dfm(quanttok, tolower = TRUE, stem = input$stemwords)
      
      #Trim out too frequent or too infrequent words based on proportion of documents they appear in
      quantdfm = dfm_trim(quantdfm,
                          min_docfreq = input$stmtermminpercent/100,
                          max_docfreq = input$stmtermmaxpercent/100,
                          docfreq_type = "prop")
      
      #Convert to stm format
      out = convert(quantdfm, to = "stm")
      
      #Extract docs, vocab, and meta for stm creation
      docs<-out$documents
      vocab<-out$vocab
      meta <-out$meta
      #Revert multiword underscore splits in vocab back to spaces
      vocab = gsub(pattern = "_", replacement = " ", x = vocab, fixed = TRUE)
      
      #Reduce usedocs vector to abstract indices of document numbers that were not removed during text processing or that were not removed due to being zero length documents:
      #Document numbers not removed by textProcessor
      usedocs <- as.integer(names(docs))
      
      #Build stm model
      #abstractstm = stm(documents = stmdfm$documents, vocab = stmdfm$vocab, K = ceiling(sqrt(length(stmdfm$documents))))
      
      
      #Find multi-word tokens (quite slow, topminer better)
      #textstat_collocations(quanttok)
      
    }else{
      #Use tm package processing
      
      #Initialize tm textprocessor using multiword tokens if they exist
      if(exists("phrases")){
        
        #Define null vector to start in case no phrases identified
        placeholder <- c()
        
        if(length(phrases) > 0){
          
          placeholder <-  apply(expand.grid(lapply(1:max(2,ceiling(log(length(phrases), base = 26))), function(i) letters))[1:length(phrases),], MARGIN = 1, paste0, collapse = "")
          placeholder <- paste0("phrasefindqqxzqcvx", placeholder,"phrasefind")
          
        }
        
        #Replace multiword phrase instances. Add leading and trailing space to make sure they don't get combined with other words
        #multdef <- mgsub(as.character(phrases), as.character(placeholder), details$abstract, ignore.case = TRUE, leadspace = TRUE, trailspace = TRUE)
        multdef <- mgsub(as.character(phrases), as.character(placeholder), details$abstractonlyletters, leadspace = TRUE, trailspace = TRUE)
        
        #Get integer position of remaining non-zero length documents
        usedocs <- which(nchar(multdef) > 0)
        
        #Process resultant text in preparation for STM/LDA modeling
        temp <- textProcessor(documents = multdef[usedocs], metadata = meta[usedocs,],  stem = FALSE)
        
        
        
      }else{
        #If multiword tokens do not exist, use standard text processor
        #Get integer position of remaining non-zero length documents
        usedocs <- which(sapply(details$abstractonlyletters, nchar) > 0)
        
        #Had to switch to abstract with all non-letters stripped due to how tm package removePunctuation was broken by update
        #temp <- textProcessor(documents = details$abstract, metadata = meta,  stem = FALSE)
        #Process resultant text in preparation for STM/LDA modeling
        temp <- textProcessor(documents = details$abstractonlyletters[usedocs], metadata = meta[usedocs,],  stem = FALSE)
        
      }
      
      #Proceed with remaining document prep for stm modeling
      #Process abstract details into format for stm document preparation
      meta<-temp$meta
      vocab<-temp$vocab
      docs<-temp$documents
      
      #Prep document for stm modeling. Removes infrequent and too frequent terms and sparse documents
      out <- prepDocuments(docs, vocab, meta,
                           lower.thresh = ceiling(input$stmtermminpercent*length(docs)/100),
                           upper.thresh = ceiling(input$stmtermmaxpercent*length(docs)/100))
      docs<-out$documents
      vocab<-out$vocab
      meta <-out$meta
      
      #Reduce usedocs vector to abstract indices of document numbers that were not removed during text processing or that were not removed due to being zero length documents:
      #Document numbers not removed by textProcessor
      usedocs <- usedocs[as.integer(names(docs))]
      
      
      if(!usequanteda){
      #tm package multi-word reversion. Need version for quanteda
      #Revert multi-word placeholders back to original multi-word phrases if TopMine process was successful
      if(exists("multdef")){
        
        vocab <- mgsub(as.character(placeholder), as.character(phrases), vocab)
        
      }
      }
      
      
    }
    
    #Calculate topics. Use "Spectral" initialization to allow for automatic topic number selection if number of words is less than 10000, otherwise use LDA initialization
    
    #Set working directory to C: temp to ensure write permission is allowed for init.type "Spectral" in stm function below
    currentwd <- getwd()
    setwd("C:/temp")
    
    #Model topics including document year as factor if there are more than 1 years present
    if(length(unique(meta$year)) > 1){
      abstractstm <- stm(docs, vocab, K = ceiling(sqrt(length(docs))), init.type = "Spectral", data=meta, prevalence = ~year)
      }else{
        abstractstm <- stm(docs, vocab, K = ceiling(sqrt(length(docs))), init.type = "Spectral", data=meta)
      }
    
    
    #Return working directory back to original setting
    setwd(currentwd)
    
    #Create topic JSON array for LDAvis visualization:
    
    topicPCAJSON <- TopicStmPCAJSON(abstractstm, docs = docs)
    
    #Create matrix of topic proportion by document sorted from most frequently represented topic to least frequently
    #Row name is document ID
    #Column name is topic ID
    
    doctopic <- abstractstm$theta
    rownames(doctopic) <- as.character(meta[,1])
    colnames(doctopic) <- paste("Topic", seq(1:ncol(doctopic)))
    #doctopic <- doctopic[,order(colSums(doctopic), decreasing = TRUE)]
    
    #toLDAvis(abstractstm, docs = docs)

    #Generate simple force directed graph of topic relationships
    #topiccorrelate <- topicCorr(abstractstm, method = "huge")
    #look at plot.topicCorr method to see how to get edges from the topicCorr output
    
    #Create graph using LDAvis PCA layout with topiccorrelate edges for connections
    #Implement LDAvis keyword highlights and add FREX and Lift keywords as tooltips
    #On click, highlight article graph with color spectrum from saturated to empty depending on how closely each article is associated with the topic
    
    #Code to get article polarity
    
    if(input$usesentenceanalysis == "Yes"){
      #browser()
      #Note, still buggy where if there are no multiword tokens (tokens with spaces) or numbers in corpus, will crash
      polartopics <- ArticleTopicPolar(stmmod = abstractstm, articlelist = details$abstract[usedocs], usestem = FALSE)
      
    }else{polartopics <- list()}
    
    
    #   #Temp code to get thoughts (articles) for specific topic
    #   findThoughts(abstractstm, texts = details$abstract[as.numeric(names(temp$documents))], topic = x)
    #   
    #   #Temp code to get topic keywords
    #   labelTopics(abstractstm, topics = x)
    
    #enable database polling search buttons
    enable("summary_search")
    enable("detailed_search")
    enable("link_search")
    
    })
    
    }
    
    #If a model has been loaded, use the loaded model unless the generate model button has been pressed
    if((input$gentopicmodelbutton == 0) & (!is.null(LoadModel()[["TopicModelLoad"]])) & (input$database == "Load Model")){
    #if((!is.null(LoadModel()[["TopicModelLoad"]])) & (input$database == "Load Model")){
      
      #Isolate internal code to prevent updates where the button has not been pressed
      isolate({
      
      #Get loaded data
      loaddat = LoadModel()
      
      #Extract correct elements for output of reactive value
      abstractstm = loaddat$TopicModelLoad$TopicModel
      topicPCAJSON = loaddat$TopicModelLoad$TopicPCAJSON
      meta = loaddat$TopicModelLoad$Metadata
      doctopic = loaddat$TopicModelLoad$TopicProb
      polartopics = loaddat$TopicModelLoad$SentenceTopics
    
      })
    }
    
    return(structure(list("TopicModel" = abstractstm, "TopicPCAJSON" = topicPCAJSON, "Metadata" = meta, "TopicProb" = doctopic, "SentenceTopics" = polartopics)))
    
  })
                                
  #Create core elements for all topic graphs so they do not need to be updated
  #After every UI change since topic graph generation is slow
  TopicGraphsCore <- reactive({
    
    #Get data for linegraph and format for Rcharts plotting
    topicmodel <- CreateTopicModel()[["TopicModel"]]
    meta <- CreateTopicModel()[["Metadata"]]
    
    #Create formula with selected topics
    formulatext <- paste(paste0("c(1:", paste(ncol(topicmodel$theta)), ")"), "~ s(year)")
    topicformula <- as.formula(formulatext)
    
    #Get estimates of all topic proportions over time
    estdat <- estimateEffect(topicformula, topicmodel, metadata = meta)
    graphdat <- plot.estimateEffect(estdat, covariate = "year", model = topicmodel, method = "continuous", xlab = "Year", printlegend = T, ci.level = 0.50, npoints = max(meta$year) - min(meta$year) + 1)
    
    return(structure(list("TopicEstimatedEffects" = estdat, "TopicGraph" = graphdat)))
    
  })
  
  #Create core document skeleton for all document java table creation so it does not need to be recreated from scratch every time which can take a very long time.
  DocTableCore <- reactive({
    
    print('DocTableCore start')
    print(Sys.time())
    
    #Get abstract list and order by closeness to new document based on topic proportions
    details <- FilterDetail()
    
    #Create data frame of title and abstract ranked by match
    tmp = data.frame(Match = 1, Title = details$title, Document_Snippet = "", stringsAsFactors = FALSE)
    
    #Add hyperlink to document title if it exists
    if(!is.null(details$hyperlink)){
      tmp$Title = paste0('<a href="',details$hyperlink,'"', "target='_blank'>",details$title,"</a>")
    }
    
    #Add document IDs as row names for identification and subsequent additional processing as needed
    rownames(tmp) = details$PMID
    
    print('DocTableCore end')
    print(Sys.time())
    
    return(list('CoreTable' = tmp))
  })
  
  #Create core sentence table for all sentence displaying and matching so it does not need to be recreated from scratch every time which takes a long time.
  SentTableCore <- reactive({
    
    #Get data from other reactive functions
    topicmodel <- CreateTopicModel()[["TopicModel"]]
    meta <- CreateTopicModel()[["Metadata"]]
    topicprob <- CreateTopicModel()[["TopicProb"]]
    sentenceanalysis <- CreateTopicModel()[["SentenceTopics"]]
    sentencetopics <- sentenceanalysis$SentenceTopicPolarity
    details <- FilterDetail()
    
    #Match source document titles to each sentence
    sourcedoc = meta$PMID[sentencetopics$num]
    
    #Extract title and add hyperlink if it exists. Otherwise, only extract title as plain text
    if(!is.null(details$hyperlink)){
      sourcedoc = paste0('<a href="',details$hyperlink[sourcedoc],'"', "target='_blank'>",details$title[sourcedoc],"</a>")
    }else{
      sourcedoc = details$title[sourcedoc]
    }
    
    #Create data frame of sentences with placeholder columns for future processing outside of core
    tmp = data.frame(Source.Document = sourcedoc,
                     #Rank = 1,
                     #Probability = 1,
                     #Information.Entropy = 1,
                     Match.Percent = 1,
                     Sentence.Text = sentencetopics$text.var)
    
    #Add document IDs as row names for identification and subsequent additional processing as needed
    #rownames(tmp) = details$PMID
    
    return(list('CoreTable' = tmp))
  })
  
  #Full text search to find documents that match document text provided in full text search
  SemanticSearch <- reactive({
    
    #Get topic model
    topicmodel <- CreateTopicModel()
    
    if(usequanteda){
      #Newer method using quanteda
      
      #Extract all multi-word tokens from topicmodel vocabulary
      phrases <- topicmodel$TopicModel$vocab[grep(" ", topicmodel$TopicModel$vocab, fixed = TRUE)]
      
      #Order from longest to smallest to make sure longest phrases are replaced with placeholder first to prevent errors
      phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
      
      #Create metadata for new article which should have unique document ID that is used in other charts as first column
      meta <- data.frame(PMID = "NEWDOC1", year = max(topicmodel$TopicModel$settings$covariates$betaindex))
      
      #Create corpus from semantic search text field
      quanttok = corpus(as.character(input$similar_document), docnames = 1, docvars = meta)
      
      #If multi-word phrases exist, tokenize using them
      if(length(phrases) > 0){
        
        #Tokenize without removing anything to allow for multiword phrase finding
        quanttok = tokens(x = quanttok)
        
        #Convert multi-word tokens to underscore joined tokens
        #NOTE: Investigate ways to speed up since this is fairly slow (takes approx 30 minutes on SOP dataset, but still faster than older way shown below in tm specific implementation)
        quanttok = tokens_compound(quanttok, pattern = phrase(as.character(phrases)))
        
      }
      
      #Tokenize while removing several undesirable items
      quanttok = tokens(x = quanttok,
                        remove_numbers = input$removenumbers,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_separators = TRUE)
      
      #Remove stopwords
      quanttok = tokens_remove(quanttok, stopwords())
      
      #Remove tokens that are less than 3 characters
      quanttok = tokens_remove(quanttok, min_nchar = 3, max_nchar = 100000)
      
      ##Define token equivalence (define synonyms). need to do more research on how to use
      #tokens_lookup(...)
      
      #Create dfm (much quicker with tokens vs. corpus)
      quantdfm = dfm(quanttok, tolower = TRUE, stem = input$stemwords)
      
      #Convert to stm format
      temp = convert(quantdfm, to = "stm")
      
      #Revert multiword underscore splits in vocab back to spaces
      temp$vocab = gsub(pattern = "_", replacement = " ", x = temp$vocab, fixed = TRUE)
      
    }else{
      #Older method using tm package to process search query. Depricate after confirming quanteda works
      
      #Get input text from text field and aggressively strip the article text, removing all non-letter items due to bugs in stm and tm (based on likely bug in removePunctuation function in tm package) not properly removing punctuation for trademark symbol anymore
      searchtext <- gsub("[^a-zA-Z ]"," ",input$similar_document)
      
      #metadata should have unique document ID that is used in other charts as first column
      meta <- data.frame(PMID = "NEWDOC1", year = max(topicmodel$TopicModel$settings$covariates$betaindex))
      
      #Extract all multi-word tokens from topicmodel vocabulary
      phrases <- topicmodel$TopicModel$vocab[grep(" ", topicmodel$TopicModel$vocab, fixed = TRUE)]
      
      #Order from longest to smallest to make sure longest phrases are replaced with placeholder first to prevent errors
      phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
      
      #Define null vector to start in case no phrases identified
      placeholder <- c()
      
      #Create replacement gibberish uniquestrings
      if(length(phrases) > 0){
        placeholder <-  apply(expand.grid(lapply(1:max(2,ceiling(log(length(phrases), base = 26))), function(i) letters))[1:length(phrases),], MARGIN = 1, paste0, collapse = "")
        placeholder <- paste0("phrasefindqqxzqcvx", placeholder,"phrasefind")
      }
      
      #Replace multiword phrase instances in text input field. Add leading and trailing space to make sure they don't get combined with other words
      multdef <- mgsub(as.character(phrases), as.character(placeholder), searchtext, leadspace = TRUE, trailspace = TRUE)
      
      #Create corpus for new document - can't use textProcessor from stm package as it is hopelessly broken for small lists of documents
      temp <- TermDocumentMatrix(SimpleCorpus(VectorSource(multdef)))
      temp$documents <- list("1" = matrix(c(temp$i, temp$v), nrow = 2, byrow = TRUE))
      temp$vocab <- temp$dimnames$Terms
      
      #Revert multi-word placeholders back to original multi-word phrases
      if(length(phrases) > 0){
        
        temp$vocab <- mgsub(as.character(placeholder), as.character(phrases), temp$vocab)
        
      }
      
    }
    
    # #Moved to tm specific section above
    # #Get input text from text field and aggressively strip the article text, removing all non-letter items due to bugs in stm and tm (based on likely bug in removePunctuation function in tm package) not properly removing punctuation for trademark symbol anymore
    # searchtext <- gsub("[^a-zA-Z ]"," ",input$similar_document)
    # 
    # #metadata should have unique document ID that is used in other charts as first column
    # meta <- data.frame(PMID = "NEWDOC1", year = max(topicmodel$TopicModel$settings$covariates$betaindex))
    # 
    # #Extract all multi-word tokens from topicmodel vocabulary
    # phrases <- topicmodel$TopicModel$vocab[grep(" ", topicmodel$TopicModel$vocab, fixed = TRUE)]
    # 
    # #Order from longest to smallest to make sure longest phrases are replaced with placeholder first to prevent errors
    # phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
    # 
    # #Define null vector to start in case no phrases identified
    # placeholder <- c()
    # 
    # #Create replacement gibberish uniquestrings
    # if(length(phrases) > 0){
    #   placeholder <-  apply(expand.grid(lapply(1:max(2,ceiling(log(length(phrases), base = 26))), function(i) letters))[1:length(phrases),], MARGIN = 1, paste0, collapse = "")
    #   placeholder <- paste0("phrasefindqqxzqcvx", placeholder,"phrasefind")
    # }
    # 
    # #Replace multiword phrase instances in text input field. Add leading and trailing space to make sure they don't get combined with other words
    # multdef <- mgsub(as.character(phrases), as.character(placeholder), searchtext, leadspace = TRUE, trailspace = TRUE)
    # 
    # #Create corpus for new document - can't use textProcessor from stm package as it is hopelessly broken for small lists of documents
    # temp <- TermDocumentMatrix(SimpleCorpus(VectorSource(multdef)))
    # temp$documents <- list("1" = matrix(c(temp$i, temp$v), nrow = 2, byrow = TRUE))
    # temp$vocab <- temp$dimnames$Terms
    # 
    # #Revert multi-word placeholders back to original multi-word phrases
    # if(length(phrases) > 0){
    #   
    #   temp$vocab <- mgsub(as.character(placeholder), as.character(phrases), temp$vocab)
    #   
    # }
    
    #Align Corpus
    temp <- alignCorpus(new = temp, old.vocab = topicmodel$TopicModel$vocab)
    
    #Fit the new topic model - may need to update with metadata at some point
    temp <- fitNewDocuments(model = topicmodel$TopicModel, documents = temp$documents)
    
    #Extract topic probabilities and name them, and order from largest to smallest
    topicvec <- c(temp$theta)
    names(topicvec) <- paste("Topic", c(1:length(topicvec)))
    
    #Find the cosine distance between search document and all existing documents by topic proportions
    cosdist <- proxy::dist(x = temp$theta, y = topicmodel$TopicModel$theta, method = "cosine")
    
    #Get document order from closest match to furthest
    closedocs <- topicmodel$Metadata$PMID[order(cosdist)]
    distperc <- cosdist[order(cosdist)]
    
    # #Old cosine distance code that calculates distance between all documents instead of just the required distances to the new document
    # #Find the cosine distance between search document and all existing documents by topic proportions
    # cosdist <- proxy::dist(rbind(temp$theta, topicmodel$TopicModel$theta), method = "cosine")
    # 
    # #Get document order from closest match to furthest
    # closedocs <- topicmodel$Metadata$PMID[order(cosdist[1:nrow(topicmodel$TopicModel$theta)])]
    # distperc <- cosdist[1:nrow(topicmodel$TopicModel$theta)]
    # distperc <- distperc[order(cosdist[1:nrow(topicmodel$TopicModel$theta)])]
    
    
    return(list("SemanticTopics" = topicvec, "DocumentMatchRank" = closedocs, "DocumentCosDistance" = distperc))
    
  })
  
  #Keyword/phrase search to find documents that match keyword(s)/keyphrase(s) provided in keyword search
  KeywordSearch <- reactive({
    
    topicmodel <- CreateTopicModel()[["TopicModel"]]
    
    topicprobs <- list()
    
    if(nchar(input$keyword_search) > 0){
      
      #Split by comma-space pattern
      searchvect <- strsplit(input$keyword_search, split = ", ", fixed = TRUE)
      
      #Find matching topic blend using regex matching for each query term
      topicprobs <- wordTopicProbs(topicmodel, unlist(searchvect))
      
    }
    
    
    
    return(list("QueryText" = input$keyword_search,
                "TopicMatchProbability" = topicprobs$TopicMatchProbability,
                "MissingWords" = topicprobs$MissingWords))
    
  })
  
  
  #Reactive value to collect keyphrase searches for plotting
  keyphraseplot <- reactiveValues()
  
  #Observer to collect keyphrase plot data
  observe ({  
    
    if(input$plot_keyphrase > 0){
      
      isolate({
        
        topicprobs = KeywordSearch()
        
        #Check to make sure file input, id column selector, and new column name are not blank
        if(!is.null(topicprobs$TopicMatchProbability)){
          
          keyphraseplot$plotdat[[length(keyphraseplot$plotdat) + 1]] = topicprobs
          
        }
        
      })
      
    }
  })
  
  #Observer to clear keyphrase plot data
  observe ({  
    
    if(input$clear_plot_keyphrase > 0){
      
      isolate({
        
        keyphraseplot$plotdat = list()
        
      })
    }
  })
  
  # NOTE: Reactive variables used as functions networkReactive()
  #   networkReactive <- reactive({
  #     if(is.null(input$connectedNodes)) {
  #       return(network)
  #     } else {
  #       t1 <- which(network$source %in% input$connectedNodes)
  #       t2 <- which(network$target %in% input$connectedNodes)
  #       idx <- unique(c(t1, t2))
  #       return(network[idx,])
  #     }
  #   })
  
  #Return summary search text
  output$summary_search_results<-renderText({
    if(is.null(DBSwitch()[["Summary"]])){
      
      textout <- NULL
      
    }else{
      
      textout <- paste("There were", DBSwitch()[["Summary"]][["count"]], "articles found", sep = " ")
      
    }
    return(textout)
  })
  
  output$link_search_results<-renderText({
    if(is.null(DBSwitch()[["NodeEdge"]])){
      textout <- NULL
    }else{
      textout <- paste("There were", nrow(DBSwitch()[["NodeEdge"]][["CitationFrame"]]), "Linkages Found,", length(DBSwitch()[["NodeEdge"]][["ReferenceList"]]), "Articles Referenced by this Collection, and",  length(unique(DBSwitch()[["NodeEdge"]][["OutsideCitationFrame"]][["CitedBy"]])), "Articles that Cite this Collection", sep = " ")
    }
    return(textout)
  })
  
  output$detailed_search_results<-renderText({
    
    # Get IDs
    IDs <- FilterDetail()[["PMID"]]
    
    if(is.null(IDs)){
      textout <- NULL
    }else{
      textout <- paste("There are", length(IDs), "articles in the collection.")
    }
    return(textout)
  })
  
  #Code to download article list
  output$ArticleSave<-downloadHandler(
    filename = function(){paste(input$ArticleSaveName,".csv",sep = "")},
    content = function(file){
      saved_articles<- CreateNetwork()[["nodeData"]]
      
      #Add topic model topic data if a model was created
      if(!is.null(CreateTopicModel()[["TopicProb"]])){
        
        topicprob <- CreateTopicModel()[["TopicProb"]]
        
        saved_articles <- merge(x = saved_articles, y = data.frame(id = rownames(topicprob), topicprob),
                                by.x = "id", by.y = "id", all.x = TRUE)
        
      }
      
      #saved_api <- reactiveValuesToList(tmp)
      write.csv(saved_articles, file = file)
    })
  
  #Code to save finalized topic model and imported data
  output$ModelSave<-downloadHandler(
    filename = function(){paste(input$model_filename,".RData",sep = "")},
    content = function(file){
      
      #Get DBSwitch Content
      DBSwitchReact = DBSwitch()
      
      #Get topic model
      TopicReact = CreateTopicModel()
      
      #Put into list
      savedmodel = list(DBSwitchReact = DBSwitchReact,TopicReact = TopicReact)
      
      #Save contents
      save(savedmodel, file = file)
    })
  
  ##TODO## Finish this data table once highlighting code in javascript is good.
  #   output$nodeDataTable <- DT::renderDataTable({
  #     
  #     
  #     tmp <- CreateNetwork()[["nodeData"]][which(CreateNetwork()[["nodeData"]][["id"]] == input$clickedNode),]
  #     DT::datatable(tmp, filter='bottom', style='bootstrap', options=list(pageLength=5))
  #   })
  
  #Remove edge data network for now. Can add back in using example below later but will need to change networkReactive() to data source
  #   output$edgeDataTable <- DT::renderDataTable({
  #     DT::datatable(networkReactive(), filter='bottom', style='bootstrap', options=list(pageLength=5))
  #   })
  
  output$clickedNode = renderPrint({
    
    tmp <- FilterDetail()[["abstract"]][which(FilterDetail()[["PMID"]] == input$clickedNode)]
    
    return(tmp)
  })
  
  #Code to select all connected nodes. Can reactivate later if desired
  output$connectedNodes = renderPrint({
    
    FilterDetail()[["title"]][which(FilterDetail()[["PMID"]] == input$connectedNodes)]
    
  })
  
  #Plot article network
  output$plot <- renderRcytoscapejs({
    
    if(input$gennetgraph == "Yes"){
      
      #Get node data
      nodeData <- CreateNetwork()[["nodeData"]]
      edgeData <- CreateNetwork()[["edgeData"]]
      
      #Get layout to use
      layout <- NetworkLayout()[["layout"]]
      
      ####In Work#####
      #layout <- input$graphlayout
      
      #Set node names to blank so they will not be printed on node
      nodeData$name <- ""
      
      
      if(input$graphlayout == "drl"){
        nodeData$x <- (NetworkLayout()[["nodeData"]][["x"]])*input$nodexspacing
        nodeData$y <- (NetworkLayout()[["nodeData"]][["y"]])*input$nodeyspacing
        temp = proxy::dist(nodeData[,c("x", "y")])
        scalefactx = 0.75*max(nodeData$width)/min(temp)
        scalefacty = 0.75*max(nodeData$height)/min(temp)
        nodeData$x = nodeData$x*scalefactx*input$nodexspacing
        nodeData$y = nodeData$y*scalefacty*input$nodexspacing
      }
      
      #Adjust scale of node positions for hierarchy layout
      if(input$graphlayout == "preset"){
        nodeData$x <- (NetworkLayout()[["nodeData"]][["x"]]*25 - min(NetworkLayout()[["nodeData"]][["x"]]*100))*input$nodexspacing
        nodeData$y <- (NetworkLayout()[["nodeData"]][["y"]]*50 - min(NetworkLayout()[["nodeData"]][["y"]]*100))*input$nodeyspacing
      }
      
      cyNetwork <- createCytoscapeJsNetwork(nodeData, edgeData)
      rcytoscapejs(nodeEntries=cyNetwork$nodes, edgeEntries=cyNetwork$edges, showPanzoom = TRUE, layout = layout)
    }
    else{NULL}
    
  })
  
  #Plot article type bar graph
  output$articletypebar <- renderChart({
    
    #Get data for histogram
    histdata <- CreateNetwork()[["articletypefreq"]]
    
    #Convert into series list for java plotting with rCharts
    displaynum <- min(nrow(histdata), 50)
    seriesplot <- list()
    for (i in 1:min(nrow(histdata), displaynum)){
      seriesplot[[i]] <- list("y" = histdata$count[[i]], "color" = histdata$color[[i]])
    }
    
    #Create the plot with options
    thegraph <- rCharts::Highcharts$new()
    thegraph$series(data = seriesplot, type = "bar", name = "Number of Articles")
    thegraph$xAxis(categories = histdata$name, title = list(text = ""))
    thegraph$yAxis(title = list(text = "Number of Articles of this Type"))
    thegraph$chart(zoomType = "x")
    thegraph$addParams(dom = 'articletypebar')
    thegraph$legend(enabled = F)
    
    thegraph$plotOptions(bar = list(stacking = "normal", 
                                    cursor = 'pointer', 
                                    point = list(events = list(click = 
                                                                 "#! function() { 
                                                             Shiny.onInputChange('click', {                                            
                                                             category: this.category, chart: 'articletype'
                                                           })
                                                           } !#")
                                    )
    )
    )
    
    return(thegraph)
  })
  
  #Plot keyword bar graph
  output$keywordbar <- renderChart({
    
    
    #Get data for histogram
    histdata <- CreateNetwork()[["keywordfreq"]]
    
    #Convert into series list for java plotting with rCharts
    displaynum <- min(nrow(histdata), 50)
    seriesplot <- list()
    for (i in 1:min(nrow(histdata), displaynum)){
      seriesplot[[i]] <- list("y" = histdata$count[[i]], "color" = histdata$color[[i]])
    }
    
    #Create the plot with options
    thegraph <- rCharts::Highcharts$new()
    thegraph$series(data = seriesplot, type = "bar", name = "Number of Articles")
    thegraph$xAxis(categories = histdata$name[1:displaynum], title = list(text = ""))
    thegraph$yAxis(title = list(text = "Number of Articles Containing Keyword"))
    thegraph$chart(zoomType = "x")
    thegraph$addParams(dom = 'keywordbar')
    thegraph$legend(enabled = F)
    
    thegraph$plotOptions(bar = list(stacking = "normal", 
                                    cursor = 'pointer', 
                                    point = list(events = list(click = 
                                                                 "#! function() { 
                                                             Shiny.onInputChange('click', {                                            
                                                             category: this.category, chart: 'keyword'
                                                           })
                                                           } !#")
                                    )
    )
    )
    
    return(thegraph)
  })
  
  #Plot Topic Trends By Year
  # output$TopicTime <- renderTaucharts({
  #   
  #   
  #   #Get data for linegraph and format for Rcharts plotting
  #   topicmodel <- CreateTopicModel()[["TopicModel"]]
  #   meta <- CreateTopicModel()[["Metadata"]]
  #   graphdat <- estimateEffect( ~ year, topicmodel, metadata = meta, uncertainty = "None")
  #   graphdat <- plot.estimateEffect(graphdat, covariate = "year", model = topicmodel, method = "continuous", xlab = "Year", printlegend = F)
  #   graphframe <- data.frame(graphdat$x, graphdat$means)
  #   colnames(graphframe) <- c("year", paste("Topic", graphdat$topics))
  #   #graphframe <- melt(graphframe, id = "year", na.rm = TRUE)
  #   
  #   #Create graph
  #   
  #   
  #   tautime <- tauchart(graphframe)
  #   thegraph <- tau_line(tautime, "year", "Topic.1")
  #   
  # #   thegraph <- Highcharts$new()
  # #   thegraph$chart(zoomType = "x")
  # #   
  # #   
  # #   #thegraph <- hPlot(value ~ year, group = "variable", data = graphframe, type = "line", title = "Topic Prevalence Over Time", radius = 6)
  # #   thegraph$yAxis(title = (list(text = "Topic Proportion")))
  # #   thegraph$addParams(dom = 'TopicTime')
  # #   
  # #   for (i in 1:length(graphdat$topics)){
  # #     
  # #     lineseries <- list()
  # #     lineseries <- javalister(data.frame(graphframe[,1], graphframe[,i+1]))
  # #     
  # #     thegraph$series(name = paste("Topic", graphdat$topics[i]), type = 'line', data = lineseries, yAxis = 0, turboThreshold = length(lineseries), marker = list(enabled = FALSE), lineWidth = 1)
  # #     
  # #   }
  #   
  #   return(thegraph)
  # })
  
  #Find topics that represent keywords in search
  output$TopicFind <- renderUI({
    
    # #Code moved to reactive statement so results can be shared across several outputs
    # topicmodel <- CreateTopicModel()[["TopicModel"]]
    # 
    # topicprobs <- list()
    # 
    # if(nchar(input$keyword_search) > 0){
    # 
    # searchvect <- strsplit(input$keyword_search, split = " ", fixed = TRUE)
    # 
    # topicprobs <- wordTopicProbs(topicmodel, unlist(searchvect))
    # 
    # if(is.null(topicprobs$TopicMatchProbability) == FALSE){
    # 
    # topicprobs$TopicMatchProbability <- topicprobs$TopicMatchProbability[order(topicprobs$TopicMatchProbability, decreasing = TRUE)]
    # 
    # }
    # 
    # }
    
    #Get topic probabilities from keyword search
    topicprobs = KeywordSearch()
    
    #Sort topics from largest probability to smallest
    if(is.null(topicprobs$TopicMatchProbability) == FALSE){
      
      topicprobs$TopicMatchProbability <- topicprobs$TopicMatchProbability[order(topicprobs$TopicMatchProbability, decreasing = TRUE)]
      
    }
    
    if(!is.null(topicprobs$TopicMatchProbability)){
    return(HTML(paste("<b>Most Probable Topic Matches: </b><br>",
                      paste0(names(topicprobs$TopicMatchProbability[1:min(5, length(topicprobs$TopicMatchProbability))]), ": ", 100*round(topicprobs$TopicMatchProbability[1:min(5, length(topicprobs$TopicMatchProbability))],4), "%", collapse = "<br>"),
                      "<br/> Words/Phrases Not Found In Any Topics: ", paste(topicprobs$MissingWords, collapse = " "))))
    }
  })
  
  
  #Find topics that represent keywords in search
  output$TopicFindProb <- renderChart({
    
    # #Code moved to reactive statement so results can be shared across several outputs
    # topicmodel <- CreateTopicModel()[["TopicModel"]]
    # 
    # topicprobs <- list()
    # 
    # if(nchar(input$keyword_search) > 0){
    #   
    #   searchvect <- strsplit(input$keyword_search, split = " ", fixed = TRUE)
    #   
    #   topicprobs <- wordTopicProbs(topicmodel, unlist(searchvect))
    #   
    #   if(is.null(topicprobs$TopicMatchProbability) == FALSE){
    #     
    #     topicprobs$TopicMatchProbability <- topicprobs$TopicMatchProbability[order(topicprobs$TopicMatchProbability, decreasing = TRUE)]
    #     
    #   }
    #   
    # }
    
    #Get topic probabilities from keyword search
    topicprobs = KeywordSearch()
    
    #Sort topic probabilities from largest to smallest
    if(is.null(topicprobs$TopicMatchProbability) == FALSE){
      
      topicprobs$TopicMatchProbability <- topicprobs$TopicMatchProbability[order(topicprobs$TopicMatchProbability, decreasing = TRUE)]
      
    }
    
    
    #Convert into series list for java plotting with rCharts
    displaynum <- min(nrow(histdata), 50)
    seriesplot <- list()
    for (i in 1:min(nrow(histdata), displaynum)){
      seriesplot[[i]] <- list("y" = histdata$count[[i]], "color" = histdata$color[[i]])
    }
    
    #Create graph
    thegraph <- rCharts::Highcharts$new()
    thegraph$series(data = seriesplot, type = "column", name = "Topics")
    thegraph$xAxis(categories = histdata$name[1:displaynum], title = list(text = ""))
    thegraph$yAxis(title = list(text = "Number of Articles Containing Keyword"))
    thegraph$chart(zoomType = "x")
    thegraph$addParams(dom = 'keywordbar')
    thegraph$legend(enabled = F)
    
    thegraph$plotOptions(bar = list(stacking = "normal", 
                                    cursor = 'pointer', 
                                    point = list(events = list(click = 
                                                                 "#! function() { 
                                                             Shiny.onInputChange('click', {                                            
                                                             category: this.category, chart: 'keyword'
                                                           })
                                                           } !#")
                                    )
    )
    )
    
    return(thegraph)
    
    return(HTML(paste("<b>Most Probable Topic Matches: </b><br>",
                      paste0(names(topicprobs$TopicMatchProbability[1:min(5, length(topicprobs$TopicMatchProbability))]),": " , 100*round(topicprobs$TopicMatchProbability[1:min(5, length(topicprobs$TopicMatchProbability))], 3), "%", collapse = "<br>"),
                      "<br/> Words Not Found In Any Topics: ",
                      paste(topicprobs$MissingWords, "hr()"))))
    
  })
  
  
  #Find documents that match document text provided in search
  output$TopicSemMatch <- renderUI({
    
    #Get semantic search topic match vector and document ranking
    semsearch = tryCatch(SemanticSearch(), error = function(e) NULL)
    
    if(!is.null(semsearch)){
    #Sort topic vector from largest to smallest
    topicvec <- semsearch$SemanticTopics[order(semsearch$SemanticTopics, decreasing = TRUE)]
    
    #Get ranked list of most likely matching documents
    closedocs <- semsearch$DocumentMatchRank
    
    #Get abstract list and order by closeness to new document based on topic proportions
    details <- FilterDetail()
    names(details$abstract) <- details$PMID
    output <- details$abstract[as.character(closedocs)]
    
    return(HTML(paste("<b>Most Probable Topic Matches: </b><br>",
                      paste0(names(topicvec[1:min(5, length(topicvec))]), ": " , 100*round(topicvec[1:min(5, length(topicvec))], 3), "%", collapse = "<br>"), "<br><br>")))
    #paste(names(topicvec[1:min(5, length(topicvec))]), topicvec[1:min(5, length(topicvec))], collapse = ", "))))
    }
    
  })
  
  #Initialize semantic document matching table
  output$SemDocMatchTable <- DT::renderDataTable({
    
    #Get core table data
    tmp = DocTableCore()[['CoreTable']]
    
    if(!is.null(tmp)){
      
      #Set reactive flag to TRUE for document table generation
      tableinitialized$DocumentDT = TRUE
      
      #Remove row names so they are not displayed
      rownames(tmp) = NULL
      
      DT::datatable(tmp, filter='none', style='bootstrap', escape = FALSE,
                    options=list(pageLength=5, columnDefs = list(list(
                      targets = 3,
                      render = JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data.length > 1000 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 1000) + '...</span>' : data;",
                        "}")
                    ))))
      
    }else{
      
      #Set reactive flag to FALSE for document table generation
      tableinitialized$DocumentDT = FALSE
      
    }
    
  })
  
  #Update semantic document table with search results to find documents that match document text provided in semantic search
  # NOTE: Updating this way is much faster than fully rebuilding the table every time the search changes.
  proxSemTable = dataTableProxy('SemDocMatchTable')
  
  # Update table when a new search query is executed
  observe({
    
    # Check to see if table has been initialized:
    if(tableinitialized$DocumentDT){
      
      #Get core static table
      tmp = tryCatch(DocTableCore()[['CoreTable']], error = function(e) NULL)
      
      if(!is.null(tmp)){
      
      #Get semantic search topic match vector and document ranking
      semsearch = tryCatch(SemanticSearch(), error = function(e) NULL)
      
      isolate({
        
        # If something was returned by the semantic search and if the table exists, update the table
        if(!is.null(semsearch)){
          
          #Sort topic vector from largest to smallest
          topicvec <- semsearch$SemanticTopics[order(semsearch$SemanticTopics, decreasing = TRUE)]
          
          #Get ranked list of most likely matching documents
          closedocs <- semsearch$DocumentMatchRank
          
          #Get document match percent
          distperc <- semsearch$DocumentCosDistance
          
          #Rank items by matching result of distance calculations to document ID in row name
          tmp = tmp[as.character(closedocs),]
          tmp$Match = round(100*(1 - distperc), digits = 1)
          
          #Remove rownames to unclutter DT display as they are no longer needed after sorting above is finished
          rownames(tmp) = NULL
          
          replaceData(proxSemTable, tmp, resetPaging = TRUE)
          
        }else{
          #If no search terms have been entered, return a blank table
          #Get core static table data
          tmp = tmp[1,]
          
          #Return blank table for rendering
          tmp[1,] = ""
          tmp[,ncol(tmp)] = "Enter search terms to return results"
          
          #Remove rownames to unclutter DT display as they are no longer needed after sorting above is finished
          rownames(tmp) = NULL
          
          replaceData(proxSemTable, tmp)
          
        }
        
        
      })
      
      }
      
    }
    
  })
  
  # Get best matching segment of each displayed document in the table using a binary tree search
  observe({
    
    # Check to see if table has been initialized:
    if(tableinitialized$DocumentDT){
      
      # Get raw data
      details = FilterDetail()
      topicmodel = CreateTopicModel()
      
      #Get core static table
      tmp = tryCatch(DocTableCore()[['CoreTable']], error = function(e) NULL)
      
      if(!is.null(tmp)){
        
        #Get semantic search topic match vector and document ranking
        semsearch = tryCatch(SemanticSearch(), error = function(e) NULL)
        
        #Get current displayed rows of table
        displayed_rows = input$SemDocMatchTable_rows_current
        
        isolate({
          
          # If something was returned by the semantic search and if the table exists, update the table
          if(!is.null(semsearch)){
            
            #Sort topic vector from largest to smallest
            topicvec <- semsearch$SemanticTopics[order(semsearch$SemanticTopics, decreasing = TRUE)]
            
            #Get ranked list of most likely matching documents
            closedocs <- semsearch$DocumentMatchRank
            
            #Get document match percent
            distperc <- semsearch$DocumentCosDistance
            
            #Rank items by matching result of distance calculations to document ID in row name
            tmp = tmp[as.character(closedocs),]
            tmp$Match = round(100*(1 - distperc), digits = 1)
            
            # Get best matching segment of each displayed document using a binary tree search
            if(!is.null(displayed_rows)){
              print("Firing snippet finder")
              table_docs_bin = c()
              for(i in displayed_rows){
                
                snippet_results = binary_doc_snippet_search(document = details$abstract[[as.character(closedocs)[i]]],
                                                            stm_model = topicmodel$TopicModel,
                                                            target_vector = semsearch$SemanticTopics)
                table_docs_bin = c(table_docs_bin, snippet_results$BestSnippet)
              }
              
              #Clean invalid characters from abstract that can cause issues with table display and replace currently displayed rows
              table_docs_bin = sapply(table_docs_bin, clean)
              tmp$Document_Snippet[displayed_rows] = table_docs_bin
              
            }
            
            #Remove rownames to unclutter DT display as they are no longer needed after sorting above is finished
            rownames(tmp) = NULL
            
            replaceData(proxSemTable, tmp, resetPaging = FALSE)
            
          }else{
            #If no search terms have been entered, return a blank table
            #Get core static table data
            tmp = tmp[1,]
            
            #Return blank table for rendering
            tmp[1,] = ""
            tmp[,ncol(tmp)] = "Enter search terms to return results"
            
            #Remove rownames to unclutter DT display as they are no longer needed after sorting above is finished
            rownames(tmp) = NULL
            
            replaceData(proxSemTable, tmp)
            
          }
          
          
        })
        
      }
      
    }
    
  })
  
  #Create plot of topic blend of document text provided in search
  output$SemanticSearchTimePlot <- renderPlot({
    
    # #Old method below that generated graph in this chunk
    # #Get data for linegraph and format for Rcharts plotting
    # topicmodel <- CreateTopicModel()[["TopicModel"]]
    # meta <- CreateTopicModel()[["Metadata"]]
    # semsearch <- SemanticSearch()
    # 
    # #Create formula with selected topics
    # formulatext <- paste(paste0("c(1:", paste(ncol(topicmodel$theta)), ")"), "~ s(year)")
    # topicformula <- as.formula(formulatext)
    # 
    # #Get estimates of all topic proportions over time
    # graphdat <- estimateEffect(topicformula, topicmodel, metadata = meta)
    # graphdat <- plot.estimateEffect(graphdat, covariate = "year", model = topicmodel, method = "continuous", xlab = "Year", printlegend = T, ci.level = 0.50)
    # 
    # allprobsmat = data.frame(graphdat$means)
    # 
    # #Project all probabilities onto topic blend for search query
    # y = as.matrix(allprobsmat)%*%semsearch$SemanticTopics
    # 
    # #Create resulting plot over time
    # graphdat = plot(x = graphdat$x, y = y, type = "l",
    #                 main = "Semantic Search Query Topic Probability Over Time", xlab = "Time", ylab = "Proportion of all Documents")
    
    
    #Get semantic search results
    semsearch = tryCatch(SemanticSearch(), error = function(e) NULL)
    
    if(!is.null(semsearch)){
      #Get base topic graph of all topics
      graphdat = TopicGraphsCore()[["TopicGraph"]]
    
      #Extract mean probabilities from graph and project onto topic blend from semantic search
      allprobsmat = data.frame(graphdat$means)
      y = as.matrix(allprobsmat)%*%semsearch$SemanticTopics
    
      #Create resulting plot over time
      graphdat = plot(x = graphdat$x, y = y, type = "l",
                    main = "Semantic Search Query Topic Probability Over Time", xlab = "Time", ylab = "Proportion of all Documents")
    
      return(graphdat)
    }
    
  })
  
  
  #Create plot of keyword/phrase topic projection trend over time search
  output$KeywordTimePlot <- renderPlot({
    
    if(!is.null(keyphraseplot$plotdat)){
    #Get base topic graph of all topics
    graphdat = TopicGraphsCore()[["TopicGraph"]]
    
    #Extract mean probabilities from graph
    allprobsmat = data.frame(graphdat$means)
    
    #Get list of keyphrases to plot
    plotlist = keyphraseplot$plotdat
    
    #Create list to hold resulting probabilities
    ylist = list()
    namevec = c()
    
    #Loop through every keyphrase
    for(i in 1:length(plotlist)){
      
      #Project mean topic probabilities over time onto topic blend from keyphrase search
      ylist[[i]] = as.matrix(allprobsmat)%*%plotlist[[i]]$TopicMatchProbability
      namevec = c(namevec, plotlist[[i]]$QueryText)
      
    }
    
    #Create graph
    graphdat = matplot(graphdat$x, y = data.frame(ylist), type = "l",
                       xlab = "Time", ylab = "Proportion of All Documents",
                       main = "KeyPhrase Query Topic Probability Over Time")
    legend("topleft", legend = namevec, col = 1:length(plotlist), lty = 1:length(plotlist))
    
    
    return(graphdat)
    }
    
  })
  
  #Plot selected topics over time
  output$TopicTime <- renderPlotly({
    
    if(length(topicclicks$selected) > 0){
    
    #Get base effect estimates over time for all topics and topic model
    graphdat = TopicGraphsCore()[["TopicGraph"]]
    topicmodel <- CreateTopicModel()[["TopicModel"]]
    meta <- CreateTopicModel()[["Metadata"]]
    
    #Extract data from the value returned by plot.estimateEffect to display on the chart for selected topics
    topicsel = unlist(topicclicks$selected)
    PlotLine = graphdat$means[topicsel]
    plotConfidenceInterval = graphdat$ci[topicsel]
    plotTopics = graphdat$labels[topicsel]
    xvals <- graphdat$x
    
    #Create a color palette to choose colors for each plot
    colorPalette = rainbow(length(unlist(topicclicks$selected)))
    
    #Create plotly object
    graphdatplotly <- plot_ly(type = 'scatter', mode = "lines")
    graphdatplotly = layout(graphdatplotly, xaxis = list(title = "Year"), yaxis = list(title = "Expected Topic Proportion"))
    
    #Loops for plotting the cofidence interval and topic line
    for(i in 1:length(plotTopics)){
      chosenColor <- colorPalette[i]
      
      # Add mean line
      graphdatplotly = add_trace(graphdatplotly, y = PlotLine[[i]], x = xvals, name = plotTopics[i], legendgroup = paste0("Line", i), line = list(color = chosenColor))
      
      ## Add upper and lower CI
      #graphdatplotly = add_trace(graphdatplotly, y = plotConfidenceInterval[[i]][1,], x = xvals, name = plotTopics[i], showlegend = FALSE, legendgroup = paste0("Line", i), line = list(color = chosenColor,dash = 'dash' ))
      #graphdatplotly = add_trace(graphdatplotly, y = plotConfidenceInterval[[i]][2,], x = xvals, name = plotTopics[i], showlegend = FALSE, legendgroup = paste0("Line", i), line = list(color = chosenColor,dash = 'dash' ))
      
    }
    
    return(graphdatplotly)
    }
  })
  
  # #Function below has been replaced by a datatable object
  # #Print most relevent sentences for selected topic(s) (experimental)
  # output$RelevantSentences <- renderText({
  #   
  #   #Get data for linegraph and format for Rcharts plotting
  #   topicmodel <- CreateTopicModel()[["TopicModel"]]
  #   meta <- CreateTopicModel()[["Metadata"]]
  #   topicprob <- CreateTopicModel()[["TopicProb"]]
  #   sentenceanalysis <- CreateTopicModel()[["SentenceTopics"]]
  #   sentencetopics <- sentenceanalysis$SentenceTopicPolarity
  #   
  #   #Create formula with selected topics
  #   selectedtopics <- c("entropy", paste("Topic", unlist(topicclicks$selected)))
  #   sentenceprob <- apply(sentencetopics[,selectedtopics], MARGIN = 1, prod)
  #   
  #   #Return top 5 sentences with highest combined product of all topics and sentence entropy
  #   output <- sentencetopics$text.var[order(sentenceprob, decreasing = TRUE)][1:5]
  #   
  #   return(output)
  # })
  
  #Initialize sentence relevance table
  output$RelevantSentencesDT <- DT::renderDataTable({
    
    #Get core static table data
    tmp = SentTableCore ()[['CoreTable']]
    
    #Check to see if core static table has yet been generated
    if(!is.null(tmp)){
      
      #Set reactive flag to TRUE for sentence table generation
      tableinitialized$SentenceDT = TRUE
      
      #Create datatable object
      DT::datatable(tmp, filter='none', style='bootstrap', escape = FALSE, options=list(pageLength=5))
      
    }else{
      
      #Set reactive flag to FALSE for sentence table generation
      tableinitialized$SentenceDT = FALSE
      
    }
    
  })
  
  #Organize sentences by relevance and present in live datatable
  #Update sentence relevance table with search results based on selected topics
  # NOTE: Updating this way is much faster than fully rebuilding the table every time the search changes.
  proxSentenceTable = dataTableProxy('RelevantSentencesDT')
  observe({
    
    # Check to see if table has been initialized:
    if(tableinitialized$SentenceDT){
      
      #Get core static table
      tmp = tryCatch(SentTableCore()[['CoreTable']], error = function(e) NULL)
      
      if(!is.null(tmp)){
      
      #Get selected topics
      topicsel = unlist(topicclicks$selected)
      
      isolate({
        
        # If something was returned by the semantic search, update the table
        if(length(topicsel) > 0){
          
          #Get data from other reactive functions
          sentenceanalysis <- CreateTopicModel()[["SentenceTopics"]]
          sentencetopics <- sentenceanalysis$SentenceTopicPolarity
          
          #Create formula with selected topics for probability
          sel = paste("Topic", unlist(topicclicks$selected))
          sentenceprob = apply(cbind(1, sentencetopics[,sel]), MARGIN = 1, prod) #THIS IS HACKY SOLUTION TO MAKE SURE PRODUCT WORKS WITH ONLY 1 TOPIC SELECTED. FIX LATER
          sentenceent = unlist(sentencetopics$entropy)
          ranksentences = unlist(Map("*", sentenceprob, sentenceent))
          
          #Enter match criteria into table
          tmp$Match.Percent = round((ranksentences/max(ranksentences, na.rm = TRUE))*100,2)
          
          #Order data frame by rank
          tmp = tmp[order(ranksentences, decreasing = TRUE),]
          
          #Replace data in pre-generated datatable object
          replaceData(proxSentenceTable, tmp)
          
        }else{
          #If no topics have been selected, return a blank table
          #Get single row of core static table data
          tmp = tmp[1,]
          
          #Return blank table for rendering
          tmp[1,] = ""
          tmp[,ncol(tmp)] = "Select topic bubbles to see relevant sentences"
          
          replaceData(proxSentenceTable, tmp)
          
        }
        
      })
      
      }
      
    }
    
  })
  
  
  
  
  
  #Plot topic hierarchy
  output$TopicHierarchy <- renderPlot({
    
    #
    #Get data for linegraph and format for Rcharts plotting
    topicmodel <- CreateTopicModel()[["TopicModel"]]
    #meta <- CreateTopicModel()[["Metadata"]]
    #topicprob <- CreateTopicModel()[["TopicProb"]]
    
    #Generate labels for leaf (choose FREX metric for now):
    topiclabs = labelTopics(topicmodel, n = 4)
    labvect = apply(topiclabs$frex, MARGIN = 1, function(x) paste(x, collapse = ", "))
    
    #Calculate cosine distance matrix using word probabilities?
    wordprobs = as.matrix(exp(topicmodel$beta$logbeta[[1]]))
    rownames(wordprobs) = labvect
    cosdist <- proxy::dist(wordprobs, method = "cosine")
    #cosdist = as.matrix(cosdist)
    
    #Cluster based on cosine distance using average linkage
    #NOTE: Uses average number of documents in topic as weighting argument (members) for clustering
    cosclust <- hclust(cosdist, method = "average", members = colSums(topicmodel$theta))
    #cosclust <- hclust(cosdist, method = "average")
    cosdend <- as.dendrogram(cosclust, label = labvect)
    plotly::plot_dendro(cosdend)
    cosclust <- pvclust::pvclust(as.matrix(cosdist), method.hclust = "average")
    
    return(graphdat)
  })
  
  #Plot topic hierarchy
  output$TopicTree <- renderCollapsibleTree({
    
    #
    #Get data for linegraph and format for Rcharts plotting
    topicmodel <- CreateTopicModel()[["TopicModel"]]
    #meta <- CreateTopicModel()[["Metadata"]]
    #topicprob <- CreateTopicModel()[["TopicProb"]]
    
    #Generate hierarchy TODO: Try adding nwords argument to see if it improves
    hierframe = topichierarchy(logbeta = topicmodel$beta$logbeta[[1]], theta = topicmodel$theta,
                               vocab = topicmodel$vocab, vocabcount = topicmodel$settings$dim$wcounts$x,
                               nwords = NULL, nlab = 4, difftype = "global")
    
    # #Generate labels for leaf (choose FREX metric for now):
    # topiclabs = labelTopics(topicmodel, n = 4)
    # labvect = apply(topiclabs$frex, MARGIN = 1, function(x) paste(x, collapse = ", "))
    # 
    # #Calculate cosine distance matrix using word probabilities?
    # wordprobs = as.matrix(exp(topicmodel$beta$logbeta[[1]]))
    # rownames(wordprobs) = labvect
    # cosdist <- proxy::dist(wordprobs, method = "cosine")
    # #cosdist = as.matrix(cosdist)
    # 
    # #Cluster based on cosine distance using average linkage
    # #NOTE: Uses average number of documents in topic as weighting argument (members) for clustering
    # cosclust <- hclust(cosdist, method = "average", members = colSums(topicmodel$theta))
    # #cosclust <- hclust(cosdist, method = "average")
    # cosdend <- as.dendrogram(cosclust, label = labvect)
    # cosdend = data.tree::as.Node(cosdend)
    
    #Create graph tooltip html:
    hierframe$tooltip = paste0(
      "Topic Content: ",
      hierframe$GlobalFrex,
      "<br><br>Split Difference: ",
      hierframe$SiblingFrex,
      "<br><br>Parent Difference: ",
      hierframe$ParentFrex
    )
    
    #Create hierarchy
    #graphout = collapsibleTreeNetwork(hierframe[,c("Parent", "Node", "FrexSplit", "NodeSize")],
    #                                 attribute = "FrexSplit", nodeSize = "NodeSize", collapsed = FALSE)
    graphout = collapsibleTreeNetwork(hierframe[,c("Parent", "Node", "tooltip", "NodeSize")],
                                      tooltipHtml = "tooltip", nodeSize = "NodeSize", collapsed = FALSE)
    return(graphout)
    #plotly::plot_dendro(cosdend)
    #cosclust <- pvclust::pvclust(as.matrix(cosdist), method.hclust = "average")
  })
  
  #Plot Topics in PCA reduced graph using LDAvis
  output$TopicPCA <- renderVis({
    
    
    #Get data for histogram
    graphTopic <- CreateTopicModel()[["TopicPCAJSON"]]
    
    return(graphTopic)
  })
  
  
  
})

