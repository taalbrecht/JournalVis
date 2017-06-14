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
#library(taucharts)
#Use line below for error tracer. To reset to default, use options(shiny.error = NULL)
#options(shiny.error=browser)


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
    
    if(input$summary_search > 0 | input$link_search > 0 | input$detailed_search > 0| input$clear_filters > 0){
      
      if(length(DBSwitch()[["Fetch"]]) > 0){
      
      entfetch <- DBSwitch()[["Fetch"]]
      
      isolate(filterinclude$ids <- entfetch$PMID)
      
      }
    }
  })
  
  ####PUBMED FETCH FUNCTIONS#####
  
  #Perform summary search to get results count
  
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
      
      
      return(structure(list(details = list("title" = title, "articletype" = articletype, "journal" = journal, "year" = year, "DOI" = DOI, "abstract" = abstract, "keywords" = keywords, "authors" = authors, "PMID" = PMID, "HoverTip" = hovertip, "ClickTip" = clicktip),
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
      #browser()
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
      arxivfetch <- arxiv_search(query = query[[1]], batchsize = 100, limit = articlecount)
      
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
      #browser()
      
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
      
      ##Temporary code to take all files in path for now. Replace with functioning search later
      filepathresults <- list.files(localfilePath(), full.names = TRUE)
      #####
      
      #ids set to dummy variable of 1 if search performed since Arxiv search doesn't need ids to fetch details
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
    #browser()
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
  

  #Function to swap fetch functions defined above based on database radio button selected
  DBSwitch <- reactive({
    
    #disable database polling search buttons
    disable("summary_search")
    disable("detailed_search")
    disable("link_search")
    
    if(isolate(input$database == "pubmed")){
      
      Summary <- EntrezSummary()
      Fetch <- EntrezFetch()[["details"]]
      NodeEdge <- EntrezFetch()[["refstructure"]]
      
    }
    
    if(isolate(input$database == "PLOS")){
      
      Summary <- PLOSSummary()
      Fetch <- PLOSFetch()[["details"]]
      NodeEdge <- PLOSFetch()[["refstructure"]]
      
    }
    
    if(isolate(input$database == "aRxiv")){
      
      Summary <- ARXIVSummary()
      Fetch <- ARXIVFetch()[["details"]]
      NodeEdge <- ARXIVFetch()[["refstructure"]]
      
    }
    
    if(isolate(input$database == "OPS patents")){
      
      Summary <- OPSSummary()
      Fetch <- OPSFetch()[["details"]]
      NodeEdge <- OPSFetch()[["refstructure"]]
      
    }
    
    if(isolate(input$database == "Local Files")){
      
      Summary <- LocalSummary()
      Fetch <- LocalFetch()[["details"]]
      NodeEdge <- LocalFetch()[["refstructure"]]
      
    }
    
    #enable database polling search buttons
    enable("summary_search")
    enable("detailed_search")
    enable("link_search")
    
    #browser()
    
    return(structure(list("Summary" = Summary, "Fetch" = Fetch, "NodeEdge" = NodeEdge)))
    
    
  })
  
  #Get important information from entrez detail search ##Current filtering problem### Investigate
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
      articletypefreq <- summary.factor(tolower(unlist(sapply(articletypefreq, FUN = as.character))))
      
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
      keywordfreq <- sapply(sapply(keywordfreq, FUN = as.character), FUN = tolower)
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
    
    #If there are connections, create a sugiyama network of connected nodes and fill orphans in around it
    if(is.null(FilterDetail()[["year"]]) == FALSE){
    
    NodeEdge <- DBSwitch()[["NodeEdge"]]
    
    edgeList <- NodeEdge[["CitationFrame"]]
    
    startnodes <- FilterDetail()[["PMID"]]
    
    if(ncol(edgeList) == 2){
    
    #Filter edgeList by node IDs
    edgeList <- edgeList[intersect(which(edgeList[,1] %in% startnodes), which(edgeList[,2] %in% startnodes)),]
    
    colnames(edgeList) <- c("source", "target")
    
    }
    
    layer <- FilterDetail()[["year"]]
    
    nodes <- startnodes
    
    id <- nodes
    name <- nodes
    nodeData <- data.frame(id, name, stringsAsFactors=FALSE)
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
      
      net <- graph.data.frame(edgeList, nodeData[connectindex,], directed = TRUE)
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
    
    details <- FilterDetail()
    #     
    # library(tm)
    # library(proxy)
    #library(pvclust)
    #     #Create corpus from article abstracts and clean tex
    #     corpus <- Corpus(VectorSource(details$abstract))
    #     corpus <- tm_map(corpus, content_transformer(tolower))
    #     corpus <- tm_map(corpus, removePunctuation)
    #     corpus <- tm_map(corpus, removeWords, stopwords("english"))
    #     corpus <- tm_map(corpus, removeNumbers)
    #     
    #     #Convert words to stems
    #     corpus <- tm_map(corpus, stemDocument)
    #     
    #     #Create term-document matrix with IDF weighting (inverse document frequency weighting)
    #     abstractTDM <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
    #     
    #     #Calculate dissimilarity matrix using cosine method
    #     abstractdissim <- dist(as.matrix(abstractTDM), by_rows = FALSE, method = "cosine")
    #     
    #     #Calculate clustering and generate plot
    #     abstractclust <- hclust(abstractdissim, method = "ward.D")
    #     
    #   
    #   ##Alternate method using LDA or CTM and automatic cluster number calculator:
    #   library(Rmpfr)
    #   
    #   harmonicMean <- function(logLikelihoods, precision=2000L) {
    #     library("Rmpfr")
    #     llMed <- median(logLikelihoods)
    #     as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
    #                                          prec = precision) + llMed))))
    #   }
    #   
    #   # generate numerous topic models with different numbers of topics
    #   sequ <- seq(2, 25, 1) # in this case a sequence of numbers from 1 to 50, by ones.
    #   fitted_many <- lapply(sequ, function(k) LDA(JSS_dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))
    #   
    #   # extract logliks from each topic
    #   logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
    #   
    #   # compute harmonic means
    #   hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
    #   
    #   # inspect
    #   plot(sequ, hm_many, type = "l")
    #   
    #   # compute optimum number of topics
    #   sequ[which.max(hm_many)]
    
    ##Second option using stm (can expand to model topics by year or other meta data later, also much faster. Must set working directory to C directory for "Spectral" method to work)
    library(qdap)
    library(stm)
    library(huge)
    library(LDAvis)
    library(tm)
    
    #Process abstract text to stm text object. Includes control objects, including number removal, punctuation removal, lowercase, and stemming
    #metadata should have unique document ID that is used in other charts as first column
    meta <- data.frame(details$PMID, details$year)
    colnames(meta) <- c("PMID", "year")
    
    #disable database polling search buttons using shinyjs before intense computation steps below
    disable("summary_search")
    disable("detailed_search")
    disable("link_search")
    
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
  
    ##New way to hold multiword phrases with TopMine
    writeLines(gsub("\n", ". ", details$abstract), paste0(currentwd,"/ToPMine/topicalPhrases/rawFiles/mod.txt"))
    
    setwd(paste0(currentwd,"/ToPMine/topicalPhrases/"))
    system(paste0(currentwd,"/ToPMine/topicalPhrases/win_run.bat"), wait = TRUE)
    
    setwd(currentwd)
    
    multiword <- ToPMinetoDTM(paste0(currentwd,"/ToPMine/topicalPhrases/TopicalPhrases/input_dataset_output/input_wordTraining.txt"))
    
    multiword <- readCorpus(multiword, type = "dtm")
    
    #Identify all multi-word phrases identified by TopMine and sort by number of characters (large to small) and create dataframe with replacement token
    
    phrases <- multiword$vocab[grep(" ", multiword$vocab, fixed = TRUE)]
    phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
    
    #Define null vector to start in case no phrases identified
    placeholder <- c()
    
    if(length(phrases) > 0){
    
    placeholder <-  apply(expand.grid(lapply(1:max(2,ceiling(log(length(phrases), base = 26))), function(i) letters))[1:length(phrases),], MARGIN = 1, paste0, collapse = "")
    placeholder <- paste0("phrasefindqqxzqcvx", placeholder,"phrasefind")
    
    }
    
    #Replace multiword phrase instances. Add leading and trailing space to make sure they don't get combined with other words
    multdef <- mgsub(as.character(phrases), as.character(placeholder), details$abstract, ignore.case = TRUE, leadspace = TRUE, trailspace = TRUE)
    
    #Process resultant text in preparation for STM/LDA modeling
    temp <- textProcessor(documents = multdef, metadata = meta,  stem = FALSE)
    
    #If TopMine is not installed, process corpus as single word tokens:
}else{temp <- textProcessor(documents = details$abstract, metadata = meta,  stem = FALSE)}
    
    currentwd <- getwd()
    
    #Process abstract details into format for stm document preparation
    meta<-temp$meta
    vocab<-temp$vocab
    docs<-temp$documents
    
    #Prep document for stm modeling. Removes infrequent and too frequent terms and sparse documents
    out <- prepDocuments(docs, vocab, meta)
    docs<-out$documents
    vocab<-out$vocab
    meta <-out$meta
    
    #Create vector of document numbers that were not removed during text processing:
    usedocs <- as.integer(names(docs))
    
    #Revert multi-word placeholders back to original multi-word phrases if TopMine process was successful
    if(exists("multdef")){
      
    vocab <- mgsub(as.character(placeholder), as.character(phrases), vocab, ignore.case = TRUE)
    
    }
    
    #Calculate topics. Use "Spectral" initialization to allow for automatic topic number selection if number of words is less than 10000, otherwise use LDA initialization
    
    #Set working directory to C: temp to ensure write permission is allowed for init.type "Spectral" in stm function below
    setwd("C:/temp")
    
    #Model topics including document year as factor
    abstractstm <- stm(docs, vocab, K = ceiling(sqrt(length(docs))), init.type = "Spectral", data=meta, prevalence = ~year)
    
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
    browser()
    #Generate simple force directed graph of topic relationships
    #topiccorrelate <- topicCorr(abstractstm, method = "huge")
    #look at plot.topicCorr method to see how to get edges from the topicCorr output
    
    #Create graph using LDAvis PCA layout with topiccorrelate edges for connections
    #Implement LDAvis keyword highlights and add FREX and Lift keywords as tooltips
    #On click, highlight article graph with color spectrum from saturated to empty depending on how closely each article is associated with the topic
    
    #Code to get article polarity
    
    if(input$usesentenceanalysis == "Yes"){
      
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
    
    return(structure(list("TopicModel" = abstractstm, "TopicPCAJSON" = topicPCAJSON, "Metadata" = meta, "TopicProb" = doctopic, "SentenceTopics" = polartopics)))
    
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
  if(is.null(FilterDetail())){
    textout <- "Detailed Search Not Yet Performed"
  }else{
    textout <- "Detailed Search Completed"
  }
  return(textout)
})

#Code to download article list
output$ArticleSave<-downloadHandler(
  filename = function(){paste(input$ArticleSaveName,".csv",sep = "")},
  content = function(file){
    saved_articles<- CreateNetwork()[["nodeData"]]
    #saved_api <- reactiveValuesToList(tmp)
    write.csv(saved_articles, file = file)
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
    
    #Get node data
    nodeData <- CreateNetwork()[["nodeData"]]
    edgeData <- CreateNetwork()[["edgeData"]]

    #Get layout to use
      layout <- NetworkLayout()[["layout"]]
    
    #Set node names to blank so they will not be printed on node
    nodeData$name <- ""
    
    #Get node positions for hierarchy layout
     nodeData$x <- (NetworkLayout()[["nodeData"]][["x"]]*25 - min(NetworkLayout()[["nodeData"]][["x"]]*100))*input$nodexspacing
     nodeData$y <- (NetworkLayout()[["nodeData"]][["y"]]*50 - min(NetworkLayout()[["nodeData"]][["y"]]*100))*input$nodeyspacing
    
    
    cyNetwork <- createCytoscapeJsNetwork(nodeData, edgeData)
    rcytoscapejs(nodeEntries=cyNetwork$nodes, edgeEntries=cyNetwork$edges, showPanzoom = TRUE, layout = layout)
    
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
  topicmodel <- CreateTopicModel()[["TopicModel"]]
  
  topicprobs <- list()
  
  if(nchar(input$topic_search) > 0){
  
  searchvect <- strsplit(input$topic_search, split = " ", fixed = TRUE)
  
  topicprobs <- wordTopicProbs(topicmodel, unlist(searchvect))
  
  if(is.null(topicprobs$TopicMatchProbability) == FALSE){
  
  topicprobs$TopicMatchProbability <- topicprobs$TopicMatchProbability[order(topicprobs$TopicMatchProbability, decreasing = TRUE)]
  
  }
  
  }
  
  #browser()
  
  return(HTML(paste("Most Probable Topics: ",
               paste(names(topicprobs$TopicMatchProbability[1:min(5, length(topicprobs$TopicMatchProbability))]), round(topicprobs$TopicMatchProbability[1:min(5, length(topicprobs$TopicMatchProbability))],4), collapse = ", "),
               "<br/> Words Not Found In Any Topics: ", paste(topicprobs$MissingWords, collapse = " "))))
  
})


#Find topics that represent keywords in search
output$TopicFindProb <- renderChart({
  topicmodel <- CreateTopicModel()[["TopicModel"]]
  
  topicprobs <- list()
  
  if(nchar(input$topic_search) > 0){
    
    searchvect <- strsplit(input$topic_search, split = " ", fixed = TRUE)
    
    topicprobs <- wordTopicProbs(topicmodel, unlist(searchvect))
    
    if(is.null(topicprobs$TopicMatchProbability) == FALSE){
      
      topicprobs$TopicMatchProbability <- topicprobs$TopicMatchProbability[order(topicprobs$TopicMatchProbability, decreasing = TRUE)]
      
    }
    
  }
  
  #browser()
  
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
  
  return(HTML(paste("Most Probable Topics: ",
                    paste(names(topicprobs$TopicMatchProbability[1:min(5, length(topicprobs$TopicMatchProbability))]), topicprobs$TopicMatchProbability[1:min(5, length(topicprobs$TopicMatchProbability))], collapse = ", "),
                    "<br/> Words Not Found In Any Topics: ", paste(topicprobs$MissingWords, collapse = " "))))
  
})

#Plot selected topics over time
output$TopicTime <- renderPlot({
  
  #
  #Get data for linegraph and format for Rcharts plotting
  topicmodel <- CreateTopicModel()[["TopicModel"]]
  meta <- CreateTopicModel()[["Metadata"]]
  topicprob <- CreateTopicModel()[["TopicProb"]]
  
  #Create formula with selected topics
  formulatext <- paste(paste("c(", paste(unlist(topicclicks$selected), collapse = ","), ")"), "~ s(year)")
  topicformula <- as.formula(formulatext)
  
  #Graph selected topics over time
  graphdat <- estimateEffect(topicformula, topicmodel, metadata = meta)
  
  graphdat <- plot.estimateEffect(graphdat, covariate = "year", model = topicmodel, method = "continuous", xlab = "Year", printlegend = T, ci.level = 0.50)
  
  return(graphdat)
})

#Print most relevent sentences for selected topic(s) (experimental)
output$RelevantSentences <- renderText({
  
  #Get data for linegraph and format for Rcharts plotting
  topicmodel <- CreateTopicModel()[["TopicModel"]]
  meta <- CreateTopicModel()[["Metadata"]]
  topicprob <- CreateTopicModel()[["TopicProb"]]
  sentenceanalysis <- CreateTopicModel()[["SentenceTopics"]]
  sentencetopics <- sentenceanalysis$SentenceTopicPolarity
  
  #Create formula with selected topics
  selectedtopics <- c("entropy", paste("Topic", unlist(topicclicks$selected)))
  sentenceprob <- apply(sentencetopics[,selectedtopics], MARGIN = 1, prod)
  
  #Return top 5 sentences with highest combined product of all topics and sentence entropy
  output <- sentencetopics$text.var[order(sentenceprob, decreasing = TRUE)][1:5]
  
  return(output)
})

#Plot topic hierarchy
output$TopicHierarchy <- renderPlot({
  
  #
  #Get data for linegraph and format for Rcharts plotting
  topicmodel <- CreateTopicModel()[["TopicModel"]]
  #meta <- CreateTopicModel()[["Metadata"]]
  #topicprob <- CreateTopicModel()[["TopicProb"]]
  
  #Calculate cosine distance matrix
  cosdist <- proxy::dist(as.matrix(exp(topicmodel$beta$logbeta[[1]])), method = "cosine")
  
  #Cluster based on cosine distance
  cosclust <- hclust(cosdist, method = "ward.D")
  cosclust <- pvclust::pvclust(as.matrix(cosdist), method.hclust = "ward")
  
  #Create formula with selected topics
  formulatext <- paste(paste("c(", paste(unlist(topicclicks$selected), collapse = ","), ")"), "~ year")
  topicformula <- as.formula(formulatext)
  
  #Graph selected topics over time
  graphdat <- estimateEffect(topicformula, topicmodel, metadata = meta, uncertainty = "None")
  
  graphdat <- plot.estimateEffect(graphdat, covariate = "year", model = topicmodel, method = "continuous", xlab = "Year", printlegend = T, ci.level = 0)
  
  return(graphdat)
})

#Plot Topics in PCA reduced graph using LDAvis
output$TopicPCA <- renderVis({
  
  
  #Get data for histogram
  graphTopic <- CreateTopicModel()[["TopicPCAJSON"]]
  
  return(graphTopic)
})



})

