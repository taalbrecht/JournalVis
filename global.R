library(rentrez)
library(RISmed)
library(shiny)
library(rcytoscapejs)
library(DT)
library(XML)
library(httr)
library(antiword)
library(qdap)
library(qdapTools)
library(quanteda)
library(pdftools)
library(readtext)

#Enable bookmarking via url
#enableBookmarking(store = "server")

#Custom functions

CiteListPubmed <- function(elinkxml){
  #Function to return data frame of citations for articles in entrez link xml document.
  #Input:
  #elinkxml: xml file from entrez_link search with cmd = neighbor_score used as option
  #Output:
  #linkframe: data frame with two columns. The first column is the source article, the second is the article that cites it
  
  library(XML)
  
  
  linklist <- list()
  SourceArticle <- c()
  CitedBy <- c()
  ReferencedArticles <- c()
  CitationFrame <- c()
  
  #Parse pubmed xml section listing citations
  path <- "//LinkSetDb/LinkName[text()='pubmed_pubmed_citedin']/../Link/"
  SourceArticle <- (XML::xpathSApply(elinkxml, paste0(path, "Score"), XML::xmlValue))
  CitedBy <- (XML::xpathSApply(elinkxml, paste0(path, "Id"), XML::xmlValue))
  
  CitationFrame <- data.frame(SourceArticle, CitedBy, stringsAsFactors = FALSE)
  
  #Parse pubmed xml section listing references (returns monatomic list because articles are not tied to one another)
  path <- "//LinkSetDb/LinkName[text()='pubmed_pubmed_refs']/../Link/"
  ReferencedArticles <- (XML::xpathSApply(elinkxml, paste0(path, "Id"), XML::xmlValue))
  
  linklist <- list("CitationFrame" = CitationFrame, "ReferencedArticles" = ReferencedArticles)
  
  return(linklist)
  
}

GetPubmedCitationLinks <- function(articleidlist){
  #Function to get list of citing articles
  #Input: text vector of article ids (pubmedID only)
  #Output:
  #CitationFrame: dataframe of citations, data frame with two columns. The first column is the source article, the second is the article that cites it.
  #ReferenceList: compiled list of references found. No information on the linkages from each reference to the citing articles
  #TimesCited: Named vector of integers. The name of each element is the PMID, the integer is the number of citations found in Pubmed
  
  
  library(rentrez)
  
  CitationFrame <- c()
  ReferenceList <- c()
  TimesCited <- c()
  
  #call in blocks of 200 per pubmed API guidance
  
  if(length(articleidlist) > 0){
    
    for (i in 1:ceiling(length(articleidlist)/200)){
      
      #get 200 article IDs
      articlevect <- articleidlist[(1+200*(i-1)):min(length(articleidlist), 200*i)]
      
      #get citation linkage info from pubmed (note that entrez_link has the required 0.33 second delay built-in)
      linkxml <- entrez_link(db = "pubmed", dbfrom = "pubmed", cmd = "neighbor_score", id = articlevect)
      
      #parse citation linkage info to get list of source and citing articles as well as list of articles referenced
      CiteList <- CiteListPubmed(elinkxml = linkxml$file)
      
      #Combine Citation Frame
      CitationFrame <- rbind.data.frame(CitationFrame, CiteList$CitationFrame)
      
      #Combine ReferenceList
      ReferenceList <- c(ReferenceList, CiteList$ReferencedArticles)
      
    }
  }
  
  #############Perhaps implement this function later, pulls all referenced articles in as nodes as well
  #   #Look up citation details for referencelisttemp (pubmed does not differentiate reference article linkages, only citing article linkages)
  #   if(length(referencelisttemp) > 0){
  #     
  #     for (i in 1:ceiling(length(referencelisttemp)/200)){
  #       
  #       #get 200 article IDs
  #       articlevect <- referencelisttemp[(1+200*(i-1)):min(length(referencelisttemp), 200*i)]
  #       
  #       #get citation linkage info from pubmed
  #       linkxml <- entrez_link(db = "pubmed", dbfrom = "pubmed", cmd = "neighbor_score", id = articlevect)
  #       
  #       #parse citation linkage info to get list of source and citing articles as well as list of articles referenced
  #       CiteList <- CiteListPubmed(elinkxml = linkxml$file)
  #       
  #       #Combine Citation Frame
  #       CitationFrame <- rbind.data.frame(CitationFrame, CiteList$CitationFrame)
  #       
  #       #Combine ReferenceList
  #       ReferenceList <- c(ReferenceList, CiteList$ReferencedArticles)
  #       
  #     }
  #   }
  #######################
  #Eliminate duplicate citations
  CitationFrame <- unique.array(CitationFrame)
  
  #Count number of citations found per article
  #TimesCited <- summary.factor(CitationFrame[,1]) - replaced with code below to eliminate issues with arbitrary cutoff
  TimesCited <- summary.factor(CitationFrame[,1], maxsum = length(unique(CitationFrame[,1])))
  
  #Create matrix of citations that are outside of this article list:
  OutsideCitationFrame <- CitationFrame
  OutsideCitationFrame <- OutsideCitationFrame[unique(c(which(!(OutsideCitationFrame[,1] %in% articleidlist)),which(!(OutsideCitationFrame[,2] %in% articleidlist)))),]
  
  #Remove citations that point to articles that were not in original articleidlist - move to parent function
  CitationFrame <- CitationFrame[which(CitationFrame[,1] %in% articleidlist),]
  CitationFrame <- CitationFrame[which(CitationFrame[,2] %in% articleidlist),]
  
  #Remove references that point to articles in original articleidlist
  ReferenceList <- ReferenceList[!(ReferenceList %in% articleidlist)]
  
  output <- list("CitationFrame" = CitationFrame, "ReferenceList" = ReferenceList, "TimesCited" = TimesCited, "OutsideCitationFrame" = OutsideCitationFrame)
  
  return(output)
  
}

##NOT DONE##
#Basic search function for SCOPUS. Returns article metadata that can be processed by additional functions in the future.
ScopusSearch <- function(query, apiKey, getall = FALSE){
  #Inputs:
  #query: search string. Can use all SCOPUS search string modifiers
  #apiKey: SCOPUS api key as string. Can be obtained for free from SCOPUS API site.
  #getall: Set as FALSE or TRUE. If FALSE, simply returns an article count and the first 25 results. If TRUE, will download article metadata for all articles in search.
  
  resultframe <- c()
  resultcount <- NULL
  
  ScopusURLBase <- "http://api.elsevier.com/content/search/scopus"
  
  result <- httr::GET(ScopusURLBase, query = list(query = query, apiKey = apiKey), verbose())
  
  #Get count of search results
  resultcount <- as.numeric(content(result)$`search-results`$`opensearch:totalResults`)
  
  #Option to only get article count and scopus IDs for further processing
  #result <- httr::GET(ScopusURLBase, query = list(query = query, apiKey = apiKey, field = "dc:identifier", httpAccept = "application/xml"))
  
  #Soon get results in 25 article count blocks. Set "start" header at 26, then 51, etc.
  
  resultframe <- jsonlite::fromJSON(content(result, as = "text"))$`search-results`$entry
  
  if((getall = TRUE) & (resultcount > 25)){
    
    for (i in 2:ceiling(resultcount/25)){
      
      #Get next 25 articles (max number of results permitted)
      result <- httr::GET(ScopusURLBase, query = list(query = query, apiKey = apiKey, start = (1+25*(i-1))), verbose())
      
      #get citation linkage info from pubmed (note that entrez_link has the required 0.33 second delay built-in)
      resulttempframe <- jsonlite::fromJSON(content(result, as = "text"))$`search-results`$entry
      
      #Combine Results Frame
      resultframe <- rbind.data.frame(resultframe, resulttempframe)

      
    }
    
  }
  output <- list("ResultCount" = resultcount, "ResultFrame" = resultframe)
  return(output)
  
}


##NOT DONE##
ScopusAbstractDetailsGet <- function(scopus_ids, apiKey){
  #Inputs:
  #scopus_ids: List or vector containing all scopus article IDs to retrieve details for
  #apiKey: SCOPUS api key as string. Can be obtained for free from SCOPUS API site.
  
  resultframe <- c()
  
  ScopusURLBase <- "http://api.elsevier.com/content/abstract/scopus_id/"
  
  
  if(is.numeric(scopus_ids[1])){
  for (i in 1:length(scopus_ids)){
    
    ScopusURL <- paste0(ScopusURLBase, scopus_ids[i])
    
    result <- httr::GET(ScopusURL, query = list(apiKey = apiKey), verbose())  
    
    #get citation linkage info from pubmed (note that entrez_link has the required 0.33 second delay built-in)
    resulttempframe <- jsonlite::fromJSON(content(result, as = "text"))$`search-results`$entry
    
    #Combine Results Frame
    resultframe <- rbind.data.frame(resultframe, resulttempframe)
    
    
  }
  }else{
    
    #If scopus ID does not appear to be properly formatted, return empty data frame to prevent quota overruns
    resultframe <- data.frame()
    
  }
  
}


#NEVER USE JSON. Basic search function for EPO patent OPS search service. Returns patent metadata that can be processed further in the future.
OPSSearch <- function(query, getall = FALSE){
  #Inputs:
  #query: search string. Can use all OPS search string modifiers. Will automatically be URL encoded by GET function below.
  #getall: Set as FALSE or TRUE. If FALSE, simply returns an article count and the first 25 results. If TRUE, will download article metadata for all articles in search.
  
  resultlist <- list()
  resultcount <- NULL
  resultlim <- 2
  
  if(getall == TRUE){
    
    resultlim <- 100
    
  }
  
  
  URLBase <- "http://ops.epo.org/3.1/rest-services/published-data/search"
  
  result <- httr::GET(URLBase, query = list(q = query, Range = paste(1, resultlim, sep = "-")), add_headers(Accept = "application/xml"), verbose())
  
  #XML parsing of results
  resultparsed <- XML::xmlInternalTreeParse(result)
  
  #Get result count
  resultcount <- as.numeric(XML::xpathSApply(resultparsed, "//ops:biblio-search", XML::xmlGetAttr, "total-result-count"))
  
  #Get list of results
  resultlist <- paste0(XML::xpathSApply(resultparsed, "//x:country", namespaces = "x", XML::xmlValue),
                       XML::xpathSApply(resultparsed, "//x:doc-number", namespaces = "x", XML::xmlValue))
  
  #Sleep for 6 seconds to meet requirements of OPS (10 query per minute max)
  Sys.sleep(6)
  
  #resultparsed <- jsonlite::fromJSON(iconv(result))
  
  #resultcount <- as.numeric(resultparsed$`ops:world-patent-data`$`ops:biblio-search`$`@total-result-count`)
  
  #resultlist <- resultparsed$`ops:world-patent-data`$`ops:biblio-search`$`ops:search-result`$`ops:publication-reference`$`document-id`
  
#   if(is.null(resultlist) == FALSE){
#   
#   Convert results to list with a row for each element
#   resultlist <- split(resultlist, rownames(resultlist))
#   
#   }
  
  #Get remaining search results in batches of 100 (max batch size)
  if((getall == TRUE) & (resultcount > resultlim)){
    
    for (i in 2:ceiling(resultcount/resultlim)){
      
      #Sleep for 6 seconds to meet requirements of OPS (10 query per minute max)
      Sys.sleep(6)
      
#       #Get next 25 articles (max number of results permitted)
#       result <- httr::GET(URLBase, query = list(q = query, Range = paste(1+resultlim*(i-1), resultlim*(i), sep = "-")), add_headers(Accept = "application/json"))
#       
#       #get citation linkage info from pubmed (note that entrez_link has the required 0.33 second delay built-in)
#       resultparsed <- jsonlite::fromJSON(iconv(result))
#       
#       resulttemplist <- resultparsed$`ops:world-patent-data`$`ops:biblio-search`$`ops:search-result`$`ops:publication-reference`$`document-id`
#       
#       #Convert results to list with a row for each element
#       resulttemplist <- split(resulttemplist, rownames(resulttemplist))
#       
#       #Combine Results List
#       resultlist <- append(resultlist, resulttemplist)
      
      result <- httr::GET(URLBase, query = list(q = query, Range = paste((i-1)*100+1, min(resultcount,(i*100)), sep = "-")), add_headers(Accept = "application/xml"), verbose())
      
      #XML parsing of results
      resultparsed <- XML::xmlInternalTreeParse(result)
      
      #Collect article ID list
      resulttemplist <- paste0(XML::xpathSApply(resultparsed, "//x:country", namespaces = "x", XML::xmlValue),
                               XML::xpathSApply(resultparsed, "//x:doc-number", namespaces = "x", XML::xmlValue))

      #Combine Results List
      resultlist <- append(resultlist, resulttemplist)
      
    }
    
  }
  output <- list("ResultCount" = resultcount, "ResultList" = resultlist)
  return(output)
  
}


#Abstract and Bibliography fetch for EPO patent OPS search service. Returns patent metadata that can be processed further in the future.
OPSAbsBib <- function(idlist, country = "US", combineduplicates = TRUE){
  #Inputs:
  #idlist: Vector of patent IDs to search for. Should include country code in front with no space, example: "US2013253645". Will be filtered by "country" input vector to only include those country codes
  #getall: Set as FALSE or TRUE. If FALSE, simply returns an article count and the first 25 results. If TRUE, will download article metadata for all articles in search.
  #country: filter idlist to only include country codes in this list. Important for patents that may not return all relevant data
  #browser()
  
  resultlist <- list()
  resultcount <- NULL
  resultlim <- 100
  ids <- list()
  famids <- list()
  appids <- list()
  titles <- list()
  abstracts <- list()
  references <- list()
  Patclasses <- list()
  nonpatreferences <- list()
  assignees <- list()
  inventors <- list()
  priorityyears <- list()
  CitationFrame <- c()
  ReferenceList <- c()
  TimesCited <- c()
  
  #Filter idlist to remove invalid entries
    #remove entries that contain a space, as they are tied to "invalid entry"
  if(length(grep(" ", idlist)) > 0){
  idlist <- idlist[-grep(" ", idlist)]
  }
  
  for (i in 1:ceiling(length(idlist)/resultlim)){
  
    #Sleep for 6 seconds to meet requirements of OPS (10 query per minute max)
    Sys.sleep(6)
    
  URLBase <- paste("http://ops.epo.org/3.1/rest-services/published-data/publication/epodoc/",
                   paste(idlist[(1+(i-1)*resultlim):min(resultlim*i, length(idlist))], collapse = ","),
                   "/biblio", sep = "")
  
  
  result <- httr::GET(URLBase, add_headers(Accept = "application/xml"), verbose())
  
  ##XML parsing of results
  resultparsed <- XML::xmlInternalTreeParse(result)
  
  for(j in 1:length(XML::getNodeSet(resultparsed, "//x:exchange-document", "x"))){
    
    resultxmlsub <- XML::xmlClone(XML::getNodeSet(resultparsed, "//x:exchange-document", "x")[[j]])
    
    #Get ID (EPO ID selected that includes country code)
    ids[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:publication-reference/x:document-id[@document-id-type='epodoc']/x:doc-number", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(ids[(i-1)*resultlim +j])) == TRUE){
      ids[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get Family ID (number traces to all manifestations of the same patent in different geographies)
    famids[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:exchange-document", namespaces = "x", XML::xmlGetAttr, "family-id")
    
    if(is.null(unlist(famids[(i-1)*resultlim +j])) == TRUE){
      famids[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get application ID(s) that match patent number
    appids[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:application-reference/x:document-id[@document-id-type='epodoc']/x:doc-number", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(appids[(i-1)*resultlim +j])) == TRUE){
      appids[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get title
    titles[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:invention-title[@lang='en']", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(titles[(i-1)*resultlim +j])) == TRUE){
      titles[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get abstract
    abstracts[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:abstract[@lang='en']", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(abstracts[(i-1)*resultlim +j])) == TRUE){
      abstracts[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get CPC classifications
    Patclasses[[(i-1)*resultlim +j]] <- paste(XML::xpathSApply(resultxmlsub, "//x:classification-scheme", namespaces = "x", XML::xmlGetAttr, "scheme"),
                                              XML::xpathSApply(resultxmlsub, "//x:patent-classification", namespaces = "x", XML::xmlValue))
    
    if(is.null(unlist(Patclasses[(i-1)*resultlim +j])) == TRUE){
      Patclasses[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get references
    references[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:patcit[@dnum-type='publication number']/x:document-id[@document-id-type='epodoc']/x:doc-number", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(references[(i-1)*resultlim +j])) == TRUE){
      references[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get non-patent references
    nonpatreferences[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:nplcit/x:text", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(nonpatreferences[(i-1)*resultlim +j])) == TRUE){
      nonpatreferences[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get original assignee
    assignees[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:applicant[@data-format='epodoc']//x:name", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(assignees[(i-1)*resultlim +j])) == TRUE){
      assignees[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get inventors
    inventors[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:inventor[@data-format='epodoc']//x:name", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(inventors[(i-1)*resultlim +j])) == TRUE){
      inventors[[(i-1)*resultlim +j]] <- NA
    }
    
    #Get priority years
    priorityyears[[(i-1)*resultlim +j]] <- XML::xpathSApply(resultxmlsub, "//x:priority-claim/x:document-id[@document-id-type='epodoc']/x:date", namespaces = "x", XML::xmlValue)
    
    if(is.null(unlist(priorityyears[(i-1)*resultlim +j])) == TRUE){
      priorityyears[[(i-1)*resultlim +j]] <- NA
    }
    
    
  }
  
  
  }
  
#   #Combine all results into list for easier post-processing
#   outlist <- list("ids" = ids,
#                   "famids" = famids,
#                   "appids" = appids,
#                   "titles" = titles,
#                   "abstracts" = abstracts,
#                   "references" = references,
#                   "nonpatreferences" = nonpatreferences,
#                   "assignees" = assignees,
#                   "inventors" = inventors,
#                   "priorityyears" = priorityyears)
  
  #Combine all results into list for easier post-processing using family id as unique identifier
  #Make sure grouping ID is in first item of list
  outlist <- list("ids" = famids,
                  "docids" = ids,
                  "appids" = appids,
                  "titles" = titles,
                  "abstracts" = abstracts,
                  "patentclasses" = Patclasses,
                  "references" = references,
                  "nonpatreferences" = nonpatreferences,
                  "assignees" = assignees,
                  "inventors" = inventors,
                  "priorityyears" = priorityyears)
  
  #For patents with no family id, use patent number as unique identifier
  outlist$ids[which(is.na(outlist$ids))] <- paste0("NoFamily-",outlist$docids[which(is.na(outlist$ids))])
  
  #Combine duplicate entries and regional patent variations that have the same family id and keep all unique info - test code. Do this in shiny server for now
  #browser()
  if((sum(duplicated(outlist$ids)) > 0) & (combineduplicates == TRUE)){
    for (i in 1:sum(duplicated(outlist$ids))){
      
      dupeloop <- which(outlist$ids == outlist$ids[duplicated(outlist$ids)][[i]])
      
      for (j in 1:length(dupeloop)){
        
        for(k in 2:length(outlist)){
        
          #Combine unique info from the two duplicated elements
          combinedinfo <- unique(unlist(outlist[[k]][dupeloop], recursive = FALSE))
          
          #If there are more than two elements, remove redundant NAs
          if((length(combinedinfo) > 1) & (sum(is.na(combinedinfo)) > 0)){
           
            #Remove all NAs
            ##combinedinfo <- Filter(function(x) !(is.na(x)), combinedinfo)
            #Old code above, bugfix attempt below
            combinedinfo <- combinedinfo[!is.na(combinedinfo)]
            
            #If there are no elements left, add a single NA
            if (length(combinedinfo) == 0){
              
              combinedinfo <- NA
              
            }
             
          }
          
          #Enter identical info into all duplicate rows
          outlist[[k]][[dupeloop[j]]] <- combinedinfo
        
        }
      }
      
    }
    
    #Remove duplicated rows
    keepvect <- !duplicated(outlist$ids)
    
    for(i in 1:length(outlist)){
    
      outlist[[i]] <- outlist[[i]][keepvect]

    }
    
  }
  
#   for(i in 1:length(outlist$ids)){
#     
#     CitationFrame <- rbind.data.frame(CitationFrame, data.frame(outlist$references[[i]], outlist$ids[[i]], stringsAsFactors = FALSE))
#     
#   }
  
  #Create dataframe to find reference structure in patent documents and trace to family ID
  for(i in 1:length(outlist$ids)){
    
    tempframe <- expand.grid(outlist$references[[i]], outlist$docids[[i]], stringsAsFactors = FALSE)
    tempframe$Var3 <- outlist$ids[[i]]
    
    CitationFrame <- rbind.data.frame(CitationFrame, tempframe)
    
  }
  
  #Eliminate duplicate citations
  CitationFrame <- unique.array(CitationFrame)
  
  #Eliminate rows containing NA
  CitationFrame <- CitationFrame[complete.cases(CitationFrame),]
  
  #Find source family ID for references in collection
  tracelist <- lapply(CitationFrame[,1], function(x, listrep = outlist) listrep$ids[[grep(x, listrep$docids)[1]]])
  tracelist[sapply(tracelist, is.null)] <- NA
  CitationFrame$Var4 <- unlist(tracelist)
  
  #Create list of all references that are NOT in the original collection
  ReferenceList <- unique(CitationFrame[,1])
  ReferenceList <- ReferenceList[!(ReferenceList %in% unlist(outlist$docids))]
  
  #Reduce citation frame to only complete family ID cases
  CitationFrame <- CitationFrame[,c(4,3)]
  
  #Name columns
  colnames(CitationFrame) <- c("source", "target")
  
  #Eliminate duplicate citations
  CitationFrame <- unique.array(CitationFrame)
  
  #Eliminate rows containing NA
  CitationFrame <- CitationFrame[complete.cases(CitationFrame),]
  
  #Count number of citations found per article
  #TimesCited <- summary.factor(CitationFrame[,1]) - modified to eliminate arbitrary upper limit to number of results returned
  TimesCited <- summary.factor(CitationFrame[,1], maxsum = length(unique(CitationFrame[,1])))

  output <- list("ID" = outlist$ids,
                 "PatentID" = outlist$docids,
                 "ApplicationID" = outlist$appids,
                 "Title" = outlist$titles,
                 "Abstract" = outlist$abstracts,
                 "PatentClassifications" = outlist$patentclasses,
                 "References" = outlist$references,
                 "NonPatentReferences" = outlist$nonpatreferences,
                 "Assignee" = outlist$assignees,
                 "Inventors" = outlist$inventors,
                 "PriorityYears" = outlist$priorityyears,
                 "CitationFrame" = CitationFrame,
                 "TimesCited" = TimesCited,
                 "ReferenceList" = ReferenceList)
  return(output)
  
}



##NOT DONE##
# #Perform search in Mendeley 
# MendeleySearch <- function(query, authtoken, type = c("journal"), getall = FALSE){
#   
#   #Inputs:
#   #query: search string. Can use all Mendeley search string modifiers. See examples here https://api.mendeley.com/apidocs
#   #type: restricts search results to only one type of result. Can also be left blank to return search results from any type
#   #authtoken: Mendeley authorization token. Currently needs token provided here: https://mendeley-show-me-access-tokens.herokuapp.com/
#   #getall: Set as FALSE or TRUE. If FALSE, simply returns an article count and the first 10 results. If TRUE, will download article metadata and abstracts for all articles in search.
#   
#   resultlist <- list()
#   resultcount <- NULL
#   
#   resultlim <- 10
#   if(getall == TRUE){
#     
#     resultlim <- 5
#     
#   }
#   
#   query <- gsub("[ ]", replacement = "+", query)
#   
#   URLBase <- "https://api.mendeley.com/search/catalog"
#   
#   result <- httr::GET(URLBase, query = list(query = query, limit = resultlim), add_headers(Authorization = paste("Bearer", authtoken)))
#   
#   resultcount <- as.numeric(headers(result)$`mendeley-count`)
#   
#   #Decode resulting response and convert json to dataframe
#   resultlist <- jsonlite::fromJSON(iconv(result))
#   
#   #Convert results to list with a row for each element
#   resultlist <- split(resultlist, rownames(resultlist))
#   
#   if((getall = TRUE) & (resultcount > resultlim)){
#     
#     for (i in 2:ceiling(resultcount/resultlim)){
#       
#       #Sleep for 5 seconds to prevent overloading Mendeley server
#       Sys.sleep(5)
#       
#       #Get all headers with "link" name
#       nextlink <- headers(result)
#       nextlink <- nextlink[which(names(nextlink) == "link")]
#       
#       #Select "link" header with "next" in string 
#       nextlink <- nextlink[grep("next", nextlink)]
#       
#       #Get only link characters
#       nextlink <- substr(nextlink, regexpr("<", nextlink)+1, regexpr(">", nextlink)-1)
#       
#       #Get next 100 articles (max number of results permitted)
#       result <- httr::GET(nextlink, add_headers(Authorization = paste("Bearer", authtoken)))
#       
#       #Convert results to dataframe
#       resulttemplist <- jsonlite::fromJSON(iconv(result))
#       
#       #name rows according to result number
#       rownames(resulttempframe) <- seq(from = i*resultlim+1, to = nrow(resulttempframe)*i+resultlim)
#       
#       #Convert results to list with a row for each element
#       resulttemplist <- split(resulttemplist, rownames(resulttemplist))
#       
#       #Combine Results Frame
#       resultlist <- append(resultlist, resulttemplist)
#       
#     }
#     
#   }
#   
#   #Convert
#   
#   output <- list("ResultCount" = resultcount, "ResultList" = resultlist)
#   return(output)
#   
# }

#Function to create principal component representation of topics in JSON format

TopicStmPCAJSON <- function (mod, docs, R = 30, plot.opts = list(xlab = "PC1", ylab = "PC2"), 
          lambda.step = 0.1, out.dir = tempfile(), open.browser = interactive(), 
          as.gist = FALSE) 
{
  if (!requireNamespace("LDAvis", quietly = TRUE)) 
    stop("Please install LDAvis package to use this function. You will also need servr.")
  theta <- mod$theta
  if (length(mod$beta$logbeta) > 1) 
    stop("This function does not yet allow content covariates.")
  phi <- exp(mod$beta$logbeta[[1]])
  if (any(phi == 0)) {
    phi <- phi + 1e-05
    phi <- phi/rowSums(phi)
  }
  vocab <- mod$vocab
  
  #This is the original code, nodes are sized based on number of terms.
   doc.length <- as.integer(unlist(lapply(docs, function(x) sum(x[2, 
                                                                  ]))))
  #Experimental code to size topics based on summed proportion of each document they make up. This number goes into a calculation to size nodes simply by sum of theta parameter for each topic. It also affects word frequency calculations for the visualizations
  #doc.length <- rep(1, times = nrow(theta))*100
  
  term.frequency <- mod$settings$dim$wcounts$x
  LDAJSON <- LDAvis::createJSON(phi = phi, theta = theta, doc.length = doc.length, 
                          vocab = vocab, term.frequency = term.frequency, lambda.step = lambda.step)
  
  return(LDAJSON)

}

#Function to find topics with highest probability of being associated with keywords (should be included with stm but is not loading for some reason)
wordTopicProbs <- function(x, wordlist) {
  #Inputs:
  #x: stm object
  #wordlist: vector of words as character strings. Will match words assuming regex type matching.
  
  #Check for proper object class
  if(class(x)!="STM"){
    stop("x must be an STM object")}
  
  #Loop to calculate summed probability for each word in
  probvect <- c()
  probmat <- c()
  wordsnotpresent <- c()
  #search through topic model vocab
#   vocabmatch <- which(x$vocab %in% wordlist)
#   
#   browser()
#   
#   if(length(vocabmatch > 0)){
#     
#     #Convert log word probability matrix to normal probability matrix
#     probmat <- as.matrix(x$beta$logbeta[[1]][,vocabmatch])
#     
#     #Normalize probabilities for each word. (Each individual word probability normalized to sum to 1 over all topics)
#     sumprobvect <- log(apply(probmat, MARGIN = 2, FUN = function(x) sum(exp(x))))
#     probmat <- exp(scale(probmat, center= sumprobvect, scale = FALSE))
#     
#     #Calculate product of probability for each topic
#     probvect <- apply(probmat, MARGIN = 1, FUN = prod)
#     names(probvect) <- paste("Topic", seq(1:length(probvect)))
#     
#   }
#   
#   wordsnotpresent <- wordlist[!(wordlist %in% x$vocab)]
  
  if(length(wordlist) > 0){
    
    #Loop through statement below for each regex word in wordlist
    for(i in 1:length(wordlist)){
     
      vocabmatch <- grep(wordlist[[i]], x$vocab)
      
      #Sum probabilities for all matched words (regex can be used to restrict partial word matches)
      if(length(vocabmatch > 0)){

        #Select only word columns that match wordlist item and sum probabilities of matched word(s) and convert back to log base e
        probmatsub <- as.matrix(log(apply(as.matrix(x$beta$logbeta[[1]][,vocabmatch]), MARGIN = 1, FUN = function(x) sum(exp(x)))))
        
        #Add summed probabilities to overall wordlist by topic probability matrix
        probmat <- cbind(probmat, probmatsub) 
        
      }else{
        
        #If no word match, add to list of words not present
        wordsnotpresent <- c(wordsnotpresent, wordlist[[i]])
        
      } 
      
      
    }
    
    #Normalize probabilities for each word and escape out of log scale to normal scale. (Each individual word probability normalized to sum to 1 over all topics)
    sumprobvect <- log(apply(probmat, MARGIN = 2, FUN = function(x) sum(exp(x))))
    probmat <- exp(scale(probmat, center= sumprobvect, scale = FALSE))
    
    #Calculate product of probability for each topic
    probvect <- apply(probmat, MARGIN = 1, FUN = prod)
    names(probvect) <- paste("Topic", seq(1:length(probvect)))
    
  }
  
  return(structure(list("TopicMatchProbability" = probvect, "MissingWords" = wordsnotpresent)))
  
}

#Function to find topics with highest probability of being associated with keywords
ArticleTopicPolar <- function(stmmod, articlelist, usestem = FALSE) {
  #Inputs:
  ##stmmod: correlated topic model (stm object) from package "stm"
  ##articlelist: vector or list of article text to be analyzed for polarity. article number in list must match position in stm object
  ##usestem: whether to allow stemmed word matches in polarity calculations or not.
  ##########Example: "I hate dunes" would also apply negative sentiment to "dune" if stemming is used but would not apply if it isn't
  #Output:
  ##DocumentTopicPolarity: data frame with polarity broken down by article and topic. Topics are in columns and articles are rows.
  #########################Polarity is normailized based on topic proportion of article.
  ##SentenceTopicPolarity: data frame with polarity by sentence. Each row contains one sentence from the original aritcle colleciton.
  #########################Sentence text = "doc", topic proportion for each sentence = "Topic x" (one for each topic),
  #########################polarity for each sentence by topic = "Polarity x" (one for each topic). Formula currently used is topic proportion*overall sentence polarity
  #browser()
  
  library(qdap)
  library(quanteda)
  
  #Check for proper object class
  if(class(stmmod)!="STM"){
    stop("stmmod must be an STM object")}
  
  #Create sentence corpus
  sentcorp = tokens(corpus(as.character(unlist(articlelist)), docnames = c(1:length(unlist(articlelist)))), what = "sentence")
  
  #Construct data frame with parent article number and associated sentences
  docnums = rep(1:length(unlist(articlelist)), sapply(sentcorp, length))
  sentdocframe = data.frame(as.character(sentcorp), docnums)
  colnames(sentdocframe) <- c("doc", "num")
  
  #Calculate polarity of each sentence
  sentdocframe <- cbind(sentdocframe, qdap::polarity(sentdocframe$doc, constrain = TRUE)$all)
  
  #Reprocess as corpus
  sentencecorpus <- qdap::as.Corpus(text.var = sentdocframe$doc, grouping.var = c(1:nrow(sentdocframe)), demographic.vars = sentdocframe[,c("num", "polarity")])
  sentencecorpus <- as.data.frame(sentencecorpus)
  
  #Substitute multiword phrase tokens back into corpus
  multdef <- UniqueSub(stmmod$vocab[grep(" ", stmmod$vocab, fixed = TRUE)], sentencecorpus$text)
  sentencecorpus <- qdap::as.dtm(multdef$keyeddocs, sentencecorpus$doc_id)
  colnames(sentencecorpus) <- mgsub(as.character(multdef$keyframe$Key), as.character(multdef$keyframe$OrigString), colnames(sentencecorpus), ignore.case = TRUE)
  
  #Generate a matrix that detects for presence of each word in the vocab file of the stm model in each sentence of the polarity matrix.
  #If the word is present, the matrix has the polarity of the sentence, if it is not, it will be NA
  vocabpolar <- matrix(data = NA, nrow = nrow(sentdocframe), ncol = length(stmmod$vocab))
  vocabpolar <- data.frame(vocabpolar)
  colnames(vocabpolar) <- stmmod$vocab
  
  #Generate stemmed lists if usestem == TRUE
  
  vocabpolarstem <- c()
  vocabtermsstem <- c()
  
  if(usestem == TRUE){
  
    vocabpolarstem <- stmmod$vocab
    vocabtermsstem <- sentencecorpus$dimnames$Terms
    
  vocabpolarstem[grep(" ", vocabpolarstem, fixed = TRUE, invert = TRUE)] <- stemmer(vocabpolarstem[grep(" ", vocabpolarstem, fixed = TRUE, invert = TRUE)], warn = FALSE, capitalize = FALSE)
  vocabtermsstem[grep(" ", vocabtermsstem, fixed = TRUE, invert = TRUE)] <- stemmer(vocabtermsstem[grep(" ", vocabtermsstem, fixed = TRUE, invert = TRUE)], warn = FALSE, capitalize = FALSE)
  
  }
    
  ##Original method that didn't do as well with splitting sentences up amongst topics. Would tend to assign all sentences to dominant topic from article.
#   for(i in 1:ncol(vocabpolar)){
#     
#     termIDs <- union(which(sentencecorpus$dimnames$Terms %in% c(colnames(vocabpolar)[[i]], vocabpolarstem[[i]])),
#                     which(vocabtermsstem %in% c(colnames(vocabpolar)[[i]], vocabpolarstem[[i]])))
#     
#     if(length(termIDs) > 0){
#     
#       #Match specific terms
#       usevect <- as.integer(sentencecorpus$dimnames$Docs[unique(sentencecorpus$i[which(sentencecorpus$j %in% termIDs)])])
#     
#       #Old approach with more general term matching based on partial strings
#     #usevect <- grep(colnames(vocabpolar)[[i]], sentdocframe$doc)
#     
#     vocabpolar[usevect,i] <- sentdocframe$polarity[usevect]
#     
#     }
#     
#   }
#   
#   #Generate a word by topic probability matrix (word = column, topic = row)
#   topicwordprobs <- exp(stmmod$beta$logbeta[[1]])
#   
#   #Generate the topic by document polarity matrix (topic = column, document = row)
#   doctopicpolar <- matrix(nrow = nrow(stmmod$theta), ncol = ncol(stmmod$theta), data = NA)
#   
#   for(i in 1:length(unique(sentdocframe$num))){
#     
#     #Determine which rows of the polarity frame apply to this document
#     usevect <- which(sentdocframe$num == unique(sentdocframe$num)[[i]])
#     
#     #Calculate word probabilities by topic for this specific document
#     doctopicwordprob <- topicwordprobs*stmmod$theta[unique(sentdocframe$num)[[i]], ]
#     
#     #Calculate the average polarity of each word in the stm model vocab for this specific document
#     meanwordpolar <- apply(vocabpolar[usevect,], MARGIN = 2, mean, na.rm = TRUE)
#     
#     #Apply doctopicwordprob weightings to the polarity ratings for each word in this document
#     doctopicwordprob <- t(doctopicwordprob)*meanwordpolar
#     
#     #Apply those polarities over the word probabilities by topic for this specific document to generate a vector of polarities by topic for this specific document
#     doctopicpolar[unique(sentdocframe$num)[[i]],] <- apply(doctopicwordprob, MARGIN = 2, sum, na.rm = TRUE)
#     
#   }
  
  #Alternate method that considers each sentence and ensures that the polarity for each sentence is fully divided amongst the topics.
  #This prevents shrinkage of polarity meaning in a document where only one sentence mentions that topic but the sentence is very polar.
  
  #Generate a word by topic probability matrix (word = column, topic = row)
  topicwordprobs <- exp(stmmod$beta$logbeta[[1]])
  
  #Generate the topic by document polarity matrix (topic = column, document = row)
  doctopicpolar <- matrix(nrow = nrow(stmmod$theta), ncol = ncol(stmmod$theta), data = NA)
  
  #Create topic proportion columns in sentdocframe
  sentdocframe[paste("Topic", c(1:ncol(stmmod$theta)))] <- NA
  
  #Create topic proportion columns in sentdocframe
  sentdocframe[paste("Polarity", c(1:ncol(stmmod$theta)))] <- NA
  
  ##Slow code, translate to apply type function and redesign to use original duples instead of sparse matrix 
  #Calculate matrix that has word count per sentence translated to vocabulary used in stmmod
      for(i in 1:ncol(vocabpolar)){
        
        termIDs <- union(which(sentencecorpus$dimnames$Terms %in% c(colnames(vocabpolar)[[i]], vocabpolarstem[[i]])),
                        which(vocabtermsstem %in% c(colnames(vocabpolar)[[i]], vocabpolarstem[[i]])))
        
        if(length(termIDs) > 0){
          
          sentcorppos <- unique(sentencecorpus$i[which(sentencecorpus$j %in% termIDs)])
          
          for (j in 1:length(sentcorppos)){
          
          #Add term count to vocab polarity matrix
          
            sentid <- as.integer(sentencecorpus$dimnames$Docs[sentcorppos[j]])
            
            #Old code below. Newer code that will hopefully be faster on last uncommented line
            vocabpolar[sentid,i] <- sum(sentencecorpus$v[intersect(which(sentencecorpus$j %in% termIDs), which(sentencecorpus$i %in% sentcorppos[j]))])
            #vocabpolar[sentid,i] <- sum(sentencecorpus$v[as.logical((sentencecorpus$j %in% termIDs)*(sentencecorpus$i %in% sentcorppos[j]))])
            #vocabpolar[sentid,i] <- sum(sentencecorpus$v[as.logical((sentencecorpus$j %in% termIDs)*(sentencecorpus$i %in% sentcorppos[j]))])
          
          }
          
        }
        
      }
  
       
  ##Slow code, translate to apply type function and redesign to use original duples instead of sparse matrix
  #Create vectors of column numbers that contain topics and polarity for each topic an
  topiccols <-  grep("Topic", colnames(sentdocframe))
  polaritycols <- grep("Polarity", colnames(sentdocframe))
  
  #Convert vocabpolar to a matrix before getting into loop below because as.matrix is very slow
  vocabpolarmatrix <- as.matrix(vocabpolar)
  
  for (i in 1:nrow(sentdocframe)){
    
    #Calculate word probabilities by topic for this specific document
    doctopicwordprob <- topicwordprobs*stmmod$theta[sentdocframe$num[[i]],]
    
    #Normalize document specific word probabilities -- test to see if this is better or not
    doctopicwordprob <- t(t(doctopicwordprob)/apply(doctopicwordprob, MARGIN = 2, sum))
    
    #Calculate probability that each sentence belongs to a given topic
    sentdocframe[i, topiccols] <- apply(t(doctopicwordprob)*vocabpolarmatrix[i,], MARGIN = 2, sum, na.rm = TRUE)
    
  }
    #Normalize sentence probabilities so that the probability of topic membership of any sentence = 1
    sentdocframe[, topiccols] <- sentdocframe[, topiccols]/apply(sentdocframe[, topiccols], MARGIN = 1, sum)
    
    #Spread polarity based on topic proportion
    sentdocframe[, polaritycols] <- sentdocframe[, topiccols]*sentdocframe$polarity
    
    for(i in 1:nrow(doctopicpolar)){

    #Apply those polarities over the word probabilities by topic for this specific document to generate a vector of polarities by topic for this specific document
    doctopicpolar[i,] <- apply(sentdocframe[sentdocframe$num == i, polaritycols], MARGIN = 2, sum, na.rm = TRUE)
    
    #Divide polarity values for article by total sentences in article about each topic
    doctopicpolar[i,] <- doctopicpolar[i,]/apply(sentdocframe[sentdocframe$num == i, topiccols], MARGIN = 2, sum, na.rm = TRUE)
    
    }
    
  
#   #Alternate method using simply polar word probability in each document and topic and ignoring sentence by sentence assignment
#   #Method does not work very well
#   #Find polar words that are in stmmod vocabulary and create dataframe of these words and associated polarity:
#   #Include stemmed word matching later if this works
#   stmpolar <- data.frame(stmmod$vocab[which(stmmod$vocab %in% qdapDictionaries::key.pol$x)],
#                          which(stmmod$vocab %in% qdapDictionaries::key.pol$x),
#                        qdapDictionaries::key.pol$y[which(qdapDictionaries::key.pol$x %in% stmmod$vocab)])
#   colnames(stmpolar) <- c("token", "vocabpos", "polar")
#   
#   for(i in 1:nrow(doctopicpolar)){
#     
#     #Calculate word probabilities by topic for this specific document
#     doctopicwordprob <- topicwordprobs*stmmod$theta[i, ]
#     
#     #Filter doctopicwordprob columns to only include words in stmpolar 
#     doctopicwordprob <- t(doctopicwordprob[,stmpolar$vocabpos])*stmpolar$polar
#     
#     #Apply those polarities over the word probabilities by topic for this specific document to generate a vector of polarities by topic for this specific document
#     doctopicpolar[i,] <- apply(doctopicwordprob, MARGIN = 2, sum, na.rm = TRUE)
#     
#   }
  
    ##################Sentence entropy calc##################
    #Calculate entropy of sentence using rough word count approximation model of english
    #Formula is log2(10) + sum(n = 1 to N) of 0.1*log(n)/n
    
    #Create entropy frame for reference
    entframe <- data.frame(wordcount = seq(from = 0, to = max(sentdocframe$wc), by = 1), entropy = seq(from = 0, to = max(sentdocframe$wc), by = 1))
    
    #Fill in entropy for word count = 1
    entframe$entropy[which(entframe$wordcount == 1)] <- log(10,2)
    
    #Set default entropy to 0
    sentdocframe$entropy <- 0
    
    #Populate entropy using formula: log2(10) + sum(n = 1 to N) of 0.1*log(n)/n
    for(i in 2:max(entframe$wordcount)){
      
      entframe$entropy[which(entframe$wordcount == i)] <- entframe$entropy[which(entframe$wordcount == (i-1))] + 0.1*log(i,2)/i
      
      #Enter entropy value into all word counts in sentdocframe
      sentdocframe$entropy[sentdocframe$wc == i] <- entframe$entropy[which(entframe$wordcount == (i-1))] + 0.1*log(i,2)/i
      
    }
    
    #Merge entropy frame with sentdocframe. Old code, integrated into loop above since merge is a broken function
    #Merge function is a complete piece of shit that constantly causes problems, deletes data, and randomly ruins data frames so don't use it anymore
    #sentdocframe <- merge(sentdocframe, entframe, by.x = c("wc"), by.y = c("wordcount"), all.x = TRUE, all.y = FALSE)
  
    
    
  output <- list("DocumentTopicPolarity" = doctopicpolar, "SentenceTopicPolarity" = sentdocframe)
  return(output)
  
}


#Function to compare groups of topics and documents to find list of words that are more or less likely in group 1 vs group 2 via proportion test:
wordProbPtest <- function(docs, stmmod, topicgroup1, topicgroup2 = NULL){
  #docs is the corpus produced by the STM package document processing functions
  #stmmod is a STM model object
  #topicgroup1 is a vector of topic numbers from the STM model by number
  #topicgroup2 is a vector of topic numbers from the STM model by number. If Not entered, will default to the entire corpus.
  
  #Check to see if topicgroup2 parameters were provided. If not, set equal to all topics
  if (is.null(topicgroup2) == TRUE){
    
    topicgroup2 <- c(1:ncol(stmmod$theta))
    
  }
  
  #expand corpus so every document has a column for all tokens in the vocabulary
  expandedcorpus <- matrix(data = 0, nrow = nrow(stmmod$theta), ncol = length(stmmod$vocab))
  
  for(i in 1:nrow(expandedcorpus)){
    
    expandedcorpus[i,c(docs[[i]][1,])] <- docs[[i]][2,]
    
  }
  
  #Calculate number of tokens per doc
  tokensperdoc <- sapply(docs, function(x) sum(x[2,]))
  
  #Calculate the estimated number of words per topicgroup
  group1tokens <- sum(tokensperdoc*apply(stmmod$theta[,topicgroup1], MARGIN = 1, sum))
  group2tokens <- sum(tokensperdoc*apply(stmmod$theta[,topicgroup2], MARGIN = 1, sum))
  
#   #Original, inexact proportion test method
#   #Create vector of estimated word counts for each term in vocab for each group
#   group1vocabcount <- apply(exp(stmmod$beta$logbeta[[1]][topicgroup1,]), MARGIN = 2, mean)*group1tokens
#   group2vocabprop <- apply(exp(stmmod$beta$logbeta[[1]][topicgroup2,]), MARGIN = 2, mean)
#   
#   #For all words that have almost infinitely small probability, assign smallest probability to allow for proportion test
#   group2vocabprop[group2vocabprop == 0] <- min(group2vocabprop[group2vocabprop != 0])
#   
#   #Create vector of p-test values that test whether the expected proportion of each token in group 1 is larger than the proportion in group 2
#   group1ptest <- apply(rbind(round(group1vocabcount, digits = 0), group2vocabprop), MARGIN = 2, function(x,tokens1) prop.test(x[1], tokens1, p = x[2], alternative = "greater")$p.value, tokens1 = group1tokens)
  
  #Update here to account for size of each topic when calculating word probabilities
  group1vocabcount <- apply(exp(stmmod$beta$logbeta[[1]][topicgroup1,]), MARGIN = 2, mean)*group1tokens
  group2vocabprop <- apply(exp(stmmod$beta$logbeta[[1]][topicgroup2,]), MARGIN = 2, mean)*group2tokens
  
  #Try fisher test
  group1ptestfish <- apply(round(rbind(group1vocabcount, group2vocabprop), digits = 0), MARGIN = 2, function(x,tokens1, tokens2) fisher.test(matrix(c(x,tokens1,tokens2), nrow = 2), alternative = "greater")$p.value, tokens1 = group1tokens, tokens2 = group2tokens)
  
  return(group1ptestfish)
  
}

#Function to create topic hierarchy from k topic by n matrix of log word probabilities
topichierarchy<- function(logbeta, theta, vocab, vocabcount = NULL, nwords = NULL, nlab = 4, difftype = "sibling"){
  ##Inputs:
  ### logbeta: matrix with one row per topic and one column per word where entries are log of probabilities of each word in the topic. Probabilities sum to 1 across each row but not across each column.
  ### theta: matrix with one column per topic and one row per document where entries are probabilities of each topic in a document. Probabilities sum to 1 across each row but not across each column.
  ### vocab: vector of word names for each column of logprobs
  ### vocabcount: Optional: vector of number of times each word in vocab occurs in entire document collection
  ### nwords: Optional: vector of number of words in each topic (not required to be integer). Same length as number of rows in lobprobs.
  ### nlab: number of top scoring words to use in label
  ### difftype: type of difference to use for FREX word calculation. sibling calculates most different yet frequent words between all siblings. parent diff calculates most frequent yet exclusive difference from parent node word probs. global calculates the most frequent yet exclusive difference from all other documents not in this topic.

  #Generate labels for each leaf topic.
  topicfrex <- calcfrex(logbeta, 0.5, vocabcount)
  labvect = apply(topicfrex, MARGIN = 2, function(x) paste(vocab[x[1:nlab]], collapse = ", "))
  
  #Calculate cosine distance matrix using word probabilities
  wordprobs = as.matrix(exp(logbeta))
  cosdist <- proxy::dist(wordprobs, method = "cosine")
  
  #Cluster based on cosine distance using average linkage
  #NOTE: Uses average number of documents in topic as weighting argument (members) for clustering
  ndocs = colSums(theta)
  #Note here, complete linkage seems to make most sense (grab topic based on maximum angle between all members of candidate cluster) since averaging angles isn't necessarily sensible. Average linkage still provides ok results sometimes though. Single linkage is bad.
  cosclust <- hclust(cosdist, method = "complete", members = ndocs)
  
  #Build parent-node-attribute... matrix for hierarchy
  dendframe = data.frame(matrix(nrow = nrow(cosclust$merge) + nrow(logbeta), ncol = 6, data = NA))
  colnames(dendframe) = c("Parent", "Node", "Name", "FrexSplit", "Children", "height")
  rownames(dendframe) = c(1:(nrow(logbeta)+nrow(cosclust$merge)))
  dendframe$Node = rownames(dendframe)
  dendframe$ParentFrex = ""
  dendframe$SiblingFrex = ""
  dendframe$GlobalFrex = ""
  
  #Populate info for leaf topics
  dendframe$Name[c(1:nrow(logbeta))] = paste("Topic:", c(1:nrow(logbeta)))
  dendframe$FrexSplit[c(1:nrow(logbeta))] = labvect
  dendframe$ParentFrex[c(1:nrow(logbeta))] = labvect
  dendframe$SiblingFrex[c(1:nrow(logbeta))] = labvect
  dendframe$GlobalFrex[c(1:nrow(logbeta))] = labvect
  dendframe$Children[c(1:nrow(logbeta))] = sapply(c(1:nrow(logbeta)), list)
  
  #Modify merge matrix from hclust to be additional node numbers based on starting point of topics + 1
  mergemat = abs(cosclust$merge) + nrow(logbeta)*as.numeric(cosclust$merge > 0)
  rownames(mergemat) = c((nrow(logbeta) + 1):nrow(dendframe))
  
  #Loop through all merge points to define merged node properties:
  for(i in 1:nrow(mergemat)){
    
    #Establish child-parent linkage
    dendframe$Parent[mergemat[i,]] = rownames(mergemat)[i]
    
    #Enter child nodes into dataframe
    dendframe$Children[as.numeric(rownames(mergemat)[i])] = list(unique(unlist(dendframe$Children[mergemat[i,]])))
    
    #Create split point word choices
    dendframe$ParentFrex[mergemat[i,]] = frexhierarchysplit(wordprobs = wordprobs, vocab = vocab, memberlist = dendframe$Children[mergemat[i,]],
                                                           vocabcount = vocabcount, ndocs = ndocs, nlab = 4, difftype = "parent")
    #Create split point word choices
    dendframe$SiblingFrex[mergemat[i,]] = frexhierarchysplit(wordprobs = wordprobs, vocab = vocab, memberlist = dendframe$Children[mergemat[i,]],
                                                           vocabcount = vocabcount, ndocs = ndocs, nlab = 4, difftype = "sibling")
    #Create split point word choices
    dendframe$GlobalFrex[mergemat[i,]] = frexhierarchysplit(wordprobs = wordprobs, vocab = vocab, memberlist = dendframe$Children[mergemat[i,]],
                                                           vocabcount = vocabcount, ndocs = ndocs, nlab = 4, difftype = "global")
    
    
  }
  
  #Calculate node size based on document count
  dendframe$NodeSize = sapply(dendframe$Children, function(x) sum(ndocs[x]))
  
  ##Create network reference
  #collapsibleTreeNetwork(dendframe[,c(1,2,4,7)], attribute = "FrexSplit", nodeSize = "NodeSize")
  #cosclust <- hclust(cosdist, method = "average")
  #cosdend <- as.dendrogram(cosclust, label = labvect)
  return(dendframe)
  
}

#Function to calculate frequent and exclusive words at each split of a topic hierarchy:
frexhierarchysplit<- function(wordprobs, vocab, memberlist, vocabcount = NULL, ndocs = rep(1, nrow(wordprobs)), nlab = 4, difftype = "sibling"){
  ##Inputs:
  ### wordprobs: matrix with one row per topic and one column per word where entries are probabilities of each word in the topic. Probabilities sum to 1 across each row but not across each column.
  ### memberlist: list of vectors of topic IDs in each group to use for frex split calculation e.g. list(c(1,2), c(3,5))
  ### vocab: vector of word names for each column of logprobs
  ### vocabcount: Optional: vector of number of times each word in vocab occurs in entire document collection
  ### nwords: Optional: vector of number of words in each topic (not required to be integer). Same length as number of rows in lobprobs.
  ### nlab: number of top scoring words to use in label
  ### difftype: type of difference to use for FREX word calculation. sibling calculates most different yet frequent words between all siblings. parent diff calculates most frequent yet exclusive difference from parent node word probs. global calculates the most frequent yet exclusive difference from all other documents not in this topic.
  
  #Calculate relative proportion of total words in each member group
  #memberlist[[length(memberlist)+1]] = unique(unlist(memberlist))
  
  #Generate frex labels for the split
  if(difftype == "sibling"){
    
    #Calculate likelihood of each topic in each member group weighted by member size
    groupwordprobs = t(sapply(memberlist, function(x) (ndocs[x]/sum(ndocs[x]))%*%wordprobs[x,]))
    
    #Logbeta (set floor to -1000 to allow for frex calculation)
    grouplogbet = log(groupwordprobs)
    grouplogbet[grouplogbet < -1000] = -1000
    
    #Extract label words
    topicfrex <- calcfrex(grouplogbet, 0.5, vocabcount)
    labvect = apply(topicfrex, MARGIN = 2, function(x) paste(vocab[x[1:nlab]], collapse = ", "))
  }
  
  if(difftype == "parent"){
    
    #Add parent of nodes to memberlist
    memberlist[[length(memberlist)+1]] = unique(unlist(memberlist))
    
    #Calculate likelihood of each topic in each member group weighted by member size
    groupwordprobs = t(sapply(memberlist, function(x) (ndocs[x]/sum(ndocs[x]))%*%wordprobs[x,]))
    
    #Logbeta (set floor to -1000 to allow for frex calculation)
    grouplogbet = log(groupwordprobs)
    grouplogbet[grouplogbet < -1000] = -1000
    
    #Extract label words
    labvect = rep(NA, length(memberlist) - 1)
    for(i in 1:(nrow(grouplogbet)-1)){
      
      topicfrex <- calcfrex(grouplogbet[c(i,nrow(grouplogbet)),], 0.5, vocabcount)
      labvect[i] = paste(vocab[topicfrex[1:nlab,1]], collapse = ", ")
    }
    
  }
  
  if(difftype == "global"){
    
    labvect = rep(NA, length(memberlist))
    for(i in 1:length(memberlist)){
      
      #Create memberlist for all topics in node and all out of node
      memberlistloop = list(memberlist[[i]], -memberlist[[i]])
      #Add parent of nodes to memberlist
      #memberlist[[length(memberlist)+1]] = unique(unlist(memberlist))
      
      #Calculate likelihood of each topic in each member group weighted by member size
      groupwordprobs = t(sapply(memberlistloop, function(x) (ndocs[x]/sum(ndocs[x]))%*%wordprobs[x,]))
      
      #Logbeta (set floor to -1000 to allow for frex calculation)
      grouplogbet = log(groupwordprobs)
      grouplogbet[grouplogbet < -1000] = -1000
      
      topicfrex <- calcfrex(grouplogbet, 0.5, vocabcount)
      labvect[i] = paste(vocab[topicfrex[1:nlab,1]], collapse = ", ")
    }
    
  }
  
  return(labvect)
  
}


#Function to find topics with keywords in the top "n" most frequent keywords (more primitive than above) (should be included with stm but is not loading for some reason)
findTopic <- function(x, list, n=20, type=c("prob", "frex", "lift","score"), verbose=TRUE) {
  type <- match.arg(type)
  if(class(x)=="STM") {
    x <- sageLabels(x, n=n)
  } else {
    if(class(x)!="sageLabels") stop("x must be an STM or sageLabels object")
  }
  counts <- apply(x$marginal[[type]],1, function(w) sum(list%in%w))
  if(max(counts)==0) {
    if(verbose) cat("No topics contained any words in the list.")
    return(invisible(0))
  } else {
    index <- which(counts==max(counts))
    if(verbose) cat(sprintf("%i topics contained %i words in the list: %s", length(index),
                            max(counts), paste(index, collapse=", ")))
    textout <- sprintf("%i topics contained %i words in the list: %s", length(index),
            max(counts), paste(index, collapse=", "))
    
    result <- list("MatchedTerms" = index, "TextOutput" = textout)
    
    return(invisible(result))
  }
}

#Function to reformat 2 column matrix or 2 column data frame into a list
#such that each row becomes: list(x = dataframein[1,i], y = dataframein[2,i]).
#The overall output is a list of these row lists. Useful for converting data for a java plotter
javalister <- function(indata){
  outlist <- list()
  for (i in 1:nrow(indata)){
    outlist[[i]] <- list(x = indata[i,1], y = indata[i,2])
  }
  return(outlist)
}

#Function to convert pdf files into .txt files that can be read by R. Note that this requires pdftotext.exe to be installed.
#Does not yet work on OCR but this site has a method to do so that could be included: https://gist.github.com/benmarwick/11333467
pdftotext <- function(sourcefolder, destfolder, includesubdirs = FALSE, UseOCR = FALSE, pdftotextlocation = "C:/PDFtoText/xpdfbin-win-3.04/bin64/pdftotext.exe", pdftoppilocation = "C:/PDFtoText/xpdfbin-win-3.04/bin64/pdftoppi.exe", imgmagicconvertloc = "C:/PDFtoText/xpdfbin-win-3.04/bin64/convert.exe", tesseractlocation = "tesseract"){
  
  ###Note: there's some preprocessing that I (sg) haven't shown here: go see the original gist
  
  ################# Wait! ####################################
  # Before proceeding, make sure you have a copy of pdf2text
  # on your computer! Details: https://en.wikipedia.org/wiki/Pdftotext
  # Download: http://www.foolabs.com/xpdf/download.html
  
  # Tell R what folder contains your 1000s of PDFs
  #sourcefolder <- "C:/PDFtoText/TestFiles"
  
  #Where to write txt versions
  #destfolder <- "C:/PDFtoText/TestOut"
  
  # make a vector of PDF file paths
  myfiles <- list.files(path = sourcefolder, pattern = "pdf|PDF",  full.names = TRUE, recursive = includesubdirs)
  
  #Create destination file names
  filebase <- list.files(path = sourcefolder, pattern = "pdf|PDF",  full.names = FALSE, recursive = includesubdirs)
  filebase <- gsub(".pdf|.PDF", "", filebase)
  
  #Trim Subdirectories out of names
  filebase <- sapply(filebase, FUN = function(x) unlist(strsplit(x, split = "/", fixed = TRUE))[[length(unlist(strsplit(x, split = "/", fixed = TRUE)))]])
  
  #filebase <- paste0(destfolder, "/", filebase)
  
  #Combine into file vector
  readwritelist <- data.frame(myfiles, filebase)
  
  #Add pdftotext.exe filepath
#   readwritelist$pdftotextlocation <- pdftotextlocation
#   readwritelist$destfolder <- destfolder
  
  # now there are a few options...
  if(UseOCR == FALSE){
  ############### PDF to TXT #################################
  # convert each PDF file that is named in the vector into a text file
  # text file is created in the same directory as the PDFs
  # note that my pdftotext.exe is in a different location to yours
  apply(readwritelist, MARGIN = 1, function(i, program = pdftotextlocation, dest = destfolder) system(paste(paste0('"', program, '"'), paste0('"', i[1], '"'), paste0('"',dest, "/", i[2], ".txt", '"')), wait = FALSE) )
  
  }
   
  if(UseOCR == TRUE){ 
  ##################PDF to PPM conversion script################
  ##convert each pdf to a ppm file format with the indicated ppi (-r 600 parameter at end, can be adjusted)
  
    for(i in 1:nrow(readwritelist)){
    apply(readwritelist[i,], MARGIN = 1, function(x, program = pdftoppilocation, dest = destfolder) system(paste(paste0('"', program, '"'), paste0('"', x[1], '"'), paste0('"',dest, "/", x[2], '"'), paste0("-r 300"))) )
    
    #convert the ppm files to tif format. Combine pages back into one tif file
    apply(readwritelist[i,], MARGIN = 1, function(x, program = imgmagicconvertloc, dest = destfolder) system(paste0(program, " ", dest, "/", x[2],   "*ppm ", dest, "/", x[2], ".tif")))
    
    #Run tesseract to extract OCR content as a .txt file
    apply(readwritelist[i,], MARGIN = 1, function(x, program = tesseractlocation, dest = destfolder) system(paste0(program, " ", dest, "/", x[2],   ".tif ", dest, "/", x[2],  " -l eng")))
    
    #Delete ppm files for this document
    garbagefiles <- list.files(path = destfolder, pattern = paste0(readwritelist[i,2],".*.ppm$"),  full.names = TRUE, recursive = FALSE)
    file.remove(garbagefiles)
    
    #Delete tif files for this document
    #garbagefiles <- list.files(path = destfolder, pattern = paste0(readwritelist[i,2],".*.tif$"),  full.names = TRUE, recursive = FALSE)
    file.remove(paste0(destfolder, "/",readwritelist[i,2],".tif"))
    
    }
    
  }
  
}

#Function to take image, run OCR, then convert
OCRimage <- function(filetarget, fileformat = "ppm", imgmagicconvertloc = "C:/PDFtoText/xpdfbin-win-3.04/bin64/convert.exe", tesseractlocation = "tesseract"){
  
  if(fileformat == "ppm"){
  #Convert .ppm format to .tif format
  system(paste0(imgmagicconvertloc, " *.ppm ", filetarget, ".tif"))
  
  }
    
  
  #Run tesseract on image
  system(paste0("tesseract ", filetarget, ".tif ", filetarget, " -l eng"))
  
}

#Function to load all txt files in a directory to a large list
loadtxtfiles <- function(sourcefolder, includesubdirs = FALSE){
  
  # Tell R what folder contains your 1000s of PDFs
  #sourcefolder <- "C:/PDFtoText/TestOut"
  
  myfiles <- list.files(path = sourcefolder, pattern = "txt",  full.names = TRUE, recursive = includesubdirs)
  
  #Create file names
  fileouts <- list.files(path = sourcefolder, pattern = "txt",  full.names = FALSE, recursive = includesubdirs)
  fileouts <- gsub(".txt", "", fileouts)
  
  #Trim Subdirectories out of names
  fileouts <- sapply(fileouts, FUN = function(x) unlist(strsplit(x, split = "/", fixed = TRUE))[[length(unlist(strsplit(x, split = "/", fixed = TRUE)))]])
  
  
  output <- sapply(myfiles, function(x) readChar(x, file.info(x)$size))
  
  return(output)
  
}

#Function to load all txt files into list with relevant attributes
loadtxtfilelist <- function(filelist, includeperc = c(0,0.25)){
  #Inputs:
  #filelist: list of filepaths to load
  #includeperc: Vector defining what proportion of document text return. Helps cut down on processing power for long articles.
  
  #Filter out non .txt files
  myfiles <- filelist[grep(".txt", filelist)]
  
  #Create file names
  filenames <- gsub(".txt", "", myfiles)
  
  #Trim Subdirectories out of names
  filenames <- sapply(filenames, FUN = function(x) unlist(strsplit(x, split = "/", fixed = TRUE))[[length(unlist(strsplit(x, split = "/", fixed = TRUE)))]])
  
  #Load full text
  
  #Create dummy list
  articlecontents <- filenames
  
  #Loop through all txt documents with readChar
  
  articlecontents <- sapply(myfiles, function(x) readChar(x, file.info(x)$size))
  
  #Loop through all .doc documents with antiword
  
  #Get creation date
  createdate <- file.info(myfiles)
  createdate <- as.numeric(format(as.Date(createdate$ctime), format = "%Y"))
  
  #Get last modification date
  modifydate <- file.info(myfiles)
  modifydate <- as.numeric(format(as.Date(modifydate$mtime), format = "%Y"))
  
  #Filepathmatch
  output <- list("Filepaths" = myfiles,
                 "Filenames" = filenames,
                 "Abstract" = articlecontents,
                 "CreateDate" = createdate,
                 "ModifyDate" = modifydate)
  
  return(output)
  
}

#Function to load all txt files into list with relevant attributes
loadlocalfilelist <- function(filelist, includeperc = c(0,0.25)){
  #Inputs:
  #filelist: list of filepaths to load
  #includeperc: Vector defining what proportion of document text return. Helps cut down on processing power for long articles.
  
#   #Filter out non .txt files
#   myfiles <- filelist[grep(".txt", filelist)]
  
  #Trim Subdirectories out of names
  filenames <- sapply(filelist, FUN = function(x) unlist(strsplit(x, split = "/", fixed = TRUE))[[length(unlist(strsplit(x, split = "/", fixed = TRUE)))]])
  
  #Older versions below that don't work quite as well
  ##Get article extension type
  #fileextension <- lapply(filenames, FUN = function(x) tolower(max(strsplit(x, ".", fixed = TRUE)[[1]])))
  #
  ##Create file names
  #filenames <- sapply(filenames, FUN = function(x) strsplit(x, ".", fixed = TRUE)[[1]][1])
  
  #Newer version using tools package (standard R library)
  #Get article extension type
  fileextension = tools::file_ext(filelist)
  #Create file names
  filenames = tools::file_path_sans_ext(filenames)
  
  #Loop through all documents to extract text supported by extracttext function
  #articlecontents <- sapply(filelist, extracttext)
  articlecontents = sapply(filelist, function(x) tryCatch(extracttext(x),
                                                          error = function(e){print(paste("Unreadable file that should be readable:", x))
                                                            print(e)
                                                            return(NA)}))
  
  # #Get article extension type
  # fileextension <- lapply(filelist, FUN = function(x) tolower(max(strsplit(x, ".", fixed = TRUE)[[1]])))
  
  #Get creation date
  createdate <- file.info(filelist)
  createdate <- as.numeric(format(as.Date(createdate$ctime), format = "%Y"))
  
  #Get last modification date
  modifydate <- file.info(filelist)
  modifydate <- as.numeric(format(as.Date(modifydate$mtime), format = "%Y"))
  
  #Filepathmatch
  output <- list("Filepaths" = filelist,
                 "Filenames" = filenames,
                 "FileExtension" = fileextension,
                 "Abstract" = articlecontents,
                 "CreateDate" = createdate,
                 "ModifyDate" = modifydate)
  
  return(output)
  
}

#Function to get linkage structure of local files based on filenames showing up in other documents
documentnamelinkage <- function(docids, doctext, searchids = docids){
  #Function establishes citation structure by finding instances of searchids in doctext. Each instance of searchids found in doctext counts as the doctext article citing the docids document
  #INPUTS:
  ##docids - list or vector of unique document IDs to use as document names when returning citation structure
  ##doctext - list of document text corresponding to docids
  ##searchids - Optional list or vector of unique search terms to use to establish linkage structure. Defaults to be the same as docids but can be a different list or vector of the same length as docids
  
  #Create quanteda corpus and tokenize (without removing numbers or converting to lowercase, etc.) for faster searching
  quantcorp = corpus(x = doctext, docnames = docids)
  #NOTE: When tokenizing, consider modifying code below to use searchids as dictionary to allow for phrase matching
  quanttok = tokens(x = quantcorp)
  
  #Find all matches
  matchframe = kwic(quanttok, pattern = searchids)
  #Cut down to unique document matches
  matchframe = matchframe[,c("keyword", "docname")]
  matchframe = unique.array(matchframe)
  
  #Extract source article and cited by vectors
  SourceArticle = matchframe$keyword
  CitedBy = matchframe$docname
  
  #Trace search term back to source document ID (works but is a little slow)
  SourceDocID = CitedBy
  for(i in 1:length(docids)){
    
    SourceDocID[SourceArticle == searchids[i]] = docids[i]
    
  }
  
  # #This method is faster but is imperfect and leaves some NA values that break later functions but is fast
  # names(docids) = searchids
  # #This creates some NAs somehow
  # SourceDocID = docids[SourceArticle]
  
  # #Old, very slow, and inaccurate code
  #   
  # #Initialize source article and cited by vectors
  # SourceArticle <- c()
  # CitedBy <- c()
  # 
  # #Loop through each element of searchids and find matching documents in doctext
  # for(i in c(1:length(searchids))){
  #   
  #   #Find all documents that reference the current element of searchids (fixed and useBytes options turned on for speed)
  #   tempvect <- grep(searchids[i], doctext, fixed = TRUE, useBytes = TRUE)
  #   
  #   #Collect citing and source aritcle IDS in citation and source lists
  #   CitedBy <- c(CitedBy, docids[tempvect])
  #   SourceArticle <- c(SourceArticle, rep(docids[i], times = length(tempvect)))
  #   
  # }
  
  #Create a citation frame
  CitationFrame <- data.frame(SourceDocID, CitedBy, stringsAsFactors = FALSE)
  #Eliminate duplicate citations
  CitationFrame <- unique.array(CitationFrame)
  
  #Count the number of times each article is cited
  TimesCited <- summary.factor(CitationFrame[,1], maxsum = length(unique(CitationFrame[,1])))
  
  #Return output as a list
  output <- list("CitationFrame" = CitationFrame, "TimesCited" = TimesCited)
  return(output)
  
}

#Function to search in all txt files in a directory for a specific search string
searchdirtxt <- function(sourcefolder, searchstring, includesubdirs = FALSE, searchperc = c(0,0.25)){
  #Inputs:
  #sourcefolder: filepath of folder to search in that contains txt files
  #searchstring: vector of strings to search for
  #includesubdirs: whether to include subdirectories or not
  #searchperc: Vector defining what proportion of document text to search. This helps to remove matches from the reference sections of articles in case someone is searching for a specific title.
  
  # Tell R what folder contains your 1000s of PDFs
  #sourcefolder <- "C:/PDFtoText/TestOut"
  
  myfiles <- list.files(path = sourcefolder, pattern = "txt",  full.names = TRUE, recursive = includesubdirs)
  
  #Create file names
  fileouts <- list.files(path = sourcefolder, pattern = "txt",  full.names = FALSE, recursive = includesubdirs)
  fileouts <- gsub(".txt", "", fileouts)
  
  #Trim Subdirectories out of names
  fileouts <- sapply(fileouts, FUN = function(x) unlist(strsplit(x, split = "/", fixed = TRUE))[[length(unlist(strsplit(x, split = "/", fixed = TRUE)))]])
  
  
  
  filematches <- lapply(myfiles, stringlocationintxt, searchstring = searchstring, percentfilt = searchperc)
  
  #Find files with matching titles
  numbermatch <- which(sapply(filematches, nrow) > 0)
  
  #Filepathmatch
  output <- list("Filepaths" = myfiles[numbermatch], "Filenames" = fileouts[numbermatch])
  
  return(output)
  
}

stringlocationintxt <- function(txtpath, searchstring, percentfilt = c(0,1)){
  #Function to find string location in a txt filepath
  
  output <- data.frame()
  
  #Read file
  filechars <- readChar(txtpath, file.info(txtpath)$size)
  
  searchtemp <- c(gregexpr(searchstring, filechars, ignore.case = TRUE))
  searchtemp <- sapply(searchtemp, function(x) x[[1]])
  searchtemp <- data.frame(which(searchtemp > 0), searchtemp[searchtemp > 0])
  
  if(nrow(searchtemp) > 0){
  
    output <- searchtemp
    
  colnames(output) <- c("ArticleNumber", "FirstMatchPosition")
  
  output$ArticleChars <- unlist(nchar(filechars[output[[1]]]))
  
  #Calculate percentage position of first search result in string list
  output$SearchPosPercent <- output[2]/output[3]
  
  #Filter based on percentage of string that should have been searched
  output <- output[intersect(which(output[4] > percentfilt[1]), which(output[4] < percentfilt[2])), ]
  
  }
  
  return(output)
}

extracttext <- function(filepath, verbose = FALSE, pdfattachments = TRUE){
  #Function to extract text from local files of various formats. Formats currently supported are:
  ##.txt, .doc, .docx, .pdf, .msg
  ##If format is supported, returns a character vector of text. Otherwise, returns empty vector
  #filepath: path to file to extract text
  #pdfattachments: whether text should be extracted from any pdf file attachments (eg. pdf portfolios)
  
  #Initialize output vector
  output <- c()
  
  if(verbose){
  print(paste("Local Extraction Processing", Sys.time()))
  }
  #Get file extension (last element of split vector):
  fileextension <- tolower(max(strsplit(filepath, ".", fixed = TRUE)[[1]]))
  
  #Read characters from .txt files
  if(fileextension == "txt"){
    output <- readChar(filepath, file.info(filepath)$size)
  }
  
  # #Read characters from .doc files (requires antiword package)
  # if(fileextension == "doc"){
  #   output <- antiword::antiword(filepath)
  # }
  # 
  # #Read characters from .docx files (requires antiword package)
  # if(fileextension == "docx"){
  #   output <- paste(qdapTools::read_docx(filepath), collapse = " ")
  # }
  # 
  # #Read characters from .pdf files (requires pdftools package)
  # if(fileextension == "pdf"){
  #   output <- paste(pdftools::pdf_text(filepath), collapse = " ")
  # }
  
  #Read formats covered by readtext package (requires readtext package)
  if(fileextension %in% c("json", "csv", "tab", "tsv", "html", "xml", "pdf", "odt", "doc", "docx", "rtf")){
    output <- paste(readtext::readtext(filepath)$text, collapse = " ")
  }
  
  #If pdf files should be examined for attachments, add text from attachment to output string
  if(fileextension == "pdf" & pdfattachments){
    attachments = pdftools::pdf_attachments(filepath)
    for(x in attachments){
      output <- paste(c(output, pdftools::pdf_text(x$data)), collapse = " ")
    }
  }
  
  #Read characters from .msg files (requires msgxtractr package which only works in windows)
  if(Sys.info()['sysname'] == 'Windows'){
    if(fileextension == "msg"){
      msgcontents = msgxtractr::read_msg(filepath)
      output <- paste(c(msgcontents$subject, msgcontents$body$text), collapse = " ")
    }
  }
  
  return(output)
  
}

texttonamedlist <- function(sourcetext, delimitvect = c("\nQuestion [0-9]*", "BSC Response [0-9]*[A-Za-z]*")){
  #Function to split single character vector into relational data frame (for splitting one large text document into sub-elements that can each be treated as an article)
  #INPUTS:
  ##sourcetext - single string of characters
  ##delimitvect - vector of splitting regex text. These splits will be recursively applied (source string split by first element, then the resulting split elements split by second element, and so on)
  #### note, alternative example with "OR" matching of same text is: delimitvect = c("\nQuestion [0-9]*|BSC Response [0-9]*[A-Za-z]*")
  
  linkframe <- data.frame(source = c(), target = c())
  outlist <- list()
  
  #looptext <- sourcetext
  
  for(i in 1:length(delimitvect)){
    
    #Break string by ith element of delimitvect and place into list 
    outlist[[i]] <- lapply(sourcetext, function(x, y = delimitvect[i]){
      output <- strsplit(x = x, split = y)[[1]]
      names(output) <- c("Leadstring", unlist(regmatches(x = x, m = gregexpr(text = x, pattern = y))))
      return(output)
      })
    # outlist[[i]] <- sapply(sourcetext, function(x, y = delimitvect[i]){
    #   output <- strsplit(x = x, split = y)[[1]]
    #   names(output) <- c("Leadstring", unlist(regmatches(x = x, m = gregexpr(text = x, pattern = y))))
    #   return(output)
    # })
    
    #Replace sourcetext with resultant text vector
    sourcetext <- unlist(outlist[[i]])
    
  }
  
  return(outlist)
  ##Code to convert to data frame if only one level is used:
  #outframe <- data.frame(outlist)
  #colnames(outframe) <- c("text")
  #outframe$splitnames <- names(unlist(outlist))
  
}

#Function to return all multi-word tokens produced by ToPMine
ToPMineExtractMultiword = function(filepath){
  
  #Read file and split all comma separated entries
  mwtoks <- read.table(filepath, header=FALSE, stringsAsFactors=FALSE, sep = "\n")
  mwtoks <- lapply(mwtoks[,1], strsplit, split = ",", fixed = TRUE)
  
  #Unlist and remove all duplicates
  mwtoks = unique(unlist(mwtoks))
  
  #Only keep characters with spaces (thus multiword tokens)
  mwtoks = grep(pattern = " ", mwtoks, fixed = TRUE, value = TRUE)
  return(mwtoks)
  
}

#Create own dtm from corpus file created by ToPMine
ToPMinetoDTM <- function(filepath){
  
  TPMcorp <- read.table(filepath, header=FALSE, stringsAsFactors=FALSE, sep = "\n")
  
  TPMcorp <- lapply(TPMcorp[,1], strsplit, split = ",", fixed = TRUE)
  
  lev <- sort(unique(unlist(TPMcorp)))
  TPMDTM <- do.call(cbind, lapply(TPMcorp, function(x, lev) {
    tabulate(factor(unlist(x), levels = lev, ordered = TRUE), nbins = length(lev))
  }, lev = lev))
  rownames(TPMDTM) <- sort(lev)
  
  return(t(TPMDTM))
  
}

UniqueSub <- function(phrases, documents){
  #Function that replaces all phrases in a sentence with unique letter tokens to avoid stripping or separating during corpus processing for text modeling.
  #This works on lists of numbers, multi-word phrases, or anything else that can be defined as a contiguous text string.
  #INPUTS:
  ##phrases = a vector of character strings representing the elements to be replaced with uniquely keyed strings with no spaces or numbers
  ##documents = a list of documents to search through. Lists and vectors tested and found to work.
  #OUTPUTS:
  ##keyeddocs = a list or vector of documents (matches initial formatting of "documents" input) containing the list of documents with key strings replaced with unique placeholders that are letters only
  ##keyframe = a dataframe
  
  #Order from longest to shortest to make sure that short phrases that are subsets of long phrases do not replace the long phrase.
  phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
  
  #Define null vector to start in case no phrases identified
  placeholder <- c()
  
  if(length(phrases) > 0){
    
    placeholder <-  apply(expand.grid(lapply(1:max(2,ceiling(log(length(phrases), base = 26))), function(i) letters))[1:length(phrases),], MARGIN = 1, paste0, collapse = "")
    placeholder <- paste0("phrasefindqqxzqcvx", placeholder,"phrasefind")
    
  }
  
  #Replace multiword phrase instances. Add leading and trailing space to make sure they don't get combined with other words
  keyeddocs <- mgsub(as.character(phrases), as.character(placeholder), documents, ignore.case = TRUE, leadspace = TRUE, trailspace = TRUE)
  
  keyframe <- data.frame(phrases, placeholder)
  colnames(keyframe) <- c("OrigString", "Key")
  
  return(list(keyeddocs = keyeddocs, keyframe = keyframe))
  
}


score_documents <- function(target_texts, topic_model, remove_numbers=TRUE, stem_words=FALSE){

  #Extract all multi-word tokens from topicmodel vocabulary
  phrases <- topic_model$vocab[grep(" ", topic_model$vocab, fixed = TRUE)]

  #Order from longest to smallest to make sure longest phrases are replaced with placeholder first to prevent errors
  phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]

  #Create metadata for new article which should have unique document ID that is used in other charts as first column
  meta <- data.frame(PMID = paste0("NEWDOC", 1:length(target_texts)), year = max(topic_model$settings$covariates$X[,"year"]))

  #Create corpus from semantic search text field
  quanttok = corpus(target_texts, docvars = meta)

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
                    remove_numbers = remove_numbers,
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
  quantdfm = dfm(quanttok, tolower = TRUE, stem = stem_words)

  #Convert to stm format
  temp = convert(quantdfm, to = "stm")

  #Revert multiword underscore splits in vocab back to spaces
  temp$vocab = gsub(pattern = "_", replacement = " ", x = temp$vocab, fixed = TRUE)
  
  #Construct matrix to hold all topic scores for each document
  topicmat = matrix(nrow = length(temp$documents), ncol = ncol(topic_model$theta))

  #Align corpus vocabulary
  temp <- alignCorpus(new = temp, old.vocab = topic_model$vocab, verbose = FALSE)
  
  #Fit the new topic model - may need to update with metadata at some point
  if(length(temp$documents) > 0){
    doc_scores <- fitNewDocuments(model = topic_model, documents = temp$documents, verbose = FALSE)
  
    # Insert document scores for documents that could be scored by the model (have some terms in common)
    topicmat[setdiff(c(1:nrow(topicmat)),  temp$docs.removed),] = doc_scores$theta
  }
  
  # Insert topic vector with equal probability across all topics for documents that had no words in the model corpus (thus assume the prior of even probability of belonging to any topic)
  if(length(temp$docs.removed) > 0){
    
    topicmat[temp$docs.removed,] = 1/(ncol(topicmat))
    
  }
  
  # Assign topic names to each column
  colnames(topicmat) = paste("Topic", c(1:ncol(topicmat)))
  
  return(topicmat)

}


distance_from_target <- function(target_vector, candidate_matrix, method="cosine"){
  
  # Reshape the target_vector to an appropriate 
  if(is.vector(target_vector)){
    
    # Try to match by column first
    if(length(target_vector) == dim(candidate_matrix)[2]){
      
      target_vector = matrix(target_vector, nrow = 1)
      
    }else{
      
      candidate_matrix = t(candidate_matrix)
      
    }
    
    target_vector = matrix(target_vector, nrow = 1)
    
  }
  
  #Find the cosine distance between the target vector and the candidate matrix
  distance_metric = proxy::dist(x = target_vector, y = candidate_matrix, method = method)
  
  #Get document order from closest match to furthest
  closedocs <- order(distance_metric)
  distperc <- distance_metric[order(distance_metric)]
  
  return(list("RankedMatches" = closedocs, "Distance" = distperc))
  
}


split_doc_into_chunks <- function(input_text, chunk_size = 4, chunk_count = NULL, split_by = 'sentence'){
  
  # Split document using specified chunk type
  sentences = tokens(input_text, what=split_by)[["text1"]]
  
  # USE tokens_chunk here instead?
  
  if(!is.null(chunk_count)){
    # If the number of chunks is provided, split into specified number of chunks
    
    output = split(sentences, ceiling(chunk_count * seq_along(sentences)/length(sentences)))
    
  }else{
    
    # Otherwise, split into chunks of a maximum target size.
    output = split(sentences, ceiling(seq_along(sentences)/chunk_size))
    
  }
  
  output = sapply(output, function(x) paste(x, collapse = " "))
  
  return(output)
  
  
}


rank_doc_snippets <- function(document, target_text, stm_model, chunk_size = 4){
  
  # Split document into chunks
  doc_chunks = split_doc_into_chunks(input_text = document, chunk_size = chunk_size)
  
  # Score each chunk along with the target text
  score_matrix = score_documents(target_texts = c(target_text, doc_chunks), topic_model = stm_model)
  
  # Calculate distances from target doc and return chunks ordered from closest match to most distant
  match_results = distance_from_target(score_matrix[1, ], score_matrix[-c(1), ])
  
  # Reorder the chunks from closest to most 
  document_snippets = doc_chunks[match_results$RankedMatches]
  
  return(list("RankedDocuments" = document_snippets, "DistanceMetric" = match_results$Distance))
  
}


binary_doc_snippet_search <- function(document, stm_model, chunk_size = 4, character_size = 500, target_text = NULL, target_vector = NULL){
  
  # Score the target text if provided
  if(!is.null(target_text)){
    
    target_vector = score_documents(target_texts = target_text, topic_model = stm_model)[1,]
    
  }
  
  # Split document into sentences
  sentences = split_doc_into_chunks(input_text = document, chunk_size = 1)
  
  # Flag used to determine if splitting should continue
  continue_splitting = TRUE
  
  while(continue_splitting){
    
    sentences = split(sentences, ceiling(2 * seq_along(sentences)/length(sentences)))
    loop_chunks = sapply(sentences, function(x) paste(x, collapse = " "))
    
    # Score each chunk along with the target text
    score_matrix = score_documents(target_texts = loop_chunks, topic_model = stm_model)
    
    # Calculate distances from target doc and return chunks ordered from closest match to most distant
    match_results = distance_from_target(target_vector, score_matrix)
    
    # Select the best matching chunk
    sentences = sentences[[match_results$RankedMatches[1]]]
    
    # Stop if the best matching chunk matches length requirements
    continue_splitting = (length(sentences) > chunk_size) & (nchar(loop_chunks[[match_results$RankedMatches[1]]]) > character_size)
    
  }
  
  # Return the best matching snippet after the target chunk size is reached
  return(list("BestSnippet" = loop_chunks[[match_results$RankedMatches[1]]], "DistanceMetric" = match_results$Distance[1]))
  
}
