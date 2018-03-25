# helper functions
# convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

# Rfacebook's getCommentReplies is not working properly
# the following code fixes the problem - thanks to Stack Overflow!

# RE-CREATE THE FONCTION getCommentReplies

# JSON replyDataToDF (return initial comment)

replyDataToDF <- function(json){
  df <- data.frame(
    from_id = json$from$id,
    from_name = json$from$name,
    message = ifelse(!is.null(json$message),json$message, NA),
    created_time = json$created_time,
    id = json$id,
    stringsAsFactors=F)
  return(df)
}

# getCommentReplies
getCommentReplies <- function(reply, token, n=1000000, comments=TRUE, likes=FALSE, n.likes=n,
                              n.comments=n){
  
  url <- paste0("https://graph.facebook.com/", reply,
                "?fields=from,message,created_time") #return initial comments
  
  if (comments==TRUE){
    url <- paste0(url, ",comments.summary(true).",
                  "fields(from,id,message,created_time,like_count)") #return reply
    if (n.comments>=500){
      url <- paste0(url, ".limit(500)")
    }
    if (n.comments<500){
      url <- paste0(url, ".limit(", n.comments, ")")
    }
  }
  if (comments==FALSE){
    url <- paste0(url, ",comments.summary(true)")
  }
  if (likes==TRUE){
    url <- paste0(url, ",likes.summary(true).",
                  "fields(id,name)")
    if (n.likes>=2000){
      url <- paste0(url, ".limit(2000)")
    }
    if (n.likes<2000){
      url <- paste0(url, ".limit(", n.likes, ")")
    }
  }
  if (likes==FALSE){
    url <- paste0(url, ",likes.summary(true)")
  }
  
  # making query
  content <- callAPI(url=url, token=token)
  
  # error traps: retry 3 times if error
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url=url, token=token)		
    if (error==3){ stop(content$error_msg) }
  }
  if (length(content)==0){ 
    stop("Reply could not be found")
  }
  
  # putting it together
  out <- list()
  #out[["reply"]] <- replyDataToDF(content)
  if (likes && n.likes > 0) out[["likes"]] <- likesDataToDF(content$likes$data)
  if (likes && n.likes > 0) n.l <- ifelse(!is.null(out$likes), dim(out$likes)[1], 0)
  if (n.likes == 0) n.l <- 0
  if (!likes) n.l <- Inf
  if (comments && n.likes > 0) out[["comments"]] <- commentsDataToDF(content$comments$data)
  if (comments && n.likes > 0) n.c <- ifelse(!is.null(out$comments), dim(out$comments)[1], 0)
  if (n.comments == 0) n.c <- 0
  if (!comments) n.c <- Inf
  
  # paging if we n.comments OR n.likes haven't been downloaded
  if (n.likes > n.l || n.comments > n.c){
    # saving URLs for next batch of likes and comments
    if (likes) url.likes <- content$likes$paging$`next`
    if (!likes) url.likes <- NULL
    if (comments) url.comments <- content$comments$paging$`next`
    if (!comments) url.comments <- NULL
    
    if (!is.null(url.likes) && likes && n.likes > n.l){
      # retrieving next batch of likes
      url <- content$likes$paging$`next`
      content <- callAPI(url=url.likes, token=token)
      out[["likes"]] <- rbind(out[["likes"]],
                              likesDataToDF(content$data))
      n.l <- dim(out$likes)[1]
      # next likes, in batches of 500
      while (n.l < n.likes & length(content$data)>0 &
             !is.null(url <- content$paging$`next`)){
        url <- content$paging$`next`
        content <- callAPI(url=url, token=token)
        out[["likes"]] <- rbind(out[["likes"]],
                                likesDataToDF(content$data))
        n.l <- dim(out$likes)[1]
      }
    }
    if (!is.null(url.comments) && comments && n.comments > n.c){
      # retriving next batch of comments
      content <- callAPI(url=url.comments, token=token)
      out[["comments"]] <- rbind(out[["comments"]],
                                 commentsDataToDF(content$data))
      n.c <- dim(out$comments)[1]
      # next comments, in batches of 500
      while (n.c < n.comments & length(content$data)>0 &
             !is.null(content$paging$`next`)){
        url <- content$paging$`next`
        content <- callAPI(url=url, token=token)
        out[["comments"]] <- rbind(out[["comments"]],
                                   commentsDataToDF(content$data))
        n.c <- dim(out$comments)[1]
      }
    }
  }
  
  return(out)
}

# JSON unlistWithNA

unlistWithNA <- function(lst, field){
  if (length(field)==1){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field]]))
  }
  if (length(field)==2){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
  }
  if (field[1]=="shares"){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
    vect <- rep(0, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
  }
  if (length(field)==3){
    notnulls <- unlist(lapply(lst, function(x) 
      tryCatch(!is.null(x[[field[1]]][[field[2]]][[field[3]]]), 
               error=function(e) FALSE)))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  if (length(field)==4 & field[1]=="to"){
    notnulls <- unlist(lapply(lst, function(x) 
      tryCatch(!is.null(x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]), 
               error=function(e) FALSE)))
    vect <- rep(NA, length(lst))
    vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
  }
  if (field[1] %in% c("comments", "likes") & !is.na(field[2])){
    notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
    vect <- rep(0, length(lst))
    vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]][[field[3]]]))
  }
  return(vect)
}

# JSON commentsDataToDF #

commentsDataToDF <- function(json){
  if (!is.null(json)){
    df <- data.frame(
      from_id = unlistWithNA(json, c('from', 'id')),
      from_name = unlistWithNA(json, c('from', 'name')),
      message = unlistWithNA(json, 'message'),
      created_time = unlistWithNA(json, 'created_time'),
      likes_count = unlistWithNA(json, 'like_count'),
      id = unlistWithNA(json, 'id'),
      stringsAsFactors=F)
  }
  if (is.null(json)){
    df <- NULL
  }
  return(df)
}