#! /usr/bin/Rscript

library(httr)
api_token        <- "token"
ai_url           <- "https://models.dobro.ai/gpt2/medium/"
url_get_server   <- "https://api.vk.com/method/groups.getLongPollServer"
url_send_message <- "https://api.vk.com/method/messages.send"
url_type_message <- "https://api.vk.com/method/messages.setActivity"

data_load_get_server <- list(
  v            = 5.103,
  group_id     = "group id",
  access_token = api_token
)

send_mesage_to <- function(peer_id, text) {
  data_load_send <- list(
    v            = 5.103,
    random_id    = 0,
    access_token = api_token,
    peer_id      = peer_id,
    message      = text
  )
  data_load_type <- list(
    v            = 5.103,
    access_token = api_token,
    group_id     = "group id",
    peer_id      = peer_id,
    type         = "typing"
  )
  POST(url_type_message,
       body   = data_load_type,
       encode = "form")
  Sys.sleep(floor(runif(1, min = 5, max = 8)))
  POST(url_send_message,
       body   = data_load_send,
       encode = "form")
}

main <- function() {
  get_server_response_raw <<- POST(url_get_server,
                                   body   = data_load_get_server,
                                   encode = "form")
  response_formed          <- content(get_server_response_raw)[["response"]]
  got_server               <- paste(response_formed["server"])
  got_key                  <- paste(response_formed["key"])
  got_ts                   <- paste(response_formed["ts"])
  
  repeat {
    url_longpoll       <- got_server
    data_load_longpoll <- list(
      wait = 25,
      act  = "a_check",
      key  = got_key,
      ts   = got_ts
    )
    get_longpoll_response_raw <<- POST(url_longpoll,
                                       body = data_load_longpoll,
                                       encode = "form")
    print(content(get_longpoll_response_raw))
    longpoll_formed <- content(get_longpoll_response_raw)
    failed_code     <- paste(longpoll_formed["failed"])
    if (failed_code == "2") {
      get_server_response_raw <- POST(url_get_server,
                                      body = data_load_get_server,
                                      encode = "form")
      response_formed         <- content(get_server_response_raw)[["response"]]
      got_key                 <- paste(response_formed["key"])
    } else if (failed_code == "3") {
      get_server_response_raw <- POST(url_get_server,
                                      body = data_load_get_server,
                                      encode = "form")
      response_formed         <- content(get_server_response_raw)[["response"]]
      got_key                 <- paste(response_formed["key"])
      got_ts                  <- paste(response_formed["ts"])
    }
    
    got_updates    <- longpoll_formed[["updates"]]
    got_ts         <- paste(longpoll_formed["ts"])
    updates_length <- length(got_updates)
    
    if (updates_length == 0) {
      next
    }
    
    for (update_number in 1:updates_length) {
      update     <- got_updates[[update_number]]
      event_type <- paste(update["type"])
      if (identical(event_type, "message_new")) {
        object       <- update[["object"]]
        message      <- object[["message"]]
        from_id      <- paste(message["from_id"])
        peer_id      <- paste(message["peer_id"])
        message_text <- paste(message["text"])
        
        if (nchar(message_text) <= 500) {
          generate_ask <- TRUE
        }
        
        if (generate_ask == TRUE) {
          chance <- floor(runif(1, min = 0, max = 16))
          if ((chance == 7) |
              (identical(peer_id, from_id))) {
            length_chance <- floor(runif(1, min = 30, max = 61))
            data_load_generate <<- list(
              length      = length_chance,
              num_samples = 1,
              prompt      = message_text
            )
            result    <<- POST(ai_url,
                            body   = data_load_generate,
                            encode = "json")
            generated  <- content(result)[["replies"]][[1]]
            print(result)
            send_mesage_to(peer_id,
                           paste(message_text,
                                 generated,
                                 sep = ""))
          }
        }
      }
    }
  }
}
repeat {
  try(main())
}
