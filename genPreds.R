

# data_dir <- c("~/Work/Training/Data Science Coursera/10-Capstone/Final_Project")
data_dir <- getwd()

cleanData <- function(data) {
    
    library(tm)
    
    # Profanity filtering
    
    f <- file(paste0(data_dir,"/datasets/profanity.txt"))
    profanities<-readLines(f,encoding="UTF-8")
    close(f)
    
    data <- tolower(data) # convert to lowercase
    data <- removeWords(data, profanities)
    
    #  Separate words connected with - or /
    data <- gsub("-", " ", data)
    data <- gsub("/", " ", data)
    
    # Establish end of sentence, abbr, number, email, html
    data <- gsub("\\? |\\?$|\\! |\\!$", " EOSS ", data)
    data <- gsub("[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\. |[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\. |[A-Za-z]\\.[A-Za-z]\\. ", " EOSS ", data)
    data <- gsub("\\. |\\.$", " EOSS ", data)
    data <- gsub("[0-9]+"," EOSS ",data)
    data <- gsub("\\S+@\\S+","EOSS",data) 
    data <- gsub("[Hh}ttp([^ ]+)","EOSS",data) 
    data <- gsub("RT | via"," EOSS ",data) # retweets
    data <- gsub("@([^ ]+)","EOSS",data) # @people
    data <- gsub("[@][a - zA - Z0 - 9_]{1,15}","EOSS",data) # usernames
    
    # Remove/replace &, @, 'm, 's, 'are, 'll, etc...
    data <- gsub(" & ", " and ", data)
    data <- gsub(" @ ", " at ", data)
    data <- replace_contraction(data)
    data <- gsub("'s", "", data) 
    data <- gsub("haven't", "have not", data)
    data <- gsub("hadn't", "had not", data)
    
    #Remove emoji's, emoticons
    data <- gsub("[^\x01-\x7F]", "", data)
    
    data <- removeNumbers(data) # remove numbers
    data <- replace_contraction(data)
    pontuacao <-  '[.,!:;?]|:-\\)|:-\\(|:\\)|:\\(|:D|=D|8\\)|:\\*|=\\*|:x|:X|:o|:O|:~\\(|T\\.T|Y\\.Y|S2|<3|:B|=B|=3|:3'
    data <- gsub(pontuacao," EOSS ",data) # substitute selected ponctuation (including smileys) with the word END
    data <- gsub("$"," EOSS",data) # make sure every line ends with an END
    data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg.                
    data <- removePunctuation(data) # remove all other punctuation
    data <- str_replace_all(data, "[[:punct:]]", "")
    data <- stripWhitespace(data) # remove excess white space
    data <- gsub("^[[:space:]]","",data) # make sure lines doesn't begin with space
    data <- gsub("[[:space:]]$","",data) # make sure lines doesn't end with space
    
    data <- gsub(" u ", " you ", data)
    data <- gsub(" [b-hj-z] ", " ", data)
    data <- removeWords(data, c(" EOSS", "EOSS"))
    
    
}



predict.1grm <- function(OneGram){
    
    prediction <- OneGram[, c("pred","GTprob","hist", "GTfreq","GTprob")]
    names(prediction) <- c("Predicted", "Katz Prob", "History", "GoodTuring Freq", "GoodTuring Prob")
    
    return(prediction)
}
###*****************************************************
### function to execute katz back-off from a 2 gram ####
###*****************************************************

# 1. extract the hist from 2 gram that corresponds to gram2.w1
# 2. extract 1grams
# 3. calculate alpha
# 4. Sort 2 and 1 grams by Katz prob, and create table to output

predict.2grm <- function(TwoGram, OneGram, gram2.w1){
    
    # subset from gram 2 where hist matches gram2.w1
    hist.gram2.match <- TwoGram[which(TwoGram$hist==gram2.w1),]
    
    # subset gram 1 into words predicted by gram 2 or not predicted
    gr1.in.gr2 <- OneGram[OneGram$pred %in% hist.gram2.match$pred,]
    gr1.notin.gr2 <- OneGram[!(OneGram$pred %in% hist.gram2.match$pred),]
    
    # calculate alpha for gram 1(call gama the denominator of alpha)
    beta.gr1 <- 1- sum(hist.gram2.match$GTprob)
    gama.gr1 <- 1 - sum(gr1.in.gr2$GTprob)
    alpha.gr1 <- beta.gr1 / gama.gr1
    
    # Calculate KatzProb for gram 1
    gr1.notin.gr2$Kprob <- gr1.notin.gr2$GTprob * alpha.gr1
    # Calculate KatzProb for gram 2
    hist.gram2.match$Kprob <- hist.gram2.match$GTprob
    
    # rbind gr1 and gr matches, and sort
    prediction <- rbind(hist.gram2.match[1:1000,],gr1.notin.gr2[1:1000,])
    prediction <- prediction[order(-prediction$Kprob),]
    prediction <- prediction[,c("pred", "Kprob", "hist", "GTfreq","GTprob")]
    names(prediction) <- c("Predicted", "Katz Prob", "History", "GoodTuring Freq", "GoodTuring Prob")
    
    rm(hist.gram2.match, gr1.in.gr2, gr1.notin.gr2, beta.gr1, gama.gr1, alpha.gr1)
    
    return(prediction)        
}


###*****************************************************
### function to execute katz back-off from a 3 gram ####
###*****************************************************

predict.3grm <- function(TriGram,TwoGram, OneGram, gram3.w12, gram2.w1){
    
    # gram 3 match and backoff to gram 2
    
    # subset from gram 3 where hist matches gram3.w12
    hist.gram3.match <- TriGram[which(TriGram$hist==gram3.w12),]
    
    # subset from gram 2 where hist matches gram2.w1
    hist.gram2.match <- TwoGram[which(TwoGram$hist==gram2.w1),]
    
    # subset gram 2 into words predicted by gram 3 or not predicted
    gr2.in.gr3 <- hist.gram2.match[hist.gram2.match$pred %in% hist.gram3.match$pred,]
    gr2.notin.gr3 <- hist.gram2.match[!(hist.gram2.match$pred %in% hist.gram3.match$pred),]
    
    # calculate alpha for gram 2(call gama the denominator of alpha)
    beta.gr2 <- 1- sum(hist.gram3.match$GTprob)
    gama.gr2 <- 1 - sum(gr2.in.gr3$GTprob)
    alpha.gr2 <- beta.gr2 / gama.gr2
    
    # Calculate KatzProb (Kprob) for gram 2
    gr2.notin.gr3$Kprob <- gr2.notin.gr3$GTprob * alpha.gr2
    # Calculate KatzProb for gram 3
    hist.gram3.match$Kprob <- hist.gram3.match$GTprob
    
    #### backoff to gram 1
    
    # subset gram 1 into words predicted by gram 2 or not predicted
    gr1.in.gr2 <- OneGram[OneGram$pred %in% hist.gram2.match$pred,]
    gr1.notin.gr2 <- OneGram[!(OneGram$pred %in% hist.gram2.match$pred),]
    
    # calculate alpha for gram 1(call gama the denominator of alpha)
    beta.gr1 <- 1- sum(hist.gram2.match$GTprob)
    gama.gr1 <- 1 - sum(gr1.in.gr2$GTprob)
    alpha.gr1 <- beta.gr1 / gama.gr1
    
    # Calculate KatzProb for gram 1
    gr1.notin.gr2$Kprob <- gr1.notin.gr2$GTprob * alpha.gr1 * alpha.gr2
    # Calculate KatzProb for gram 2
    
    # rbind gr1 and gr matches, and sort
    prediction <- rbind(hist.gram3.match[1:1000,],gr2.notin.gr3[1:1000,] ,gr1.notin.gr2[1:1000,])
    prediction <- prediction[order(-prediction$Kprob),]
    prediction <- prediction[,c("pred", "Kprob", "hist", "GTfreq","GTprob")]
    names(prediction) <- c("Predicted", "Katz Prob", "History", "GoodTuring Freq", "GoodTuring Prob")
    
    rm(hist.gram3.match, hist.gram2.match, gr2.in.gr3, gr2.notin.gr3, 
       gr1.in.gr2, gr1.notin.gr2, beta.gr2, gama.gr2, alpha.gr2,
       beta.gr1, gama.gr1, alpha.gr1)
    
    return(prediction)
}
###*****************************************************
### function to execute katz back-off from a 4 gram ####
###*****************************************************

predict.4grm <- function(FourGram, TriGram,TwoGram, OneGram, gram4.w123, gram3.w12, gram2.w1){
    
    # gram 4 match and backoff to gram 1
    
    # subset from gram 4 where hist matches gram4.w123
    hist.gram4.match <- FourGram[which(FourGram$hist==gram4.w123),]
    
    # subset from gram 3 where hist matches gram3.w12
    hist.gram3.match <- TriGram[which(TriGram$hist==gram3.w12),]
    
    # subset gram 3 into words predicted by gram 4 or not predicted
    gr3.in.gr4 <- hist.gram3.match[hist.gram3.match$pred %in% hist.gram4.match$pred,]
    gr3.notin.gr4 <- hist.gram3.match[!(hist.gram3.match$pred %in% hist.gram4.match$pred),]
    
    # calculate alpha for gram 2(call gama the denominator of alpha)
    beta.gr3 <- 1- sum(hist.gram4.match$GTprob)
    gama.gr3 <- 1 - sum(gr3.in.gr4$GTprob)
    alpha.gr3 <- beta.gr3 / gama.gr3
    
    # Calculate KatzProb (Kprob) for gram 3
    gr3.notin.gr4$Kprob <- gr3.notin.gr4$GTprob * alpha.gr3
    # Calculate KatzProb for gram 4
    hist.gram4.match$Kprob <- hist.gram4.match$GTprob
    
    # gram 3 match and backoff to gram 2
    
    # subset from gram 2 where hist matches gram2.w1
    hist.gram2.match <- TwoGram[which(TwoGram$hist==gram2.w1),]
    
    # subset gram 2 into words predicted by gram 3 or not predicted
    gr2.in.gr3 <- hist.gram2.match[hist.gram2.match$pred %in% hist.gram3.match$pred,]
    gr2.notin.gr3 <- hist.gram2.match[!(hist.gram2.match$pred %in% hist.gram3.match$pred),]
    
    # calculate alpha for gram 2(call gama the denominator of alpha)
    beta.gr2 <- 1- sum(hist.gram3.match$GTprob)
    gama.gr2 <- 1 - sum(gr2.in.gr3$GTprob)
    alpha.gr2 <- beta.gr2 / gama.gr2
    
    # Calculate KatzProb (Kprob) for gram 2
    gr2.notin.gr3$Kprob <- gr2.notin.gr3$GTprob * alpha.gr2 * alpha.gr3
    
    #### backoff to gram 1
    
    # subset gram 1 into words predicted by gram 2 or not predicted
    gr1.in.gr2 <- OneGram[OneGram$pred %in% hist.gram2.match$pred,]
    gr1.notin.gr2 <- OneGram[!(OneGram$pred %in% hist.gram2.match$pred),]
    
    # calculate alpha for gram 1(call gama the denominator of alpha)
    beta.gr1 <- 1- sum(hist.gram2.match$GTprob)
    gama.gr1 <- 1 - sum(gr1.in.gr2$GTprob)
    alpha.gr1 <- beta.gr1 / gama.gr1
    
    # Calculate KatzProb for gram 1
    gr1.notin.gr2$Kprob <- gr1.notin.gr2$GTprob * alpha.gr1 * alpha.gr2 * alpha.gr3
    # Calculate KatzProb for gram 2
    
    # rbind gr1 and gr matches, and sort
    prediction <- rbind(hist.gram4.match[1:1000,], gr3.notin.gr4[1:1000,], gr2.notin.gr3[1:1000,] ,gr1.notin.gr2[1:1000,])
    prediction <- prediction[order(-prediction$Kprob),]
    prediction <- prediction[,c("pred", "Kprob", "hist", "GTfreq","GTprob")]
    names(prediction) <- c("Predicted", "Katz Prob", "History", "GoodTuring Freq", "GoodTuring Prob")
    
    rm(hist.gram4.match, hist.gram3.match, hist.gram2.match, 
       gr3.in.gr4, gr3.notin.gr4, gr2.in.gr3, gr2.notin.gr3, 
       gr1.in.gr2, gr1.notin.gr2, beta.gr3, gama.gr3, alpha.gr3,
       beta.gr2, gama.gr2, alpha.gr2, beta.gr1, gama.gr1, alpha.gr1)
    
    return(prediction)
}


###***********************************************
### main function ####
###***********************************************

predWord.4grm <- function(input, OneGram, TwoGram, TriGram, FourGram){
    
    # 1. Check of the phrase is longer than one word
    # 2. Clean the phrase
    # 3. Check if the last word in the phrase is a break.word (defined below) and if yes = error
    # 4. Check if any of the words in the phrase is a break.word (defined below) and if yes 
    # move to an n-1 gram and repeat step 4 until you find an n-gram with no break words
    
    # number of words in user input, pre-cleaning
    n.words.input <-length(strsplit(input, "\\s+")[[1]])
    
    clean.input <- input
    
    # error message if no input 
    if (n.words.input <1) stop("Please input at least one word")    # error handling
    
    # clean the input phrase and Count number of words in the phrase
    # clean.input <- cleanInput(input)
    clean.input.words <- strsplit(clean.input, "\\s+")[[1]] # vector of the words in the clean input
    n.words <-length(clean.input.words)
    
    # a vector with words that break an ngram if located within the ngram
    ngram.break <- as.list(c("eeoss", "aabrr", "nnumm", "eemaill", "hhtmll", "rtvia", "atpple", "uusrnmss"))
    
    # if the last word is a ngram.break word, something that's not an english word, stop
    if (any(unlist(lapply(ngram.break, function(x) grepl(x,clean.input.words[n.words]))))) 
        stop("The last sequence of characters is something other than an English word.\n",
             "Please input at least one word.")    # error handling 
    
    ###*******************************
    # phrase is at least 3 words long 
    ###*******************************
    
    # 1. extract 3,2,1 last words from phrase
    # 2. check longest 3 word extract for break.words. If yes go down to 2 and then 1 word
    # 3. once a group of words contains no break.words then we execute Katz backoff from this level of ngram
    # suppose the last 3 words contain a break.word, but the last 2 don't, then
    # we execute Katz backoff from ngram 3.
    
    if (n.words >= 3) {
        # extract the (n-1) words from ngrams from the last words in the phrase
        gram4.w123 <- paste(clean.input.words[n.words - 2],
                            clean.input.words[n.words - 1],
                            clean.input.words[n.words], sep=" ")
        gram3.w12 <- sub("^[a-z]+ ","",gram4.w123)
        gram2.w1 <- sub("^[a-z]+ ","",gram3.w12)
        
        # if any of the words in the ngram4.w123 is a .ngram.break word, then fail, move to n-1 ngram
        if ( any(unlist(lapply(ngram.break, function(x) grepl(x,gram4.w123)))) ){
            # if any of the words in the ngram3.w12 is a .ngram.break word, then fail, move to n-1 ngram
            if ( any(unlist(lapply(ngram.break, function(x) grepl(x,gram3.w12)))) ){
                ### execute from a 2 gram as the two gram has already been checked for EOS breakwords
                match.w1.count <- sum(TwoGram[which(TwoGram$hist==gram2.w1),"GTfreq"])
                if (match.w1.count == 0) { # match.w1.count=0 in gram2 therefore use Katz backoff to gram1
                    # export matches from OneGram table
                    prediction <- predict.1grm(OneGram)
                }
                
                else { # export matches from TwoGram$hist==gram2.w1
                    prediction <- predict.2grm(TwoGram, OneGram, gram2.w1)
                }
                
            }
            else { ### execute from a 3 gram, Katz back-off 
                match.w12.count <- sum(TriGram[which(TriGram$hist==gram3.w12),"GTfreq"])
                if (match.w12.count == 0) { # match.w12.count=0 therefore use Katz backoff to gram2.w1
                    match.w1.count <- sum(TwoGram[which(TwoGram$hist==gram2.w1),"GTfreq"])
                    if (match.w1.count == 0) { # match.w1.count=0 in gram2 therefore use Katz backoff to gram1
                        # export matches from OneGram table
                        prediction <- predict.1grm(OneGram)
                    }
                    
                    else { # export matches from TwoGram$hist==gram2.w1
                        prediction <- predict.2grm(TwoGram, OneGram, gram2.w1)
                    }
                }
                else { # export matches from TriGram$w12==gram3.w12
                    prediction <- predict.3grm(TriGram,TwoGram, OneGram, gram3.w12, gram2.w1)            
                }              
            }
            
        } else { # There are no break words in 4gram gram4.w123.
            # execute from a 4 gram, Katz back-of
            
            ###
            # start checking for matches and working backwards w Katz back-off when necessary
            ###
            
            # Count how many times we find the gram4.w123 in the 4 gram table, in FourGram$w123
            match.w123.count <- sum(FourGram[which(FourGram$hist==gram4.w123),"GTfreq"])
            
            if (match.w123.count == 0) { # match.w123.count=0 therefore use Katz backoff to gram3.w12
                match.w12.count <- sum(TriGram[which(TriGram$hist==gram3.w12),"GTfreq"])
                if (match.w12.count == 0) { # match.w12.count=0 therefore use Katz backoff to gram2.w1
                    match.w1.count <- sum(TwoGram[which(TwoGram$hist==gram2.w1),"GTfreq"])
                    if (match.w1.count == 0) { # match.w1.count=0 in gram2 therefore use Katz backoff to gram1
                        # export matches from OneGram table
                        prediction <- predict.1grm(OneGram)
                    }
                    
                    else { # export matches from TwoGram$hist==gram2.w1
                        prediction <- predict.2grm(TwoGram, OneGram, gram2.w1)
                    }
                }
                else { # export matches from TriGram$w12==gram3.w12
                    prediction <- predict.3grm(TriGram,TwoGram, OneGram, gram3.w12, gram2.w1)            
                }        
            }
            else { # export matches from FourGram$w123==gram4.w123
                prediction <- predict.4grm(FourGram, TriGram,TwoGram, OneGram, gram4.w123, gram3.w12, gram2.w1)
            }
        }
    }
    ###*********************
    # phrase is 2 words long
    ###*********************
    else if (n.words==2 ){
        # extract the (n-1) words from ngrams from the last words in the phrase
        gram3.w12 <- clean.input
        gram2.w1 <- sub("^[a-z]+ ","",gram3.w12)
        
        # if any of the words in the ngram3.w12 is a .ngram.break word, then fail, move to n-1 ngram
        if ( any(unlist(lapply(ngram.break, function(x) grepl(x,gram3.w12)))) ){
            ### execute from a 2 gram as the two gram has already been checked for EOS breakwords
            match.w1.count <- sum(TwoGram[which(TwoGram$hist==gram2.w1),"GTfreq"])
            if (match.w1.count == 0) { # match.w1.count=0 in gram2 therefore use Katz backoff to gram1
                # export matches from OneGram table
                prediction <- predict.1grm(OneGram)
            }
            
            else { # export matches from TwoGram$hist==gram2.w1
                prediction <- predict.2grm(TwoGram, OneGram, gram2.w1)
            }
            
        }
        else { ### execute from a 3 gram, Katz back-off 
            match.w12.count <- sum(TriGram[which(TriGram$hist==gram3.w12),"GTfreq"])
            if (match.w12.count == 0) { # match.w12.count=0 therefore use Katz backoff to gram2.w1
                match.w1.count <- sum(TwoGram[which(TwoGram$hist==gram2.w1),"GTfreq"])
                if (match.w1.count == 0) { # match.w1.count=0 in gram2 therefore use Katz backoff to gram1
                    # export matches from OneGram table
                    prediction <- predict.1grm(OneGram)
                }
                
                else { # export matches from TwoGram$hist==gram2.w1
                    prediction <- predict.2grm(TwoGram, OneGram, gram2.w1)
                }
            }
            else { # export matches from TriGram$w12==gram3.w12
                prediction <- predict.3grm(TriGram,TwoGram, OneGram, gram3.w12, gram2.w1)            
            }              
        }
        
    }
    ###*************************
    # phrase is just 1 word long 
    ###*************************
    else {
        gram2.w1 <- clean.input
        
        ### execute from a 2 gram as the two gram has already been checked for EOS breakwords
        match.w1.count <- sum(TwoGram[which(TwoGram$hist==gram2.w1),"GTfreq"])
        if (match.w1.count == 0) { # match.w1.count=0 in gram2 therefore use Katz backoff to gram1
            # export matches from OneGram table
            prediction <- predict.1grm(OneGram)
        }
        
        else { # export matches from TwoGram$hist==gram2.w1
            prediction <- predict.2grm(TwoGram, OneGram, gram2.w1)
        }
    }
    return(prediction)       
}


