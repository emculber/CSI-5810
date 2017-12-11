###############################################################
#
#  description:  returns the normalized google distance as
#                numeric value
#
#  usage:        NGD(words, language, print, list, ...)
#
#  arguments:
#                words      TWO terms to measure for in
#                           vector form e.g. c("wiki","R")
#                language   in which lnguage to search.
#                           Either "en" (english) or
#                           "de" (german)
#                print      print alls results (NGD, counts)
#                           to console (no default)
#                list       returns list of results (no default)
#                           containing NGD and all counts.
#                ...        at the moment nothing


NGD <- function(words, language="de", print=FALSE,
                list=FALSE, ...){

  # check for arguments
  if(!hasArg(words)) stop('NGD needs TWO strings like
                          c("word","word2") as word argument!')
                          if(length(words)!=2) stop('word arguments has to be of
                                                    length two, e.g. c("word","word2")')

                                                    # M: total number of web pages searched by google (2007)
                                                    if(hasArg(M)) M <- list(...)$M else M <- 8058044651    

                                                    x <- words[1]
                                                    y <- words[2]

                                                    # using getGoogleCount() function (see here)
                                                    freq.x  <- getGoogleCount(x, language=language)
                                                    freq.y  <- getGoogleCount(y, language=language)
                                                    freq.xy <- getGoogleCount(c(x,y), language=language)

                                                    # apply formula
                                                    NGD = (max(log(freq.x), log(freq.y)) - log(freq.xy)) /
                                                      (log(M) - min( log(freq.x), log(freq.y)) )

                                                    # print results to console if requested
                                                    if(print==TRUE){
                                                      cat("\t", x,":", freq.x, "\n",
                                                          "\t", y,":", freq.y, "\n",
                                                          "\t", x,"+", y,":", freq.xy, "\n",
                                                          "\t", "normalized google distance (NGD):",
                                                          NGD, "\n", "\n")
                                                    }


                                                    # return list of results if requested (no default)
                                                    # containing NGD and all counts. As default only one
                                                    # the NGD is returned as numeric value

                                                    results <- list(NGD=NGD,
                                                                    x=c(x, freq.x),
                                                                    y=c(y, freq.y),
                                                                    xy=c(paste(x,"+",y), freq.xy)) 

                                                    if(list==TRUE) return(results) else  return(NGD)
}


# NOT RUN:

NGD(c("rider","horse", "criket"), print=T)
NGD(c("rider","horse"), list=TRUE)             # returns a list
# may be applied to dataframes
DF <- data.frame(A=c("rider","religion"), B=c("horse","god"))
apply(DF, 1, NGD, print=TRUE)    

###############################################################
