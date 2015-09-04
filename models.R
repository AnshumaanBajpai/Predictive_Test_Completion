## This script contains all the various models that are available as options
## for the user. It has details about three models. 1) Stupid back Off
## 2) Good Turing Smoothing and 3) Kneser Ney Smoothing

## Loading the datatables for n-grams. On loading the n-grams are saved as
## object named uni, bi, tri and quad.
library(data.table)

load('dts')

sub_char <- function(data){
        data <- gsub("\\_+", "", data)
        data <- gsub(" [Uu][\\.][Ss][\\.][Aa]?[\\.]?", " usa.", data)
        data <- gsub("\\$+", "$", data)
        data <- gsub(" & ", " and ", data)
        data <- gsub(" \\'+|\\+' ", " ", data)
        data <- gsub(" \\*+|\\*+ ", " ", data)
        data <- gsub(" [Aa]\\.[Mm][\\.]?", " am.", data)
        data <- gsub(" [Pp]\\.[Mm][\\.]?", " pm.", data)
        data <- gsub("!+|\\^+", ". ", data)
        data <- gsub("=+|\\-+", " ", data)
        data <- gsub("[0-9]*\\-*>+|<+\\-*[0-9]*", " ", data)
        data <- gsub("[\\)|\\(]:", " ", data)
        data <- gsub(" [\\(|\\)| D| Pp|\\/][\\']?[\\-]?[:|;]", " <emoticon> ", data)
        data <- gsub(" [:|;][\\-]?[\\']?[\\(|\\)|D |Pp |\\/]+", " <emoticon> ", data)
        data <- gsub(" *\\?+", ".", data)
        data <- gsub("#+", " hashtag ", data)
        data <- gsub("[a-zA-Z]*[Ff][Uu][Cc][Kk][a-zA-Z]*|[Cc][Uu][Nn][Tt]|[Ss][Hh][Ii][Tt]|[Cc][Oo][Cc][Kk][Ss][Uu][Cc][Kk][Ee][Rr]|[Pp][Ii][Ss][Ss]|[Tt][Ii][Tt][Ss]", " <profanity> ", data)
        data <- gsub("[a-zA-Z]+\\*+[a-zA-Z]+", " <profanity> ", data)
        data <- gsub("\\$+ *[0-9]+[ |,|.]?[0-9]*", " <dollaramount> ", data)
        data <- gsub(" [0-9][0-9]?[\\/][0-9][0-9]?[\\/][0-9][0-9][0-9]?[0-9]?", " <date> ", data)
        data <- gsub("[0-9]+\\.?[0-9]+|[0-9]+", " <num> ", data)
        data <- gsub(" *\\.+ *\\.*", "\\. ", data)
        data <- gsub(" \\'+ ", " ", data)
        data <- gsub("%", " ", data)
        data <- tolower(data)
        
        return(data)
}



## Algorithm for stupid back off
StupidBackOff <- function(input_str, n){
        
        input_str <- sub_char(input_str)
        ## A dataframe to hold the top 20 predicted words, five of each based on uni, bi, tri and quad grams
        output_df_SBO <- data.frame(next_w = character(), s_value = double())
        
        ## Taking care of quadgrams available for the input string
        if (length(unlist(strsplit(input_str, " "))) >=3){
                ## Find out the available quadgrams
                quad_avail = quad[quad$tri == paste(tail(unlist(strsplit(input_str, " ")), 3), collapse = " "), ][order(-freq, term)]
                if (dim(quad_avail)[1] > 0){
                        quad_avail$next_w <- lapply(strsplit(quad_avail$term, " "), last_w <- function(lst){
                                return(paste(lst[4], collapse = ""))})
                        quad_avail$s_value <- quad_avail$freq/tri[term == paste(tail(unlist(strsplit(input_str, " ")), 3), collapse = " "), freq]
                        output_df_SBO <- rbind(output_df_SBO, as.data.frame(quad_avail[1:5, c("next_w", "s_value"), with = FALSE]))
                }
        }
        
        ## Taking care of trigrams available for the input string
        if (length(unlist(strsplit(input_str, " "))) >=2){
                ## Find out the available trigrams
                tri_avail = tri[tri$bi == paste(tail(unlist(strsplit(input_str, " ")), 2), collapse = " "), ][order(-freq, term)]
                if (dim(tri_avail)[1] > 0){
                        tri_avail$next_w <- lapply(strsplit(tri_avail$term, " "), last_w <- function(lst){
                                return(paste(lst[3], collapse = ""))})
                        tri_avail$s_value <- 0.4*tri_avail$freq/bi[term == paste(tail(unlist(strsplit(input_str, " ")), 2), collapse = " "), freq]
                        output_df_SBO <- rbind(output_df_SBO, as.data.frame(tri_avail[1:5, c("next_w", "s_value"), with = FALSE]))
                }
        }

        ## Taking care of bigrams available for the input string
        if (length(unlist(strsplit(input_str, " "))) >=1){
                ## Find out the available trigrams
                bi_avail = bi[bi$uni == paste(tail(unlist(strsplit(input_str, " ")), 1), collapse = " "), ][order(-freq, term)]
                if (dim(bi_avail)[1] > 0){
                        bi_avail$next_w <- lapply(strsplit(bi_avail$term, " "), last_w <- function(lst){
                                return(paste(lst[2], collapse = ""))})
                        bi_avail$s_value <- 0.4*0.4*bi_avail$freq/uni[term == paste(tail(unlist(strsplit(input_str, " ")), 1), collapse = " "), freq]
                        output_df_SBO <- rbind(output_df_SBO, as.data.frame(bi_avail[1:5, c("next_w", "s_value"), with = FALSE]))
                }
        }

        ## Taking care of Unigrams available for the input string
        if (length(unlist(strsplit(input_str, " "))) >=0){
                ## Find out the available Unigrams
                uni_avail = uni[order(-freq, term)]
                uni_avail$next_w <- uni_avail$term
                uni_avail$s_value <- 0.4*0.4*0.4*uni_avail$freq/sum(uni_avail$freq)
                output_df_SBO <- rbind(output_df_SBO, as.data.frame(uni_avail[1:5, c("next_w", "s_value"), with = FALSE]))
        }
        output_df_SBO <- na.omit(output_df_SBO)
        output_df_SBO <- output_df_SBO[order(-output_df_SBO$s_value),]
        output_df_SBO <- output_df_SBO[!duplicated(output_df_SBO[,1]),]
        return(unlist(output_df_SBO$next_w[1:n]))
}
