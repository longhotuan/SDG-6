#### import libraries ####
# data wrangling

library(tidyverse)
library(reshape)
library(data.table)
library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(rscopus)
library(rlist)
library(rgeos)
library(future)
library(parallel)
library(doParallel)

# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(corrplot)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(bibliometrix)
library(ggwordcloud)
library(colorspace)
library(rworldmap)
library(countrycode)
library(usethis)

#### From Scopus website (csv) ####

temp <- list.files(pattern="*.csv")
myfiles <- lapply(temp, read_csv)
myfiles <- lapply(myfiles, function(x) x <- x[,-1])
colnames_chosen <- colnames(myfiles[[2]])[c(1, 3:5, 12, 13, 15, 17:19, 41:43, 45, 47)]

#choose the useful columns
myfiles <- lapply(myfiles, subset, select = colnames_chosen)

sdg6 <- rbindlist(myfiles)

sdg6$`Source title` <- as.factor(sdg6$`Source title`)
sdg6$`Language of Original Document`<- as.factor(sdg6$`Language of Original Document`)
sdg6$`Document Type` <- as.factor(sdg6$`Document Type`)
sdg6$`Access Type` <- as.factor(sdg6$`Access Type`)
sdg6$`Abbreviated Source Title` <- as.factor(sdg6$`Abbreviated Source Title`)
sdg6 <- sdg6 %>% filter(Year < 2020 & Year > 1999)

#### General info ####

# document types

sdg6_list <- split(x = sdg6, f= as.factor(sdg6$Year))

save_DT <- function(x){
    y <- summary(x$`Document Type`)
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
}


sdg6_DT <- lapply(sdg6_list, save_DT)
lapply(names(sdg6_DT), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = sdg6_DT[x], row.names = FALSE))


write.csv(file = "SDG6_v2_DT.csv", x = save_DT(sdg6), row.names = FALSE)

# language of the document 
save_language <- function(x){
    y <- summary(as.factor(x$`Language of Original Document`))
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
    z <- as.data.frame(y$y*100/sum(y$y))
    colnames(z) <- "Percentage"
    y <- bind_cols(y,z)
}

SDG6_Lang <- lapply(sdg6_list, save_language)
lapply(names(SDG6_Lang), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang[x], row.names = FALSE))

write.csv(file = "SDG6_v2_Lang.csv", x = save_language(sdg6), row.names = FALSE)

# Open access

save_access <- function(x){
    y <- summary(as.factor(x$`Access Type`))
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
    z <- as.data.frame(y$y*100/sum(y$y))
    colnames(z) <- "Percentage"
    y <- bind_cols(y,z)
}

SDG6_AT <- lapply(sdg6_list, save_access)
lapply(names(SDG6_AT), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT[x], row.names = FALSE))

write.csv(file = "SDG6_v2_AT.csv", x = save_access(sdg6), row.names = FALSE)

# Citation

save_citation <- function(x, y){
    x <- x %>% 
        arrange(desc(`Cited by`)) %>% 
        slice(1:y)
}

SDG6_TC <- lapply(sdg6_list, save_citation, 20)
lapply(names(SDG6_TC), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC[x], row.names = FALSE))

write.csv(file = "SDG6_v2_TC.csv", x = save_citation(sdg6, 20), row.names = FALSE)


#** Publication years ----

save_pubyear <- function(x){
    y <- x %>% select(Year,`Document Type`, `Access Type`) %>% 
        dplyr::group_by(Year) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(Year)
    z <- as.data.frame(ave(y$n, FUN = cumsum))
    colnames(z) <- "cum"
    y <- bind_cols(y, z)
    t <- ggplot(y, aes(x=Year, y=cum))+
        geom_point(size = 2) +
        geom_smooth(colour="gray20", size =0.5, method = "lm") +
        labs(x = "Years", y = "Cumulative publications", fill = NULL, title = NULL) +
        scale_x_continuous(breaks = c(2008:2020))+
        theme_bw()
}

SDG_PU <- lapply(sdg6_list, save_pubyear)

lapply(names(SDG_PU), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU[[x]], units = 'cm', height = 20, width = 20, dpi = 300))

ggsave(filename="sdg6_v2_PU.jpeg", plot=save_pubyear(sdg6), units = 'cm', height = 20, width = 20, dpi = 300)


#** Top Journal  ----

save_topjournal <- function(x){
    y <- x %>% select(`Source title`) %>% 
        dplyr::group_by(`Source title`) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        slice(1:10) %>% 
        ggplot(aes(x=reorder(`Source title`, n),y = n)) +
        geom_bar(stat = "identity",
                 position = position_stack(reverse = TRUE), 
                 fill = "tomato") +
        coord_flip() +
        theme_bw() +
        xlab("Journals") +
        ylab("Number of publications") +
        theme(text=element_text(family = "Arial")) +
        theme(axis.text.x = element_text(size = 14)) +
        theme(axis.text.y = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        theme(axis.title.y = element_blank())
}

SDG6_Topjournal <- lapply(sdg6_list, save_topjournal)

lapply(names(SDG6_Topjournal), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal[[x]], units = 'cm', height = 20, width = 30, dpi = 300))

ggsave(filename="sdg6_v2_TopJ.jpeg", plot=save_topjournal(sdg6), units = 'cm', height = 20, width = 30, dpi = 300)

##### Top authors all ----

AU2 <- function(x){
    author <- strsplit(x, ", ")
    for (i in 1:length(author)){
        author[i] <- as.data.frame(matrix(as.data.frame(author[i])))
    }
    author2 <- data.table::rbindlist(author)
    colnames(author2)[1]<- "author"
    author2<- author2[complete.cases(author2),]
    author3 <- author2 %>%
        dplyr::group_by(author) %>%
        dplyr::summarise(n=n()) %>%
        dplyr::arrange(desc(n))
    return(author3)
}
AU_name <- function(x, y){
    # Find the overlapped name of the y-most productive authors
    
    x1 <- vector("list", y)
    for (i in 1:y){
        x1[[i]] <- as.character(x$author[i])
        for (j in 1:nrow(x)){
            if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) != "van" & 
                str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) != "de"){# if no "Van"
                if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) == # if the first names are the same
                    str_to_lower(str_split_fixed(x$author[i], ' ',2)[1])){
                    if( i !=j){
                        x1[[i]] <- list.append(x1[[i]], as.character(x$author[j]))
                    }
                }
            } else {
                if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[2]) == # if having "Van"
                    str_to_lower(str_split_fixed(x$author[i], ' ',2)[2])){# if the first names are the same
                    if( i !=j){
                        x1[[i]] <- list.append(x1[[i]], as.character(x$author[j]))
                    }
                }
            }
        }
    }
    
    # replace the one with no problems
    
    for (i in length(x1):1){
        if (length(x1[[i]]) == 1){
            x1 <- x1[-i]
        }
    }
    # choose the names that were from the same authors
    x2 <- list()
    for (i in 1:length(x1)){
        for (j in 2:length(x1[[i]])){
            if (str_to_lower(str_split_fixed(x1[[i]][1], "\\.",2)[1]) == # before the dot is the same indicates the same authors
                str_to_lower(str_split_fixed(x1[[i]][j], "\\.",2)[1])){
                x1[[i]]<-append(x1[[i]][1],x1[[i]][j]) # only choose the overlapped ones
                x2 <- list.append(x2,x1[[i]])
            }
        }
    }
    # replace the ones that are the same authors
    for (i in length(x2):1){
        for(j in length(x2):1){
            if (str_to_lower(str_split_fixed(x2[[i]][1], "\\.",2)[1]) ==
                str_to_lower(str_split_fixed(x2[[j]][1], "\\.",2)[1])
                & j<i){
                x2 <- x2[-i]
            }
        }
    }
    
    # replace the overlapped name
    
    x$author <- as.character(x$author)
    
    for (i in 1:nrow(x)){
        for(j in 1:length(x2)){
            if (length(x2[[j]]) == 2){
                if(nchar(x2[[j]][1]) > nchar(x2[[j]][2])){
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][1], x2[[j]][2])
                } else {
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][2], x2[[j]][1])
                }
            } else {
                for(k in 2:length(x2[[j]])){
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][k], x2[[j]][1])
                }
            }
        }
    }
    
    # rerank these names
    
    x <-aggregate(n ~ author, data=x, sum) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::filter(author != "Jr.")
    
    return(x)
}
AU_list <- function(x, y){
    # Find the overlapped name of the y-most productive authors
    
    x1 <- vector("list", y)
    for (i in 1:y){
        x1[[i]] <- as.character(x$author[i])
        for (j in 1:nrow(x)){
            if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) != "van" & 
                str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) != "de"){# if no "Van"
                if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[1]) == # if the first names are the same
                    str_to_lower(str_split_fixed(x$author[i], ' ',2)[1])){
                    if( i !=j){
                        x1[[i]] <- list.append(x1[[i]], as.character(x$author[j]))
                    }
                }
            } else {
                if (str_to_lower(str_split_fixed(x$author[j], ' ',2)[2]) == # if having "Van"
                    str_to_lower(str_split_fixed(x$author[i], ' ',2)[2])){# if the first names are the same
                    if( i !=j){
                        x1[[i]] <- list.append(x1[[i]], as.character(x$author[j]))
                    }
                }
            }
        }
    }
    
    # replace the one with no problems
    
    for (i in length(x1):1){
        if (length(x1[[i]]) == 1){
            x1 <- x1[-i]
        }
    }
    # choose the names that were from the same authors
    x2 <- list()
    for (i in 1:length(x1)){
        for (j in 2:length(x1[[i]])){
            if (str_to_lower(str_split_fixed(x1[[i]][1], "\\.",2)[1]) == # before the dot is the same indicates the same authors
                str_to_lower(str_split_fixed(x1[[i]][j], "\\.",2)[1])){
                x1[[i]]<-append(x1[[i]][1],x1[[i]][j]) # only choose the overlapped ones
                x2 <- list.append(x2,x1[[i]])
            }
        }
    }
    # replace the ones that are the same authors
    for (i in length(x2):1){
        for(j in length(x2):1){
            if (str_to_lower(str_split_fixed(x2[[i]][1], "\\.",2)[1]) ==
                str_to_lower(str_split_fixed(x2[[j]][1], "\\.",2)[1])
                & j<i){
                x2 <- x2[-i]
            }
        }
    }
    
    # replace the overlapped name
    
    x$author <- as.character(x$author)
    
    for (i in 1:nrow(x)){
        for(j in 1:length(x2)){
            if (length(x2[[j]]) == 2){
                if(nchar(x2[[j]][1]) > nchar(x2[[j]][2])){
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][1], x2[[j]][2])
                } else {
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][2], x2[[j]][1])
                }
            } else {
                for(k in 2:length(x2[[j]])){
                    x$author[i] <- str_replace_all(x$author[i], x2[[j]][k], x2[[j]][1])
                }
            }
        }
    }
    
    # rerank these names
    # 
    # x <-aggregate(n ~ author, data=x, sum) %>%
    #     dplyr::arrange(desc(n)) %>%
    #     dplyr::filter(author != "Jr.")
    # 
    return(x)
}

WN_topau_sdg6 <- AU2(sdg6$Authors)
# WN_TopAU2 <- WN_TopAU2 %>% filter(n >= 10) # doesn't count the ones less than 10 publications
WN_topau_sdg6_2 <- AU_name(WN_topau_sdg6, 20)

ggsave("WN_TopAU_sdg6.jpeg", ggplot(WN_topau_sdg6_2[1:20,], aes(x=reorder(author, n),y = n)) +
           geom_bar(stat = "identity",
                    position = position_stack(reverse = TRUE),
                    fill = "tomato") +
           coord_flip() +
           theme_bw() +
           xlab("Authors") +
           ylab("Number of publications (-)") +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)) +
           theme(axis.title.y = element_blank()),
       units = 'cm', height = 20, width = 30, dpi = 300
)

# For each year

AU <- function(x){
    author <- strsplit(x$Authors, ", ")
    for (i in 1:length(author)){
        author[i] <- as.data.frame(matrix(as.data.frame(author[i])))
    }
    author2 <- data.table::rbindlist(author)
    colnames(author2)[1]<- "author"
    author2<- author2[complete.cases(author2),]
    author3 <- author2 %>%
        dplyr::group_by(author) %>%
        dplyr::summarise(n=n()) %>%
        dplyr::arrange(desc(n))
    return(author3)
}

WN_topau_sdg6_list <- lapply(sdg6_list, AU)
WN_topau_sdg6_list_2 <- lapply(WN_topau_sdg6_list, AU_name, 50)


save_topau_2 <- function(x){
    y <- ggplot(x[1:20,], aes(x=reorder(author, n),y = n)) +
        geom_bar(stat = "identity",
                 position = position_stack(reverse = TRUE),
                 fill = "tomato") +
        coord_flip() +
        theme_bw() +
        xlab("Authors") +
        ylab("Number of publications (-)") +
        theme(text=element_text(family = "Arial")) +
        theme(axis.text.x = element_text(size = 14)) +
        theme(axis.text.y = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        theme(axis.title.y = element_blank())
}

SDG6_Topau <- lapply(WN_topau_sdg6_list, save_topau_2)


lapply(names(SDG6_Topau), function(x) ggsave(filename=paste(x,"_Topau.jpeg",sep=""), plot=SDG6_Topau[[x]], units = 'cm', height = 20, width = 30, dpi = 300))

