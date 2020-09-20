# import libraries ####
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
library(feather)

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

# Import datasets  ####

temp <- list.files(pattern="*.csv")
myfiles <- lapply(temp, read_csv)
colnames_chosen <- colnames(myfiles[[1]])[c(2, 4:6, 13, 14, 16, 18, 19, 20, 31:34, 39:42, 43, 44)]

# choose the useful columns
myfiles <- lapply(myfiles, subset, select = colnames_chosen)

factor_convert <- function(x){
    x$`Source title` <- as.factor(x$`Source title`)
    x$`Language of Original Document`<- as.factor(x$`Language of Original Document`)
    x$`Document Type` <- as.factor(x$`Document Type`)
    x$`Access Type` <- as.factor(x$`Access Type`)
    x$`Abbreviated Source Title` <- as.factor(x$`Abbreviated Source Title`)
    x <- x %>% filter(Year < 2019)
}

myfiles <- lapply(myfiles, factor_convert)
names(myfiles) <- str_split_fixed(temp, "\\.", 2)[,1]

# Divide into different periods 2010-2019; 2000-2009; before 2000 ########

period_division <- function(x, y, z){
    x <- x %>% filter(Year < y & Year > z)
}

myfiles_1 <- lapply(myfiles, period_division, y = 2020, z = 2009)
myfiles_2 <- lapply(myfiles, period_division, y = 2010, z = 1999)
myfiles_3 <- lapply(myfiles, period_division, y = 2000, z = 1900)


# Split affiliation and country ####

#** put the affiliation into the dataset ####

affi_split <- function(y){
    if (nrow(y) == 0){
        return(y)
    } else{
        y$Affi <- NA
        for (j in 1: nrow(y)){
            y$Affi[j] <- list(as.list(str_split_fixed(y$Affiliations[j], "; ", 
                                                      n =1+ str_count(y$Affiliations[j], "; "))))
        }
        return(y)
    }
}

myfiles2 <- lapply(myfiles, affi_split) # Split the affiliations into each affiliaton stored in a list
myfiles2_1 <- lapply(myfiles_1, affi_split)
myfiles2_2 <- lapply(myfiles_2, affi_split)
myfiles2_3 <- lapply(myfiles_3, affi_split)


# split into country

myfiles3 <- lapply(myfiles2, function(x) {x$Country <- x$Affi; return(x)})
myfiles3_1 <- lapply(myfiles2_1, function(x) {x$Country <- x$Affi; return(x)})
myfiles3_2 <- lapply(myfiles2_2, function(x) {x$Country <- x$Affi; return(x)})
myfiles3_3 <- lapply(myfiles2_3, function(x) {x$Country <- x$Affi; return(x)})

country_split <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        for (i in 1:nrow(x)){
            for (j in 1:length(x$Affi[[i]])){
                x$Country[[i]][[j]] <- as.list(str_split_fixed(x$Affi[[i]][[j]], ", ", n = 1 + str_count(x$Affi[[i]][[j]], ", ")))
                k <- length(x$Country[[i]][[j]])
                x$Country[[i]][[j]] <-  x$Country[[i]][[j]][[k]]
            }
        }
        return(x)
    }
}

myfiles3 <- lapply(myfiles3, country_split) # split the country in the affiliations to column country as a list
myfiles3_1 <- lapply(myfiles3_1, country_split) # split the country in the affiliations to column country as a list
myfiles3_2 <- lapply(myfiles3_2, country_split) # split the country in the affiliations to column country as a list
myfiles3_3 <- lapply(myfiles3_3, country_split) # split the country in the affiliations to column country as a list


#** Split country name ####

# check with lat long with correct country name 
check_country <- function(x, y){
    if (nrow(x) == 0){
        y <- NA
        return(y)
    } else {
        y <- setdiff(unique(unlist(x$Country)), lat_long2$Country)
        return(y)
    }
}


lat_long2 <- as.data.frame(gCentroid(getMap(resolution="high"), byid=TRUE))
lat_long2$Country <- rownames(lat_long2)
rownames(lat_long2) <- c(1:nrow(lat_long2))
lat_long2 <- bind_rows(lat_long2, data.frame(x = 31.9522, y = 35.2332, Country = 'Palestine'))
lat_long2 <- bind_rows(lat_long2, data.frame(x = 16.2650, y = 61.5510, Country = 'Guadeloupe'))

country_diff <- vector(mode = "list", 9)
country2_diff <- unique(unlist(map2(myfiles3, country_diff, check_country)))

country2_diff_1 <- unique(unlist(map2(myfiles3_1, country_diff, check_country)))
country2_diff_2 <- unique(unlist(map2(myfiles3_2, country_diff, check_country)))
country2_diff_3 <- unique(unlist(map2(myfiles3_3, country_diff, check_country)))

old <- country2_diff

new <- c("United States of America", "Belgium", "Belgium", "Vietnam", "Belgium", "United Republic of Tanzania", "Republic of the Congo", "Belgium", 
          "Ivory Coast", "Poland", "Russia", "Libya", "United States of America", "United States of America", "Italy", "Italy", "France", 
          "Hong Kong S.A.R.", "Syria", "France", "France", "France", "Belgium", "Republic of Serbia", "Democratic Republic of the Congo", 
          "France", "United Kingdom", "Belgium", "Belgium", "Macau S.A.R", "Netherlands", "Belgium", "Belgium", "Belgium", "Belgium", "Netherlands",
          "Belgium", "Saudi Arabia", "Saudi Arabia", "Macedonia", "United Kingdom", "Egypt", "Belgium", "Rwanda", "Netherlands", "Belgium", "Netherlands",
          "Germany", "Germany", "Austria", "France", "France", "Belgium", "Belgium", "France", "Poland")

# change name of the country in myfiles

change_name <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        for(i in 1:nrow(x)){
            for(j in 1:length(x$Country[[i]])){
                for(k in 1:length(old)){
                    if (x$Country[[i]][[j]] == old[k]){
                        x$Country[[i]][[j]] <- new[k]
                    }
                }
            }
        }
        return(x)
    }
}


myfiles4 <- lapply(myfiles3, change_name) # Change thanme of the countries
myfiles4_1 <- lapply(myfiles3_1, change_name) # Change thanme of the countries
myfiles4_2 <- lapply(myfiles3_2, change_name) # Change thanme of the countries
myfiles4_3 <- lapply(myfiles3_3, change_name) # Change thanme of the countries

# Checking if the name is ok 

country3_diff <- vector(mode = "list", 9)
country3_diff <- map2(myfiles4, country3_diff, check_country)
country3_diff2 <- unique(unlist(country3_diff))

# ok 

#### add the names of the country as columns

add_namecolumn <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        b <- as.data.frame(matrix(data = NA, nrow = nrow(x), ncol = length(levels(as.factor(unlist(x$Country))))))
        colnames(b) <- levels(as.factor(unlist(x$Country)))
        y <- bind_cols(x, b)
        return(y)
    }
}

myfiles6 <- lapply(myfiles4, add_namecolumn) # add country column
myfiles6_1 <- lapply(myfiles4_1, add_namecolumn) # add country column
myfiles6_2 <- lapply(myfiles4_2, add_namecolumn) # add country column
myfiles6_3 <- lapply(myfiles4_3, add_namecolumn) # add country column


# remove the country in lat long 

names_country <- lapply(myfiles6, function(x) {y<- unique(unlist(x$Country)); return(y)})
names_country_1 <- lapply(myfiles6_1, function(x) {y<- unique(unlist(x$Country)); return(y)})
names_country_2 <- lapply(myfiles6_2, function(x) {y<- unique(unlist(x$Country)); return(y)})
names_country_3 <- lapply(myfiles6_3, function(x) {if (nrow(x) == 0){ x <- NA; return(x)} else {y<- unique(unlist(x$Country)); return(y)}})

list_latlong <- vector(mode = 'list', 9)
list_latlong <- lapply(list_latlong, function(x) {x <- lat_long2; return(x)})
list_latlong_1 <- list_latlong
list_latlong_2 <- list_latlong
list_latlong_3 <- list_latlong

create_latlong <- function(x, y){
    z <- x %>% filter(Country %in% y)
    z <- z[order(z$Country),]
    return(z)
}

for(i in 1:length(list_latlong)){
    
    list_latlong[[i]] <- list_latlong[[i]] %>% filter(Country %in% names_country[[i]])
    list_latlong[[i]] <- list_latlong[[i]][order(list_latlong[[i]]$Country),]
    
    list_latlong_1[[i]] <- list_latlong_1[[i]] %>% filter(Country %in% names_country_1[[i]])
    list_latlong_1[[i]] <- list_latlong_1[[i]][order(list_latlong_1[[i]]$Country),]
    
    list_latlong_2[[i]] <- list_latlong_2[[i]] %>% filter(Country %in% names_country_2[[i]])
    list_latlong_2[[i]] <- list_latlong_2[[i]][order(list_latlong_2[[i]]$Country),]
    
    list_latlong_3[[i]] <- list_latlong[[i]] %>% filter(Country %in% names_country_3[[i]])
    list_latlong_3[[i]] <- list_latlong[[i]][order(list_latlong_3[[i]]$Country),]
}



# list_latlong <- map2(list_latlong, names_country, create_latlong) # Doesn't work without reasons ?!?!?!?!

create_df <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        y <- x[, (which(colnames(x) == 'Country')+1):ncol(x), drop = F] 
        y <- y[, order(names(y))]
        x[, (which(colnames(x) == 'Country')+1):ncol(x)] <- NULL
        x <- bind_cols(x,y)
        return(x)
    }
}


myfiles7 <- lapply(myfiles6, create_df) # order the name of the country column
myfiles7_1 <- lapply(myfiles6_1, create_df) # order the name of the country column
myfiles7_2 <- lapply(myfiles6_2, create_df) # order the name of the country column
myfiles7_3 <- lapply(myfiles6_3, create_df) # order the name of the country column

# unlist first

unlist_country <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        for(i in seq_len(nrow(x))){
            x$Country[[i]] <- unlist(x$Country[[i]])
        }
        return(x)
    }
}

myfiles7.1 <- lapply(myfiles7, unlist_country)
myfiles7.1_1 <- lapply(myfiles7_1, unlist_country)
myfiles7.1_2 <- lapply(myfiles7_2, unlist_country)
myfiles7.1_3 <- lapply(myfiles7_3, unlist_country)


##** remove the list in the dataframe####
# 
# remove_list <- function(x){
#     if (nrow(x) == 0){
#         return(x)
#     } else {
#         x <- x %>% select(-c("Affi", "Country"))
#     }
# }
# 
# myfiles7.2 <- lapply(myfiles7.1, remove_list)
# myfiles7.2_1 <- lapply(myfiles7.1_1, remove_list)
# myfiles7.2_2 <- lapply(myfiles7.1_2, remove_list)
# myfiles7.2_3 <- lapply(myfiles7.1_3, remove_list)
# 
# names(myfiles7.2_1) <- c("1_SDG 6", "1_Target 6.1", "1_Target 6.2", "1_Target 6.3", "1_Target 6.4", "1_Target 6.5", "1_Target 6.6", "1_Target 6.a", "1_Target 6.b")
# names(myfiles7.2_2) <- c("2_SDG 6", "2_Target 6.1", "2_Target 6.2", "2_Target 6.3", "2_Target 6.4", "2_Target 6.5", "2_Target 6.6", "2_Target 6.a", "2_Target 6.b")
# names(myfiles7.2_3) <- c("3_SDG 6", "3_Target 6.1", "3_Target 6.2", "3_Target 6.3", "3_Target 6.4", "3_Target 6.5", "3_Target 6.6", "3_Target 6.a", "3_Target 6.b")
# 
# save_df <- function(x, y){
#     write.csv(x, file = paste(y, ".csv", sep=""), row.names = FALSE)
#     write_feather(x, path = paste(y, ".feather", sep=""))
# }
# 
# map2(myfiles7.2, names(myfiles7.2), save_df)
# map2(myfiles7.2_1, names(myfiles7.2_1), save_df)
# map2(myfiles7.2_2, names(myfiles7.2_2), save_df)
# map2(myfiles7.2_3, names(myfiles7.2_3), save_df)
# 
#** Making new lat and long columns ####

create_df_latlong <- function(x,z){
    if (nrow(x) == 0){
        return(x)
    } else {
        x_country <- x[, (which(colnames(x) == 'Country')+1):ncol(x), drop = F]

        for (i in seq_len(nrow(x))){ # now it's correct. Need to be seq_len(nrow(data))
            for (j in seq_along(x$Country[[i]])){
                for (k in seq_along(x_country)){
                    if(colnames(x_country[,k]) == x$Country[[i]][j]){
                        x_country[i,k] <- x$Country[[i]][j]
                    }
                }
            }
        }

        x_lat <- x[, (which(colnames(x) == 'Country')+1):ncol(x), drop = F]
        colnames(x_lat) <- paste("lat",colnames(x[,(which(colnames(x) == 'Country')+1):ncol(x)]), sep = "_")
        x_long <- x[, (which(colnames(x) == 'Country')+1):ncol(x), drop = F]
        colnames(x_long) <- paste("long",colnames(x[,(which(colnames(x) == 'Country')+1):ncol(x)]), sep = "_")

        for(i in 1:ncol(x_country)){
            for(j in 1:nrow(x_country)){
                if(!is.na(x_country[j,i])){
                    x_lat[j,i] <- z$x[i]
                    x_long[j,i] <- z$y[i]
                }
            }
        }
        x[, (which(colnames(x) == 'Country')+1):ncol(x)] <- NULL
        x <- bind_cols(x, x_country, x_lat, x_long)
        return(x)
    }
}

myfiles8 <- map2(myfiles7.1, list_latlong, create_df_latlong) # lat long has to contain the df of lat-long of each df in the list
myfiles8_1 <- map2(myfiles7.1_1, list_latlong, create_df_latlong) # lat long has to contain the df of lat-long of each df in the list
myfiles8_2 <- map2(myfiles7.1_2, list_latlong, create_df_latlong) # lat long has to contain the df of lat-long of each df in the list
myfiles8_3 <- map2(myfiles7.1_3, list_latlong, create_df_latlong) # lat long has to contain the df of lat-long of each df in the list


#**Save the dataframe ####

# remove the list in the dataframe

remove_list <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        x <- x %>% select(-c("Affi", "Country"))
    }
}

myfiles9 <- lapply(myfiles8, remove_list)
myfiles9_1 <- lapply(myfiles8_1, remove_list)
myfiles9_2 <- lapply(myfiles8_2, remove_list)
myfiles9_3 <- lapply(myfiles8_3, remove_list)

names(myfiles9_1) <- c("1_SDG 6", "1_Target 6.1", "1_Target 6.2", "1_Target 6.3", "1_Target 6.4", "1_Target 6.5", "1_Target 6.6", "1_Target 6.a", "1_Target 6.b")
names(myfiles9_2) <- c("2_SDG 6", "2_Target 6.1", "2_Target 6.2", "2_Target 6.3", "2_Target 6.4", "2_Target 6.5", "2_Target 6.6", "2_Target 6.a", "2_Target 6.b")
names(myfiles9_3) <- c("3_SDG 6", "3_Target 6.1", "3_Target 6.2", "3_Target 6.3", "3_Target 6.4", "3_Target 6.5", "3_Target 6.6", "3_Target 6.a", "3_Target 6.b")

save_df_LL <- function(x, y){
    write.csv(x, file = paste(y, "LL.csv", sep=""), row.names = FALSE)
    write_feather(x, path = paste(y, "LL.feather", sep=""))
}

map2(myfiles9, names(myfiles8), save_df_LL)
map2(myfiles9_1, names(myfiles9_1), save_df_LL)
map2(myfiles9_2, names(myfiles9_2), save_df_LL)
map2(myfiles9_3, names(myfiles9_3), save_df_LL)


# Global South filtration ####

global_south <- read_csv("Global_South.csv", locale = readr::locale(encoding = "latin1"))
global_south_2 <- read_csv("Global_South_fixed.csv", locale = readr::locale(encoding = "latin1")) # good one

# different between Global South list and GS in the Belgian WR

diff_gs <- setdiff(colnames(myfiles9[[1]])[21:(str_which(colnames(myfiles9[[1]]), "lat")-1)[1]], global_south_2$Country)
diff_gs2 <- setdiff(global_south$Country, colnames(myfiles9[[1]])[21:(str_which(colnames(myfiles9[[1]]), "lat")-1)[1]])

diff_gs_1 <- setdiff(colnames(myfiles9_1[[1]])[21:(str_which(colnames(myfiles9_1[[1]]), "lat")-1)[1]], global_south_2$Country)
diff_gs_2 <- setdiff(colnames(myfiles9_2[[1]])[21:(str_which(colnames(myfiles9_2[[1]]), "lat")-1)[1]], global_south_2$Country)
diff_gs_3 <- setdiff(colnames(myfiles9_3[[1]])[21:(str_which(colnames(myfiles9_3[[1]]), "lat")-1)[1]], global_south_2$Country)

# ok

# common nation in the list

common_gs <- intersect(colnames(myfiles9[[1]])[21:(str_which(colnames(myfiles9[[1]]), "lat")-1)[1]], global_south_2$Country)
common_gs_1 <- intersect(colnames(myfiles9_1[[1]])[21:(str_which(colnames(myfiles9_1[[1]]), "lat")-1)[1]], global_south_2$Country)
common_gs_2 <- intersect(colnames(myfiles9_2[[1]])[21:(str_which(colnames(myfiles9_2[[1]]), "lat")-1)[1]], global_south_2$Country)
common_gs_3 <- intersect(colnames(myfiles9_3[[1]])[21:(str_which(colnames(myfiles9_3[[1]]), "lat")-1)[1]], global_south_2$Country)

# take the correct dataframe 

rm_latlong <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        y <- str_which(colnames(x), "lat_")[1]
    x <- x[,1:(y-1)]
    }
}

myfiles7.2 <- lapply(myfiles9, rm_latlong)
myfiles7.2_1 <- lapply(myfiles9_1, rm_latlong)
myfiles7.2_2 <- lapply(myfiles9_2, rm_latlong)
myfiles7.2_3 <- lapply(myfiles9_3, rm_latlong)
myfiles7.2_4 <- lapply(myfiles7.2_3, function(x) {x <- x[which(x$Year > 1989),]; return(x)})

save_df <- function(x, y){
    write.csv(x, file = paste(y, ".csv", sep=""), row.names = FALSE)
    write_feather(x, path = paste(y, ".feather", sep=""))
}

map2(myfiles7.2, names(myfiles7.2), save_df)
map2(myfiles7.2_1, names(myfiles7.2_1), save_df)
map2(myfiles7.2_2, names(myfiles7.2_2), save_df)
map2(myfiles7.2_3, names(myfiles7.2_3), save_df)

choose_PC <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        y <- x[, which(colnames(x) %in% common_gs)]
        y <- bind_cols(x[,1:20], y)
        y <- y[rowSums(is.na(y[21:ncol(y)])) != ncol(y[21:ncol(y)]), ]
    }
}


GS <- lapply(myfiles7.2, choose_PC)
GS_1 <- lapply(myfiles7.2_1, choose_PC)
GS_2 <- lapply(myfiles7.2_2, choose_PC)
GS_3 <- lapply(myfiles7.2_3, choose_PC)

# save csv and feather

save_df_GS <- function(x, y){
    write.csv(x, file = paste(y, "_GS.csv", sep=""), row.names = FALSE)
    write_feather(x, path = paste(y, "_GS.feather", sep=""))
}

map2(GS, names(GS), save_df_GS)
map2(GS_1, names(GS_1), save_df_GS)
map2(GS_2, names(GS_2), save_df_GS)
map2(GS_3, names(GS_3), save_df_GS)

# DGD filtration ####

choose_PC <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        y1 <- x[x$Year > 2014 , which(colnames(x) %in% c("Benin", "Burkina Faso", "Burundi", "Democratic Republic Congo", "Democratic Republic of the Congo", "Guinea", "Mali", "Morocco",
                                                         "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania", "United Republic of Tanzania"))] 
        
        y2 <- x[x$Year < 2015, which(colnames(x) %in% c("Vietnam", "Peru", "Ecuador", "Bolivia", "Algeria", "South Africa", "Benin", "Burundi", 
                                                        "Democratic Republic Congo", "Democratic Republic of the Congo", "Mali", "Morocco", "Mozambique", 
                                                        "Niger", "Uganda", "Palestine",  "Rwanda", "Senegal", "Tanzania", "United Republic of Tanzania"))]
        
        y1_1 <- x[x$Year > 2014 , which(colnames(x) %in% c("Vietnam", "Peru", "Ecuador", "Bolivia", "Algeria", "South Africa", "Benin", "Burkina Faso", "Burundi",
                                                           "Democratic Republic Congo", "Democratic Republic of the Congo","Guinea", "Mali", "Morocco", "Mozambique",
                                                           "Niger", "Uganda", "Palestine",  "Rwanda", "Senegal", "Tanzania", "United Republic of Tanzania"))]
        
        y2_1 <- x[x$Year < 2015 , which(colnames(x) %in% c("Vietnam", "Peru", "Ecuador", "Bolivia", "Algeria", "South Africa", "Benin", "Burkina Faso", "Burundi",
                                                           "Democratic Republic Congo", "Democratic Republic of the Congo","Guinea", "Mali", "Morocco", "Mozambique",
                                                           "Niger", "Uganda", "Palestine",  "Rwanda", "Senegal", "Tanzania", "United Republic of Tanzania"))]
        
        
        y1_2 <- bind_cols(x[which(x$Year > 2014),1:19], y1_1)
        
        y2_2 <- bind_cols(x[which(x$Year < 2015),1:19], y2_1)
        
        y1_3 <- y1_2[rowSums(is.na(y1[1:ncol(y1)])) != ncol(y1[1:ncol(y1)]), ]
        
        y2_3 <- y2_2[rowSums(is.na(y2[1:ncol(y2)])) != ncol(y2[1:ncol(y2)]), ]
        
        y <- bind_rows(y1_3, y2_3)
        
        return(y)
    }
}


PC <- lapply(myfiles7.2, choose_PC)
PC_1 <- lapply(myfiles7.2_1, choose_PC)
PC_2 <- lapply(myfiles7.2_2, choose_PC)
# PC_3 <- lapply(myfiles7.2_3, choose_PC)

save_df_PC <- function(x, y){
    write.csv(x, file = paste(y, "_PC.csv", sep=""), row.names = FALSE)
    write_feather(x, path = paste(y, "_PC.feather", sep=""))
}

map2(PC, names(PC), save_df_PC)
map2(PC_1, names(PC_1), save_df_PC)
map2(PC_2, names(PC_2), save_df_PC)

# VLIR filtration ####

vlir <- c("Burundi", "Cambodia", "Cuba", "Democratic Republic of the Congo", "Ethiopia", "Morocco", "Mozambique", "Rwanda", "Bolivia", 
          "Nicaragua", "Suriname", "Ecuador", "Indonesia", "Kenya", "Peru", "Philippines", "South Africa", "United Republic of Tanzania", "Uganda", "Vietnam")

common_vlir <- intersect(colnames(myfiles9[[1]])[21:(str_which(colnames(myfiles9[[1]]), "lat")-1)[1]], vlir)
common_vlir_1 <- intersect(colnames(myfiles9_1[[1]])[21:(str_which(colnames(myfiles9_1[[1]]), "lat")-1)[1]], vlir)
common_vlir_2 <- intersect(colnames(myfiles9_2[[1]])[21:(str_which(colnames(myfiles9_2[[1]]), "lat")-1)[1]], vlir)
common_vlir_3 <- intersect(colnames(myfiles9_3[[1]])[21:(str_which(colnames(myfiles9_3[[1]]), "lat")-1)[1]], vlir)

choose_VLIR <- function(x, y){
    if (nrow(x) == 0){
        return(x)
    } else {
        y <- x[, which(colnames(x) %in% common_vlir)]
        y <- bind_cols(x[,1:20], y)
        y <- y[rowSums(is.na(y[21:ncol(y)])) != ncol(y[21:ncol(y)]), ]
    }
}


VLIR <- lapply(myfiles7.2, choose_VLIR)
VLIR_1 <- lapply(myfiles7.2_1, choose_VLIR)
VLIR_2 <- lapply(myfiles7.2_2, choose_VLIR)
VLIR_3 <- lapply(myfiles7.2_3, choose_VLIR)

# save csv and feather

save_df_VLIR <- function(x, y){
    write.csv(x, file = paste(y, "_VLIR.csv", sep=""), row.names = FALSE)
    write_feather(x, path = paste(y, "_VLIR.feather", sep=""))
}

map2(VLIR, names(VLIR), save_df_VLIR)
map2(VLIR_1, names(VLIR_1), save_df_VLIR)
map2(VLIR_2, names(VLIR_2), save_df_VLIR)
map2(VLIR_3, names(VLIR_3), save_df_VLIR)

# ARES filtration ####

ares <- c("Peru", "Bolivia", "Ecuador", "Cuba", "Haiti", "Cambodia", "Vietnam", "Philippines", "Benin", "Burkina Faso", "Republic of the Congo", 
          "Niger", "Madagascar", "Rwanda", "Burundi", "Senegal", "Cameroon", "Mali")

common_ares <- intersect(colnames(myfiles9[[1]])[21:(str_which(colnames(myfiles9[[1]]), "lat")-1)[1]], ares)
common_ares_1 <- intersect(colnames(myfiles9_1[[1]])[21:(str_which(colnames(myfiles9_1[[1]]), "lat")-1)[1]], ares)
common_ares_2 <- intersect(colnames(myfiles9_2[[1]])[21:(str_which(colnames(myfiles9_2[[1]]), "lat")-1)[1]], ares)
common_ares_3 <- intersect(colnames(myfiles9_3[[1]])[21:(str_which(colnames(myfiles9_3[[1]]), "lat")-1)[1]], ares)

choose_ARES <- function(x, y){
    if (nrow(x) == 0){
        return(x)
    } else {
        y <- x[, which(colnames(x) %in% common_ares)]
        y <- bind_cols(x[,1:20], y)
        y <- y[rowSums(is.na(y[21:ncol(y)])) != ncol(y[21:ncol(y)]), ]
    }
}


ARES <- lapply(myfiles7.2, choose_ARES)
ARES_1 <- lapply(myfiles7.2_1, choose_ARES)
ARES_2 <- lapply(myfiles7.2_2, choose_ARES)
# ARES_3 <- lapply(myfiles7.2_3, choose_ARES)

# save csv and feather

save_df_ARES <- function(x, y){
    write.csv(x, file = paste(y, "_ARES.csv", sep=""), row.names = FALSE)
    write_feather(x, path = paste(y, "_ARES.feather", sep=""))
}

map2(ARES, names(ARES), save_df_ARES)
map2(ARES_1, names(ARES_1), save_df_ARES)
map2(ARES_2, names(ARES_2), save_df_ARES)


# Number and top of keywords ----

KW <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        keyword <- strsplit(x$`Author Keywords`, "; ")
        for (i in 1:length(keyword)){
            keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
        }
        keyword2 <- rbindlist(keyword)
        colnames(keyword2)[1]<- "keyword"
        keyword2<- keyword2[complete.cases(keyword2),]
        keyword2$keyword <- str_to_title(keyword2$keyword)
        keyword3 <- keyword2 %>%
            dplyr::group_by(keyword) %>% 
            dplyr::summarise(n=n()) %>% 
            dplyr::arrange(desc(n)) 
        return(keyword3[1:20,])
    }
}

# print the graphs

plot_kw <- function(x, y){
    if (nrow(x) == 0){
        return(x)
    } else {
        ggplot(x, aes(label = keyword, size =n ,color = rainbow_hcl(20))) +
            geom_text_wordcloud_area(shape = "star") +
            scale_size_area(max_size = 15) +
            theme_minimal()+
            ggtitle(paste(y))+
            theme(plot.title = element_text(hjust = 0.5, size = 18))
    }
}
# general dataset

myplot <- lapply(myfiles7.2, KW)
myplot_1 <- lapply(myfiles7.2_1, KW)
myplot_2 <- lapply(myfiles7.2_2, KW)
myplot_3 <- lapply(myfiles7.2_3, KW)

KW_all <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        keyword <- strsplit(x$`Author Keywords`, "; ")
        for (i in 1:length(keyword)){
            keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
        }
        keyword2 <- rbindlist(keyword)
        colnames(keyword2)[1]<- "keyword"
        keyword2<- keyword2[complete.cases(keyword2),]
        keyword2$keyword <- str_to_title(keyword2$keyword)
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'modeling', "modelling")
        keyword3 <- keyword2 %>%
            dplyr::group_by(keyword) %>% 
            dplyr::summarise(n=n()) %>% 
            dplyr::arrange(desc(n)) 
        return(keyword3)
    }
}

myplot_all <- lapply(myfiles7.2, KW_all)
myplot_all[[8]] <- myplot_all[["Target 6.a"]][-c(8,13,15),] # remove study on water maze in neuroscience 
myplot_all[["Target 6.2"]] <- myplot_all[["Target 6.2"]][-c(2, 6, 11, 22), ]

myplot_all[["Target 6.3"]][which(myplot_all[["Target 6.3"]]$keyword == "Modelling"), ]$n <- 47
myplot_all[["Target 6.3"]] <- myplot_all[["Target 6.3"]] %>% arrange((desc(n)))

myplot_all[["Target 6.5"]][which(myplot_all[["Target 6.5"]]$keyword == "Monitoring"), ]$n <- 107
myplot_all[["Target 6.5"]][which(myplot_all[["Target 6.5"]]$keyword == "Modelling"), ]$n <- 110
myplot_all[["Target 6.5"]] <- myplot_all[["Target 6.5"]] %>% arrange((desc(n)))

sum(myplot_all[["Target 6.6"]][str_detect(myplot_all[["Target 6.6"]]$keyword,pattern = 'Modelling'),]$n, na.rm=TRUE)

myplot_all[["Target 6.6"]][which(myplot_all[["Target 6.6"]]$keyword == "Monitoring"), ]$n <- 48
myplot_all[["Target 6.6"]][which(myplot_all[["Target 6.6"]]$keyword == "Modelling"), ]$n <- 29
myplot_all[["Target 6.6"]][which(myplot_all[["Target 6.6"]]$keyword == "Rivers"), ]$n <- 49
myplot_all[["Target 6.6"]] <- myplot_all[["Target 6.6"]] %>% arrange((desc(n)))

myplot_all[["Target 6.a"]][which(myplot_all[["Target 6.a"]]$keyword == "Monitoring"), ]$n <- 13
myplot_all[["Target 6.a"]][which(myplot_all[["Target 6.a"]]$keyword == "Modelling"), ]$n <- 10
myplot_all[["Target 6.a"]] <- myplot_all[["Target 6.a"]] %>% arrange((desc(n)))

myplot_all[["Target 6.1"]][which(myplot_all[["Target 6.1"]]$keyword == "Monitoring"), ]$n <- 25
myplot_all[["Target 6.1"]] <- myplot_all[["Target 6.1"]] %>% arrange((desc(n)))

myplot_all[["Target 6.2"]][which(myplot_all[["Target 6.2"]]$keyword == "Monitoring"), ]$n <- 9
myplot_all[["Target 6.2"]] <- myplot_all[["Target 6.2"]] %>% arrange((desc(n)))

myplot_all <- lapply(myplot_all, function(x) x <- x[1:20,])

# Disrupt for modelling-related author keywords

myplot_model <- lapply(myplot_all, function(x) {y <- sum(x[str_detect(x$keyword,pattern = 'Model'),]$n, na.rm=TRUE); return (unlist(y))})
myplot_model <- as.data.frame(myplot_model)
myplot_model <- gather(myplot_model, "SDG 6", "Count") 

ggsave(filename = "SDG6_model.tiff", ggplot(myplot_model, aes(x=reorder(`SDG 6`, Count),y = Count)) +
    geom_bar(stat = "identity",
             position = position_stack(reverse = TRUE), 
             fill = "tomato") +
    coord_flip() +
    theme_bw() +
    ylab("Number of occurrence of modelling-related author keywords") +
    theme(text=element_text(family = "Arial")) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.title = element_text(size = 14)) +
    theme(axis.title.y = element_blank()), 
    units = 'cm', height = 20, width = 20, dpi = 300)

# continue before the disrupt

plot_topkw <- map2(myplot, names(myplot), plot_kw)
plot_topkw_1 <- map2(myplot_1, names(myplot_1), plot_kw)
plot_topkw_2 <- map2(myplot_2, names(myplot_2), plot_kw)
plot_topkw_3 <- map2(myplot_3, names(myplot_3), plot_kw)
plot_topkw_4 <- map2(myplot_all, names(myplot_all), plot_kw)

ggsave(filename="alltime.jpeg", arrangeGrob(grobs = plot_topkw), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2010-2019.jpeg", arrangeGrob(grobs = plot_topkw_1), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2000-2009.jpeg", arrangeGrob(grobs = plot_topkw_2), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="before2000.jpeg", arrangeGrob(grobs = plot_topkw_3), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="alltime_6a_edited.jpeg", arrangeGrob(grobs = plot_topkw_4), units = 'cm', height = 50, width = 75, dpi = 300)

# GS dataset

myGS <- lapply(GS, KW)
myGS_1 <- lapply(GS_1, KW)
myGS_2 <- lapply(GS_2, KW)
myGS_3 <- lapply(GS_3, KW)
myGS_all <- lapply(GS, KW_all)
myGS_all_1 <- lapply(GS_1, KW_all)

# similarity between myGS_all and myplot_all

same_GS_plot_all <- map2(myGS_all, myplot_all, function(x, y) {z <-intersect(x$keyword[1:20],y$keyword[1:20]) ; return(z)})
same_GS_plot_all_1 <- map2(myGS_1, myplot_1, function(x, y) {z <-intersect(x$keyword[1:20],y$keyword[1:20]) ; return(z)})
same_GS_plot_all_2 <- map2(myGS_2, myplot_2, function(x, y) {z <-intersect(x$keyword[1:20],y$keyword[1:20]) ; return(z)})
same_GS_plot_all_3 <- map2(myGS_3, myplot_3, function(x, y) {z <-intersect(x$keyword[1:20],y$keyword[1:20]) ; return(z)})

# make a plot out of these similarities

same_1 <- data.frame(Goal = names(same_GS_plot_all), Similarity = data.frame(unlist(lapply(same_GS_plot_all_1, length)))[,1], Year = rep.int("2010-2019", times = 9))
same_2 <- data.frame(Goal = names(same_GS_plot_all), Similarity = data.frame(unlist(lapply(same_GS_plot_all_2, length)))[,1], Year = rep.int("2000-2009", times = 9))
same_3 <- data.frame(Goal = names(same_GS_plot_all), Similarity = data.frame(unlist(lapply(same_GS_plot_all_3, length)))[,1], Year = rep.int("Before 2000", times = 9))
same_4 <- data.frame(Goal = names(same_GS_plot_all), Similarity = data.frame(unlist(lapply(same_GS_plot_all, length)))[,1], Year = rep.int("All time", times = 9))

same_all <- bind_rows(same_1, same_2, same_3, same_4)
same_all$Year <- as.factor(same_all$Year)
same_all$Year <- ordered(same_all$Year, levels = c("Before 2000", "2000-2009", "2010-2019", "All time"))

ggsave("same_all_v2.tiff", ggplot(same_all, aes(x = Goal, y = Similarity, fill = Year)) +
           geom_bar(position="dodge", stat="identity") +
           theme_classic() +
           scale_fill_brewer(palette = 'Set1') +
           theme(text=element_text(family = "Arial")) +
           theme(axis.text.x = element_text(size = 14)) +
           theme(axis.title.x = element_blank()) +
           theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)) +
           theme(legend.title = element_blank()) +
           theme(legend.text = element_text(size = 14)),
       units = 'cm', height = 20, width = 30, dpi = 300
)

# Different between myGS_all and myplot_all

diff_GS_plot_all <- map2(myGS_all, myplot_all, function(x, y) {z <-setdiff(x$keyword[1:20],y$keyword[1:20]) ; return(z)})
diff_GS_plot_all_1 <- map2(myGS_1, myplot_1, function(x, y) {z <-setdiff(x$keyword[1:20],y$keyword[1:20]) ; return(z)})
diff_GS_plot_all_2 <- map2(myGS_2, myplot_2, function(x, y) {z <-setdiff(x$keyword[1:20],y$keyword[1:20]) ; return(z)})
diff_GS_plot_all_3 <- map2(myGS_3, myplot_3, function(x, y) {z <-setdiff(x$keyword[1:20],y$keyword[1:20]) ; return(z)})

# continue without the disruption

myplot_model_GS <- lapply(myGS_all, function(x) {y <- sum(x[str_detect(x$keyword,pattern = 'Model'),]$n, na.rm=TRUE); return (unlist(y))})
myplot_model_GS <- as.data.frame(myplot_model_GS)
myplot_model_GS <- gather(myplot_model_GS, "SDG 6", "Count") 

plot_topkw_GS <- map2(myGS, names(myGS), plot_kw)
plot_topkw_GS_1 <- map2(myGS, names(myGS_1), plot_kw)
plot_topkw_GS_2 <- map2(myGS, names(myGS_2), plot_kw)
plot_topkw_GS_3 <- map2(myGS, names(myGS_3), plot_kw)


ggsave(filename="alltimeGS.jpeg", arrangeGrob(grobs = plot_topkw_GS), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2010-2019GS.jpeg", arrangeGrob(grobs = plot_topkw_GS_1), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2000-2009GS.jpeg", arrangeGrob(grobs = plot_topkw_GS_2), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="before2000GS.jpeg", arrangeGrob(grobs = plot_topkw_GS_3), units = 'cm', height = 50, width = 75, dpi = 300)

# PC dataset 

myPC <- lapply(PC, KW)
myPC_1 <- lapply(PC_1, KW)
myPC_2 <- lapply(PC_2, KW)
# myPC_3 <- lapply(PC_3, KW)

plot_topkw_PC <- map2(myPC, names(myPC), plot_kw)
plot_topkw_PC_1 <- map2(myPC, names(myPC_1), plot_kw)
plot_topkw_PC_2 <- map2(myPC, names(myPC_2), plot_kw)
# plot_topkw_PC_3 <- map2(myPC, names(myPC_3), plot_kw)


ggsave(filename="alltimePC.jpeg", arrangeGrob(grobs = plot_topkw_PC), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2010-2019PC.jpeg", arrangeGrob(grobs = plot_topkw_PC_1), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2000-2009PC.jpeg", arrangeGrob(grobs = plot_topkw_PC_2), units = 'cm', height = 50, width = 75, dpi = 300)
# ggsave(filename="before2000PC.jpeg", arrangeGrob(grobs = plot_topkw_PC_3), units = 'cm', height = 50, width = 75, dpi = 300)

# VLIR dataset 

myVLIR <- lapply(VLIR, KW)
myVLIR_1 <- lapply(VLIR_1, KW)
myVLIR_2 <- lapply(VLIR_2, KW)
myVLIR_3 <- lapply(VLIR_3, KW)

plot_topkw_VLIR <- map2(myVLIR, names(myVLIR), plot_kw)
plot_topkw_VLIR_1 <- map2(myVLIR, names(myVLIR_1), plot_kw)
plot_topkw_VLIR_2 <- map2(myVLIR, names(myVLIR_2), plot_kw)
plot_topkw_VLIR_3 <- map2(myVLIR, names(myVLIR_3), plot_kw)

ggsave(filename="alltimeVLIR.jpeg", arrangeGrob(grobs = plot_topkw_VLIR), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2010-2019VLIR.jpeg", arrangeGrob(grobs = plot_topkw_VLIR_1), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2000-2009VLIR.jpeg", arrangeGrob(grobs = plot_topkw_VLIR_2), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="before2000VLIR.jpeg", arrangeGrob(grobs = plot_topkw_VLIR_3), units = 'cm', height = 50, width = 75, dpi = 300)

# ARES dataset 

myARES <- lapply(ARES, KW)
myARES_1 <- lapply(ARES_1, KW)
myARES_2 <- lapply(ARES_2, KW)
# myARES_3 <- lapply(ARES_3, KW)

plot_topkw_ARES <- map2(myARES, names(myARES), plot_kw)
plot_topkw_ARES_1 <- map2(myARES, names(myARES_1), plot_kw)
plot_topkw_ARES_2 <- map2(myARES, names(myARES_2), plot_kw)
# plot_topkw_ARES_3 <- map2(myARES, names(myARES_3), plot_kw)

ggsave(filename="alltimeARES.jpeg", arrangeGrob(grobs = plot_topkw_ARES), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2010-2019ARES.jpeg", arrangeGrob(grobs = plot_topkw_ARES_1), units = 'cm', height = 50, width = 75, dpi = 300)
ggsave(filename="2000-2009ARES.jpeg", arrangeGrob(grobs = plot_topkw_ARES_2), units = 'cm', height = 50, width = 75, dpi = 300)
# ggsave(filename="before2000ARES.jpeg", arrangeGrob(grobs = plot_topkw_ARES_3), units = 'cm', height = 50, width = 75, dpi = 300)

# Top keyword over time ---- 

KW2 <- function(x1){
    if (nrow(x1) == 0){
        return(x1)
    } else {
        allplotKW <- list()
        y <-levels(as.factor(x1$Year))
        for (j in seq_along(y)){
            x <- x1 %>% dplyr::filter(Year == y[j])
            keyword <- strsplit(x$`Author Keywords`, "; ")
            for (i in 1:length(keyword)){
                keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
            }
            keyword2 <- rbindlist(keyword)
            colnames(keyword2)[1]<- "keyword"
            keyword2<- keyword2[complete.cases(keyword2),]
            keyword2$keyword <- str_to_title(keyword2$keyword)
            keyword3 <- keyword2 %>%
                dplyr::group_by(keyword) %>% 
                dplyr::summarise(n=n()) %>% 
                dplyr::arrange(desc(n)) 
            plotKW <- ggplot(keyword3[1:20,], aes(label = keyword, size =n ,color = rainbow_hcl(20))) +
                geom_text_wordcloud_area(shape = "star") +
                scale_size_area(max_size = 12) +
                theme_minimal() +
                ggtitle(paste(y[j])) +
                theme(plot.title = element_text(hjust = 0.5, size = 18))
            allplotKW <- list.append(allplotKW, plotKW)
        }
        return(allplotKW)
    }
}

# general dataset 

plot_topkw2_1 <- lapply(myfiles7.2_1, KW2)
plot_topkw2_2 <- lapply(myfiles7.2_2, KW2)
myfiles7.2_3 <- lapply(myfiles7.2_3, function(x) x <- x %>% filter(Year > 1989))
plot_topkw2_3 <- lapply(myfiles7.2_3, KW2)


plot_topkw3_1 <- lapply(plot_topkw2_1, function(x) arrangeGrob(grobs = x, nrow =3))
plot_topkw3_2 <- lapply(plot_topkw2_2, function(x) arrangeGrob(grobs = x, nrow =3))
plot_topkw3_3 <- lapply(plot_topkw2_3, function(x) arrangeGrob(grobs = x, nrow =3))


lapply(names(plot_topkw3_1), function(x) ggsave(filename=paste(x,".jpeg",sep=""), plot=plot_topkw3_1[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
lapply(names(plot_topkw3_2), function(x) ggsave(filename=paste(x,".jpeg",sep=""), plot=plot_topkw3_2[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
lapply(names(plot_topkw3_3), function(x) ggsave(filename=paste(x,".jpeg",sep=""), plot=plot_topkw3_3[[x]], units = 'cm', height = 50, width = 75, dpi = 300))

# GS dataset 

plot_topkw_GS2_1 <- lapply(GS_1, KW2)
plot_topkw_GS2_2 <- lapply(GS_2, KW2)
GS_3 <- lapply(GS_3, function(x) x <- x %>% filter(Year > 1989))
plot_topkw_GS2_3 <- lapply(GS_3, KW2)

plot_topkw_GS3_1 <- lapply(plot_topkw_GS2_1, function(x) arrangeGrob(grobs = x, nrow =3))
plot_topkw_GS3_2 <- lapply(plot_topkw_GS2_2, function(x) arrangeGrob(grobs = x, nrow =3))
plot_topkw_GS3_3 <- lapply(plot_topkw_GS2_3, function(x) arrangeGrob(grobs = x, nrow =3))

lapply(names(plot_topkw_GS3_1), function(x) ggsave(filename=paste(x,"GS.jpeg",sep=""), plot=plot_topkw_GS3_1[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
lapply(names(plot_topkw_GS3_2), function(x) ggsave(filename=paste(x,"GS.jpeg",sep=""), plot=plot_topkw_GS3_2[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
lapply(names(plot_topkw_GS3_3), function(x) ggsave(filename=paste(x,"GS.jpeg",sep=""), plot=plot_topkw_GS3_3[[x]], units = 'cm', height = 50, width = 75, dpi = 300))

# PC dataset 

plot_topkw_PC2_1 <- lapply(PC_1, KW2)
plot_topkw_PC2_2 <- lapply(PC_2, KW2)
# PC_3 <- lapply(PC_3, function(x) x <- x %>% filter(Year > 1990))
# plot_topkw_PC2_3 <- lapply(PC_3, KW2)

plot_topkw_PC3_1 <- lapply(plot_topkw_PC2_1, function(x) arrangeGrob(grobs = x, nrow =3))
plot_topkw_PC3_2 <- lapply(plot_topkw_PC2_2, function(x) arrangeGrob(grobs = x, nrow =3))
# plot_topkw_PC3_3 <- lapply(plot_topkw_PC2_3, function(x) arrangeGrob(grobs = x, nrow =3))

lapply(names(plot_topkw_PC3_1), function(x) ggsave(filename=paste(x,"PC.jpeg",sep=""), plot=plot_topkw_PC3_1[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
lapply(names(plot_topkw_PC3_2), function(x) ggsave(filename=paste(x,"PC.jpeg",sep=""), plot=plot_topkw_PC3_2[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
# lapply(names(plot_topkw_PC3_3), function(x) ggsave(filename=paste(x,"PC.jpeg",sep=""), plot=plot_topkw_PC3_3[[x]], units = 'cm', height = 50, width = 75, dpi = 300))


# VLIR dataset 

plot_topkw_VLIR2_1 <- lapply(VLIR_1, KW2)
plot_topkw_VLIR2_2 <- lapply(VLIR_2, KW2)
VLIR_3 <- lapply(VLIR_3, function(x) x <- x %>% filter(Year > 1989))
plot_topkw_VLIR2_3 <- lapply(VLIR_3, KW2)

plot_topkw_VLIR3_1 <- lapply(plot_topkw_VLIR2_1, function(x) arrangeGrob(grobs = x, nrow =3))
plot_topkw_VLIR3_2 <- lapply(plot_topkw_VLIR2_2, function(x) arrangeGrob(grobs = x, nrow =3))
plot_topkw_VLIR3_3 <- lapply(plot_topkw_VLIR2_3, function(x) arrangeGrob(grobs = x, nrow =3))

lapply(names(plot_topkw_VLIR3_1), function(x) ggsave(filename=paste(x,"VLIR.jpeg",sep=""), plot=plot_topkw_VLIR3_1[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
lapply(names(plot_topkw_VLIR3_2), function(x) ggsave(filename=paste(x,"VLIR.jpeg",sep=""), plot=plot_topkw_VLIR3_2[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
lapply(names(plot_topkw_VLIR3_3), function(x) ggsave(filename=paste(x,"VLIR.jpeg",sep=""), plot=plot_topkw_VLIR3_3[[x]], units = 'cm', height = 50, width = 75, dpi = 300))


# ARES dataset 

plot_topkw_ARES2_1 <- lapply(ARES_1, KW2)
plot_topkw_ARES2_2 <- lapply(ARES_2, KW2)
# ARES_3 <- lapply(ARES_3, function(x) x <- x %>% filter(Year > 1990))
# plot_topkw_ARES2_3 <- lapply(ARES_3, KW2)

plot_topkw_ARES3_1 <- lapply(plot_topkw_ARES2_1, function(x) arrangeGrob(grobs = x, nrow =3))
plot_topkw_ARES3_2 <- lapply(plot_topkw_ARES2_2, function(x) arrangeGrob(grobs = x, nrow =3))
# plot_topkw_ARES3_3 <- lapply(plot_topkw_ARES2_3, function(x) arrangeGrob(grobs = x, nrow =3))

lapply(names(plot_topkw_ARES3_1), function(x) ggsave(filename=paste(x,"ARES.jpeg",sep=""), plot=plot_topkw_ARES3_1[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
lapply(names(plot_topkw_ARES3_2), function(x) ggsave(filename=paste(x,"ARES.jpeg",sep=""), plot=plot_topkw_ARES3_2[[x]], units = 'cm', height = 50, width = 75, dpi = 300))
# lapply(names(plot_topkw_ARES3_3), function(x) ggsave(filename=paste(x,"ARES.jpeg",sep=""), plot=plot_topkw_ARES3_3[[x]], units = 'cm', height = 50, width = 75, dpi = 300))


# General info ####

# document types
save_DT <- function(x){
    y <- summary(x$`Document Type`)
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
    z <- as.data.frame(y$y*100/sum(y$y))
    colnames(z) <- "Percentage"
    y <- bind_cols(y,z)
}

# language of the document 
save_language <- function(x){
    y <- summary(as.factor(x$`Language of Original Document`))
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
    z <- as.data.frame(y$y*100/sum(y$y))
    colnames(z) <- "Percentage"
    y <- bind_cols(y,z)
}

# Open access
save_access <- function(x){
    y <- summary(as.factor(x$`Access Type`))
    y <- rownames_to_column(as.data.frame(y), var = "rowname")
    z <- as.data.frame(y$y*100/sum(y$y))
    colnames(z) <- "Percentage"
    y <- bind_cols(y,z)
}

# Citation
save_citation <- function(x, y){
    x <- x %>% 
        arrange(desc(`Cited by`)) %>% 
        slice(1:y)
}

total_citation <- function(x, z){
    y <- sum(x$`Cited by`, na.rm = TRUE)
    t <- data.frame(z, y)
    colnames(t) <- c("SDG", "Citation")
    return(t)
}

# Publication years
save_pubyear <- function(x){
    y <- x %>% select(Year,`Document Type`, `Access Type`) %>% 
        dplyr::group_by(Year) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(Year)
    # z <- as.data.frame(ave(y$n, FUN = cumsum))
    # colnames(z) <- "cum"
    # y <- bind_cols(y, z)
    t <- ggplot(y, aes(x=Year, y=n))+
        geom_point(size = 2) +
        # geom_smooth(colour="gray20", size =0.5, method = "lm") +
        labs(x = "Years", y = "Cumulative publications", fill = NULL, title = NULL) +
        # scale_x_continuous(breaks = c(2008:2020))+
        theme_bw()
}

# Top Journal 
save_topjournal <- function(x){
    y <- x %>% select(`Source title`) %>% 
        dplyr::group_by(`Source title`) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        slice(1:20) %>% 
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

#** General dataset -----

SDG6_DT <- lapply(myfiles7.2, save_DT)
SDG6_DT_1 <- lapply(myfiles7.2_1, save_DT)
SDG6_DT_2 <- lapply(myfiles7.2_2, save_DT)
SDG6_DT_3 <- lapply(myfiles7.2_3, save_DT)

lapply(names(SDG6_DT), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT[x], row.names = FALSE))
lapply(names(SDG6_DT_1), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_1[x], row.names = FALSE))
lapply(names(SDG6_DT_2), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_2[x], row.names = FALSE))
lapply(names(SDG6_DT_3), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_3[x], row.names = FALSE))

SDG6_Lang <- lapply(myfiles7.2, save_language)
SDG6_Lang_1 <- lapply(myfiles7.2_1, save_language)
SDG6_Lang_2 <- lapply(myfiles7.2_2, save_language)
SDG6_Lang_3 <- lapply(myfiles7.2_3, save_language)


lapply(names(SDG6_Lang), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang[x], row.names = FALSE))
lapply(names(SDG6_Lang_1), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_1[x], row.names = FALSE))
lapply(names(SDG6_Lang_2), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_2[x], row.names = FALSE))
lapply(names(SDG6_Lang_3), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_3[x], row.names = FALSE))


SDG6_AT <- lapply(myfiles7.2, save_access)
SDG6_AT_1 <- lapply(myfiles7.2_1, save_access)
SDG6_AT_2 <- lapply(myfiles7.2_2, save_access)
SDG6_AT_3 <- lapply(myfiles7.2_3, save_access)

lapply(names(SDG6_AT), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT[x], row.names = FALSE))
lapply(names(SDG6_AT_1), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_1[x], row.names = FALSE))
lapply(names(SDG6_AT_2), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_2[x], row.names = FALSE))
lapply(names(SDG6_AT_3), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_3[x], row.names = FALSE))

SDG6_SC <- lapply(myfiles7.2, save_citation, 20)
SDG6_SC_1 <- lapply(myfiles7.2_1, save_citation, 20)
SDG6_SC_2 <- lapply(myfiles7.2_2, save_citation, 20)
SDG6_SC_3 <- lapply(myfiles7.2_3, save_citation, 20)

lapply(names(SDG6_SC), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC[x], row.names = FALSE))
lapply(names(SDG6_SC_1), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_1[x], row.names = FALSE))
lapply(names(SDG6_SC_2), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_2[x], row.names = FALSE))
lapply(names(SDG6_SC_3), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_3[x], row.names = FALSE))

SDG6_TC <-map2(myfiles7.2, names(myfiles7.2), total_citation)
SDG6_TC_1 <- map2(myfiles7.2_1, names(myfiles7.2_1), total_citation)
SDG6_TC_2 <- map2(myfiles7.2_2, names(myfiles7.2_2), total_citation)
SDG6_TC_3 <- map2(myfiles7.2_3, names(myfiles7.2_3), total_citation)
SDG6_TC_4 <- map2(myfiles7.2_4, names(myfiles7.2_4), total_citation)

write.csv(rbindlist(SDG6_TC), file = "TC_Alltime.csv", row.names = FALSE)
write.csv(rbindlist(SDG6_TC_1), file = "TC_2010_2019.csv", row.names = FALSE)
write.csv(rbindlist(SDG6_TC_2), file = "TC_2000_2009.csv", row.names = FALSE)
write.csv(rbindlist(SDG6_TC_3), file = "TC_Before_2000.csv", row.names = FALSE)
write.csv(rbindlist(SDG6_TC_4), file = "TC_1990_1999.csv", row.names = FALSE)


# citation per publication

SDG6_CP <- map2(myfiles7.2, SDG6_TC, function(x, y){z <- y$Citation/nrow(x); return(z)})
SDG6_CP_1 <- map2(myfiles7.2_1, SDG6_TC_1, function(x, y){z <- y$Citation/nrow(x); return(z)})
SDG6_CP_2 <- map2(myfiles7.2_2, SDG6_TC_2, function(x, y){z <- y$Citation/nrow(x); return(z)})
SDG6_CP_3 <- map2(myfiles7.2_3, SDG6_TC_3, function(x, y){z <- y$Citation/nrow(x); return(z)})
SDG6_CP_4 <- map2(myfiles7.2_4, SDG6_TC_4, function(x, y){z <- y$Citation/nrow(x); return(z)})

# citations within the last 10 years 


SDG_PU <- lapply(myfiles7.2, save_pubyear)
SDG_PU_1 <- lapply(myfiles7.2_1, save_pubyear)
SDG_PU_2 <- lapply(myfiles7.2_2, save_pubyear)
SDG_PU_3 <- lapply(myfiles7.2_3, save_pubyear)

lapply(names(SDG_PU), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_1), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_1[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_2), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_2[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_3), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_3[[x]], units = 'cm', height = 20, width = 20, dpi = 300))

SDG6_Topjournal <- lapply(myfiles7.2, save_topjournal)
SDG6_Topjournal_1 <- lapply(myfiles7.2_1, save_topjournal)
SDG6_Topjournal_2 <- lapply(myfiles7.2_2, save_topjournal)
SDG6_Topjournal_3 <- lapply(myfiles7.2_3, save_topjournal)

lapply(names(SDG6_Topjournal), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_1), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_1[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_2), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_2[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_3), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_3[[x]], units = 'cm', height = 20, width = 30, dpi = 300))


#** Global South dataset ----

SDG6_DT <- lapply(GS, save_DT)
SDG6_DT_1 <- lapply(GS_1, save_DT)
SDG6_DT_2 <- lapply(GS_2, save_DT)
SDG6_DT_3 <- lapply(GS_3, save_DT)

lapply(names(SDG6_DT), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT[x], row.names = FALSE))
lapply(names(SDG6_DT_1), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_1[x], row.names = FALSE))
lapply(names(SDG6_DT_2), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_2[x], row.names = FALSE))
lapply(names(SDG6_DT_3), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_3[x], row.names = FALSE))

SDG6_Lang <- lapply(GS, save_language)
SDG6_Lang_1 <- lapply(GS_1, save_language)
SDG6_Lang_2 <- lapply(GS_2, save_language)
SDG6_Lang_3 <- lapply(GS_3, save_language)


lapply(names(SDG6_Lang), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang[x], row.names = FALSE))
lapply(names(SDG6_Lang_1), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_1[x], row.names = FALSE))
lapply(names(SDG6_Lang_2), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_2[x], row.names = FALSE))
lapply(names(SDG6_Lang_3), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_3[x], row.names = FALSE))


SDG6_AT <- lapply(GS, save_access)
SDG6_AT_1 <- lapply(GS_1, save_access)
SDG6_AT_2 <- lapply(GS_2, save_access)
SDG6_AT_3 <- lapply(GS_3, save_access)

lapply(names(SDG6_AT), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT[x], row.names = FALSE))
lapply(names(SDG6_AT_1), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_1[x], row.names = FALSE))
lapply(names(SDG6_AT_2), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_2[x], row.names = FALSE))
lapply(names(SDG6_AT_3), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_3[x], row.names = FALSE))


SDG6_SC <- lapply(GS, save_citation, 20)
SDG6_SC_1 <- lapply(GS_1, save_citation, 20)
SDG6_SC_2 <- lapply(GS_2, save_citation, 20)
SDG6_SC_3 <- lapply(GS_3, save_citation, 20)

lapply(names(SDG6_SC), function(x) write.csv(file = paste(x, "_SC.csv", sep = ""), x = SDG6_TC[x], row.names = FALSE))
lapply(names(SDG6_SC_1), function(x) write.csv(file = paste(x, "_SC.csv", sep = ""), x = SDG6_TC_1[x], row.names = FALSE))
lapply(names(SDG6_SC_2), function(x) write.csv(file = paste(x, "_SC.csv", sep = ""), x = SDG6_TC_2[x], row.names = FALSE))
lapply(names(SDG6_SC_3), function(x) write.csv(file = paste(x, "_SC.csv", sep = ""), x = SDG6_TC_3[x], row.names = FALSE))


# GS over total Belgian water research

GS_Belgian_WR <- map2(GS, myfiles7.2, function(x, y) {z <- nrow(x)*100/nrow(y); return(z)})
GS_Belgian_WR_1 <- map2(GS_1, myfiles7.2_1, function(x, y) {z <- nrow(x)*100/nrow(y); return(z)})
GS_Belgian_WR_2 <- map2(GS_2, myfiles7.2_2, function(x, y) {z <- nrow(x)*100/nrow(y); return(z)})
GS_Belgian_WR_3 <- map2(GS_3, myfiles7.2_3, function(x, y) {z <- nrow(x)*100/nrow(y); return(z)})

# citation per publication

SDG6_TC_GS <-map2(GS, names(GS), total_citation)
SDG6_TC_GS_1 <- map2(GS_1, names(GS_1), total_citation)
SDG6_TC_GS_2 <- map2(GS_2, names(GS_2), total_citation)
SDG6_TC_GS_3 <- map2(GS_3, names(GS_3), total_citation)


SDG6_CP_GS <- map2(GS, SDG6_TC, function(x, y){z <- y$Citation/nrow(x); return(z)})
SDG6_CP_GS_1 <- map2(GS_1, SDG6_TC_GS_1, function(x, y){z <- y$Citation/nrow(x); return(z)})
SDG6_CP_GS_2 <- map2(GS_2, SDG6_TC_GS_2, function(x, y){z <- y$Citation/nrow(x); return(z)})
SDG6_CP_GS_3 <- map2(GS_3, SDG6_TC_GS_3, function(x, y){z <- y$Citation/nrow(x); return(z)})


write.csv(rbindlist(map2(GS, names(GS), total_citation)), file = "TC_Alltime_GS.csv", row.names = FALSE)
write.csv(rbindlist(map2(GS_1, names(GS_1), total_citation)), file = "TC_2010_2019_GS.csv", row.names = FALSE)
write.csv(rbindlist(map2(GS_2, names(GS_2), total_citation)), file = "TC_2000_2009_GS.csv", row.names = FALSE)
write.csv(rbindlist(map2(GS_3, names(GS_3), total_citation)), file = "TC_Before_2000_GS.csv", row.names = FALSE)

SDG_PU <- lapply(GS, save_pubyear)
SDG_PU_1 <- lapply(GS_1, save_pubyear)
SDG_PU_2 <- lapply(GS_2, save_pubyear)
SDG_PU_3 <- lapply(GS_3, save_pubyear)

lapply(names(SDG_PU), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_1), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_1[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_2), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_2[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_3), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_3[[x]], units = 'cm', height = 20, width = 20, dpi = 300))

SDG6_Topjournal <- lapply(GS, save_topjournal)
SDG6_Topjournal_1 <- lapply(GS_1, save_topjournal)
SDG6_Topjournal_2 <- lapply(GS_2, save_topjournal)
SDG6_Topjournal_3 <- lapply(GS_3, save_topjournal)

lapply(names(SDG6_Topjournal), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_1), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_1[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_2), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_2[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_3), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_3[[x]], units = 'cm', height = 20, width = 30, dpi = 300))


#** PC dataset -----

SDG6_DT <- lapply(PC, save_DT)
SDG6_DT_1 <- lapply(PC_1, save_DT)
SDG6_DT_2 <- lapply(PC_2, save_DT)
SDG6_DT_3 <- lapply(PC_3, save_DT)

lapply(names(SDG6_DT), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT[x], row.names = FALSE))
lapply(names(SDG6_DT_1), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_1[x], row.names = FALSE))
lapply(names(SDG6_DT_2), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_2[x], row.names = FALSE))
lapply(names(SDG6_DT_3), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_3[x], row.names = FALSE))

SDG6_Lang <- lapply(PC, save_language)
SDG6_Lang_1 <- lapply(PC_1, save_language)
SDG6_Lang_2 <- lapply(PC_2, save_language)
SDG6_Lang_3 <- lapply(PC_3, save_language)


lapply(names(SDG6_Lang), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang[x], row.names = FALSE))
lapply(names(SDG6_Lang_1), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_1[x], row.names = FALSE))
lapply(names(SDG6_Lang_2), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_2[x], row.names = FALSE))
lapply(names(SDG6_Lang_3), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_3[x], row.names = FALSE))


SDG6_AT <- lapply(PC, save_access)
SDG6_AT_1 <- lapply(PC_1, save_access)
SDG6_AT_2 <- lapply(PC_2, save_access)
SDG6_AT_3 <- lapply(PC_3, save_access)

lapply(names(SDG6_AT), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT[x], row.names = FALSE))
lapply(names(SDG6_AT_1), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_1[x], row.names = FALSE))
lapply(names(SDG6_AT_2), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_2[x], row.names = FALSE))
lapply(names(SDG6_AT_3), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_3[x], row.names = FALSE))


SDG6_TC <- lapply(PC, save_citation, 20)
SDG6_TC_1 <- lapply(PC_1, save_citation, 20)
SDG6_TC_2 <- lapply(PC_2, save_citation, 20)
SDG6_TC_3 <- lapply(PC_3, save_citation, 20)

lapply(names(SDG6_TC), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC[x], row.names = FALSE))
lapply(names(SDG6_TC_1), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_1[x], row.names = FALSE))
lapply(names(SDG6_TC_2), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_2[x], row.names = FALSE))
lapply(names(SDG6_TC_3), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_3[x], row.names = FALSE))

write.csv(rbindlist(map2(PC, names(PC), total_citation)), file = "TC_Alltime_PC.csv", row.names = FALSE)
write.csv(rbindlist(map2(PC_1, names(PC_1), total_citation)), file = "TC_2010_2019_PC.csv", row.names = FALSE)
write.csv(rbindlist(map2(PC_2, names(PC_2), total_citation)), file = "TC_2000_2009_PC.csv", row.names = FALSE)
write.csv(rbindlist(map2(PC_3, names(PC_3), total_citation)), file = "TC_Before_2000_PC.csv", row.names = FALSE)

SDG_PU <- lapply(PC, save_pubyear)
SDG_PU_1 <- lapply(PC_1, save_pubyear)
SDG_PU_2 <- lapply(PC_2, save_pubyear)
SDG_PU_3 <- lapply(PC_3, save_pubyear)

lapply(names(SDG_PU), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_1), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_1[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_2), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_2[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_3), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_3[[x]], units = 'cm', height = 20, width = 20, dpi = 300))

SDG6_Topjournal <- lapply(PC, save_topjournal)
SDG6_Topjournal_1 <- lapply(PC_1, save_topjournal)
SDG6_Topjournal_2 <- lapply(PC_2, save_topjournal)
SDG6_Topjournal_3 <- lapply(PC_3, save_topjournal)

lapply(names(SDG6_Topjournal), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_1), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_1[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_2), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_2[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_3), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_3[[x]], units = 'cm', height = 20, width = 30, dpi = 300))


#** VLIR dataset -----

SDG6_DT <- lapply(VLIR, save_DT)
SDG6_DT_1 <- lapply(VLIR_1, save_DT)
SDG6_DT_2 <- lapply(VLIR_2, save_DT)
SDG6_DT_3 <- lapply(VLIR_3, save_DT)

lapply(names(SDG6_DT), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT[x], row.names = FALSE))
lapply(names(SDG6_DT_1), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_1[x], row.names = FALSE))
lapply(names(SDG6_DT_2), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_2[x], row.names = FALSE))
lapply(names(SDG6_DT_3), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_3[x], row.names = FALSE))

SDG6_Lang <- lapply(VLIR, save_language)
SDG6_Lang_1 <- lapply(VLIR_1, save_language)
SDG6_Lang_2 <- lapply(VLIR_2, save_language)
SDG6_Lang_3 <- lapply(VLIR_3, save_language)


lapply(names(SDG6_Lang), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang[x], row.names = FALSE))
lapply(names(SDG6_Lang_1), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_1[x], row.names = FALSE))
lapply(names(SDG6_Lang_2), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_2[x], row.names = FALSE))
lapply(names(SDG6_Lang_3), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_3[x], row.names = FALSE))


SDG6_AT <- lapply(VLIR, save_access)
SDG6_AT_1 <- lapply(VLIR_1, save_access)
SDG6_AT_2 <- lapply(VLIR_2, save_access)
SDG6_AT_3 <- lapply(VLIR_3, save_access)

lapply(names(SDG6_AT), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT[x], row.names = FALSE))
lapply(names(SDG6_AT_1), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_1[x], row.names = FALSE))
lapply(names(SDG6_AT_2), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_2[x], row.names = FALSE))
lapply(names(SDG6_AT_3), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_3[x], row.names = FALSE))


SDG6_TC <- lapply(VLIR, save_citation, 20)
SDG6_TC_1 <- lapply(VLIR_1, save_citation, 20)
SDG6_TC_2 <- lapply(VLIR_2, save_citation, 20)
SDG6_TC_3 <- lapply(VLIR_3, save_citation, 20)

lapply(names(SDG6_TC), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC[x], row.names = FALSE))
lapply(names(SDG6_TC_1), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_1[x], row.names = FALSE))
lapply(names(SDG6_TC_2), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_2[x], row.names = FALSE))
lapply(names(SDG6_TC_3), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_3[x], row.names = FALSE))

write.csv(rbindlist(map2(VLIR, names(VLIR), total_citation)), file = "TC_Alltime_VLIR.csv", row.names = FALSE)
write.csv(rbindlist(map2(VLIR_1, names(VLIR_1), total_citation)), file = "TC_2010_2019_VLIR.csv", row.names = FALSE)
write.csv(rbindlist(map2(VLIR_2, names(VLIR_2), total_citation)), file = "TC_2000_2009_VLIR.csv", row.names = FALSE)
write.csv(rbindlist(map2(VLIR_3, names(VLIR_3), total_citation)), file = "TC_Before_2000_VLIR.csv", row.names = FALSE)

SDG_PU <- lapply(VLIR, save_pubyear)
SDG_PU_1 <- lapply(VLIR_1, save_pubyear)
SDG_PU_2 <- lapply(VLIR_2, save_pubyear)
SDG_PU_3 <- lapply(VLIR_3, save_pubyear)

lapply(names(SDG_PU), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_1), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_1[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_2), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_2[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_3), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_3[[x]], units = 'cm', height = 20, width = 20, dpi = 300))

SDG6_Topjournal <- lapply(VLIR, save_topjournal)
SDG6_Topjournal_1 <- lapply(VLIR_1, save_topjournal)
SDG6_Topjournal_2 <- lapply(VLIR_2, save_topjournal)
SDG6_Topjournal_3 <- lapply(VLIR_3, save_topjournal)

lapply(names(SDG6_Topjournal), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_1), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_1[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_2), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_2[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_3), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_3[[x]], units = 'cm', height = 20, width = 30, dpi = 300))


#** ARES dataset -----

SDG6_DT <- lapply(ARES, save_DT)
SDG6_DT_1 <- lapply(ARES_1, save_DT)
SDG6_DT_2 <- lapply(ARES_2, save_DT)
SDG6_DT_3 <- lapply(ARES_3, save_DT)

lapply(names(SDG6_DT), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT[x], row.names = FALSE))
lapply(names(SDG6_DT_1), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_1[x], row.names = FALSE))
lapply(names(SDG6_DT_2), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_2[x], row.names = FALSE))
lapply(names(SDG6_DT_3), function(x) write.csv(file = paste(x, "_DT.csv", sep = ""), x = SDG6_DT_3[x], row.names = FALSE))

SDG6_Lang <- lapply(ARES, save_language)
SDG6_Lang_1 <- lapply(ARES_1, save_language)
SDG6_Lang_2 <- lapply(ARES_2, save_language)
SDG6_Lang_3 <- lapply(ARES_3, save_language)


lapply(names(SDG6_Lang), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang[x], row.names = FALSE))
lapply(names(SDG6_Lang_1), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_1[x], row.names = FALSE))
lapply(names(SDG6_Lang_2), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_2[x], row.names = FALSE))
lapply(names(SDG6_Lang_3), function(x) write.csv(file = paste(x, "_Lang.csv", sep = ""), x = SDG6_Lang_3[x], row.names = FALSE))


SDG6_AT <- lapply(ARES, save_access)
SDG6_AT_1 <- lapply(ARES_1, save_access)
SDG6_AT_2 <- lapply(ARES_2, save_access)
SDG6_AT_3 <- lapply(ARES_3, save_access)

lapply(names(SDG6_AT), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT[x], row.names = FALSE))
lapply(names(SDG6_AT_1), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_1[x], row.names = FALSE))
lapply(names(SDG6_AT_2), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_2[x], row.names = FALSE))
lapply(names(SDG6_AT_3), function(x) write.csv(file = paste(x, "_AT.csv", sep = ""), x = SDG6_AT_3[x], row.names = FALSE))


SDG6_TC <- lapply(ARES, save_citation, 20)
SDG6_TC_1 <- lapply(ARES_1, save_citation, 20)
SDG6_TC_2 <- lapply(ARES_2, save_citation, 20)
SDG6_TC_3 <- lapply(ARES_3, save_citation, 20)

lapply(names(SDG6_TC), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC[x], row.names = FALSE))
lapply(names(SDG6_TC_1), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_1[x], row.names = FALSE))
lapply(names(SDG6_TC_2), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_2[x], row.names = FALSE))
lapply(names(SDG6_TC_3), function(x) write.csv(file = paste(x, "_TC.csv", sep = ""), x = SDG6_TC_3[x], row.names = FALSE))

write.csv(rbindlist(map2(ARES, names(ARES), total_citation)), file = "TC_Alltime_ARES.csv", row.names = FALSE)
write.csv(rbindlist(map2(ARES_1, names(ARES_1), total_citation)), file = "TC_2010_2019_ARES.csv", row.names = FALSE)
write.csv(rbindlist(map2(ARES_2, names(ARES_2), total_citation)), file = "TC_2000_2009_ARES.csv", row.names = FALSE)
write.csv(rbindlist(map2(ARES_3, names(ARES_3), total_citation)), file = "TC_Before_2000_ARES.csv", row.names = FALSE)

SDG_PU <- lapply(ARES, save_pubyear)
SDG_PU_1 <- lapply(ARES_1, save_pubyear)
SDG_PU_2 <- lapply(ARES_2, save_pubyear)
SDG_PU_3 <- lapply(ARES_3, save_pubyear)

lapply(names(SDG_PU), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_1), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_1[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_2), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_2[[x]], units = 'cm', height = 20, width = 20, dpi = 300))
lapply(names(SDG_PU_3), function(x) ggsave(filename=paste(x,"_PU.jpeg",sep=""), plot=SDG_PU_3[[x]], units = 'cm', height = 20, width = 20, dpi = 300))

SDG6_Topjournal <- lapply(ARES, save_topjournal)
SDG6_Topjournal_1 <- lapply(ARES_1, save_topjournal)
SDG6_Topjournal_2 <- lapply(ARES_2, save_topjournal)
SDG6_Topjournal_3 <- lapply(ARES_3, save_topjournal)

lapply(names(SDG6_Topjournal), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_1), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_1[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_2), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_2[[x]], units = 'cm', height = 20, width = 30, dpi = 300))
lapply(names(SDG6_Topjournal_3), function(x) ggsave(filename=paste(x,"_TopJ.jpeg",sep=""), plot=SDG6_Topjournal_3[[x]], units = 'cm', height = 20, width = 30, dpi = 300))


pal <- function(col, border = "light gray"){
    n <- length(col)
    plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

tiff(filename = "Rainbow.tiff", width = 480, height = 400, units = "px", res=200)
pal(rainbow_hcl(20))
dev.off()


# make dataframe for dashboard

myfiles9$`SDG 6`$`Target 6.1` <- NA
myfiles9$`SDG 6`$`Target 6.2` <- NA
myfiles9$`SDG 6`$`Target 6.3` <- NA
myfiles9$`SDG 6`$`Target 6.4` <- NA
myfiles9$`SDG 6`$`Target 6.5` <- NA
myfiles9$`SDG 6`$`Target 6.6` <- NA
myfiles9$`SDG 6`$`Target 6.a` <- NA
myfiles9$`SDG 6`$`Target 6.b` <- NA


for (i in 1:nrow(myfiles9$`Target 6.1`)){
    for (j in 1:nrow(myfiles9$`SDG 6`)){
        if(myfiles9$`Target 6.1`$Title[i] == myfiles9$`SDG 6`$Title[j]){
            myfiles9$`SDG 6`[j,which(colnames( myfiles9$`SDG 6`) == "Target 6.1")] <- "Target 6.1"
        }
    }
}

for (i in 1:nrow(myfiles9$`Target 6.2`)){
    for (j in 1:nrow(myfiles9$`SDG 6`)){
        if(myfiles9$`Target 6.2`$Title[i] == myfiles9$`SDG 6`$Title[j]){
            myfiles9$`SDG 6`[j,which(colnames( myfiles9$`SDG 6`) == "Target 6.2")] <- "Target 6.2"
        }
    }
}

for (i in 1:nrow(myfiles9$`Target 6.3`)){
    for (j in 1:nrow(myfiles9$`SDG 6`)){
        if(myfiles9$`Target 6.3`$Title[i] == myfiles9$`SDG 6`$Title[j]){
            myfiles9$`SDG 6`[j,which(colnames( myfiles9$`SDG 6`) == "Target 6.3")] <- "Target 6.3"
        }
    }
}

for (i in 1:nrow(myfiles9$`Target 6.4`)){
    for (j in 1:nrow(myfiles9$`SDG 6`)){
        if(myfiles9$`Target 6.4`$Title[i] == myfiles9$`SDG 6`$Title[j]){
            myfiles9$`SDG 6`[j,which(colnames( myfiles9$`SDG 6`) == "Target 6.4")] <- "Target 6.4"
        }
    }
}

for (i in 1:nrow(myfiles9$`Target 6.5`)){
    for (j in 1:nrow(myfiles9$`SDG 6`)){
        if(myfiles9$`Target 6.5`$Title[i] == myfiles9$`SDG 6`$Title[j]){
            myfiles9$`SDG 6`[j,which(colnames( myfiles9$`SDG 6`) == "Target 6.5")] <- "Target 6.5"
        }
    }
}

for (i in 1:nrow(myfiles9$`Target 6.6`)){
    for (j in 1:nrow(myfiles9$`SDG 6`)){
        if(myfiles9$`Target 6.6`$Title[i] == myfiles9$`SDG 6`$Title[j]){
            myfiles9$`SDG 6`[j,which(colnames( myfiles9$`SDG 6`) == "Target 6.6")] <- "Target 6.6"
        }
    }
}

for (i in 1:nrow(myfiles9$`Target 6.a`)){
    for (j in 1:nrow(myfiles9$`SDG 6`)){
        if(myfiles9$`Target 6.a`$Title[i] == myfiles9$`SDG 6`$Title[j]){
            myfiles9$`SDG 6`[j,which(colnames( myfiles9$`SDG 6`) == "Target 6.a")] <- "Target 6.a"
        }
    }
}

for (i in 1:nrow(myfiles9$`Target 6.b`)){
    for (j in 1:nrow(myfiles9$`SDG 6`)){
        if(myfiles9$`Target 6.b`$Title[i] == myfiles9$`SDG 6`$Title[j]){
            myfiles9$`SDG 6`[j,which(colnames( myfiles9$`SDG 6`) == "Target 6.b")] <- "Target 6.b"
        }
    }
}

write_feather(myfiles9$`SDG 6`,"wn3_dashboard.feather")
