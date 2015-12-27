library(magrittr)
library(httr)
library(XML)
library(stringr)
library(tools)
library(stringi)
library(R.utils)
#library(rlist)
#library(parallel)

set_root <- function(file_name){
    gunzip(file_name, remove = FALSE, overwrite=TRUE)
    regexp_rule = paste0("\\.",file_ext(file_name),"$")
    sub(regexp_rule, x = file_name, replacement = "") %>% 
        htmlTreeParse %>% xmlRoot
    
}

node_value_list <- function(root, path){
    getNodeSet(root, path)%>%
        lapply(xmlValue) %>% 
        lapply(convert_empty_str) %>%
        lapply(remove_space) %>%
        Filter(f = function(x) !grepl(pattern = "紅色字體", x, fixed = TRUE))
}

get_prefix <- function(root, path, indx){
    tmp <- getNodeSet(root, path) %>%
        lapply(xmlValue) %>% 
        lapply(convert_empty_str) %>%
        lapply(remove_space) %>%
        Filter(f = function(x) !grepl(pattern = "紅色字體", x, fixed = TRUE))
    
    tmp[[indx]]
}

convert_empty_str <- function(instr){
    outstr <- if(identical(instr, character(0))){ "" }else{ instr }
}

remove_space <- function(instr){
    stri_replace_all_charclass(instr, "\\p{WHITE_SPACE}", "")
}

remove_html <- function(file_name, rm_html){
    if(rm_html)
        file_path_sans_ext(file_name) %>%
        file.remove
}

remove_by_name <- function(inlist, pattern){
    inlist[!grepl(pattern = pattern, names(inlist))]  
}

pattern_match <- function(instr, pattern_list){
    TRUE %in% lapply(pattern_list, grepl, x=instr, fixed = TRUE)
}

get_subtable <- function(root, path1, path2, prefix, add_prefix = FALSE, shift = 0, check_tender = FALSE, keep_all = FALSE, pattern_list=NULL){
    # get XMLnode set 
    key <- node_value_list(root, path1)
    value <- node_value_list(root, path2)


    subtable <- list()
    tender_tag <- ""
    prefix_tag <- if(add_prefix) paste0(prefix, ".")  else ""
    # retrive all tuples of subtable
    for(i in c(1:length(key))){
        if(keep_all || pattern_match(key[i], pattern_list)){

            attr_name <- paste0(prefix_tag, key[i]) 
            if(check_tender)
                if(grepl(pattern = "投標廠商[0-9]+", key[i]))
                    tender_tag <- paste0(key[i], ".")
                else
                    attr_name <- paste0(prefix_tag, tender_tag, key[i])

            subtable[attr_name] <- value[i+shift]
        }
    }
    subtable
}


export_to_string_list <- function(file_name = "temp.csv", ...){
    arg <- list(...)
    attr_name_str <- ""
    attr_value_str <- ""
    for(a in arg[]){
        a_names <- paste(names(a), collapse = '\", \"', sep='')
        attr_name_str <- {
            if(attr_name_str != "") 
                paste0(attr_name_str, '\", \"', a_names) 
            else
                paste0(attr_name_str, a_names)
        }
        
        a_values <- paste(a, collapse = '\", \"', sep='')
        attr_value_str <- {
            if(attr_value_str != "")
                paste0(attr_value_str, '\", \"', a_values)
            else
                paste0(attr_value_str, a_values)
        }
    }
    
    attr_name_str <- paste0('\"', attr_name_str, '\"')
    attr_value_str<- paste0('\"', attr_value_str, '\"')
    list(attr_name_str, attr_value_str)
    #write(paste0(attr_name_str, '\n', attr_value_str), paste0(file_path_sans_ext(file_name), ".csv"))
}



export_to_csv <- function(file_name = "temp.csv", ...){
    arg <- list(...)
    attr_name_str <- ""
    attr_value_str <- ""
    for(a in arg[]){
        a_names <- paste(names(a), collapse = '\", \"', sep='')
        attr_name_str <- {
            if(attr_name_str != "") 
                paste0(attr_name_str, '\", \"', a_names) 
            else
                paste0(attr_name_str, a_names)
        }
        
        a_values <- paste(a, collapse = '\", \"', sep='')
        attr_value_str <- {
            if(attr_value_str != "")
                paste0(attr_value_str, '\", \"', a_values)
            else
                paste0(attr_value_str, a_values)
        }
    }
    
    attr_name_str <- paste0('\"', attr_name_str, '\"')
    attr_value_str<- paste0('\"', attr_value_str, '\"')
    
    write(paste0(attr_name_str, '\n', attr_value_str), paste0(file_path_sans_ext(file_name), ".csv"))
}

brute_force_parse <- function(file_name, rm_html = TRUE, save_a_csv_for_each = FALSE){
    #0 initiate
    root <- set_root(file_name)
    
    #1 機關資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_1']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_1']/td"
    prefix <- get_prefix(root, path2, 1)
    org_info <- get_subtable(root, path1, path2, prefix, add_prefix = TRUE, shift = 1, keep_all = TRUE)
    
    #2 採購資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_2']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_2']/td"
    prefix <- get_prefix(root, path2, 1)
    purchase_info <- get_subtable(root, path1, path2, prefix, add_prefix = TRUE, shift = 1, keep_all = TRUE)
    
    #3 投標廠商
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td/table/tr/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td/table/tr/td"
    pathp <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td"
    prefix <- get_prefix(root, pathp, 1)
    tender_company_info <- get_subtable(root, path1, path2, add_prefix = TRUE, prefix, shift = 0, check_tender = TRUE, keep_all = TRUE)
    
    #4 決標品項
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_4']/td/table/tr/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_4']/td/table/tr/td"
    pathp <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_4']/td"
    prefix <- get_prefix(root, pathp, 1)
    tender_award_item_info <- get_subtable(root, path1, path2, add_prefix = TRUE, prefix, shift = 0, keep_all = TRUE)
    
    #6 決標資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_6']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_6']/td"
    prefix <- get_prefix(root, path2, 1)
    tender_award_info <- get_subtable(root, path1, path2, add_prefix = TRUE, prefix, shift = 1, keep_all = TRUE)
    
    remove_html(file_name, rm_html)   
    if(save_a_csv_for_each) 
        export_to_csv(file_name, purchase_info, tender_award_info, tender_company_info)
    else
        export_to_string_list(file_name, purchase_info, tender_award_info, tender_company_info)
}

minimum_parse <-function(file_name, rm_html = TRUE, save_a_csv_for_each = FALSE){
    # 標案案號
    # 標案名稱
    # 決標日期
    # 總決標金額
    # 投標廠商家數
    # 廠商代碼
    # 廠商名稱
    # 是否得標

    #0 initiate
    root <- set_root(file_name)
    
    #1 機關資料

    #2 採購資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_2']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_2']/td"
    pattern_list <- list("標案案號")
    purchase_info <- get_subtable(root, path1, path2, prefix = "", add_prefix = FALSE, shift = 1, keep_all = FALSE, pattern_list = pattern_list)  
      
    #3 投標廠商
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td/table/tr/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td/table/tr/td"
    pattern_list <- list("投標廠商","投標廠商家數", "廠商代碼", "廠商名稱", "是否得標")
    tender_company_info <- get_subtable(root, path1, path2, prefix = "", add_prefix = FALSE, shift = 0, check_tender = TRUE, keep_all = FALSE, pattern_list = pattern_list)
    tender_company_info <- remove_by_name(tender_company_info, "投標廠商[0-9]+$")
        
    #4 決標品項

    #6 決標資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_6']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_6']/td"
    pattern_list <- list("決標日期", "總決標金額") 
    tender_award_info <- get_subtable(root, path1, path2,prefix = "", add_prefix = FALSE, shift = 1, keep_all = FALSE, pattern_list = pattern_list)    
    tender_award_info <- remove_by_name(tender_award_info, "總決標金額.+")

    remove_html(file_name, rm_html)   
    if(save_a_csv_for_each) 
        export_to_csv(file_name, purchase_info, tender_award_info, tender_company_info)
    else
        export_to_string_list(file_name, purchase_info, tender_award_info, tender_company_info)
}


gen_log_name <- function(instr){
    tmp <- str_replace_all(instr, pattern="[:]", replacement="") %>%  str_replace(pattern=" ", replacement="-")
    paste0(tmp, ".log")
}

roaming_dir <- function(start_dir= "./", write_log = FALSE, use_minimum_parse = TRUE, dont_save_each_csv = TRUE){
    start_time <- Sys.time()
    paste("start roaming_dir, timestamp = ", as.character(start_time)) %>% print

    all_dir <- dir(start_dir, recursive = TRUE)
    
    if(write_log)
        log_name <- as.character(start_time) %>% gen_log_name 
        write(c("dir index", "basename", "dirname", "exec time"), ncolumns = 4, file=log_name, append = TRUE)    

    all_column_names <- ""
    column_number <- 0
        
    iter_cnt <- 1
    
    for(d in all_dir){
        iter_start_time <- Sys.time()
        bname <- basename(d)
        dname <- dirname(d)
        
        content <-{
            if(use_minimum_parse)
                minimum_parse(paste0(start_dir, "/", d), rm_html = TRUE, save_a_csv_for_each = !dont_save_each_csv)
            else
                brute_force_parse(paste0(start_dir, "/", d), rm_html = TRUE, save_a_csv_for_each = !dont_save_each_csv)            
        }
        
        if(dont_save_each_csv){
            if(iter_cnt %% 2000 == 1)
                paste0(as.character(iter_cnt), " files parsed, timestamp = ", as.character(Sys.time())) %>% print
                fname <- paste0("tender", as.character(iter_cnt %/% 2000),".csv")
            
            write(content[[2]], file = fname, append = TRUE )
                
            if(length(strsplit(content[[1]], ", ", fixed=TRUE)) > column_number){
                all_column_names <- content[[1]]
                column_number <- length(strsplit(content[[1]], ", ", fixed=TRUE))
            }
        }
        
        
        iter_end_time <- Sys.time()
        if(write_log)
            log_content = paste(as.character(iter_cnt), bname, dname, as.character(iter_start_time - iter_end_time), sep=", ")
            write(log_content, ncolumns = 4, file=log_name, append = TRUE)
        iter_cnt = iter_cnt + 1
    }
    
    end_time <- Sys.time()
    paste(as.character(iter_cnt), " files parsed, elapse time: ", as.character(end_time - start_time)) %>% print
    
    invisible(all_column_names)
}

file_add_header <- function(start_dir= "./", all_column_names){
    start_time <- Sys.time()
    d <- dir(start_dir)
    tender_csv <- d[grepl(pattern = "tender[0-9]+", basename(d))]
    write(all_column_names, file="tender_total.csv", append = TRUE)
    for( t in tender_csv){
        rl <- readLines(t,-1)
        write(rl, file="tender_total.csv", append = TRUE)        
    }
    end_time <- Sys.time()
    paste("all tender*.csv concated, elapse time: ", as.character(end_time - start_time)) %>% print    
}