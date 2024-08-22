library(tidyverse)

# Carabidae ----------------------------------------------------------------
new <- readxl::read_excel("clean_data10.xlsx", sheet = "Carabidae.Lines") %>% 
    pivot_longer(names_to = "taxa", values_to = "num", -c("year", "zone", "line", "tur", "n.traps")) %>% 
    filter(num > 0) %>% 
    mutate(taxa = str_replace_all(taxa, "_+", "_"),
        species = sapply(taxa, function(a){
        a <- strsplit(a, "_")[[1]]
        str_glue("{substr(a[1], 1, 5)}_{substr(a[2], 1, 4)}")
        })
    ) 

old.b <- readxl::read_excel("Carabidae_long_Бельская_2022-01-28.xlsx") %>% 
    mutate_if(is.timepoint, as.Date) %>% 
    mutate(
        tur0 = tur, 
        tur = case_when(str_detect(date1, "-05-") ~ 1, TRUE ~ 2)) %>% 
    group_by(zone, year, tur, line, species) %>% 
    summarise(num = sum(num), n_traps = length(unique(trap)), .groups = "drop")

expand_grid(zone = c("fon", "imp"), 
            year = c(2005, 2018), 
            tur  = 1:2, 
            line = 1:7) %>% 
    mutate(type = paste0(year, zone, tur, line)) %>% 
    split(.$type) %>% 
    # `[`(1:2) %>%
    lapply(function(x){
        old_df <- old.b %>%
            filter(zone == x$zone, year == x$year, tur == x$tur, line == x$line, species != "_no_species")
        new_df <- new %>%
            filter(zone == x$zone, year == x$year, tur == x$tur, line == x$line)
        species <- list(old_df, new_df) %>% 
            map(~.x %>% 
                    pull(species) %>%
                    unique() %>%
                    sort())
        
        not_in12 <- paste0(species[[1]][!(species[[1]] %in% species[[2]])], collapse = ", ")
        not_in21 <- paste0(species[[2]][!(species[[2]] %in% species[[1]])], collapse = ", ")
        
        tibble(
            equal_length = length(species[[1]]) == length(species[[2]]), 
            not_in12,
            not_in21, 
            abu_diff = sum(old_df$num) - sum(new_df$num)
            )
    }) %>% 
    map_dfr(rbind, .id = "id") %>% 
    filter(abu_diff != 0 | not_in12 != "")

# Arachnida ----------------------------------------------------------------
new <- readxl::read_excel("clean_data10.xlsx", sheet = "Arachnida_united") %>% 
    pivot_longer(names_to = "taxa", values_to = "num", -c("year", "zone", "line", "tur", "n.traps")) %>% 
    mutate(tur = case_when(tur == "beg" ~ 1, TRUE ~ 2),
           taxa = str_replace_all(taxa, "-m", "--m"), 
           taxa = str_replace_all(taxa, "-f", "--f")) %>% 
    separate(taxa, into = c("sp", "sex"), sep = "--") %>% 
    filter(num > 0)

old.z <- readxl::read_excel("clean_data10.xlsx", sheet = "Arachnida_separated") %>% 
    mutate_if(is.timepoint, as.Date) %>% 
    filter(start %in% old.b$date1, end %in% old.b$date2) %>% 
    mutate(
        tur = case_when(str_detect(start, "-05-") ~ 1, TRUE ~ 2),
        # end = as.Date(end), 
        # tur = case_when(str_detect(end, "-08-") ~ 2, TRUE ~ 1),
        sp = case_when(is.na(sp) ~ paste0(family, " gen. sp."), 
                          TRUE ~ sp)) %>% 
    filter(num > 0, age == "ad") %>% 
    select(zone, year, tur, trap, line, sp, sex, num) # %>% select(-trap)
    


# errors <- 
expand_grid(zone = c("fon", "imp"), 
                      year = c(2005, 2018), 
                      tur  = 1:2, 
                      line = 1:7) %>% 
    mutate(type = paste0(year, zone, tur, line)) %>% 
    split(.$type) %>% 
    # `[`(1:2) %>%
    lapply(function(x){
        old_df <- old.z %>%
            filter(zone == x$zone, year == x$year, tur == x$tur, line == x$line) %>% # species != "_no_species")
            group_by(zone, year, tur, line, sp, sex) %>% 
            summarise(num = sum(num), .groups = "drop")
        new_df <- new %>%
            filter(zone == x$zone, year == x$year, tur == x$tur, line == x$line)
        
        
        # species <- list(old_df, new_df) %>% 
        #     map(~.x %>% 
        #             pull(sp) %>%
        #             unique() %>%
        #             sort())
        full_join(
            select(old_df, sp, sex, old_num = num),
            select(new_df, sp, sex, new_num = num),
            by = c("sp", "sex")
        ) %>% 
            filter(old_num != new_num)
            # mutate(
            #     old_num = case_when(is.na(old_num) ~ 0, TRUE ~ old_num),
            #     new_num = case_when(is.na(new_num) ~ 0, TRUE ~ new_num),
            #     PROBLEMS = old_num != new_num) #%>% 
            # list(data = ., nm = paste0(x$zone, "_", x$year, "_t", x$tur, "_l", x$line))
    }) %>% 
    sapply(nrow)
    # transpose()
errors$data %>% 
    `names<-`(errors$nm) %>% 
    writexl::write_xlsx("tmp.xlsx")

 
sp_to_remove <- full_join(
    summarise(group_by(new, sp), num_new = sum(num)), 
    summarise(group_by(old.z, sp), num_old = sum(num)),
    by = 'sp') %>% 
    filter(is.na(num_new) | is.na(num_old)) %>% 
    filter(is.na(num_new)) %>% 
    pull(sp)

old.z <- old.z %>% 
    filter(!(sp %in% sp_to_remove))

# arj ---------------------------------------------------------------------
expand_grid(zone = c("fon", "imp"), 
            year = c(2005, 2018), 
            tur  = 1:2, 
            line = 1:7) %>% 
    mutate(type = paste0(year, zone, tur, line)) %>% 
    split(.$type) %>% 
    # `[`(1:2) %>%
    lapply(function(x){
        old_df <- old.z %>%
            filter(zone == x$zone, year == x$year, tur == x$tur, line == x$line) # species != "_no_species")
        new_df <- new %>%
            filter(zone == x$zone, year == x$year, tur == x$tur, line == x$line)
        species <- list(old_df, new_df) %>% 
            map(~.x %>% 
                    pull(sp) %>%
                    unique() %>%
                    sort())
        
        not_in12 <- paste0(species[[1]][!(species[[1]] %in% species[[2]])], collapse = ", ")
        not_in21 <- paste0(species[[2]][!(species[[2]] %in% species[[1]])], collapse = ", ")
        
        tibble(
            equal_length = length(species[[1]]) == length(species[[2]]), 
            not_in12,
            not_in21, 
            abu_diff = sum(old_df$num) - sum(new_df$num)
        )
    }) %>% 
    map_dfr(rbind, .id = "id") %>% 
    filter(abu_diff != 0 | not_in12 != "")





expand_grid(zone = c("fon", "imp"), 
            year = c(2005, 2018), 
            tur  = 1:2, 
            line = 1:7) %>% 
    mutate(type = paste0(year, zone, tur, line)) %>% 
    split(.$type) %>% 
    # `[`(1:2) %>%
    lapply(function(x){
        old_df <- old.z %>%
            filter(zone == x$zone, year == x$year, tur == x$tur, line == x$line) %>% # species != "_no_species")
            group_by(zone, year, tur, line, sp, sex) %>% 
            summarise(num = sum(num), .groups = "drop")
        new_df <- new %>%
            filter(zone == x$zone, year == x$year, tur == x$tur, line == x$line)
        # paste0(x$zone, "_", x$year, "_t", x$tur, "_l", x$line)
        species <- list(old_df, new_df) %>% 
            map(~.x %>% 
                    pull(sp) %>%
                    unique() %>%
                    sort())
        
        not_in12 <- paste0(species[[1]][!(species[[1]] %in% species[[2]])], collapse = ", ")
        not_in21 <- paste0(species[[2]][!(species[[2]] %in% species[[1]])], collapse = ", ")
        
        tibble(
            equal_length = length(species[[1]]) == length(species[[2]]), 
            not_in12,
            not_in21, 
            abu_diff = sum(old_df$num) - sum(new_df$num)
        )
    })



