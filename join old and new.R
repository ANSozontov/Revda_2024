new <- readxl::read_excel("clean_data10.xlsx", sheet = "Arachnida_united") %>% 
    pivot_longer(names_to = "taxa", values_to = "num", -c("year", "zone", "line", "tur", "n.traps")) %>% 
    separate(taxa, into = c("sp", "sex"), sep = "-")
    # group_by(year, zone, line) %>% 
    # mutate(part = num/sum(num)) %>% 
    # ungroup()

old <- readxl::read_excel("clean_data10.xlsx", sheet = "Arachnida_separated") %>% 
    mutate(end = as.Date(end)) %>% 
    select(-n_traps, -start)
    



    pull(start) %>% unique
    filter(str_detect(`Примечание 1`, "исключить", negate = TRUE) | is.na(`Примечание 1`)) %>% 
    mutate(Tur = case_when(tur == 1 ~ "beg", tur == 2 ~ "end"), 
           duration = case_when(is.na(duration) ~ 5, TRUE ~ duration)) %>% 
    select(year, zone, season = tur, line, trap, taxa, num)
