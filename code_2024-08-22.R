# 0.0. License --------------------------------------------------------------
# ʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘ
#                          Artëm Sozontov ©
# ʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘ
# _________________________________________________________________________
# This code is stored on the GitHub repository 
# URL: https://github.com/ANSozontov/Revda_2021 
# and has a CC BY SA 4.0 licence 
# URL: https://creativecommons.org/licenses/by-sa/4.0/ 
# 
#                         What does it mean? 
# 
# You are free to use, transform and distribute this code or its parts, 
# including commercial purposes. Just keep in mind only two restrictions: 
# `SA`: You are obligated to publish and distribute you derivate code 
#     under the license not stricter than the current one (CC-BY-SA). 
# `BY`: You are obligated to cite the article where authorship was claimed:
#     Belskaya E.A., Zolotarev M.P., Sozontov A.N., Vorobeichik E.L. 
#     %The article name% // Russian Journal of Ecology. 2022. № 3. P. xx-xxx.
# _________________________________________________________________________

# ʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘ
#                          Артём Созонтов ©
# ʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘʘ
# _________________________________________________________________________
# Код размещен на репозитории GitHub 
# URL: https://github.com/ANSozontov/Revda_2021 
# и распространяется под лицензией CC BY SA 4.0 
# URL: https://creativecommons.org/licenses/by-sa/4.0/ 
# 
#                         Что это означает? 
# 
# Вы можете свободно использовать, модифицировать и распространять этот код
# (включая коммерческое использование), учитывая лишь два ограничения:
# `SA`: Вы обязаны публиковать и распространять свой собственный код, 
#     производный от данного, под лицензией не строже чем текущая - СС-BY-SA
# `BY`: Используя данный скрипт или его фрагменты вы обязаны процитировать 
#     статью, где было заявлено авторство: Бельская Е.А., Золотарев  М.П., 
#     Созонтов А.Н., Воробейчик Е.Л. %Имя% // Экология. 2022. № 3. P. xx-xxx
#     %The article name% // Экология. 2022. № 3. С. xx-xxx. 
# _________________________________________________________________________

# 0.1. SpiderCharts -------------------------------------------------------
# Spider Charts function comes from: 
#     https://github.com/ANSozontov/SpiderCharts
# availiable for regular run via: 
#     source("https://raw.github.com/ANSozontov/spider_charts/master/spider_charts.R")
# Here it has a couple of changes:

spiderchart <- function(
        df = NULL, 
        top = NULL, 
        subdivisions = 4, # manual
        zero_shift = 15, # manual
        scale_shist = -5, # manual
        label_shift = 5, # manual
        label_size = 3.5, # manual
        webtype = "dotted", # manual
        flexure = 0.2, #manual
        need_label = TRUE, #manual
        need_scale = TRUE, # manual
        need_web = TRUE # manual
) {
    # require(crayon)
    if(is.null(df))
    {
        cat(crayon::red("There is no data!
"))
        cat(crayon::green("Do you need an example?
"))
        df <- tibble::tibble(id = c("A", "B", "C"), 
                             Z =      c(85, 25, 25), 
                             M =      c(25, 85, 25), 
                             big =    c(25, 25, 85), 
                             med =    c(85, 25, 25), 
                             sml =    c(25, 85, 25), 
                             runing = c(25, 25, 85), 
                             jumping= c(25, 85, 25))
        df
    } else {
        require(tidyverse)
        ID = max(which(!sapply(df, is.numeric)))
        N = ncol(df) - ID
        AL = 2*pi/(N)
        if(is.null(top)) {top = ceiling(max(df[,-c(1:ID)])/10)*10} # but can be manual
        
        
        df <- df %>% 
            rename(id = ID) %>% 
            pivot_longer(names_to = "lab", values_to = "L", -all_of(c(1:ID))) %>% 
            mutate(i = 0:(nrow(.)-1), 
                   M = L + max(L)/3, 
                   L = L + zero_shift,
                   x = L*cos(pi/2-AL*i), 
                   y = L*sin(pi/2-AL*i)) 
        
        G1 <- data.frame( # Web-Grid
            x1 = rep(seq(0, top, by = top/subdivisions) + zero_shift, each = N) * cos(pi/2-AL*rep(c(1:N), subdivisions+1)),
            y1 = rep(seq(0, top, by = top/subdivisions) + zero_shift, each = N) * sin(pi/2-AL*rep(c(1:N), subdivisions+1)), 
            x2 = rep(seq(0, top, by = top/subdivisions) + zero_shift, each = N) * cos(pi/2-AL*rep(c(2:N, 1), subdivisions+1)),
            y2 = rep(seq(0, top, by = top/subdivisions) + zero_shift, each = N) * sin(pi/2-AL*rep(c(2:N, 1), subdivisions+1))
        )
        G2 <- data.frame( # rays
            x1 = top*cos(pi/2-AL*1:N),
            y1 = top*sin(pi/2-AL*1:N), 
            x2 = zero_shift*cos(pi/2-AL*1:N),
            y2 = zero_shift*sin(pi/2-AL*1:N)
        )
        G3 <- data.frame(
            x = -5, 
            L = seq(0, top, by = top/subdivisions),
            y = seq(0, top, by = top/subdivisions) + zero_shift)
        G4 <- data.frame(
            lb = unique(df$lab),
            x = (max(df$L) + zero_shift + label_shift) * cos(pi/2-AL*0:(N-1)), 
            y = (max(df$L) + zero_shift + label_shift) * sin(pi/2-AL*0:(N-1))
        )
        
        ggplot() + 
            {if(need_web) # web: segments
                geom_curve(aes(x1, y1, xend = x2, yend = y2), data = G1, curvature = flexure, linetype = webtype)
            } +
            {if(need_web)  # web: radial rays
                geom_segment(aes(x1, y1, xend = x2, yend = y2), data = G2, linetype = webtype)
            } + 
            {if(need_scale) # scale
                geom_text(aes(x, y, label = L), data = G3)
            } +
            geom_polygon(
                aes(x, y), 
                data = filter(df, id == "fon"),
                color = "darkolivegreen1", 
                fill = rgb(0, 1, 0, alpha = 0),
                linewidth = 0.8
            ) + # polygons
            geom_polygon(
                aes(x, y), 
                data = filter(df, id == "imp"),
                color = rgb(0.8, 0.01, 0.1, 1), 
                fill = rgb(0.8, 0.01, 0.1, 0.2),
                linewidth = 0.75
            ) + 
            geom_point(
                aes(x, y, color = id), 
                shape = 15, 
                size = 2.5,
                data = df) + # peaks
            {if(need_label) # peak labels
                geom_text(aes(x, y, label = lb), data = G4, size = label_size)
            } + 
            coord_equal() + 
            scale_color_manual(values = c("darkolivegreen1", "#CC031AFF")) +
            theme_void() 
    }
}
# 0.2. Data load ------------------------------------------------------------

library(tidyverse)

theme_set(
    theme_bw() + 
    theme(
        legend.position = "bottom",
        panel.grid = element_blank())
)
type = "lines" # lines/traps

carlong <- readxl::read_excel("clean_data12.xlsx", sheet = "Carabidae_separated") %>% 
    select(-date1, -date2) %>% 
    pivot_wider(names_from = sp, values_from = num, values_fill = 0, values_fn = sum) %>% 
    pivot_longer(names_to = "sp", values_to = "num", -1:-6)
if(type == "lines") {
    carlong <- carlong %>% 
        group_by(zone, year, line, sp) %>% 
        summarise(num = sum(num), .groups = "drop") 
} else if(type == "traps") {
    carlong <- carlong %>% 
        group_by(zone, year, trap, sp) %>% 
        summarise(num = sum(num), .groups = "drop") %>% 
        rename(line = trap) 
} else {
    break
}

carlong <- carlong %>% 
    mutate(num = case_when(
        year == 2005 ~ num/3/7*100, 
        year == 2018 ~ num/5/7*100)) %>% 
    group_by(zone, year, line) %>% 
    mutate(part = num/sum(num)) %>% 
    ungroup()

cartraits <- readxl::read_excel("clean_data12.xlsx", sheet = "Carabidae.Traits") %>% 
    select(sp, fenol:size____) %>% 
    right_join(., filter(carlong, sp != "_no_species"), by = "sp") %>% 
    mutate(type = paste0(zone, "_", year))

aralong <- readxl::read_excel("clean_data12.xlsx", sheet = "Arachnida_separated") %>% 
    pivot_wider(names_from = sp, values_from = num, values_fill = 0, values_fn = sum) %>% 
    pivot_longer(names_to = "sp", values_to = "num", -1:-6)
if(type == "lines") {
    aralong <- aralong %>% 
        group_by(zone, year, line, sp, sex) %>% 
        summarise(num = sum(num), .groups = "drop") 
} else if(type == "traps") {
    aralong <- aralong %>% 
        group_by(zone, year, trap, sp, sex) %>% 
        summarise(num = sum(num), .groups = "drop") %>% 
        rename(line = trap) 
} else {
    break
}
aralong <- aralong %>% 
    mutate(num = case_when(
        year == 2005 ~ num/3/7*100, 
        year == 2018 ~ num/5/7*100)) %>% 
    group_by(zone, year, line) %>% 
    mutate(part = num/sum(num)) %>% 
    ungroup()

aratraits <- readxl::read_excel("clean_data12.xlsx", sheet = "Arachnida.Traits") %>% 
    select(taxa,  size____:storey__) %>% 
    right_join(., unite(aralong, "taxa", sp, sex, sep = "-"), by = "taxa") %>% 
    mutate(type = paste0(zone, "_", year))
"SUCCESS"
# 0.2. Rarefication ---------------------------------------------------------
rar.car <- carlong %>% 
    unite("type", year, zone, line, sep = "_") %>% 
    group_by(type, sp) %>% 
    summarise(num = sum(num), .groups = "drop") %>% 
    arrange(sp) %>% 
    pivot_wider(names_from = sp, values_from = num, values_fill = 0) %>% 
    column_to_rownames("type") %>% 
    t() %>% 
    as_tibble() %>% 
    lapply(., function(a){sort(round(a[a>0]), decreasing = TRUE)}) %>% 
    iNEXT::iNEXT(., q = 0, size = c(100, 1000), 
                    datatype = "abundance", nboot = 9) %>%  # 999
    pluck("iNextEst", "size_based") %>% 
    as_tibble %>% 
    filter(m == 100 | Method == "Observed") %>% 
    transmute(
        year = as.numeric(substr(Assemblage, 1, 4)), 
        zone = substr(Assemblage, 6, 8), 
        line = as.numeric(substr(Assemblage, 10, 10)), 
        valu = case_when(m == 100 ~ "c.nsp.d100", 
                             TRUE ~ "c.nsp.obs"), 
        qD) %>% 
    pivot_wider(names_from = valu, values_from = qD)

rar.ara <- aralong %>% 
    unite("type", year, zone, line, sep = "_") %>% 
    group_by(type, sp) %>% 
    summarise(num = sum(num), .groups = "drop") %>% 
    arrange(sp) %>% 
    pivot_wider(names_from = sp, values_from = num, values_fill = 0) %>% 
    column_to_rownames("type") %>% 
    t() %>% 
    as_tibble() %>% 
    lapply(., function(a){sort(round(a[a>0]), decreasing = TRUE)}) %>% 
    iNEXT::iNEXT(., q = 0, size = c(100, 1000), 
               datatype = "abundance", nboot = 9) %>%   # 999
    pluck("iNextEst", "size_based") %>% 
    as_tibble %>% 
    filter(m == 100 | Method == "Observed") %>% 
    transmute(year = as.numeric(substr(Assemblage, 1, 4)), 
            zone = substr(Assemblage, 6, 8), 
            line = as.numeric(substr(Assemblage, 10, 10)), 
            valu = case_when(m == 100 ~ "a.nsp.d100", 
                             TRUE ~ "a.nsp.obs"), 
            qD) %>% 
    arrange(year, zone, line) %>% 
    pivot_wider(names_from = valu, values_from = qD)

# 0.3. Aggregation: counts -----------------------------------------------------
agg <- function(df1, gr, tax) {
    # df1 - aratraits/cartraits
    # gr - grouping variable
    # tax - add in the beg. of new variables
    # gr <- c(gr, "year", "zone", "line")
    df1 %>% 
        rename(GR = gr) %>% 
        group_by(GR, year, zone, line) %>% 
        summarise(num = sum(num), .groups = "drop") %>% #num/part
        rename(tp = 1) %>% 
        mutate(tp = paste0(tax, gr, "_", tp)) %>% 
        pivot_wider(names_from = tp, values_from = num, values_fill = 0) %>% #num/part
        return(.)
}
res1 <- expand_grid(year = c(2005,2018), 
                   zone = c("fon", "imp"), 
                   line = 1:7)
bbyy <- c("year", "zone", "line")
res1 <- res1 %>% 
    full_join(agg(cartraits, "foraging", "c."), by = bbyy) %>%
    full_join(agg(cartraits, "storey__", "c."), by = bbyy) %>%
    full_join(agg(cartraits, "habitats", "c."), by = bbyy) %>%
    full_join(agg(cartraits, "humidity", "c."), by = bbyy) %>%
    full_join(agg(cartraits, "dispersl", "c."), by = bbyy) %>%
    full_join(agg(cartraits, "size____", "c."), by = bbyy) %>%
    full_join(agg(aratraits, "size____", "a."), by = bbyy) %>%
    full_join(agg(aratraits, "humidity", "a."), by = bbyy) %>%
    full_join(agg(aratraits, "habitats", "a."), by = bbyy) %>%
    full_join(agg(aratraits, "lifeform", "a."), by = bbyy) %>%
    full_join(agg(aratraits, "storey__", "a."), by = bbyy)
for.table <- res1 %>%
    pivot_longer(values_to = "vl", names_to = "vr", -1:-3) %>%
    group_by(year, zone, vr) %>%
    summarise(sdvl = sd(vl), vl = mean(vl),  .groups = "drop")

# 1. Effect Size ----------------------------------------------------------
# Effect.Size counts
res2 <- res1 %>% 
    full_join(group_by(carlong, year, zone, line) %>%
                  summarise(c.abundance = sum(num), .groups = "drop"),
              by = bbyy) %>%
    full_join(rar.car, by = bbyy) %>%
    full_join(group_by(aralong, year, zone, line) %>% 
                  summarise(a.abundance = sum(num), .groups = "drop"), 
              by = bbyy) %>% 
    full_join(rar.ara, by = bbyy)

effs <- expand_grid(vr = colnames(res2)[4:ncol(res2)], year = c(2005, 2018)) 
effs <- map_dfr(1:nrow(effs), 
    ~SingleCaseES::LRRi( # effs[i, 3:7] <-  
        res2[res2$zone == "fon" & res2$year == effs$year[.x], effs$vr[.x]],
        res2[res2$zone == "imp" & res2$year == effs$year[.x], effs$vr[.x]])
    ) %>% 
    cbind(effs, .) %>% 
    as_tibble()
# Effect.Size prepare
dictionary <- matrix(byrow = TRUE, ncol = 2, data = {c(
    "a.abundance", "AD", 
    "c.abundance", "AD",
    "a.nsp.obs", "Sp",
    "c.nsp.obs", "Sp",
    "a.nsp.d100", "Sp'",
    "c.nsp.d100", "Sp'",
    "a.size_____small", "S", 
    "c.size_____small", "S", 
    "a.size_____medium", "M", 
    "c.size_____medium", "M",
    "a.size_____big", "B", 
    "c.size_____big", "B", 
    "a.humidity_hygroph", "hyg", 
    "c.humidity_hygroph", "hyg", 
    "a.humidity_mesoph", "mes", 
    "c.humidity_mesoph", "mes", 
    "a.storey___strato", "LS", 
    "c.storey___strato", "LS", 
    "a.storey___gerpeto", "SS",
    "c.storey___epigeo", "SS", 
    "a.habitats_forest", "F",
    "c.habitats_forest", "F", 
    "a.habitats_for.meadow", "E", 
    "c.habitats_for.meadow", "E", 
    "a.lifeform_hunter", "H", 
    "a.lifeform_web", "W", 
    "a.lifeform_web.hnt", "WH",
    "a.lifeform_scavenger", "SH", 
    "c.foraging_zoo.phg", "Z", 
    "c.foraging_mix.phg", "Mx",
    "c.dispersl_not.mobile", "lm",
    "c.dispersl_low.mobile", "mm", 
    "c.dispersl_high.mobile", "hm"
    )}) %>% 
    data.frame() %>% 
    `names<-`(c("vr", "lab"))
e <- dictionary %>% 
    as_tibble() %>% 
    left_join(effs, by = "vr") %>% 
    # filter(substr(vr, 1, 2) != "a.") %>% 
    transmute(vr, year = as.character(year), Est, CI_lower, CI_upper, 
        taxa = case_when(substr(vr, 1,1) == "a" ~ "Arachnida", TRUE ~ "Carabidae"),
        lab) 
    

eviz <- rbind(
    e %>%
        filter(taxa == "Arachnida") %>%
        mutate(nn = sort(c(seq(51, 5, -3), seq(50, 5, -3)), decreasing = TRUE),
               .before = "taxa") %>%
        mutate(lab = ifelse(duplicated(lab), ".", lab)),
    expand_grid(vr = NA, year = NA, Est = NA,
                CI_lower = NA, CI_upper = NA,
                nn = seq(49+24, 1, -3),
                taxa = c("Arachnida", "Carabidae" ),
                lab = "."),
    e %>% 
        filter(taxa == "Carabidae") %>% 
        mutate(nn = sort(c(seq(51, 2, -3), seq(50, 2, -3)), decreasing = TRUE), 
               .before = "taxa") %>% 
        mutate(lab = ifelse(duplicated(lab), ".", lab))
    ) %>% 
    arrange(desc(nn))
    
# Effect.Size Viz
headings <- {tibble(
            x = seq(53.5, 77, by = 3),
            y = seq(-3, -2.5, length.out = 8),
            year = NA,
            CI_lower = NA,
            CI_upper = NA,
            label = c("Abundance & Diversity", "Size", "Moisture", "Stratum",
                      "Habitat", "Hunting", "Feeding", "Mobility")[8:1]
            ) %>% 
    mutate(year = as.character(year),
           CI_lower = as.numeric(CI_lower), 
           CI_upper = as.numeric(CI_upper))}
p1 <- ggplot(eviz, aes(x = nn, y = Est, ymin = CI_lower, 
                       ymax = CI_upper, fill = year)) + 
    coord_flip()+
    geom_vline(xintercept = seq(1, 73, by = 3), color = "grey", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_text(
        mapping = aes(x = x, y = y, label = label),
        color = "black",
        data = headings) +
    geom_pointrange(shape = 22) +
    geom_text(data = filter(eviz, taxa == "Arachnida", lab != "."), 
              mapping = aes(x = nn-0.5, y = -7, label = lab)) +
    geom_text(data = filter(eviz, taxa == "Carabidae", lab != "."), 
              mapping = aes(x = nn - 0.5, y = 7, label = lab)) +
    scale_fill_manual(values = c("black", "white")) + 
    facet_wrap(~taxa) + 
    labs(x = NULL, y = NULL) +
    theme_bw() + 
    theme(legend.position = "bottom", 
          axis.text.y=element_blank(), axis.line.y = element_blank(),
          axis.ticks.y=element_blank(), 
          panel.grid = element_blank(), 
          panel.background = element_blank())

p1
ggsave(paste0("Fig.1_", Sys.Date(), "_", type, ".pdf"),
       plot = p1,
       dpi = 1200, width = 9, height = 9)

# 2.1. Ordination -----------------------------------------------------------
pcoa.run <- function(data, tp) { # , name
    # tp = type where "num" and "part" are possible
    if(sum(stringr::str_detect(data$taxa, "-")) > 0 | 
       "sex" %in% colnames(data)
       ){
        #block romeves sex val. differences
        data <- data %>% 
            mutate(taxa = substr(taxa, 1, nchar(taxa) - 2)) %>% 
            group_by(year, zone, line, taxa) %>%  
            summarise(num = sum(num),
                      part = sum(part),
                      .groups = "drop") 
    }
    wide <- data %>% 
        select(year, zone, line, taxa, val = all_of(tp)) %>% 
        pivot_wider(names_from = taxa, values_from = val, values_fill = 0)
    dis <- vegan::vegdist(
        wide[,4:ncol(wide)], 
        method = "bray", binary = FALSE)
    pcoa <- ape::pcoa(dis)
    eig <- pcoa$values$Eigenvalues 
    eig <- round(eig/sum(eig)*100)
    pcoa <- cbind(wide[,1:3], pcoa$vectors[,1:2]) %>% 
        mutate(year = as.character(year)) 
    lst(pcoa, eig, dis, wide)
}

pcoa.viz <- function(obj, NM = NULL, SBT = NULL, x = FALSE, y = FALSE){
    df <- obj$pcoa
    if(x){df <- mutate(df, Axis.1 = Axis.1 * -1)}
    if(y){df <- mutate(df, Axis.2 = Axis.2 * -1)}
    ggplot(df, aes(x = Axis.1, y = Axis.2, shape = year, fill = zone)) + 
        geom_point(size = 2) + 
        stat_ellipse() + 
        geom_vline(xintercept = 0, linetype = "dotted") + 
        geom_hline(yintercept = 0, linetype = "dotted") + 
        scale_shape_manual(values = c(21, 24)) + 
        scale_fill_manual(values = c("darkolivegreen1", "#CC031AFF")) +
        scale_x_continuous(limits = c(-0.615, 0.635)) + 
        scale_y_continuous(limits = c(-0.6, 0.51)) +
        labs(title = NM, subtitle = SBT,
            shape = "Год",
            fill = "Зона",
            x = paste0("Ось 1 (", obj$eig[1], "%)"), 
            y = paste0("Ось 2 (", obj$eig[2], "%)"))
}

PCOA <- list(aralong, aralong, carlong, carlong) %>% 
    map(~rename(.x, taxa = sp)) %>% 
    map2(c("part", "num", "part", "num"), 
         ~pcoa.run(.x, .y)) %>% 
    `names<-`(c("Arachnida_Доли", "Arachnida_Обилия", 
                "Carabidae_Доли", "Carabidae_Обилия"))

p2 <- gridExtra::grid.arrange(
    pcoa.viz(PCOA[[2]], "Arachnida", "Обилия"),
    pcoa.viz(PCOA[[1]], "Arachnida", "Доли") ,
    pcoa.viz(PCOA[[4]], "Carabidae", "Обилия", F, T),
    pcoa.viz(PCOA[[3]], "Carabidae", "Доли"),
    ncol = 2)

ggsave(paste0("Fig.2_", Sys.Date(), "_", type, ".pdf"), plot = p2, height = 7, width = 9)

# 3. Aggregation: pics ------------------------------------------------------
dictionary <- matrix(byrow = TRUE, ncol = 2, data = {c(
        "small", "S",
        "medium", "M", 
        "big", "B", 
        "hygroph", "hyg", 
        "mesoph", "mes",
        "xeroph", "xer",
        "strato", "LS",
        "gerpeto", "SS",
        "epigeo", "SS",
        "horto.up", "HSD",
        "gerp.hort", "HSD",
        "forest", "F",
        "for.meadow", "E", 
        "meadow","Oh", 
        "hunter", "H", 
        "web", "W", 
        "web.hnt", "WH",
        "scavenger", "SH", 
        "zoo.phg",  "Z", 
        "mix.phg", "Mx",
        "not.mobile", "lm",
        "low.mobile", "mm", 
        "high.mobile", "hm"
    )}) %>% 
    as_tibble(.name_repair = "unique") %>% 
    rename(grps = 1, grps2 = 2)

p3_data <- rbind(
    pivot_longer(select(aratraits, size____:num), 
        names_to = "categ", values_to = "grps", -c("year", "zone", "line", "num")) %>% 
        mutate(taxa = "Arachnida"), 
    pivot_longer(select(cartraits, foraging:num), 
        names_to = "categ", values_to = "grps", -c("year", "zone", "line", "num")) %>% 
        mutate(taxa = "Carabidae")
    ) %>% 
    mutate(
        grps = case_when(
            taxa == "Arachnida" & grps == "water" ~ "nodata",
            taxa == "Carabidae" & grps == "water" ~ "meadow", 
            TRUE ~ grps),
        type = paste0(taxa, "_", year)
        ) %>% 
    split(.$type) %>% 
    map(~.x %>% 
            group_by(zone, categ, grps) %>% 
            summarise(v = sum(num), .groups = "drop_last") %>% 
            mutate(v = round(v/sum(v)*100, 1)) %>%
            ungroup() %>% 
            rbind(expand_grid(zone = c("fon", "imp"), categ = NA,
                grps = c("epigeo", "gerp.hort", "strato", "meadow"), v = 0)) %>% 
            left_join(dictionary, ., by = "grps") %>% 
            filter(grps != "nodata") %>% 
            select(-categ, -grps) %>% 
            filter(!is.na(zone), !is.na(v)) %>% 
            pivot_wider(names_from = grps2, values_from = v, values_fn = sum) %>% 
            mutate_all(function(x){x[is.na(x)] <- 0; x})
    )

p3 <- p3_data %>% 
    lapply(function(x){x[is.na(x)] <- 0; x}) %>% 
    map(~spiderchart(.x) + 
            theme(legend.position = "bottom")
    ) %>% 
    map2(names(p3_data), ~.x + labs(title = .y))

gridExtra::grid.arrange(p3[[1]], p3[[3]], p3[[2]], p3[[4]], ncol = 2) %>% 
    ggsave(paste0("Fig.3_", Sys.Date(), "_", type, ".pdf"), 
           plot = ., height = 12, width = 12)


# 2.2. Permanova / Anosim --------------------------------------------------
dummy <- matrix(ncol = 5, byrow = TRUE, data = c(
    # z1    y1      z2      y2      
    "fon", "2005", "fon", "2018", "year",
    "imp", "2005", "imp", "2018", "year",
    "fon", "2005", "imp", "2005", "zone",
    "fon", "2018", "imp", "2018", "zone"
)) %>% 
    as.data.frame() %>% 
    rename(z1 = 1, y1 = 2, z2 = 3, y2 = 4, compr = 5) %>% 
    expand_grid(i = 1:4, .) %>% 
    mutate_at(c("y1", "y2"), as.numeric)

permanova <- 1:16 %>% 
    lapply(function(x){
        data <- PCOA[[dummy$i[x]]] %>% 
            pluck("wide") %>% 
            select(-line)
        data1 <- filter(data, zone == dummy$z1[x], year == dummy$y1[x])
        dis1 <- mean(vegan::vegdist(data1[,-1:-2]))
        data2 <- filter(data, zone == dummy$z2[x], year == dummy$y2[x])
        dis2 <- mean(vegan::vegdist(data2[,-1:-2]))
        data  <- rbind(data1, data2)
        dis <- vegan::vegdist(data[,-1:-2], method = "bray", binary = FALSE)
        permanova <- vegan::adonis2(as.formula(paste0("dis ~ ", dummy$compr[x])), data = data)
        dis.table <- dis %>% 
            as.matrix() %>% 
            as_tibble() %>% 
            rownames_to_column("id_a") %>% 
            mutate(year_a = data$year, zone_a = data$zone, .after = id_a) %>% 
            pivot_longer(-c(id_a, year_a, zone_a), names_to = "id_b", values_to = "dis") %>% 
            mutate_at(c("id_a", "id_b"), as.numeric)
        dis.table <- dis.table %>% 
            transmute(id_b = id_a, year_b = year_a, zone_b = zone_a) %>% 
            distinct() %>% 
            left_join(dis.table, ., by = "id_b") %>% 
            filter(id_a < id_b)
        d1 <- dis.table %>% 
            filter(
                year_a == dummy$y1[x], zone_a == dummy$z1[x],
                year_b == dummy$y1[x], zone_b == dummy$z1[x]
            )
        d2 <- dis.table %>% 
            filter(
                year_a == dummy$y2[x], zone_a == dummy$z2[x],
                year_b == dummy$y2[x], zone_b == dummy$z2[x])
        d12 <- dis.table %>% 
            filter(
                year_a == dummy$y1[x], zone_a == dummy$z1[x],
                year_b == dummy$y2[x], zone_b == dummy$z2[x]
            )
        list(
            type = names(PCOA[dummy$i[x]]),
            d1  = mean(d1$dis),
            dis1 = dis1,
            d2  = mean(d2$dis),
            dis2 = dis2,
            d12 = mean(d12$dis),
            n_d1 = nrow(d1),
            n_d2 = nrow(d2),
            n_d12= nrow(d12),
            n_tot=nrow(dis.table),
            r2  = permanova$R2[1], 
            p   = permanova$`Pr(>F)`[1]
        ) %>% 
            as_tibble()
    }) %>% 
    map_dfr(rbind) %>% 
    mutate(test3 = n_tot == n_d1 + n_d2 + n_d12, 
           test2 = d2 == dis2, 
           test1 = d1 == dis1)
if(permanova %>% 
   select(starts_with("test")) %>% 
   as_vector() %>% 
   unique() == TRUE){
    print("SUCCESS")
} else {
    print("FAIL")
}

permanova <- permanova %>% 
    separate(type, sep = "_", into = c("taxa", "type")) %>% 
    select(
        -starts_with("test"), 
        -starts_with("n_"),
        -starts_with("dis")) %>% 
    mutate_at(c("d1", "d2", "d12", "r2"), function(a){round(a, 2)}) %>% 
    cbind(dummy, .)
writexl::write_xlsx(permanova, paste0("permanova_", Sys.Date(), "_", type, ".xlsx"))


# Table 2 -----------------------------------------------------------------
for.table %>% 
    transmute(
        g = paste(year, zone), 
        vr = str_replace_all(vr, "a\\.", "Arachnida_"), 
        vr = str_replace_all(vr, "c\\.", "Carabidae_"), 
        v = paste0(round(vl), "+-", round(sdvl))) %>% 
    pivot_wider(names_from = g, values_from = v) %>% 
    separate(vr, c("taxa", "trait_type", "trait_value"), sep = "_+") %>% 
    mutate(trait_type = factor(trait_type, levels = c("size", "humidity", 
        "storey", "habitats", "dispersl", "lifeform", "foraging"))) %>% 
    arrange(taxa, trait_type) 

aralong %>% 
    select(sp) %>% 
    distinct() %>% 
    pull(sp) %>% 
    map_dfr(~rgbif::name_backbone(.x, phylum = "Arthropoda")) %>% 
    select(sp = canonicalName, family) %>% 
    left_join(aralong, ., by = "sp") %>% 
    mutate(family = case_when(str_detect(sp, "sp-") ~ "Linyphiidae", TRUE ~ family)) %>% 
    filter(family %in% c("Linyphiidae", "Lycosidae")) %>% 
    select(zone, year, line, num, family) %>% 
    pivot_wider(names_from = family, values_from = num, 
                values_fill = 0, values_fn = sum) %>% 
    transmute(g = paste(year, zone), Linyphiidae, Lycosidae) %>% 
    group_by(g) %>% 
    summarise_all(function(x){paste0(round(mean(x)), "±", round(sd(x)))}) %>% 
    column_to_rownames("g") %>% 
    t %>% 
    as.data.frame() %>% 
    rownames_to_column("taxa") %>% 
    writexl::write_xlsx(paste0("add.table2_", Sys.Date(), "_", type, ".xlsx"))

# Table 4 -----------------------------------------------------------------
list(Carabidae_dom.comp = carlong %>% 
    mutate(sp = case_when(sp %in% c("Trechus secalis", "Pterostichus oblongopunctatus",
        "Pterostichus urengaicus", "Amara brunnea", "Pterostichus niger", 
        "Cychrus caraboides", "Calathus micropterus") ~ paste0("dom.comp_", sp), 
        TRUE ~ paste0("оthers_", sp))) %>% 
    group_by(zone, year, sp) %>% 
    summarise(ssd = sd(num), num = mean(num), .groups = "drop_last") %>% 
    mutate(p = num/sum(num)*100) %>% 
    ungroup() %>% 
    arrange(desc(num)) %>% 
    mutate(
        v = paste0(round(num), "±", round(ssd), " (", round(p, 1), ")"),
        g = paste0(year, "_", zone),
        .keep = "unused") %>% 
    arrange(g, sp) %>% 
    pivot_wider(names_from = g, values_from = v),

    Arachnida.dom.comp = aralong %>% 
    mutate(sp = case_when(sp %in% c("Allomengea scopigera", "Lacinius ephippiatus",
        "Alopecosa taeniata", "Ceratinella brevis", "Asthenargus paganus", 
        "Nemastoma lugubre", "Pardosa lugubris", "Tapinocyba insecta", 
        "Tenuiphantes mengei", "Agyneta affinis", "Alopecosa pulverulenta",
        "Diplocentria bidentata") ~ paste0("dom.comp_", sp), TRUE ~ paste0("оthers_", sp))) %>% 
    group_by(zone, year, sp) %>% 
    summarise(ssd = sd(num), num = mean(num), .groups = "drop_last") %>% 
    mutate(p = num/sum(num)*100) %>% 
    ungroup() %>% 
    arrange(desc(num)) %>% 
    mutate(
        v = paste0(round(num), "±", round(ssd), " (", round(p, 1), ")"),
        g = paste0(year, "_", zone),
        .keep = "unused") %>% 
    arrange(g, sp) %>% 
    pivot_wider(names_from = g, values_from = v),
    
    rarefic_extrap = lst(rar.car, rar.ara) %>%
    map(~.x  %>% 
        group_by(year, zone) %>% 
        rename_with(~ str_replace(., '^(a|c)\\.', ''), ends_with('.d100') | ends_with('.obs')) %>% 
        summarise(
            ssd.d100 = sd(nsp.d100), mn.d100 = mean(nsp.d100), 
            ssd.obs = sd(nsp.obs), mn.obs = mean(nsp.obs), 
            .groups = "drop") %>% 
        transmute(
            g = paste0(year, "_", zone), 
            d100 = paste0(round(mn.d100, 1), "±", round(ssd.d100, 1)), 
            obs = paste0(round(mn.obs, 1), "±", round(ssd.obs, 1))) %>% 
        pivot_longer(names_to = "n_species", values_to = "v", -g) %>% 
        pivot_wider(names_from = g, values_from = v)
    ) %>% 
    `names<-`(c("Carabidae", "Arachnida")) %>% 
    map_dfr(rbind, .id = "taxa") 
) %>% 
    writexl::write_xlsx(paste0("add.table4_", Sys.Date(), "_", type, ".xlsx"))
