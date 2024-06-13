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

# 0.1. Data load ------------------------------------------------------------
# library(foreach)
library(tidyverse)
source("https://raw.github.com/ANSozontov/spider_charts/master/spider_charts.R")
theme_set(
    theme_bw() + 
    theme(
        legend.position = "bottom",
        panel.grid = element_blank())
)
carlong <- readxl::read_excel("clean_data9.xlsx", sheet = "Carabidae.Lines2") %>% 
  pivot_longer(names_to = "taxa", values_to = "num", -c("year", "zone", "line")) %>% 
  group_by(year, zone, line) %>% 
  mutate(part = num/sum(num)) %>% 
  ungroup()
cartraits <- readxl::read_excel("clean_data9.xlsx", sheet = "Carabidae.Traits") %>% 
  select(taxa, fenol:size____) %>% 
  right_join(., carlong, by = "taxa") %>% 
  mutate(type = paste0(zone, "_", year))
aralong <- readxl::read_excel("clean_data9.xlsx", sheet = "Arachnida.Lines2") %>% 
  # select(-n.traps, -season) %>% #
  pivot_longer(names_to = "taxa", values_to = "num", -c("year", "zone", "line")) %>% 
  group_by(year, zone, line) %>% 
  mutate(part = num/sum(num)) %>% 
  ungroup()
aratraits <- readxl::read_excel("clean_data9.xlsx", sheet = "Arachnida.Traits") %>% 
  select(taxa,  size____:storey__) %>% 
  right_join(., aralong, by = "taxa") %>% 
  mutate(type = paste0(zone, "_", year))
veg <- readxl::read_excel("clean_data9.xlsx", sheet = "vegetation_cover") %>% 
  select(-trap) %>% 
  group_by(year, zone, line) %>% 
  summarise_all(mean) %>% 
  ungroup

# 0.2. Rarefication ---------------------------------------------------------
rar.car <- readxl::read_excel("clean_data9.xlsx", sheet = "Carabidae.Lines2") %>% 
  unite(type, year, zone, line, sep = "_") %>% 
  column_to_rownames("type") %>% 
  t() %>% 
  as_tibble() %>% 
  lapply(., function(a){sort(round(a[a>0]), decreasing = TRUE)}) %>% 
  iNEXT::iNEXT(., q = 0, size = c(100, 1000), 
                    datatype = "abundance", nboot = 9) %>%  # 999
  pluck("iNextEst", "size_based") %>% 
  as_tibble %>% 
  filter(m == 100 | Method == "Observed") %>% 
  transmute(year = as.numeric(substr(Assemblage, 1, 4)), 
            zone = substr(Assemblage, 6, 8), 
            line = as.numeric(substr(Assemblage, 10, 10)), 
            valu = case_when(m == 100 ~ "c.nsp.d100", 
                             TRUE ~ "c.nsp.obs"), 
            qD) %>% 
  pivot_wider(names_from = valu, values_from = qD)

rar.ara <- readxl::read_excel("clean_data9.xlsx", 
                              sheet = "Arachnida.Lines2") %>% 
    unite(type, year, zone, line, sep = "_") %>% 
    pivot_longer(names_to = "sp", values_to = "val", -type) %>% #start
    mutate(sp = substr(sp, 1, nchar(sp) - 2)) %>% 
    group_by(type, sp) %>%  #block romeves sex val. differences
    summarise(val = sum(val), .groups = "drop") %>% 
    pivot_wider(names_from = sp, values_from = val) %>% # end
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
    pivot_wider(names_from = tp, values_from = num) %>% #num/part
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
  full_join(agg(aratraits, "storey__", "a."), by = bbyy) %>% #
  full_join(veg, by = bbyy)#
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
dictionary <- matrix(byrow = TRUE, ncol = 2, data = c(
    "a.abundance", "N", 
    "c.abundance", "N",
    "a.nsp.obs", "S",
    "c.nsp.obs", "S",
    "a.nsp.d100", "S'",
    "c.nsp.d100", "S'",
    "a.size_____big", "B", 
    "c.size_____big", "B", 
    "a.size_____medium", "M", 
    "c.size_____medium", "M",
    "a.size_____small", "S", 
    "c.size_____small", "S", 
    "a.humidity_mesoph", "mes", 
    "c.humidity_mesoph", "mes", 
    "a.humidity_hygroph", "hyg", 
    "c.humidity_hygroph", "hyg", 
    "a.storey___strato", "LS", 
    "c.storey___strato", "LS", 
    "a.storey___gerpeto", "SS",
    "c.storey___epigeo", "SS", 
    "a.habitats_forest", "F",
    "c.habitats_forest", "F", 
    "a.habitats_for.meadow", "E", 
    "c.habitats_for.meadow", "E", 
    "a.lifeform_hunter", "H", 
    "c.foraging_zoo.phg", "Z", 
    "a.lifeform_web", "W", 
    "c.foraging_mix.phg", "Mx",
    "a.lifeform_web.hnt", "WH",
    "c.dispersl_high.mobile", "hm", 
    "a.lifeform_scavenger", "SH", 
    "c.dispersl_low.mobile", "mm", 
    "c.dispersl_not.mobile", "lm"
    )) %>% 
    data.frame() %>% 
    `names<-`(c("vr", "lab"))
e <- dictionary %>% 
    left_join(effs, by = "vr") %>% 
    transmute(vr, year = as.character(year), Est, CI_lower, CI_upper, 
        taxa = case_when(substr(vr, 1,1) == "a" ~ "Arachnida", TRUE ~ "Carabidae"),
        lab)
ea <- e %>% 
    filter(taxa == "Arachnida") %>% 
    mutate(nn = sort(c(seq(51, 5, -3), seq(50, 5, -3)), decreasing = TRUE), 
           .before = "taxa") 
eb <- expand.grid(vr = NA, year = NA, Est = NA, 
            CI_lower = NA, CI_upper = NA, 
            nn = seq(49, 1, -3), 
            taxa = c("Arachnida", "Carabidae" ), 
            lab = ".", 
            stringsAsFactors = FALSE)
ec <- e %>% 
    filter(taxa == "Carabidae") %>% 
    mutate(nn = sort(c(seq(51, 1, -3), seq(50, 1, -3)), decreasing = TRUE), 
           .before = "taxa") 
eviz <- rbind(ea, eb, ec) %>% 
    arrange(desc(nn))
    

# Effect.Size Viz
theme_set(theme_classic())
p1 <- ggplot(eviz, aes(x = nn, y = Est, ymin = CI_lower, 
                       ymax = CI_upper, shape = year, fill = year)) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange() + 
    geom_text(data = filter(eviz, taxa == "Arachnida", lab != "."), 
              mapping = aes(x = nn-0.5, y = -7, label = lab)) +
    geom_text(data = filter(eviz, taxa == "Carabidae", lab != "."), 
              mapping = aes(x = nn - 0.5, y = 7, label = lab)) +
    coord_flip()+
    scale_x_continuous(minor_breaks = seq(1, 50, by = 3)) + 
    scale_y_continuous(minor_breaks = NULL) +
    scale_shape_manual(values = c(22, 24))+
    facet_wrap(~taxa) + 
    labs(x = NULL, y = NULL) +
    theme_bw() + 
    theme(legend.position = "bottom", 
          axis.text.y=element_blank(), axis.line.y = element_blank(),
          axis.ticks.y=element_blank(), panel.grid.major = element_blank(), 
          panel.background = element_blank())

ggsave(paste0("Fig.1_color", Sys.Date(), ".pdf"),
       plot = p1,
       dpi = 1200, width = 9, height = 9)
ggsave(paste0("Fig.1_grays", Sys.Date(), ".pdf"),
       plot = p1+scale_fill_manual(values = c("grey", "white")),
       dpi = 1200, width = 9, height = 9)

# 2. Ordination -----------------------------------------------------------
pcoa.run <- function(data, tp) { # , name
    # tp = type where "num" and "part" are possible
    if(sum(stringr::str_detect(data$taxa, "-")) > 0){
        #block romeves sex val. differences
        data <- data %>% 
            mutate(taxa = substr(taxa, 1, nchar(taxa) - 2)) %>% 
            group_by(year, zone, line, taxa) %>%  
            summarise(num = sum(num),
                      part = sum(part),
                      .groups = "drop") 
    }
    data <- data %>% 
        select(year, zone, line, taxa, val = all_of(tp)) %>% 
        pivot_wider(names_from = taxa, values_from = val)
    dis <- vegan::vegdist(
        data[,4:ncol(data)], 
        method = "bray", binary = FALSE)
    pcoa <- ape::pcoa(dis)
    eig <- pcoa$values$Eigenvalues 
    eig <- round(eig/sum(eig)*100)
    pcoa <- cbind(data[,1:3], pcoa$vectors[,1:2]) %>% 
        mutate(year = as.character(year))
    lst(pcoa, eig, dis, data)
}

pcoa.viz <- function(obj, NM = NULL, SBT = NULL){
    ggplot(obj$pcoa, aes(x = Axis.1, y = Axis.2, shape = year, fill = zone)) + 
        geom_point(size = 2) + 
        stat_ellipse() + 
        geom_vline(xintercept = 0, linetype = "dotted") + 
        geom_hline(yintercept = 0, linetype = "dotted") + 
        scale_shape_manual(values = c(21, 24)) + 
        scale_fill_manual(values = c("white", "darkgrey")) +
        labs(title = NM, subtitle = SBT,
            shape = "Год",
            fill = "Зона",
            x = paste0("Ось 1 (", obj$eig[1], "%)"), 
            y = paste0("Ось 2 (", obj$eig[2], "%)"))
}

PCOA <- list(aralong, aralong, carlong, carlong) %>% 
    map2(c("part", "num", "part", "num"), 
         ~pcoa.run(.x, .y)) %>% 
    `names<-`(c("Arachnida_Доли", "Arachnida_Обилия", 
                "Carabidae_Доли", "Carabidae_Обилия"))

gridExtra::grid.arrange(
    pcoa.viz(PCOA[[1]], "Arachnida", "Доли") + scale_y_reverse() + 
        scale_fill_manual(values =  c(rgb(0, 0.8, 0.4), rgb(1, 0.5, 0.15))),
    pcoa.viz(PCOA[[2]], "Arachnida", "Обилия") + scale_y_reverse() + scale_x_reverse() + 
        scale_fill_manual(values =  c(rgb(0, 0.8, 0.4), rgb(1, 0.5, 0.15))),
    pcoa.viz(PCOA[[3]], "Carabidae", "Доли") + 
        scale_fill_manual(values =  c(rgb(0, 0.8, 0.4), rgb(1, 0.5, 0.15))),
    pcoa.viz(PCOA[[4]], "Carabidae", "Обилия") + scale_y_reverse() + 
        scale_fill_manual(values =  c(rgb(0, 0.8, 0.4), rgb(1, 0.5, 0.15))),
    ncol = 2) %>% 
    ggsave(paste0("Fig.2_color", Sys.Date(), ".pdf"), plot = ., height = 7, width = 9)

gridExtra::grid.arrange(
  pcoa.viz(PCOA[[1]], "Arachnida", "Доли") + scale_y_reverse(),
  pcoa.viz(PCOA[[2]], "Arachnida", "Обилия") + scale_y_reverse() + scale_x_reverse(),
  pcoa.viz(PCOA[[3]], "Carabidae", "Доли"),
  pcoa.viz(PCOA[[4]], "Carabidae", "Обилия") + scale_y_reverse(),
  ncol = 2) %>% 
  ggsave(paste0("Fig.2_grays", Sys.Date(), ".pdf"), plot = ., height = 7, width = 9)

map(PCOA, ~vegan::adonis2(.x$dis ~ year, data = .x$data, permutations = 999))
map(PCOA, ~vegan::adonis2(.x$dis ~ zone, data = .x$data, permutations = 999))

# 3. Aggregation: pics ------------------------------------------------------
dummy = tibble(grps = c("big", "epigeo", "for.meadow", "forest", 
                        "gerp.hort", "high.mobile", "hygroph", "low.mobile", "meadow",
                        "medium", "mesoph", "mix.phg", "not.mobile", "small", "strato", 
                        "xeroph", "zoo.phg", "hunter", "scavenger", "web", "web.hnt", 
                        "gerpeto", "horto.up"), 
               grp2 = c("B", "SS", "E", "F", "HSD", "hm", "hyg", "mm", "Oh", 
                        "M", "mes", "Mx", "lm", "S", "LS", "xer", "Z", "H", "SH", 
                        "W", "WH", "SS", "HSD"))
p1234 <- list()
p1234[1:2] <- aratraits %>% 
    mutate(habitats = case_when(habitats == "water" ~ "meadow",
                                TRUE ~ habitats)) %>%
    select(size____:num) %>% 
    pivot_longer(names_to = "categ", values_to = "grps", 
                 -c("year", "zone", "line", "num")) %>% 
    group_by(year, zone, categ, grps) %>% 
    summarise(v = sum(num), .groups = "drop_last") %>% 
    mutate(v = round(v/sum(v)*100, 1)) %>% 
    ungroup() %>% 
    left_join(dummy, by = "grps") %>% 
    filter(grps != "nodata") %>% 
    split(.$year) %>% 
    map(~.x %>% 
            select(-year, -categ, -grps) %>% 
            pivot_wider(names_from = grp2, values_from = v) %>% 
            select(-xer) %>% 
            select(id = zone, LS, B, M, S, SH, W, WH, H,
                   Oh, E, `F`, hyg, mes, SS) %>%
            spiderchart()
    )

p1234[3:4] <- cartraits %>% 
    mutate(habitats = case_when(habitats == "water" ~ "meadow", 
                                TRUE ~ habitats)) %>% 
    select(foraging:num) %>% 
    pivot_longer(names_to = "categ", values_to = "grps", 
                 -c("year", "zone", "line", "num")) %>% 
    group_by(year, zone, categ, grps) %>% 
    summarise(v = sum(num), .groups = "drop_last") %>% 
    mutate(v = round(v/sum(v)*100, 1)) %>% 
    ungroup() %>% 
    left_join(dummy, by = "grps") %>% 
    split(.$year) %>% map(~.x %>% 
                              select(-year, -categ, -grps) %>% 
                              pivot_wider(names_from = grp2, values_from = v) %>% 
                              select(-xer) %>% 
                              select(id = zone, LS, B, M, S, lm, mm, hm, 
                                     Z, Mx, Oh, E, `F`, hyg, mes, SS, 
                              ) %>% 
                              spiderchart()
    ) 

color <- map(p1234, ~.x + 
                 scale_color_manual(values = c(rgb(0, 0.8, 0.4, alpha = 1), rgb(1, 0.5, 0.15, 1))) +
                 scale_fill_manual(values =  c(rgb(0, 0.8, 0.4, alpha = 0.4), rgb(1, 0.5, 0.15, 0.2))) + 
                 scale_linetype_manual(values = c("solid", "solid")) +
                 theme(legend.position = "bottom")
) 
grayscale <- map(p1234, ~.x + 
                     scale_color_manual(values = c("black", "black")) +
                     scale_fill_manual(values =  c("white", "lightgrey")) + 
                     scale_shape_manual(values = c(13, 15)) +
                     scale_linetype_manual(values = c("solid", "solid")) +
                     theme(legend.position = "bottom")
)

gridExtra::grid.arrange(color[[1]], color[[3]], 
                        color[[2]], color[[4]], 
                        ncol = 2, nrow = 2) %>% 
    ggsave(paste0("Fig.3_color_", Sys.Date(), ".pdf"), 
           plot = ., 
           height = 7, 
           width = 7)

gridExtra::grid.arrange(grayscale[[1]], grayscale[[3]], 
                        grayscale[[2]], grayscale[[4]], 
                        ncol = 2, nrow = 2) %>% 
    ggsave(paste0("Fig.3_grays_", Sys.Date(), ".pdf"), 
           plot = ., 
           height = 7, 
           width = 7)
