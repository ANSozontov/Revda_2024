# 0. License --------------------------------------------------------------
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
#     Belskaya E.A., Zolotarev M.P., Sozontov A.N., Vorobeichik E.L. 
# _________________________________________________________________________

# 1. Data load ------------------------------------------------------------
library(foreach)
library(tidyverse)
carlong <- readxl::read_excel("Data/clean_data9.xlsx", sheet = "Carabidae.Lines2") %>% 
  pivot_longer(names_to = "taxa", values_to = "num", -c("year", "zone", "line")) %>% 
  group_by(year, zone, line) %>% 
  mutate(part = num/sum(num)) %>% 
  ungroup()
cartraits <- readxl::read_excel("Data/clean_data9.xlsx", sheet = "Carabidae.Traits") %>% 
  select(taxa, fenol:size____) %>% 
  right_join(., carlong, by = "taxa") %>% 
  mutate(type = paste0(zone, "_", year))
aralong <- readxl::read_excel("Data/clean_data9.xlsx", sheet = "Arachnida.Lines2") %>% 
  pivot_longer(names_to = "taxa", values_to = "num", -c("year", "zone", "line")) %>% 
  group_by(year, zone, line) %>% 
  mutate(part = num/sum(num)) %>% 
  ungroup()
aratraits <- readxl::read_excel("Data/clean_data9.xlsx", sheet = "Arachnida.Traits") %>% 
  select(taxa,  size____:storey__) %>% 
  right_join(., aralong, by = "taxa") %>% 
  mutate(type = paste0(zone, "_", year))
veg <- readxl::read_excel("Data/clean_data9.xlsx", sheet = "vegetation_cover") %>% 
  select(-trap) %>% 
  group_by(year, zone, line) %>% 
  summarise_all(mean) %>% 
  ungroup

# 2. Rarefication ---------------------------------------------------------
rar.car <- readxl::read_excel("Data/clean_data9.xlsx", sheet = "Carabidae.Lines2") %>% 
  unite(type, year, zone, line, sep = "_") %>% 
  column_to_rownames("type") %>% 
  t() %>% 
  as_tibble() %>% 
  lapply(., function(a){sort(a[a>0], decreasing = TRUE)}) %>% 
  iNEXT::iNEXT(., q = 0, size = c(100, 1000), 
                    datatype = "abundance", nboot = 99) %>%  # 999
  .$iNextEst %>% 
  map2(., as.list(names(.)), 
       .f = function(a, b){
         select(cbind(a, type = b), m, method, qD, type)
       }
  ) %>% 
  map_dfr(as_tibble) %>% 
  filter(m == 100 | method == "observed") %>% 
  transmute(year = as.numeric(substr(type, 1, 4)), 
            zone = substr(type, 6, 8), 
            line = as.numeric(substr(type, 10, 10)), 
            valu = case_when(m == 100 ~ "c.nsp.d100", 
                             TRUE ~ "c.nsp.obs"), 
            qD) %>% 
  pivot_wider(names_from = valu, values_from = qD)
rar.car[rar.car$year == 2005 & 
        rar.car$zone == "fon" & 
        rar.car$line == "7",]$c.nsp.obs <- nrow(filter(carlong, 
                                                      year == 2005, 
                                                      zone == "fon", 
                                                      line == "7", 
                                                      num>0))

rar.ara <- readxl::read_excel("Data/clean_data9.xlsx", 
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
    lapply(., function(a){sort(a[a>0], decreasing = TRUE)}) %>% 
    iNEXT::iNEXT(., q = 0, size = c(100, 1000), 
               datatype = "abundance", nboot = 99) %>%  # 999
    .$iNextEst %>% 
    map2(., as.list(names(.)), 
       .f = function(a, b){
         select(cbind(a, type = b), m, method, qD, type)
       }
    ) %>% 
    map_dfr(as_tibble) %>% 
    filter(m == 100 | method == "observed") %>% 
    transmute(year = as.numeric(substr(type, 1, 4)), 
            zone = substr(type, 6, 8), 
            line = as.numeric(substr(type, 10, 10)), 
            valu = case_when(m == 100 ~ "a.nsp.d100", 
                             TRUE ~ "a.nsp.obs"), 
            qD) %>% 
   pivot_wider(names_from = valu, values_from = qD)

# 3. Aggregation: counts -----------------------------------------------------

agg <- function(df1, gr, tax) {
  # df1 - aratraits/cartraits
  # gr - grouping variable
  # tax - add in the beg. of new variables
  gr <- c(gr, "year", "zone", "line")
  df1 %>% 
    group_by_({gr[1]}, {gr[2]}, {gr[3]}, {gr[4]}) %>% 
    summarise(num = sum(num), .groups = "drop") %>% #num/part
    rename(tp = 1) %>% 
    mutate(tp = paste0(tax, gr[1], "_", tp)) %>% 
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

# 4. Aggregation: pics ------------------------------------------------------

par(mfrow = c(2,2))
# Araneae: imp
for.table %>% 
  mutate(tx = substr(vr, 1, 1), 
         cl = substr(vr, 3, 10), 
         gr = substr(vr, 12, nchar(vr))) %>%
  group_by(cl, year, zone, tx) %>% 
  mutate(val = vl/sum(vl)*100, year = as.character(year)) %>% 
  ungroup() %>% 
  select(-sdvl, -vl) %>%
  filter(gr != "nodata", tx == "a", zone == "imp") %>% # , tx == "a", 
  select(-cl, -tx, -vr) %>% 
  pivot_wider(names_from = gr, values_from = val) %>% 
  select(year, hunter, small, medium, big, for.meadow,
         forest,strato, gerpeto, web.hnt, scavenger) %>% 
  column_to_rownames("year") %>% 
  rbind(rep((floor(max(.)/10)+1)*10, ncol(.)), 
        rep(0, ncol(.)),
        .) %>% 
  fmsb::radarchart(title = "Arachnida, imp zone", 
             axistype = 1, 
             caxislabels=seq(0, max(.), by = 20),
             axislabcol = "darkgrey", #make darker
             cglcol = "darkgrey", 
             pcol = c(rgb(1, 0.5, 0.15, 1), rgb(0, 0.8, 0.4, alpha = 1)),
             pfcol = c(rgb(1, 0.5, 0.15, 0.4), rgb(0, 0.8, 0.4, alpha = 0.4)), 
             plty = c(1,1)
             ); legend(
               x = 0.9, y=0.9,  pch=19, bty	= "n",
               legend = c(2005, 2018), 
               col = c(rgb(1, 0.5, 0.15), rgb(0, 0.8, 0.4))
             )
# Araneae: fon
for.table %>% 
  mutate(tx = substr(vr, 1, 1), 
         cl = substr(vr, 3, 10), 
         gr = substr(vr, 12, nchar(vr))) %>%
  group_by(cl, year, zone, tx) %>% 
  mutate(val = vl/sum(vl)*100, year = as.character(year)) %>% 
  ungroup() %>% 
  select(-sdvl, -vl) %>%
  filter(gr != "nodata", tx == "a", zone == "fon") %>% # , tx == "a", 
  select(-cl, -tx, -vr) %>% 
  pivot_wider(names_from = gr, values_from = val) %>% 
  select(year, hunter, small, medium, big, for.meadow,
         forest,strato, gerpeto, web.hnt, scavenger) %>% 
  column_to_rownames("year") %>% 
  rbind(rep((floor(max(.)/10)+1)*10, ncol(.)), 
        rep(0, ncol(.)),
        .) %>% 
  fmsb::radarchart(title = "Arachnida, fon zone", 
             axistype = 1, 
             caxislabels=seq(0, max(.), by = 20),
             axislabcol = "darkgrey", #make darker
             cglcol = "darkgrey", 
             pcol = c(rgb(1, 0.5, 0.15, 1), rgb(0, 0.8, 0.4, alpha = 1)),
             pfcol = c(rgb(1, 0.5, 0.15, 0.4), rgb(0, 0.8, 0.4, alpha = 0.4)), 
             plty = c(1,1)
  ); legend(
    x = 0.9, y=0.9,  pch=19,  bty	= "n",
    legend = c(2005, 2018), #fill = "white", border = "black",
    col = c(rgb(1, 0.5, 0.15), rgb(0, 0.8, 0.4))
  ) 
# Carabidae: imp
for.table %>% 
  mutate(tx = substr(vr, 1, 1), 
         cl = substr(vr, 3, 10), 
         gr = substr(vr, 12, nchar(vr))) %>%
  group_by(cl, year, zone, tx) %>% 
  mutate(val = vl/sum(vl)*100, year = as.character(year)) %>% 
  ungroup() %>% 
  select(-sdvl, -vl) %>%
  filter(gr != "nodata", tx == "c", zone == "imp") %>% 
  select(-cl, -tx, -vr) %>% 
  pivot_wider(names_from = gr, values_from = val) %>% 
  select(year, big, zoo.phg, mix.phg, mesoph, hygroph, not.mobile, 
         low.mobile, high.mobile, forest, for.meadow, 
         strato, epigeo, medium, small) %>% 
  column_to_rownames("year") %>% 
  rbind(rep((floor(max(.)/10))*10, ncol(.)), 
        rep(0, ncol(.)),
        .) %>% 
  fmsb::radarchart(title = "Carabidae, imp zone", 
             axistype = 1, 
             caxislabels=seq(0, max(.), by = 25),
             axislabcol = "darkgrey", #make darker
             cglcol = "darkgrey", 
             pcol = c(rgb(1, 0.5, 0.15, 1), rgb(0, 0.8, 0.4, alpha = 1)),
             pfcol = c(rgb(1, 0.5, 0.15, 0.4), rgb(0, 0.8, 0.4, alpha = 0.4)), 
             plty = c(1,1)
  ); legend(
    x = 0.8, y=1.3,  pch=19 , bty	= "n",
    legend = c(2005, 2018), #fill = "white", border = "black",
    col = c(rgb(1, 0.5, 0.15), rgb(0, 0.8, 0.4))
  )

# Carabidae: imp
for.table %>% 
  mutate(tx = substr(vr, 1, 1), 
         cl = substr(vr, 3, 10), 
         gr = substr(vr, 12, nchar(vr))) %>%
  group_by(cl, year, zone, tx) %>% 
  mutate(val = vl/sum(vl)*100, year = as.character(year)) %>% 
  ungroup() %>% 
  select(-sdvl, -vl) %>%
  filter(gr != "nodata", tx == "c", zone == "fon") %>% 
  select(-cl, -tx, -vr) %>% 
  pivot_wider(names_from = gr, values_from = val) %>% 
  select(year, big, zoo.phg, mix.phg, mesoph, hygroph, not.mobile, 
         low.mobile, high.mobile, forest, for.meadow, 
         strato, epigeo, medium, small) %>% 
  column_to_rownames("year") %>% 
  rbind(rep((floor(max(.)/10))*10, ncol(.)), 
        rep(0, ncol(.)),
        .) %>% 
  fmsb::radarchart(title = "Carabidae, fon zone", 
             axistype = 1, 
             caxislabels=seq(0, max(.), by = 25),
             axislabcol = "darkgrey",
             cglcol = "darkgrey", 
             pcol = c(rgb(1, 0.5, 0.15, 1), rgb(0, 0.8, 0.4, alpha = 1)),
             pfcol = c(rgb(1, 0.5, 0.15, 0.4), rgb(0, 0.8, 0.4, alpha = 0.4)), 
             plty = c(1,1)
  ); legend(
    x = 0.8, y=1.3,  pch=19 , bty	= "n",
    legend = c(2005, 2018), 
    col = c(rgb(1, 0.5, 0.15), rgb(0, 0.8, 0.4))
  )
par(mfrow = c(1,1)) # save pdf 9*14

# 5. Effect.Size counts ---------------------------------------------------
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
effs <- cbind(effs, 
  foreach(i = 1:nrow(effs), .combine = rbind) %do% {
  SingleCaseES::LRRi( # effs[i, 3:7] <-  
    res2[res2$zone == "fon" & res2$year == effs$year[i], effs$vr[i]],
    res2[res2$zone == "imp" & res2$year == effs$year[i], effs$vr[i]])
  }
)

list( # export exact data
  simple.part = res %>% 
    select(-line) %>% 
    group_by(year, zone) %>% 
    summarise_all(function(a){
      round(mean(a), 2)
    }) %>% 
    ungroup(),
  conf.int.part = res %>% 
    select(-line) %>% 
    group_by(year, zone) %>% 
    summarise_all(function(a){
      paste0(round(mean(a), 2), "+-", round(1.96*sd(a), 2))
    }) %>% 
    ungroup(),
  simple.num = for.table %>%
    transmute(year, zone, vr, vl = round(vl, 1)) %>%
    pivot_wider(names_from = vr, values_from = vl),
  conf.int.num = for.table %>%
    transmute(year, zone, vr,
      vl = paste0(round(vl, 1), "+-", round(1.96*sdvl, 1))) %>%
    pivot_wider(names_from = vr, values_from = vl)
) %>%
  writexl::write_xlsx(paste0("exact_tables_", Sys.Date(), ".xlsx"))

# 6. Effect.Size Viz ------------------------------------------------------
theme_set(theme_classic())

ara.eff <- effs %>% 
  as_tibble() %>% 
  filter(substr(vr, 1,1) != "c") %>% 
  filter(!(vr %in% c("a.humidity_nodata", "a.humidity_xeroph", 
                   "a.habitats_meadow", "a.habitats_nodata", 
                   "a.habitats_water", "a.lifeform_ambusher", 
                   "a.storey___horto.up", "a.storey___nodata"))) %>%
  mutate(year = as.character(year),
         nn = sort(c(seq(1, nrow(.)/2*3, by = 3), seq(2, nrow(.)/2*3, by = 3))) 
         ) 
ara.eff <- ara.eff %>% 
  rbind(cbind(matrix(ncol = ncol(ara.eff)-1, 
                     nrow = nrow(ara.eff)/2, 
                     dimnames = list(NULL, colnames(ara.eff)[1:ncol(ara.eff)-1])), 
              nn = seq(3, nrow(ara.eff)/2*3, by = 3))) %>% 
  distinct() %>% 
  arrange(nn) %>% 
  mutate(vr = case_when(year == 2005 ~ vr, TRUE ~ " ."))
# ara.eff[ara.eff == "NA"] <- NA
ara.eff %>% 
  ggplot(aes(color = year, x = nn, 
             y = Est, ymin = CI_lower, ymax = CI_upper)) + 
    geom_errorbar(size = 0.5) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = seq(3, nrow(ara.eff), by = 3), linetype = "dashed") +
    geom_vline(xintercept = c(39, 45), color = "red", size = 1) + ###
    coord_flip() + 
    theme(legend.position = "bottom") +
    scale_x_discrete(limits = ara.eff$vr) +
    labs(x = "", y = "Effect Size", title = "Arachnida (abundance, /100 trap-days)")
    

car.eff <- effs %>% 
  as_tibble() %>% 
  filter(substr(vr, 1,1) == "c") %>% 
  filter(!(vr %in% c("c.habitats_meadow", "c.habitats_water", 
                     "c.humidity_xeroph", "c.storey___gerp.hort"))) %>%
  mutate(year = as.character(year),
         nn = sort(c(seq(1, nrow(.)/2*3, by = 3), seq(2, nrow(.)/2*3, by = 3))) 
  ) 
car.eff <- car.eff %>% 
  rbind(cbind(matrix(ncol = ncol(car.eff)-1, 
                     nrow = nrow(car.eff)/2, 
                     dimnames = list(NULL, colnames(car.eff)[1:ncol(car.eff)-1])), 
              nn = seq(3, nrow(car.eff)/2*3, by = 3))) %>% 
  distinct() %>% 
  arrange(nn) %>% 
  mutate(vr = case_when(year == 2005 ~ vr, TRUE ~ " ."))
car.eff %>% 
  ggplot(aes(color = year, x = nn, 
             y = Est, ymin = CI_lower, ymax = CI_upper)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = seq(3, nrow(car.eff), by = 3), linetype = "dashed") +
  geom_errorbar(size = 0.5) +
  geom_point(size = 1.5) +
  coord_flip() + 
  theme(legend.position = "bottom") +
  labs(x = "", y = "Effect Size", title = "Carabidae (abundance, /100 trap-days)") +
  scale_x_discrete(limits = car.eff$vr) 

# 7. Ordination -----------------------------------------------------------
pcoaplot <- function(data, tp, name) {
# tp = type where "num" ansd "part" are possible
data <- data %>% 
  select(year, zone, line, taxa, val = tp) %>% 
  pivot_wider(names_from = taxa, values_from = val)
pcoa <- ape::pcoa(vegan::vegdist(data[,4:ncol(data)], 
                    method = "bray", binary = FALSE))
eig <- pcoa$values$Eigenvalues 
eig <- round(eig/sum(eig)*100)
pcoa <- cbind(data[,1:3], pcoa$vectors[,1:2]) %>% 
  mutate(year = as.character(year))
p <- ggplot(pcoa, aes(x = Axis.1, y = Axis.2, shape = year, fill = zone)) + 
  geom_point(size = 2) + 
  stat_ellipse() + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  scale_shape_manual(values = c(21, 24)) + 
  scale_fill_manual(values = c("white", "darkgrey")) +
  labs(title = name, subtitle = tp,
       x = paste0("Ось 1 (", eig[1], "%)"), 
       y = paste0("Ось 2 (", eig[2], "%)")  ) + 
  theme(legend.position = "bottom")
return(p)
}

gridExtra::grid.arrange(
  pcoaplot(aralong, "part", "Arachnida") + scale_y_reverse(),
  pcoaplot(aralong, "num", "Arachnida") + scale_y_reverse(),
  pcoaplot(carlong, "part", "Carabidae"),
  pcoaplot(carlong, "num", "Carabidae"),
  ncol = 2)


# 8. Final remarks --------------------------------------------------------

aralong %>% #how many species
  mutate(taxa = substr(taxa, 1, nchar(taxa)-2)) %>% 
  pull(taxa) %>% 
  unique() %>% 
  length 
aralong %>%  #how many species across zones
  mutate(taxa = substr(taxa, 1, nchar(taxa)-2)) %>% 
  select(-part, -line, -year) %>% 
  pivot_wider(names_from = taxa, values_from = num, 
              values_fill = 0, values_fn = sum) %>% 
  column_to_rownames("zone") %>% 
  apply(1, function(a){length(a[a>0])})
  
carlong %>% #how many species
  mutate(taxa = substr(taxa, 1, nchar(taxa)-2)) %>% 
  pull(taxa) %>% 
  unique() %>% 
  length 
carlong %>%  #how many species across zones
  mutate(taxa = substr(taxa, 1, nchar(taxa)-2)) %>% 
  select(-part, -line, -year) %>% 
  pivot_wider(names_from = taxa, values_from = num, 
              values_fill = 0, values_fn = sum) %>% 
  column_to_rownames("zone") %>% 
  apply(1, function(a){length(a[a>0])})
