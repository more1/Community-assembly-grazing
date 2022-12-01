
# Import packages ---------------------------------------------------------


library(ranger)
library(tidyverse)
library(nlme)
library(ggpubr)


# Import data -------------------------------------------------------------


plant <- read.csv("../Data/plants/plant_8thNov2022.csv")
meta <- read.csv("../Data/metadata/metadata.csv")
dfplantrf <- plant %>% select(c(exclosure, spotted_knapweed:daisy_sp))
# 
# nw_df <- dfplantrf %>% 
#   mutate(across(c(spotted_knapweed:vicia_sp)))
dfplantrf$exclosure <- as.factor(dfplantrf$exclosure)


# Random forest model -----------------------------------------------------


#donot run
# df = data.frame()
# for (i in seq(5, 89, by = 1)){
#   for(j in seq(5, 500, by = 2)){
#   set.seed(1234)
#   rf_plant <-  ranger(exclosure ~ ., data = dfplantrf,mtry = i, num.tree =j, classification = T, importance = "impurity_corrected")
#   mtry_no  <-  i
#   tree_try <-  j
#   error <- rf_plant$prediction.error
#   df <- rbind(df, cbind( mtry_no, tree_try, error))
#   print(dim(df))
#  }
# }

#least error rate with mtry 19, num.tree = 91
set.seed(1234)
rg.plant <- ranger(exclosure ~ ., data = dfplantrf, mtry = 19, num.tree = 91, classification = T, importance = "impurity_corrected")
dfplantvi <- rg.plant$variable.importance %>% as.data.frame() %>% rename(importance = ".") 
print(rg.plant$prediction.error)

# df_imp <- dfplantvi %>%
#   rownames_to_column(., var = "plants") %>%
#   arrange(desc(importance)) %>% 
#   head(n =20)



# Get important plants ----------------------------------------------------


set.seed(2134)
set.seed(54654654)
p_val <- importance_pvalues(
  rg.plant,
  method = "altmann", formula = exclosure ~ .,data = dfplantrf,
  num.permutations = 9999) %>% as.data.frame() %>% filter(pvalue < 0.05) %>%
  rownames_to_column(var = "plants")

rf_plant <- plant %>% select(c(all_of(p_val$plants), site_id, exclosure, quadrat, age)) %>% 
  full_join(., meta, by = c("site_id", "exclosure", "quadrat", "age")) 
# %>% 
#   select(c(all_of(df_imp$plants), avg_plant))

rf_plant %>% 
  select(c(all_of(p_val$plants), age,exclosure)) %>% 
  pivot_longer(-c(exclosure, age)) %>% 
  ggplot(., aes(x = age, y = value, color = exclosure)) +
  geom_smooth(method = "lm") +
  # geom_jitter()+
  facet_wrap(.~ name, scales = "free")
         

# Native vs Exotic --------------------------------------------------------


fun_grps <- readxl::read_xlsx("../Data/plants/func_groups.xlsx") 

##Data wrangling
native <- fun_grps %>% 
  filter(type == "Native",
         plant %in% p_val$plants)

native_abund <- rf_plant %>% 
  select(site_id:litter, native$plant) %>% 
  rowwise() %>% 
  mutate(sumVar = sum(c_across(milkvetch_sp:big_sage)))

exotic <- fun_grps %>% 
  filter(type == "Exotic",
         plant %in% p_val$plants)



exotic_abund <- rf_plant %>% 
  select(site_id:litter,exotic$plant) %>% 
  rowwise() %>% 
  mutate(sumVar = sum(c_across(spotted_knapweed:birdsfoot_trefoil)),
         exo = if_else(sumVar == 0, 0, 1))


# Set theme ---------------------------------------------------------------


theme_box <-  
  theme_classic() +
  theme(axis.title = element_text(size=20,  color="black", face = "bold"),
        axis.text.x = element_text(size=12,  color="black"),
        axis.line = element_line(size = 0.9),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.subtitle=element_text(size=18, hjust=0.5, face="bold", color="black"),
        axis.text.y = element_text(size=12,  color="black"))



# Models for native exotic ------------------------------------------------


mod_native <- lme(sumVar ~ exclosure * age   +
                      exclosure * scale(elevation)+
                      exclosure * scale(precip),
                    random = ~ 1|site_id,  data = native_abund)

anova.lme(mod_native)
hist(resid(mod_native))

stat_native <-  anova.lme(mod_native) %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))

mod_exotic <- lme( log(sumVar + 1, base = 1.1) ~ exclosure * age   +
                    exclosure * scale(elevation)+
                    exclosure * scale(precip),
                  random = ~ 1|site_id, data = exotic_abund)
hist(resid(mod_exotic))
anova.lme(mod_exotic)
lattice::qqmath(resid(mod_exotic))
qqnorm(resid(mod_exotic))

stat_exotic <-  anova.lme(mod_exotic) %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))




# Plotting Native and exotic ----------------------------------------------

plot_native <- ggplot(native_abund, aes( x = age, y = sumVar, color = exclosure)) +
  geom_smooth(method = "lm") +
  ggsci::scale_color_aaas(name = "Exclosure",
                          labels = c("Inside", "Outside")) +
  annotate(geom = "text", x = 20, y = 15,
           label = glue::glue("Exclosure:Age
                              F-value = {round(stat_native$`F-value`[6],2)}; p-value {stat_native$p[6]}"),
           size = 4, hjust = 0)+
  theme_box  +
  theme(legend.position = c(0.1,0.85)) +
  labs(subtitle = "Native Plants",
       x = "\n",
       y = NULL)
plot_native

#ggsave(plot_native, filename = "../Plot/plot_native.jpeg", height = 6, width = 8, dpi = 1000)


plot_exotic <- ggplot(exotic_abund, aes( x = age, y = sumVar, color = exclosure)) +
  geom_smooth(method = "lm") +
  ggsci::scale_color_aaas(name = "Exclosure",
                          labels = c("Inside", "Outside")) +
  annotate(geom = "text", x = 20, y = -1,
           label = glue::glue("Exclosure:Age
                              F-value = {round(stat_exotic$`F-value`[6],2)}; p-value {stat_exotic$p[6]}"),
           size = 4, hjust = 0)+
  theme_box  +
  theme(legend.position = c(0.1,0.85)) +
  labs(subtitle = "Exotic Plants",
    x = "\n",
    y = NULL)
plot_exotic

#ggsave(plot_exotic, filename = "../Plot/plot_exotic.jpeg", height = 6, width = 8, dpi = 1000)






# Grasses -----------------------------------------------------------------



grass <- fun_grps %>% 
  filter(func == "grass",
         plant %in% p_val$plants)

grass_abund <- rf_plant %>% 
  select(site_id:litter, grass$plant) %>% 
  rowwise() %>% 
  mutate(sumVar = sum(c_across(corn_brome:needlegrass_sp)))

mod_grass <- lme(sumVar ~ exclosure * age   +
                    exclosure * scale(elevation)+
                    exclosure * scale(precip),
                  random = ~ 1|site_id,  data = grass_abund)
hist(resid(mod_grass))
anova.lme(mod_grass)
qqnorm(resid(mod_grass))

stat_grass <-  anova.lme(mod_grass) %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))

plot_grass <- ggplot(grass_abund, aes( x = age, y = sumVar, color = exclosure)) +
  geom_smooth(method = "lm") +
  ggsci::scale_color_aaas(name = "Exclosure",
                          labels = c("Inside", "Outside")) +
  annotate(geom = "text", x = 20, y = 45,
           label = glue::glue("Exclosure:Age
                              F-value = {round(stat_grass$`F-value`[6],2)}; p-value {stat_grass$p[6]}"),
           size = 4, hjust = 0)+
  theme_box  +
  theme(legend.position = c(0.1,0.85)) +
  labs(subtitle = "Grass",
    x = "Age of Exclosure",
       y = NULL)
plot_grass




# Forbs -------------------------------------------------------------------

forbs <- fun_grps %>% 
  filter(func == "forb",
         plant %in% p_val$plants)

forbs_abund <- rf_plant %>% 
  select(site_id:litter, forbs$plant) %>% 
  rowwise() %>% 
  mutate(sumVar = sum(c_across(spotted_knapweed:pussytoe_sp)))

mod_forbs <- lme(log(sumVar + 1, base = 1.1) ~ exclosure * age   +
                   exclosure * scale(elevation)+
                   exclosure * scale(precip),
                 random = ~ 1|site_id,  data = forbs_abund)
hist(resid(mod_forbs))
anova.lme(mod_forbs)
qqnorm(resid(mod_forbs))

stat_forbs <-  anova.lme(mod_forbs) %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))

plot_forbs <- ggplot(forbs_abund, aes( x = age, y = sumVar, color = exclosure)) +
  geom_smooth(method = "lm") +
  ggsci::scale_color_aaas(name = "Exclosure",
                          labels = c("Inside", "Outside")) +
  annotate(geom = "text", x = 20, y = -1,
           label = glue::glue("Exclosure:Age
                              F-value = {round(stat_forbs$`F-value`[6],2)}; p-value {stat_forbs$p[6]}"),
           size = 4, hjust = 0)+
  theme_box  +
  theme(legend.position = c(0.8,0.95)) +
  labs(subtitle = "Forbs",
    x = "Age of Exclosure",
       y = NULL)

plot_forbs


# Arranging all plots -----------------------------------------------------



lab <- c("A","", "B", "C","", "D")


fig1 <-  annotate_figure(
  ggarrange(plot_native, NULL, plot_exotic,
            plot_grass, NULL, plot_forbs,
             ncol = 3, nrow = 2, common.legend = T, legend = "bottom",
            widths = c(1, 0.15, 1), labels = lab, label.x = -0.04),
  left = text_grob("Coverage\n", size = 20, rot = 90, face = "bold" ))

fig1

ggsave(fig1, filename = "plant_function.jpeg",
       width = 15, height = 12, units = "in", dpi = 1000, path = "../Plot/")
