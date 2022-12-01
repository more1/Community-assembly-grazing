

# Import packages ---------------------------------------------------------


library(tidyverse)
library(vegan)
library(janitor)
library(ggpubr)
library(patchwork)
library(nlme)



# Import RC data ----------------------------------------------------------

all_RC <- read.csv("../Data/metadata/metadata.csv")



# Stats for RC calculations -----------------------------------------------


# RC stats for plants -----------------------------------------------------

plant_mod_RC <- lme(avg_plant ~ exclosure * age   +
                      exclosure * scale(elevation)+
                      exclosure * scale(precip),
                    random = ~ 1|site,  data = all_RC)
anova.lme(plant_mod_RC)

plant_RC_annot <- anova.lme(plant_mod_RC) %>% 
  as.data.frame() %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))


# RC stats for fungi ------------------------------------------------------

fungi_mod_RC <- lme(fungi_avg ~ exclosure * age   +
                      exclosure * scale(elevation)+
                      exclosure * scale(precip),
                    random = ~ 1|site,  data = all_RC,
                    na.action = na.omit)

fungi_RC_annot <- anova.lme(fungi_mod_RC) %>% 
  as.data.frame() %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))



# RC stats for bacteria ---------------------------------------------------

bac_mod_RC <- lme(bac_avg ~ exclosure * age   +
                    exclosure * scale(elevation)+
                    exclosure * scale(precip),
                  random = ~ 1|site,  data = all_RC,
                  na.action = na.omit)

bac_RC_annot <- anova.lme(bac_mod_RC) %>% 
  as.data.frame() %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))


# Setting theme -----------------------------------------------------------

theme_blank <- 
  theme_classic() +
  theme(axis.title.y =  element_text(size = 16, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(size = 0.9),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.title=element_text(size=14, hjust = 0.5, face="bold", color="black"),
        axis.text.y = element_text(size=11,  color="black")
  )  

theme_blank1 <- theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=20,  color="black", face = "bold"),
        axis.text.x = element_text(size=12,  color="black"),
        axis.line = element_line(size = 0.9),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.subtitle=element_text(size=18, hjust=0.5, face="bold", color="black"),
        axis.text.y = element_text(size=12,  color="black"))



mycomparisons <- list(c("in", "out"))



#Boxplot RC agains exclosure ---------------------------------------------
  
row1_1 <- ggplot(all_RC, aes(x = exclosure, y = avg_plant, fill = exclosure)) +
  geom_violin(alpha = 0.2, show.legend = F)+
  geom_boxplot(width = 0.25, show.legend = T,alpha = 0.6, outlier.alpha = 0) +
#  geom_jitter(pch = 21, size = 1, width = 0.1, alpha = 0.2, show.legend = F)+
  ggpubr::stat_compare_means(label = "p.signif",
                             comparisons = mycomparisons) +
  ggsci::scale_fill_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(title = "PLANTS",
       x = NULL,
       y = "Raup-Crick Index")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01),
    breaks = seq(-0.5, 1.5, by = 0.5)) 
row1_1 <- row1_1 +  theme_blank

row1_2 <- ggplot(all_RC, aes(x = exclosure, y =fungi_avg, fill = exclosure)) +
  geom_violin(alpha = 0.2, show.legend = F)+
  geom_boxplot(width = 0.25, show.legend = T,alpha = 0.6, outlier.alpha = 0) +
 # geom_jitter(pch = 21, size = 1, width = 0.1, alpha = 0.2, show.legend = F)+
  ggpubr::stat_compare_means(label = "p.signif",
                             comparisons = mycomparisons) +
  ggsci::scale_fill_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(title = "FUNGI",
       x = NULL,
       y = NULL)+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01),
    breaks = seq(-0.5, 1.5, by = 0.5)) 
row1_2 <- row1_2+   theme_blank

row1_3 <- ggplot(all_RC, aes(x = exclosure, y = bac_avg, fill = exclosure)) +
  geom_violin(alpha = 0.2, show.legend = F)+
  geom_boxplot(width = 0.25, show.legend = T,alpha = 0.6, outlier.alpha = 0) +
 # geom_jitter(pch = 21, size = 1, width = 0.1, alpha = 0.2, show.legend = F)+
  ggpubr::stat_compare_means(label = "p.signif",
                             comparisons = mycomparisons) +
  ggsci::scale_fill_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(title = "BACTERIA",
       x = NULL,
       y = NULL)+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01),
    breaks = seq(-0.5, 1.5, by = 0.5)) 
row1_3 <- row1_3+  theme_blank


RC_box_plot <- ggarrange( row1_1 , row1_2 , row1_3, 
                          labels = c("A", "B", "C"),
                          ncol = 3, nrow = 1,common.legend = T, legend = "bottom")

ggsave(RC_box_plot, filename = "../Plot/RC_boxplot.jpeg", units = "in", height = 7, width = 17, dpi = 1000)



# Plot RC against exclosure*age -------------------------------------------

row2_1 <- ggplot(all_RC, aes(x = age, y = avg_plant, color = exclosure)) +
  geom_smooth(method = "lm", show.legend = F) +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(subtitle = "Plants",
       x = " \n")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01)) +
  annotate(geom = "text", x = 20, y = -0.15,
           label = glue::glue("Exclosure:Age
                              F-value = {round(plant_RC_annot$`F-value`[6],2)}; p-value {plant_RC_annot$p[6]}"),
           size = 4, hjust = 0)

row2_1 <- row2_1 + theme_blank1

row2_2 <- ggplot(all_RC, aes(x = age, y =fungi_avg, color = exclosure)) +
  geom_smooth(method = "lm",  show.legend = F, linetype = "dashed") +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(subtitle = "Fungi",
       x = "Age of exclosure\n")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))+
  annotate(geom = "text", x = 20, y = 0.6,
           label = glue::glue("Exclosure:Age
                              F-value = {round(fungi_RC_annot$`F-value`[6],2)}; p-value {fungi_RC_annot$p[6]}"),
           size = 4, hjust = 0)


row2_2 <- row2_2 +theme_blank1


row2_3 <- ggplot(all_RC, aes(x = age, y = bac_avg, color = exclosure))+
  geom_smooth(method = "lm", show.legend = F, linetype = "dashed") +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(subtitle = "Bacteria",
       x = " \n")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))+
  annotate(geom = "text", x = 20, y = 0.8,
           label = glue::glue("Exclosure:Age
                              F-value = {round(bac_RC_annot$`F-value`[6],2)}; p-value {bac_RC_annot$p[6]}"),
           size = 4, hjust = 0)

row2_3 <- row2_3 + theme_blank1



# Plot RC again exclsoure*elevation ---------------------------------------------

row3_1 <- ggplot(all_RC, aes(x = elevation, y = avg_plant, color = exclosure)) +
  geom_smooth(method = "lm",  show.legend = F) +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(x = " \n") +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))+
  annotate(geom = "text", x = 700, y = -0.25,
           label = glue::glue("Exclosure:Elevation
                              F-value = {round(plant_RC_annot$`F-value`[7],2)}; p-value {plant_RC_annot$p[7]}"),
           size = 4, hjust = 0)


row3_1 <- row3_1 + theme_blank1

row3_2 <- ggplot(all_RC, aes(x = elevation, y =fungi_avg, color = exclosure)) +
  geom_smooth(method = "lm",  show.legend = F, linetype = "dashed") +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(x = "Elevation (m)\n")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))+
  annotate(geom = "text", x = 700, y = 0.6,
           label = glue::glue("Exclosure:Elevation
                              F-value = {round(fungi_RC_annot$`F-value`[7],2)}; p-value {fungi_RC_annot$p[7]}"),
           size = 4, hjust = 0)
row3_2 <- row3_2 +  theme_blank1


row3_3 <- ggplot(all_RC, aes(x = elevation, y = bac_avg, color = exclosure))+
  geom_smooth(method = "lm", show.legend = F) +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(x = " \n")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))+
  annotate(geom = "text", x = 650, y = 0.8,
           label = glue::glue("Exclosure:Elevation
                              F-value = {round(bac_RC_annot$`F-value`[7],2)}; p-value {bac_RC_annot$p[7]}"),
           size = 4, hjust = 0)

row3_3 <- row3_3 +theme_blank1





# Plot all RC again exclosure*precipitation -------------------------------------

row4_1 <- ggplot(all_RC, aes(x = precip, y = avg_plant, color = exclosure)) +
  geom_smooth(method = "lm", show.legend = T) +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(x = " \n")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))+
  annotate(geom = "text", x = 315, y = -0.3,
           label = glue::glue("Exclosure:Precipitation
                              F-value = {round(plant_RC_annot$`F-value`[8],2)}; p-value {plant_RC_annot$p[8]}"),
           size = 4, hjust = 0)


row4_1 <- row4_1 +theme_blank1

row4_2 <- ggplot(all_RC, aes(x = precip, y =fungi_avg, color = exclosure)) +
  geom_smooth(method = "lm", show.legend = F, linetype = "dashed") +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(x = "Precipitation (mm)\n")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))+
  annotate(geom = "text", x = 300, y = 0.9,
           label = glue::glue("Exclosure:Precipitation
                              F-value = {round(fungi_RC_annot$`F-value`[8],2)}; p-value {fungi_RC_annot$p[8]}"),
           size = 4, hjust = 0)

row4_2 <- row4_2 +theme_blank1


row4_3 <- ggplot(all_RC, aes(x = precip, y = bac_avg, color = exclosure))+
  geom_smooth(method = "lm",  show.legend =F, linetype = "dashed" ) +
  ggsci::scale_color_aaas(name = "Exclosure",
                        labels = c("Inside", "Outside")) +
  labs(x = " \n")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))+
  annotate(geom = "text", x = 315, y = 0.92,
           label = glue::glue("Exclosure:Precipitation
                              F-value = {round(bac_RC_annot$`F-value`[8],2)}; p-value {bac_RC_annot$p[8]}"),
           size = 4, hjust = 0)

row4_3 <- row4_3 + theme_blank1



# Arranging the plot for the second figure ---------------------------------

lab <- c("A","", "B","", "C", "D","", "E","", "F", "G","", "H","", "I")
fig1 <-  annotate_figure(
  ggarrange(row2_1, NULL, row2_2, NULL, row2_3,
            row3_1, NULL, row3_2, NULL, row3_3,
            row4_1, NULL, row4_2, NULL, row4_3, ncol = 5, nrow = 3., common.legend = T, legend = "bottom",
            widths = c(1, 0.15, 1, 0.15, 1), labels = lab, label.x = -0.04),
  left = text_grob("Raup-Crick index\n", size = 20, rot = 90, face = "bold" ))

ggsave(fig1, filename = "interac_fig.jpeg",
       width = 18, height = 14, units = "in", dpi = 1200, path = "../Plot/")
