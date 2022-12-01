
# Import packages ---------------------------------------------------------


library(tidyverse)
library(nlme)
library(ggpubr)



# Import data -------------------------------------------------------------


RC <- read.csv("../Data/metadata/metadata.csv")




# Models ------------------------------------------------------------------



m1 <- lme(fungi_avg ~ avg_plant*exclosure,
          random = ~1|site,
          data = RC,
          na.action = na.omit)

anova.lme(m1)
MuMIn::r.squaredGLMM(m1)

fungal_rc_plant_RC_annot <- anova.lme(m1) %>% 
  as.data.frame() %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))

  


m2 <- lme(bac_avg ~ avg_plant*exclosure,
          random = ~1|site,
          data = RC,
          na.action = na.omit)
anova.lme(m2)
MuMIn::r.squaredGLMM(m2)




bac_rc_plant_RC_annot <- anova.lme(m2) %>% 
  as.data.frame() %>% 
  mutate(p = case_when(
    `p-value` > 0.05 ~ "> 0.05",
    `p-value` < 0.05 & `p-value` > 0.01 ~ "< 0.05",
    `p-value` < 0.01 & `p-value` > 0.001 ~ "< 0.01",
    TRUE ~ "< 0.001"))


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


# Plotting ----------------------------------------------------------------


bac_plant_rc <- RC %>% 
   ggplot(., aes(y = bac_avg, x = avg_plant, color = exclosure)) +
    geom_smooth(method = "lm") +
  theme_bw() +
  ggsci::scale_color_aaas(name = "Exclosure",
                          labels = c("Inside", "Outside"))+
  theme_box +
  labs(subtitle = "Bacteria",
    x = "Plant RC index",
       y =  "RC index") +
  annotate(geom = "text", x = -0.18, y = 0.83,
           label = glue::glue("Exclosure:Age
                              F-value = {round(bac_rc_plant_RC_annot$`F-value`[4],2)}; p-value {bac_rc_plant_RC_annot$p[4]}"),
           size = 4, hjust = 0)

bac_plant_rc



fungi_plant_rc <- RC %>% 
  ggplot(., aes(y = fungi_avg, x = avg_plant, color = exclosure)) +
  geom_smooth(method = "lm", linetype = "dashed") +
  theme_bw() +
  ggsci::scale_color_aaas(name = "Exclosure",
                          labels = c("Inside", "Outside"))+
  theme_box +
  labs(subtitle = "Fungi",
       x = "Plant RC index",
       y =  NULL)+
  guides(color = guide_legend(reverse=TRUE)) +
  annotate(geom = "text", x = -0.4, y = 0.9,
           label = glue::glue("Exclosure:Age
                              F-value = {round(fungal_rc_plant_RC_annot$`F-value`[4],2)}; p-value {fungal_rc_plant_RC_annot$p[4]}"),
           size = 4, hjust = 0)


fungi_plant_rc

# Arranging all plots -----------------------------------------------------

lab <- c("A","", "B")


plant_micro_rc <- ggarrange(bac_plant_rc, NULL, fungi_plant_rc,
          ncol = 3, nrow = 1, common.legend = T, legend = "bottom",
          widths = c(1, 0.15, 1), labels = lab)

ggsave(plant_micro_rc, filename = "plant_micro_rc.jpeg",
       width = 18, height =9, units = "in", dpi = 1000, path = "../Plot/")

