library("readxl")
library("ggplot2")

# reading
dataValues <- read_excel("cultvalueorientationscores.xls")

# plot - Embeddedness/Autonomy
ggplot(dataValues, aes(x = embedded, y = auton)) + 
  geom_point(aes(color = as.factor(profile)), size = 2) +
  xlim(NA, 5) +
  ylim(NA, 5) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title.x = element_text(vjust = -6, size = 16, family = "sans"),
        axis.title.y = element_text(vjust = 6, size = 16, family = "sans"),
        #top, right, bottom, left
        plot.margin=unit(c(1.2, 1.2, 1.2, 1.2),"cm"),
        legend.position = "none") +
  xlab("Embeddedness") + ylab("Autonomy")

# save as png
ggsave(
  "EmbAutENG.png",
  device = "png",
  scale = 1,
  # because I like when I can zoom my plot a lot)
  dpi = 500
)

#-------------------------------------------------------

# plot - Hierarchy/Egalitarianism
ggplot(dataValues, aes(x = hierarchy, y = egalitar)) + 
  geom_point(aes(color = as.factor(profile)), size = 2) +
  #xlim(NA, 4) +
  #ylim(NA, 5.4) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title.x = element_text(vjust = -6, size = 16, family = "sans"),
        axis.title.y = element_text(vjust = 6, size = 16, family = "sans"),
        #top, right, bottom, left
        plot.margin=unit(c(1.2, 1.2, 1.2, 1.2),"cm"),
        legend.position = "none") +
  xlab("Hierarchy") + ylab("Egalitarianism")

# save as png
ggsave(
  "HieEgaENG.png",
  device = "png",
  scale = 1,
  # because I like when I can zoom my plot a lot)
  dpi = 500
)

#-------------------------------------------------------

# plot - Mastery/Harmony
ggplot(dataValues, aes(x = mastery, y = harmony)) + 
  geom_point(aes(color = as.factor(profile)), size = 2) +
  xlim(NA, 4.5) +
  ylim(NA, 4.8) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title.x = element_text(vjust = -6, size = 16, family = "sans"),
        axis.title.y = element_text(vjust = 6, size = 16, family = "sans"),
        #top, right, bottom, left
        plot.margin=unit(c(1.2, 1.2, 1.2, 1.2),"cm"),
        legend.position = "none") +
  xlab("Mastery") + ylab("Harmony")

# save as png
ggsave(
  "MasHarENG.png",
  device = "png",
  scale = 1,
  # because I like when I can zoom my plot a lot)
  dpi = 500
)
