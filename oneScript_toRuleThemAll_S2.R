# Dependencies ------------------------------------------------------------
packages <- c("ggplot2",
              "reshape2",
              "scales",
              "RColorBrewer",
              "dplyr",
              "vegetarian",
              "vegan",
              "reshape2",
              "ggsignif",
              "TukeyC",
              "ggpubr",
              "ggpmisc",
              "lavaan",
              "semPlot",
              "OpenMx",
              "GGally",
              "vegetarian",
              "corrplot",
              "MVN",
              "piecewiseSEM",
              "lme4",
              "lmerTest",
              "nlme",
              "DiagrammeR",
              "TukeyC")

is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}
for(package in packages){
  # check if package is installed
  if (!is.installed(package)){
    install.packages(package)
  }
  else {print(paste(package, " library already installed"))}
}
for(package in packages){
  eval(bquote(library(.(package))))
}
# Relative abundances and plots -------------------------------------------
  # File setup
# genus
pro_genus <- read.csv("genus-taxa-table-count.csv", header = TRUE)
pro_genus <- cbind(pro_genus[1], prop.table(as.matrix(pro_genus[-1]), margin = 1)) # convert count table to proportions
write.csv(pro_genus, "pro_genus.csv") # couldn't figure out how to remove all columns below 1%, doing it manually in excel
pro_genus <- read.csv("pro_genus-average1%.csv")
genus_1df <- melt(pro_genus)
# phylum
pro_phylum <- read.csv("phylum-taxa-table-count.csv", header = TRUE)
pro_phylum <- cbind(pro_phylum[1], prop.table(as.matrix(pro_phylum[-1]), margin = 1)) # convert count table to proportions
write.csv(pro_phylum, "pro_phylum.csv") # couldn't figure out how to remove all columns below 1%, doing it manually in excel
pro_phylum <- read.csv("pro_phylum-average1%.csv")
phylum_1df <- melt(pro_phylum)

  # relative abundance barplots
# making a color set with enough for how many taxa i have
nb.cols <- 19
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols)

# overall averages of genus done in excel
pga <- read.csv("pro_genus-avgs.csv")
ppa <- read.csv("pro_phylum-avgs.csv")

# plot
(ge <- ggplot(pga,aes(x = X, y = Proportion, fill = Genus)) + 
    geom_bar(position = "fill",stat = "identity") +
    # or:
    # geom_bar(position = position_fill(), stat = "identity") 
    scale_y_continuous() + 
    theme_classic()  +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13,
                                  face="bold")) +
    theme(legend.text = element_text(size=14),
          legend.background = element_rect(size=0.5, 
                                           linetype="solid", 
                                           colour ="black"),
          legend.title = element_text(size = 15),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +  
    labs(x="Overall average",
         y="Proportion of community",
         fill = "Genus") +
    scale_fill_manual(values = mycolors)
)

# making a color set with enough for how many taxa i have
nb.cols <- 6
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols)

(ph <- ggplot(ppa,aes(x = X, y = Proportion, fill = Phylum)) + 
    geom_bar(position = "fill",stat = "identity") +
    # or:
    # geom_bar(position = position_fill(), stat = "identity") 
    scale_y_continuous() + 
    theme_classic()  +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13,
                                  face="bold")) +
    theme(legend.text = element_text(size=14),
          legend.background = element_rect(size=0.5, 
                                           linetype="solid", 
                                           colour ="black"),
          legend.title = element_text(size = 15),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +  
    labs(x="Overall average",
         y="Proportion of community",
         fill = "Phylum") +
    scale_fill_manual(values = mycolors)
)



# relative abundance barplots - by treatment
# making a color set with enough for how many taxa i have
nb.cols <- 19
mycolors <- colorRampPalette(brewer.pal(11, "Spectral"))(nb.cols)

# plot
(ge <- ggplot(genus_1df,aes(x = treatment, y = value, fill = variable)) + 
    geom_bar(position = "fill",stat = "identity") +
    # or:
    # geom_bar(position = position_fill(), stat = "identity") 
    scale_y_continuous() + 
    theme_classic()  +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13,
                                  face="bold")) +
    theme(legend.text = element_text(size=14),
          legend.background = element_rect(size=0.5, 
                                           linetype="solid", 
                                           colour ="black"),
          legend.title = element_text(size = 15),) +  
    labs(y="Proportion of community",
         fill = "Genus") +
    scale_fill_manual(values = mycolors)
)


# Alpha diversity ~ treatment (lm)----------------------------------------
dat <- read.csv("all-metadata.csv")

lm_lmer <- lmer(q0 ~ treatment + (1|order) + (1|sex/Bird), 
                dat = dat)
summary(lm_lmer)
anova(lm_lmer)
TukeyC(lm_lmer, which = "treatment")

lm_lmer <- lmer(q1 ~ treatment + (1|order) + (1|sex/Bird), 
                dat = dat)
summary(lm_lmer)
anova(lm_lmer)
tukey.test_re <- TukeyC(lm_lmer, which = "treatment")
tukey.test_re

lm_lmer <- lmer(q2 ~ treatment + (1|order) + (1|sex/Bird), 
                dat = dat)
summary(lm_lmer)
anova(lm_lmer)
tukey.test_re <- TukeyC(lm_lmer, which = "treatment")
tukey.test_re

lm_lmer <- lmer(faith_pd ~ treatment + (1|order) + (1|sex/Bird), 
                dat = dat)
summary(lm_lmer)
anova(lm_lmer)
tukey.test_re <- TukeyC(lm_lmer, which = "treatment")
tukey.test_re


#plotting
# plot 
(p1 <- ggplot(dat, aes(treatment, q0)) +
    geom_boxplot()  +
    theme_classic() +
    ylab("Effective # sp. (q0)")
    # ylim(0, 16) 
)
(p2 <- ggplot(dat, aes(treatment, q1)) +
    geom_boxplot()  +
    theme_classic() +
    ylab("Effective # sp. (q1)")
  # ylim(0, 16) 
)
(p3 <- ggplot(dat, aes(treatment, q2)) +
    geom_boxplot()  +
    theme_classic() +
    ylab("Effective # sp. (q2)")
  # ylim(0, 16) 
)
(p4 <- ggplot(dat, aes(treatment, faith_pd)) +
    geom_boxplot()  +
    theme_classic() +
    ylab("Faith's pd")
  # ylim(0, 16) 
)
ggarrange(p1, p2, p3, p4,
          labels = c("a)", "b)", "c)", "d)"),
          ncol = 2,
          nrow = 2)

# Path analysis -----------------------------------------------------------
# file setup

dat <- read.csv("all-metadata.csv", header = TRUE)
# remove 3 extreme outliers
dat <- dat[which(dat$cort<30),]
# dat <- na.omit(dat[which(dat$treatment=="C"|dat$treatment=="N"),])
dat[,5:11] <- scale(dat[,5:11])
dat$treatment <- as.numeric(dat$treatment)

# PSEM
# including bird as random factor
modelList <- psem(
  lmer(q0 ~ treatment + PMtotal_food + cort + (1|sex/Bird) + (1|order),  data = dat),
  lmer(cort ~ treatment + (1|sex/Bird) + (1|order),  data = dat),
  lmer(PMtotal_food ~ treatment + (1|sex/Bird) + (1|order), data = dat),
  dat
)
summary(modelList)


modelList <- psem(
  lmer(q1 ~ treatment + PMtotal_food + cort + (1|sex/Bird) + (1|order),  data = dat),
  lmer(cort ~ treatment + (1|sex/Bird) + (1|order),  data = dat),
  lmer(PMtotal_food ~ treatment + (1|sex/Bird) + (1|order), data = dat),
  dat
)
summary(modelList)


modelList <- psem(
  lmer(q2 ~ treatment + PMtotal_food + cort + (1|sex/Bird) + (1|order),  data = dat),
  lmer(cort ~ treatment + (1|sex/Bird) + (1|order),  data = dat),
  lmer(PMtotal_food ~ treatment + (1|sex/Bird) + (1|order), data = dat),
  dat
)
summary(modelList)


modelList <- psem(
  lmer(faith_pd ~ treatment + PMtotal_food + cort + (1|sex/Bird) + (1|order),  data = dat),
  lmer(cort ~ treatment + (1|sex/Bird) + (1|order),  data = dat),
  lmer(PMtotal_food ~ treatment + (1|sex/Bird) + (1|order), data = dat),
  dat
)
summary(modelList)

# Made path figures on lucid chart
# # best SEM version
# model <-  'q0 ~ treatment + PMtotal_food + cort
# cort ~ treatment
# PMtotal_food ~ treatment'
# fit <-  sem(model, data = dat)
# fitMeasures(fit, "aic")
# summary(fit, 
#         fit.measures = TRUE, 
#         standardized = TRUE, 
#         rsquare = TRUE)
# semPaths(fit, 
#          'std', 
#          style = "lisrel", 
#          layout = 'tree2', 
#          fade = FALSE, 
#          nCharNodes = 6, 
#          edge.label.cex=1.25)
# CORT ~ treatment (lm) ---------------------------------------------------

dat <- read.csv("all-metadata.csv")
# remove 3 extreme outliers
dat <- dat[which(dat$cort<30),]
lm_lmer <- lmer(cort ~ treatment + (1|order) + (1|sex/Bird), 
                dat = dat)
summary(lm_lmer)
anova(lm_lmer)
TukeyC(lm_lmer, which = "treatment")

# plot 
(crt <- ggplot(dat, aes(treatment, cort)) +
    geom_boxplot()  +
    theme_classic() +
    ylab("CORT")
)

# plotting orders separately
cnr <- dat[which(dat$order=="CNR"),]
# plot 
(cnrp <- ggplot(cnr, aes(treatment, cort)) +
    geom_boxplot()  +
    theme_classic() +
    ylab("CORT") +
    ylim(0, 16)
)
nrc <- dat[which(dat$order=="NRC"),]
# reordering treatment to reflect actual treatment
nrc <- nrc %>% 
  dplyr::mutate(treatment = factor(treatment, 
                                    levels = c("A", "N", "R", "C")))
# plot 
(nrcp <- ggplot(nrc, aes(treatment, cort)) +
  geom_boxplot()  +
  theme_classic() +
  ylab("CORT") +
  ylim(0, 16) +
  ylab("")
)
ggarrange(cnrp, nrcp,
          labels = c("a)", "b)"),
          ncol = 2,
          nrow = 1)

# Splitting into before, during and after noise
nrc$bna <- "Before"
nrc$bna[nrc$treatment=="N"] <- "During"
nrc$bna[nrc$treatment=="R"] <- "After"
nrc$bna[nrc$treatment=="C"] <- "After"
nrc$bna <- factor(nrc$bna, levels=c("Before", "During", "After"))

cnr$bna <- "Before"
cnr$bna[cnr$treatment=="N"] <- "During"
cnr$bna[cnr$treatment=="R"] <- "After"
cnr$bna <- factor(nrc$bna, levels=c("Before", "During", "After"))

dat <- rbind(cnr, nrc)
dat$bna <- factor(dat$bna, levels=c("Before", "During", "After"))

(datp <- ggplot(dat, aes(bna, cort, fill = bna)) +
    geom_boxplot()  +
    theme_classic() +
    theme(legend.position = "none") +
    ylab("CORT") +
    ylim(0, 16) +
    xlab("")  +
    scale_fill_manual(values=c("#9E0142", "#C52C4B", "#E25249"))
)


# 
dat$treatment <- factor(dat$treatment,levels = c("A", "N", "R", "C"))

(co <- ggplot(dat, aes(treatment, cort)) +
    geom_boxplot()  +
    theme_classic() +
    theme(legend.position = "none") +
    ylab("CORT") +
    ylim(0, 16) +
    xlab("")  +
    scale_fill_manual(values=c("#9E0142", "#DC494C", "#F88D51")) +
    facet_wrap(~bna, scale="free") + 
    geom_point(aes(x = treatment))
  )



(co <- ggplot(dat, aes(treatment, cort)) +
    geom_boxplot()  +
    theme_classic() +
    theme(legend.position = "none") +
    ylab("CORT") +
    ylim(0, 16) +
    xlab("") 
)

# Food intake ~ treatment (lm) --------------------------------------------
dat <- read.csv("all-metadata.csv")
lm_lmer <- lmer(AMtotal_food ~ treatment + (1|order) + (1|sex/Bird), 
                dat = dat)
summary(lm_lmer)
anova(lm_lmer)
TukeyC(lm_lmer, which = "treatment")

dat <- read.csv("all-metadata.csv")
lm_lmer <- lmer(PMtotal_food ~ treatment + (1|order) + (1|sex/Bird), 
                dat = dat)
summary(lm_lmer)
anova(lm_lmer)
TukeyC(lm_lmer, which = "treatment")

dat <- read.csv("all-metadata.csv")
lm_lmer <- lmer(wholeDay_totalFood ~ treatment + (1|order) + (1|sex/Bird), 
                dat = dat)
summary(lm_lmer)
anova(lm_lmer)
TukeyC(lm_lmer, which = "treatment")

#Plots
(amTOT <- ggplot(dat, aes(treatment, AMtotal_food, fill = treatment)) +
    geom_boxplot()  +
    theme_classic() +
    theme(legend.position = "none") +
    ylab("Total food (AM)")  +
    scale_fill_manual(values=c("#F57647", "#FBA45C", "#FDCA78", "#FEE899"))
)
(pmTOT <- ggplot(dat, aes(treatment, PMtotal_food, fill = treatment)) +
    geom_boxplot()  +
    theme_classic() +
    ylab("Total food (PM)") +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FFFFBF", "#EDF7A3", "#CCEA9D", "#A1D9A4"))
)
(whoTOT <- ggplot(dat, aes(treatment, wholeDay_totalFood, fill = treatment)) +
    geom_boxplot()  +
    theme_classic() +
    ylab("Total food intake")+
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#6FC5A4", "#48A0B2", "#3E77B5", "#5E4FA2"))
)
# arranging in grid
ggarrange(amTOT, pmTOT, whoTOT,
          labels = c("a)", "b)", "c)"),
          ncol = 2,
          nrow = 2)

# Beta diversity ~ treatment (permanova) ----------------------------------
dat <- read.csv("seqD_catalog.csv", header = TRUE)

# Distance matricies
j_dm <- read.table("distanceMatricies/j-distance-matrix.tsv", header = TRUE, row.names = 1)
dm <- as.dist(j_dm)
uwu_dm <- read.table("distanceMatricies/uwu-distance-matrix.tsv", header = TRUE, row.names = 1)
dm <- as.dist(uwu_dm)
bc_dm <- read.table("distanceMatricies/bc-distance-matrix.tsv", header = TRUE, row.names = 1)
dm <- as.dist(bc_dm)
wu_dm <- read.table("distanceMatricies/wu-distance-matrix.tsv", header = TRUE, row.names = 1)
dm <- as.dist(wu_dm)

# PERMANOVA (adonis)
# by treatment
set.seed(1990)
adonis(formula = j_dm ~  treatment,
       strata = dat$order,
       data = dat,
       permutations = 9999,
       by = margin) #type3 sum of sq

set.seed(1990)
adonis(formula = uwu_dm ~  treatment,
       strata = dat$Bird,
       data = dat,
       permutations = 9999,
       by = margin) #type3 sum of sq

set.seed(1990)
adonis(formula = bc_dm ~  treatment,
       # strata = tab ~ Location,
       data = dat,
       permutations = 9999,
       by = margin) #type3 sum of sq

set.seed(1990)
adonis(formula = wu_dm ~  treatment,
       # strata = tab ~ Location,
       data = dat,
       permutations = 9999,
       by = margin) #type3 sum of sq
