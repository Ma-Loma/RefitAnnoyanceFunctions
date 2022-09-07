# Dies soll die Rohdaten der Einzelstudien zur Belästigung durch Straßenlärm von Guski einlesen und neu fitten
# Geschaffen von Matthias Lochmann ab 5.4.2022


#install.packages("devtools")
# library(devtools)
# devtools::install_github("padpadpadpad/nlsLoop", build_vignettes = TRUE)
# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("MathiasHarrer/dmetar")
if (!require("janitor")) install.packages("janitor")
install.packages("readODS")
install.packages("dmetar")
install.packages("meta")

#clear workspace
rm(list = ls())


library(meta)
library(metafor)
library(dmetar)
library(tidyverse)
library(janitor)
library(readODS)
library(lme4)
#library(nlme)
#library(investr)
#library(broom)
#library(nlshelper)
#library(MASS)


inpath<-"data/WHO Annoyance Data Road 2017_Namen_einheitlich.ods"
outpath<-"graphs/"

get_num_sheets_in_ods(inpath)
list_ods_sheets(inpath)
raw<-read_ods(
  path = inpath,
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0,
  formula_as_formula = FALSE,
  range = "A1:AM29",
 # row_names = TRUE,
  strings_as_factors = FALSE,
  verbose = FALSE
)

nList <- raw[c("Location", "N Location", "Sqrt N/10")] %>%
  clean_names(., "none") %>%
  .[complete.cases(.), ] %>%
  mutate(N = N_Location,
         Sqrt_N_10 = NULL,
         .keep = "unused")
HAList <- raw %>%
  select(Lden, starts_with("%HA")) %>%
  select(!contains("M&O")) %>%
  rename_all(.,  ~ str_replace(., "%HA ", "")) %>%
  rename_all(.,  ~ str_replace(., " Road", "")) %>%
  pivot_longer(
    .,
    cols = !contains("Lden"),
    names_to = "Location",
    values_to = "ProzHA"
  ) %>%
  .[complete.cases(.), ] %>%
  left_join(., nList) %>%
  group_by(Location) %>% filter(n() >= 3)# ignoriere die mit weniger als 3 Datenpunkten

HAList<-HAList %>% mutate(Location=as.factor(Location))

regrList <-
  raw %>% select(Lden, contains(c("M&O", "Regr", "Perc"))) %>% .[complete.cases(.[, 2:11]), ]# Fit Ergebnisse aus Guski Tabelle, zu vergleichen mit meinen Ergebnissen

nmbStud<-length(linFitEinzel)
xSeq<-seq(40,85)


#fit a mixed model with control of corellations 
HAmodel<-lmer(log(ProzHA)~Lden + (Lden | Location), HAList,weights = sqrt(N))
HAList$fit<-predict(HAmodel)
HAList$uncFit<-predict(HAmodel,re.form=NA)


HAList<-regrList %>% select(Lden,`Regr WHO Road data`,`Regr WHO Road ohne Alp+Asia`) %>% 
  left_join(x=HAList,y=.)
HAmodel

plWHOfit<-ggplot(HAList,aes(
  x = Lden)) +
  geom_line(
    data = HAList[!is.na(HAList$`Regr WHO Road data`), ],
    mapping = aes(
   #   x = Lden,
      y=  `Regr WHO Road data`
    ),
    color = "blue",
    size=3,
    na.rm = TRUE
  )+
  geom_ribbon(
    data = regrList,
    mapping = aes(
    #  x = Lden,
      ymin = `Percentile 5`,
      ymax =  `Percentile 95`
    ),
    fill = "blue",
    alpha = 0.2
  )

g <- guide_legend("title")
plWHOfit + guides(fill = g, size = g, shape = g)
vglFit<-plWHOfit +
  geom_point(data = HAList,
             aes(
     #          x = Lden,
               y = ProzHA,
               size = sqrt(N) / 10,
               color = Location
             )) +
  geom_line(data = HAList,
            aes(#x = Lden,
                y = exp(fit),
                color = Location))+
  geom_line(data = HAList,
            aes(#x = Lden,
                y = exp(uncFit)),
            color = "black",
            size=3)+scale_y_log10()
vglFit
vglFit+facet_wrap(vars(Location))
fitted(HAmodel)

pl1+
  stat_smooth(color="red",method = 'nls', formula = 'y~I(a*x^2+b*x+c)',
              method.args = list(start=c(a=0.01, b=-3,c=100)), se=FALSE)# Fitte beliebige Parabel
ggsave(paste(outpath,"alle.png",sep=""))
# pl1+ stat_smooth(color="blue",method = 'nls', formula = 'y~I(pnorm(x,mean=alpha,sd=beta))',
#                  method.args =list(start=c(alpha=50, beta=10)), se=FALSE)# fitte Integralgauss

# curve(pnorm(x, mean = 60, sd = 10),
#       from = 40,
#       to = 80,
#       main = "pnorm")


pl2<-pl1+stat_smooth(formula=y~I(x^2),method = "lm")# jetzt zusätzlich mit der Parabel aus lm gefittet.
pl2
pl1+geom_line(data=parFitCurve,aes(color="red"))
pl1lin<-pl1+aes(color=Location)+geom_line(data=linFitCurves)+coord_cartesian(ylim = c(0, 70))
pl1lin
ggsave(paste(outpath,"einzelLin.png",sep=""))
facPl<-pl1lin+
    facet_wrap(vars(Location))
facPl
ggsave(paste(outpath,"einzelLinFac.png",sep=""))
facPl2<-pl2+
  aes(color=Location)+
  facet_wrap(vars(Location))+coord_cartesian(ylim = c(0, 70))

facPl2
facPl+stat_smooth(method = "lm",color="black")

facPl+stat_smooth(method = "glm")

ggplot(HAList, aes(x = Lden, y = ProzHA)) +
  geom_point(aes(size = sqrt(N) / 10)) +
  stat_smooth(
    method = "nls",
    method.args = list(
      formula = HAList$ProzHA ~ a * HAList$Lden ^ 2 + b * HAList$Lden + c,
      start = list(a = 0.1, b = -10, c =
                     50)
    )
  )


#  geom_errorbar(aes(ymin = ProzHA-100/sqrt(N), ymax = ProzHA+100/sqrt(N)))
       
       # ggplot(topRisk,
       #        aes(
       #          x = as.numeric(rownames(topRisk)),
       #          y = Value*0.074,
       #          label = Risk.factor
       #        ))+
       #   geom_point(stat = "identity",shape=15,size=5)+ 
       #   geom_text(size = 4, angle = 55,hjust=-0.1)+
       #   geom_errorbar(aes(ymin = Lower.bound*0.074, ymax = Upper.bound*0.074)) + ggtitle("Risikofaktoren alle Ursachen Hessen 2016/17") +
       # 