pacman::p_load(usethis,pacman,rio,ggplot2,ggthemes,quantmod,dplyr,data.table,lubridate,forecast,gifski,av,tidyr,gganimate,zoo,RCurl,Cairo,datetime,stringr,pollster,tidyquant,hrbrthemes,plotly,fredr,gcookbook,tidyverse,RCurl)
setwd(dir = "/Users/alexanderalbrecht/Library/CloudStorage/OneDrive-Personal/Alex_s Zeug/articles/Dienstleistungen und Güter")


df <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Alex_s Zeug/articles/Dienstleistungen und Güter/Goods and Services.csv", sep=";", comment.char="#")
df <- select(df, -c(Statistik_Code, Statistik_Label, X3_Auspraegung_Label, Zeit_Label, Zeit_Code, X1_Merkmal_Label, X1_Merkmal_Code))                            
df <- rename(df, Month= X2_Merkmal_Code)
names(df)[names(df) == "UMS103__Umsatzindex__2015.100"] <- 'Value'
df$X2_Auspraegung_Code <- substr(df$X2_Auspraegung_Code, 6, 7)
df$X2_Auspraegung_Code <- as.numeric(df$X2_Auspraegung_Code, fixed=TRUE)
df$Value <- as.numeric(sub(",", ".", df$Value, fixed = TRUE))
df$Date <- as.yearmon(paste(df$Zeit, df$X2_Auspraegung_Code), "%Y %m")
df <- subset(df, df$X5_Auspraegung_Code =="X13JDKSB")
df <- na.omit(df)

df2 <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Alex_s Zeug/articles/Dienstleistungen und Güter/Goods.csv", sep=";", comment.char="#")
df2$X5_Auspraegung_Code <- substr(df2$X5_Auspraegung_Code, 6, 7)
df2$X5_Auspraegung_Code <- as.numeric(df2$X5_Auspraegung_Code, fixed=TRUE)
names(df2)[names(df2) == "UMS101__Umsatz._Wertindex__2015.100"] <- 'Value'


df2$Value <- as.numeric(sub(",", ".", df2$Value, fixed = TRUE))
df2$Date <- as.yearmon(paste(df2$Zeit, df2$X5_Auspraegung_Code), "%Y %m")
df2 <- df2 %>% subset(Zeit !=2018 & Value != "NA" & X3_Auspraegung_Code =="INSGESAMT" & X4_Auspraegung_Code =="GUT-KONSUM-01")


theme_alex <- theme_modern_rc() +
  theme(axis.line = element_line(colour = "white"), legend.position = c(.9,.2),
        legend.text = element_text(size = 11, color = "white"), 
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5))

  ggplot() +
  geom_line(data=df, aes(x=Date, y = Value, color = "Dienstleistungen")) +
  scale_fill_discrete(name = "") +
  geom_line(data=df6, aes(x=Date, y = Value, color = "Gebrauchsgüter")) +
  geom_line(data=df4, aes(x=Date, y = Value, color = "Verbrauchsgüter")) +
  ggtitle("Dienstleistungen über Konsumgüter") +
  labs(caption = "Graph von @alex_albrecht unter Benutzung von GENESIS-Daten")  + theme(legend.position = c(1,.05)) +
  ylab("Umsatzindex (2015=100), kalender- und saisonbereinigt") +
  xlab("Monat") +
  scale_y_continuous(limits = c(70,125), breaks = c(70,80,90,100,110,120,130), expand = c(0,0)) +
  scale_color_manual(name= NULL,values = c("#33b1ff","#fa4d56", "#42be65")) +
  coord_cartesian(clip = "off") +
  theme_alex

#Erzeugerpreise
df3 <- read.csv("Erzeugerpreise_real.csv", sep=";", comment.char="#")
df3$Quartal <- substr(df3$Quartal, 2,2)
df3$Date <- as.yearqtr(paste0(df3$Jahr, df3$Quartal), format = "%Y %q")

ggplot() +
  geom_line(data = df3, aes(x=Date, y=Gesamtindex, color="Erzeugerpreise Dienstleistungen Gesamt")) +
    scale_fill_discrete(name = "") +
  geom_line(data = df3, aes(x=Date, y= Verkehr.u..Lagerei, color= "Erzeugerpreise Verkehr und Lagerei")) +
  ggtitle("Transport ist teuer!") +
  labs(subtitle = "Erzeugerpreise bei Dienstleistern ")  + 
  ylab("Erzeugerpreise Dienstleistungen im Vergleich zum Vorjahresquartal") +
  xlab("Quartale") +
  scale_y_continuous(limits = c(0,30), breaks = c(5,10,15,20,25), expand = c(0,0)) +
  scale_color_manual(name= NULL,values = c("#33b1ff","#fa4d56")) +
  coord_cartesian(clip = "off") + 
  labs(caption = "Graph von @alex__albrecht unter Benutzung von GENESIS-Daten") +
  theme_modern_rc() +
  theme(axis.line = element_line(colour = "white"), legend.position = c(.3,.8),
        legend.text = element_text(size = 11, color = "white"), 
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5))

#Verbrauchs und Konsumgüter
df6 <- read.csv("Gebrauchsgüter.csv", sep = ";", comment.char = "#")
df6$Date <- as.yearmon(paste0(df6$Year, df6$Monat), format = "%Y %m")
df6$Value <- df6$Kalender..und.saisonbereinigt.nach.X13.JDemetra.
df4 <- read.csv("Verbrauchsgüter.csv", sep = ";", comment.char = "#")
df4$Date <- as.yearmon(paste0(df4$Year, df4$Monat), format = "%Y %m")
df4$Value <- df4$Kalender..und.saisonbereinigt.nach.X13.JDemetra.


#Gastgewerbe
df5 <- read.csv("Gastgewerbe.csv", sep=";", comment.char="#")
df5$X5_Auspraegung_Code <- substr(df5$X5_Auspraegung_Code, 6, 7)
df5$X5_Auspraegung_Code <- as.numeric(df5$X5_Auspraegung_Code, fixed=TRUE)
names(df5)[names(df5) == "UMS002__Umsatz__2015.100"] <- 'Value'
df5$Value <- as.numeric(sub(",", ".", df5$Value, fixed = TRUE))
df5$Date <- as.yearmon(paste(df5$Zeit, df5$X5_Auspraegung_Code), "%Y %m")
df5 <- subset(df5, df5$Value != "NA")
df5 <- subset(df5, df5$X4_Auspraegung_Code == "WZ08-55-01")
df5 <- subset(df5, df5$X2_Auspraegung_Code == "REAL")

ggplot() + 
  geom_line(data=df5, aes(x=Date, y = Value, color="Gastgewerbe")) +
  ggtitle("Das Gastgewerbe hinkt hinterher") +
  theme(legend.position = c(.85,.155)) +
  ylab("Umsatzindex (2015=100)") +
  xlab("Monat") +
  labs(caption = "Graph von @alex__albrecht unter Benutzung von GENESIS-Daten") +
  scale_color_manual(name= NULL,values = c("#33b1ff","#fa4d56")) +
  coord_cartesian(clip = "off") + 
  theme_alex