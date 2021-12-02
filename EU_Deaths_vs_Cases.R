# EU plot deaths and cases : current state & 
# evolution between two days.
# clean the environment
remove(list=ls())

library(xts)
library(tidyr)
library(COVID19)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(grid)
library(ggpubr)
library(gridExtra)

# Download country-level data. Make sure to use v3.0.0
x <- covid19()

# Compute cumulative counts per 100k population
x$deaths_per_100k <- x$deaths/x$population*100000
x$confirmed_per_100k <- x$confirmed/x$population*100000

# Compute the time-series of notification rates for each country...
ts <- pivot_wider(x, id_cols = "date", 
                  names_from = "administrative_area_level_1", 
                  values_from = c("confirmed_per_100k", "deaths_per_100k"))

# Check the data if needed
# View(ts)
ts <- xts(ts[,-1], order.by = ts$date)
ts_dly <- diff(ts)
ts_rate <- rollsumr(ts_dly, k = 14)

# Convert to tabular format..
latest <- data.frame(ts_rate)
latest$date <- rownames(latest)
rate <- pivot_longer(latest, cols = -date, 
                     names_sep = "_per_100k_", names_to = c("type", "country"))
rate <- pivot_wider(rate, id_cols = c("country", "date"), 
                    names_from = "type", values_from = "value")
rate <- rate[!is.na(rate$confirmed) | !is.na(rate$deaths),]

# See e.g., the latest data for Belgium
rate[rate$country=="Belgium",]
rate[rate$country=="Belgium"  & rate$date=="2021-11-24",]

# rate %>% dplyr::filter((country=="Belgium" | country=="Portugal") & date=="2021-11-22")

SelectedCountries <- c("Austria", "Belgium", "Bulgaria", "Croatia","Cyprus",
                    "Denmark", "Finland", "France", "Germany", "Greece","Hungary",
                    "Ireland", "Italy", "Lithuania" , "Luxembourg",
                    "Malta", "Netherlands", "Norway", "Portugal","Slovenia", "Spain")

# SelectedCountries <- c("Belgium", "Denmark", "France", "Germany", 
#                        "Italy", "Luxembourg",
#                        "Netherlands", "Portugal","Spain")  

SelectedDates <-c("2021-11-01","2021-11-30")

EU_test1 <- rate %>% dplyr::filter(country %in% SelectedCountries & date==SelectedDates[1])
EU_test2 <- rate %>% dplyr::filter(country %in% SelectedCountries & date==SelectedDates[2])

EU2<-merge(x=EU_test1[,-2],y=EU_test2[,-2],by.x="country",by.y = "country",all= T)
names(EU2) <- c("Country", "Case.x", "Deaths.x", "Case.y", "Deaths.y")   
EU2[,c(3,5)]<-EU2[,c(3,5)]*10

EU2$Group <- "blue"
EU2$Group[EU2$Country=="Belgium"] <- "red"

#initial trial
p <- ggplot(EU2, aes(x=Case.x, y=Deaths.x, color=Group)) +
  geom_point() +
  geom_point(data = EU2, aes(x=Case.y, y=Deaths.y, color=Group)) +
  geom_segment(aes(x = Case.x, y = Deaths.x, xend = Case.y, yend = Deaths.y, color = Group, size = Group, linetype=Group),
               arrow = arrow(type="closed",length = unit(0.2, "cm"))
  ) +
  geom_text_repel(aes(label = Country), nudge_x = 0.04) +
  labs(title = paste0("Deaths & cases in EU: evolution between ", SelectedDates[1], " & ", SelectedDates[2])
  ) +
  theme(plot.title = element_text(lineheight = 0.9))+
  labs(x = "14-day case notification rate per 100 000",
       y = "14-day death notification rate per 1 000 000") +
  scale_colour_manual(values = c("black", "red"), guide = "none")+
  theme_bw(base_size = 12) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12.5,face="bold"))+ 
  scale_x_log10()+scale_y_log10()  + 
  scale_color_manual(values=c("steelblue","red")) +
  scale_size_manual(values=c(0.15,0.7))+
  scale_linetype_manual(values=c("dotted","solid"))+
  theme(legend.position = "none")

##################
#Set up empty dataframe
datelength <- as.numeric(difftime(as.POSIXct(SelectedDates[2]), as.POSIXct(SelectedDates[1]), 
                                  units="days"))

countrylength <- length(SelectedCountries)

shell <- data.frame(date=rep(seq.Date(min(as.Date(SelectedDates)), 
                                      max(as.Date(SelectedDates)), by="days"), 
                             times= countrylength),
                    Country=rep(unique(SelectedCountries), each=datelength+1))

#Import actual data for start and end dates
shell <- shell %>% 
  merge(EU2 %>% gather(metric, count, c(2:5)) %>% 
          mutate(metric2=case_when(
            substr(metric, 1, 2)=="Ca" ~ "Cases",
            TRUE ~ "Deaths"),
            date=case_when(
              substr(metric, nchar(metric)-1, nchar(metric))==".x" ~ min(as.Date(SelectedDates)),
              TRUE ~ max(as.Date(SelectedDates)))) %>%
          select(-metric) %>% 
          spread(metric2, count), all.x=TRUE) %>% 
  #Interpolate missing dates linearly (on a log scale)
  group_by(Country) %>% 
  mutate(logCases=na.approx(log10(Cases)), logDeaths=na.approx(log10(Deaths)),
         dayspast=as.numeric(difftime(as.POSIXct(date), as.POSIXct(SelectedDates[1]), 
                             units="days")),
         Group=if_else(Country=="Belgium", "red", "blue"))

# The plot with the points only can be obtained by deleting the geom_path part of the code.
p2 <- ggplot() +
  geom_path(data=shell, aes(x=10^(logCases), y=10^(logDeaths), alpha=dayspast, group=Country, colour=Group),
            show.legend=FALSE)+
  geom_point(data=EU2, aes(x=Case.y, y=Deaths.y, fill=Group), shape=21, size=rel(2.5),
             show.legend=FALSE)+
  geom_text_repel(data=EU2, aes(x=Case.y, y=Deaths.y, colour=Group, label=Country),
                  show.legend=FALSE, nudge_x = 0.03, nudge_y = - 0.025,size=2.7)+
  scale_fill_manual(values=c("steelblue", "red"))+
  scale_colour_manual(values=c("steelblue", "red"))+
  scale_x_log10(name="Cases per 100k in the past 14 days (log scale)",
                expand = expansion(mult = 0.1))+
  scale_y_log10(name="Deaths per 100k in the past 14 days (log scale)")+
  theme_bw(base_size = 11) +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=11
                                # , face="bold"
                                ),
        plot.caption=element_text(colour="DarkGrey", face="italic"),
        plot.title=element_text(face="bold", size=11),
        plot.subtitle=element_text(size=9),
        plot.title.position = "plot"
        )+
  labs(title=paste0("COVID-19 case & mortality rates across the EU as per ",format(as.Date(SelectedDates[2]), '%d/%m/%y')),
       subtitle=paste0("Lines represent changes in the past ", datelength, " days"),
       caption="Data source: COVID-19 Data Hub (https://covid19datahub.io/)
 Guidotti, E., Ardia, D., (2020), Journal of Open Source Software 5(51):2376") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

p2