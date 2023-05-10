library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(leaflet)

display.brewer.all()

cast = read.csv("cast_with_gender_and_ethnicity.csv")

# characters with only first name weren't identified through api
cast[is.na(cast$raceEthnicity),]

cast = cast %>% mutate(raceEthnicity = ifelse(name == "Zendaya", "B_NL", raceEthnicity))
cast = cast %>% mutate(raceEthnicity = ifelse(name == "Awkwafina", "A", raceEthnicity))
cast = cast %>% mutate(raceEthnicity = ifelse(name == "Halsey", "W_NL", raceEthnicity))
cast = cast %>% mutate(raceEthnicity = ifelse(name == "Bono", "W_NL", raceEthnicity))

cast_nums = cast %>% select(c("raceEthnicity")) %>% group_by(raceEthnicity) %>% summarize(total=n())

cast_nums %>% ggplot(aes(x=raceEthnicity,y=total)) +
  geom_bar(stat = "identity", fill="darkgoldenrod3") +
  geom_text(aes(label=total), color="white", vjust=1.4, size=3.5) +
  labs(x="Ethnicity", y="Count") +
  ggtitle("Count of Leading Cast in Top Hollywood Movies (2010-2022)") +
  scale_x_discrete(labels=c("Asian", "Black", "Hispanic", "White")) +
  theme_minimal()

cast$year = as.integer(str_extract(cast$filename, pattern="[0-9]+"))

cast_nums_year = cast %>% group_by(year, raceEthnicity) %>% summarize(total=n())
cast_nums_year_gender = cast %>% group_by(year, likelyGender) %>% summarize(total=n())

cast_nums_year %>% ggplot() +
  aes(x=year, y=total, group=raceEthnicity, color=raceEthnicity) +
  geom_line() +
  theme_minimal() +
  labs(color="Ethnicity", x = "Year", y = "Count of Leading Actors", title = "Leading Actors of different Ethnicities in Top 10 Movies") +
  scale_color_manual(labels = c("Asian", "Black", "Hispanic", "White"), values=brewer.pal(4, "Spectral")) +
  scale_x_continuous(breaks=seq(2010,2022, by=4), minor_breaks = seq(2010, 2022, by=2)) 

cast_nums_year_gender %>% ggplot() +
  aes(x=year, y=total, group=likelyGender, color=likelyGender) +
  geom_line() +
  theme_minimal() +
  labs(color="Ethnicity", x = "Year", y = "Count of Leading Actors", title = "Male vs Female Leads in Top 10 Hollywood Movies (2010-2022)") +
  # scale_color_manual(labels = c("Asian", "Black"), values=brewer.pal(4, "Spectral")) +
  scale_x_continuous(breaks=seq(2010,2022, by=4), minor_breaks = seq(2010, 2022, by=2)) 


# only a few movies perform very well
ggplot() +
  geom_histogram(mapping=aes(x=domestic_box_office), fill="burlywood3", data = domestic_movies, stat = "bin", bins = 50) +
  xlab("Domestic Box Office Earnings (In Millions)") +
  ylab("Count of Movies") +
  scale_x_continuous(labels = function(x) format(x/1000000, nsmall = 2)) +
  ggtitle("Earnings of Top 10 Hollywood movies (2010-2022)") +
  theme_minimal()

best_international %>% group_by(Territory) %>% summarize(total=sum(box_office)) %>% arrange(desc(total)) %>% head(5) %>%
  mutate(Territory = factor(Territory, levels = unique(Territory))) %>% ggplot(aes(x=Territory, y=total)) +
  geom_bar(stat = "identity", fill="gold4") +
  geom_text(aes(label=format(total/1e9, digits=2)), color="gold4", vjust=-0.7, size=3.5) +
  labs(x="Territory", y="Earning in International Box Office (In Billion)", title="Aggregate Earnings for Popular Movies (2010-2022)") +
  scale_y_continuous(labels = function(x) format(x/1000000000, nsmall = 2)) +
  coord_cartesian(ylim = c(0, 7000000000)) +
  theme_minimal()
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# -----------------------------------------------

map_data('world') %>% ggplot() + geom_polygon(aes(x=long, y=lat, group = group), color='white') 


leaflet() %>% 
  addPolygons(data = map_data('word'))
