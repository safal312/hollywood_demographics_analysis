library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(httr)
library(jsonlite)

# read all domestic movies
domestic_movies = data.frame()

for (year in 2010:2022) {
  df = read.csv(paste("top_domestic/domestic-", year, ".csv", sep=""))
  df$year = year
  domestic_movies = rbind(domestic_movies, df)
}

# how many movies we have each year? 100
# domestic_movies %>% group_by(year) %>% summarize(count = n())

# filter unnecessary columns and rename some
# For international box office, we only have information on top 11 movies from 2010-2022, so we'll limit the analysis to that
domestic_movies = domestic_movies %>% select(-c("X", "link", "Opening.Weekend.Box.Office")) %>% 
  rename(c("domestic_box_office" = "Domestic.Box.Office",
          "theater_count"  = "Max.Theater.Count")) %>% 
  subset(Rank <= 11)

# international box office filenames were changed in windows system after unzipping it, : were replaced with _
domestic_movies$Movie = str_replace(domestic_movies$Movie, pattern=":", replacement="_")

# convert box office column to integer
domestic_movies$domestic_box_office = str_replace_all(domestic_movies$domestic_box_office, pattern = "[$,]", replacement="")
domestic_movies$domestic_box_office = as.integer(domestic_movies$domestic_box_office)
# str(domestic_movies)

# normalize box office earnings by dividing box office earnings by theater count
# domestic_movies$norm_earnings = domestic_movies$domestic_box_office / domestic_movies$theater_count

# ----------------------------------------------------------------

# load all the cast files for each of the movies
cast_files = list.files("cast_crew")
cast_files = cast_files[cast_files %>% str_detect(pattern="20[0-9][0-9]_[0-9]_")]

cast = data.frame()

for (file in cast_files) {
  df = read.csv(paste("cast_crew/", file, sep=""))
  df = df %>% select(-c("X","X1")) %>% rename(c("name" = "X0", "character_name" = "X2"))
  df$filename = file
  cast = rbind(cast, df)
}

# we need to match and replace some special characters in names
# alpha chars between special characters but more than 1
# examples: Chris 'Ludacris' Bridges, Daisuke 'Dice' Tsutsumi
# don't treat names like: J.K. Rowling
cast$name = str_replace_all(cast$name, pattern="[*]", replacement="")
cast$name = str_replace(cast$name, pattern="[:punct:][:alpha:]{2,}[:punct:]", replacement="")

# we'll only select the lead cast members for this case
lead_cast_names = cast %>% subset(type == "Leading Cast" | type == "Lead Ensemble Members")

all_names_json = lead_cast_names %>% select(name) %>% unique() %>% list()

# Namesor API key goes here
api_key = "<INSERT API KEY>"
gender_base = "https://v2.namsor.com/NamSorAPIv2/api2/json/genderFullBatch"

payload = c("personalNames" = all_names_json)
payload_json = toJSON(payload)

response = POST(gender_base, body=payload_json, add_headers("Accept" = "application/json", "Content-Type" = "application/json",
                                                           "X-API-KEY" = api_key))
parsed_gender_result = fromJSON(content(response, "text"))$personalNames
gender_dataset = parsed_gender_result %>% select(c("name", "likelyGender"))

# no missing values
cast_with_gender = lead_cast_names %>% left_join(gender_dataset, by=c("name"="name"))
# write.csv(cast_with_gender, file="all_cast_with_gender.csv")

# sum(is.na(cast_with_gender))

# --------------------------------------------------------------------
# Namesor api endpoint to get US ethnicity category
ethnicity_base = "https://v2.namsor.com/NamSorAPIv2/api2/json/usRaceEthnicityBatch"

# we'll need to split the name into first and last names for this case
unique_names = lead_cast_names %>% select(name) %>% unique()
all_names_split = str_replace(unique_names$name, pattern="[,].+|II", replacement="")
all_names_split = as.data.frame(all_names_split)
names(all_names_split) = c("name")

all_names_split = separate(all_names_split, name, into = c("firstName", "lastName"), sep=" ", extra="merge")
all_names_split$last_name = replace_na(all_names_split$last_name, "")
all_names_split$last_name = str_replace(all_names_split$last_name, pattern=".+[:space:]", replacement="")

# we'll send the names as payload to the api call
first_last = all_names_split %>% list()
payload = c("personalNames" = first_last)
payload_json = toJSON(payload)

response = POST(ethnicity_base, body=payload_json, add_headers("Accept" = "application/json", "Content-Type" = "application/json",
                                                            "X-API-KEY" = api_key))
parsed_eth_result = fromJSON(content(response, "text"))$personalNames

ethnicity_dataset = parsed_eth_result %>% select(c("firstName", "lastName", "raceEthnicity"))

names_df = as.data.frame(unique_names)
names(names_df) = c("name")

# we'll first left join the ethnicity dataset to the split name dataset just to ensure that they are in order
ethnicity_names = all_names_split %>% left_join(ethnicity_dataset, by=c("firstName"="firstName", "lastName"="lastName"))
ethnicity_names$fullName = unique_names$name

# combine with the previous gender information and export
cast_bio = cast_with_gender %>% left_join(ethnicity_names, by=c("name"="fullName"))
write.csv(cast_bio, file="cast_with_gender_and_ethnicity.csv")
# --------------------------------------------------------------------
# gather all international box office information for the top 11 movies
int_box_files = list.files("box_office_international")
# For international box office, we only have information on top 11 movies from 2010-2022, so we'll limit the analysis to that
int_box_files = int_box_files[int_box_files %>% str_detect(pattern = "20[0-9][0-9]_[0-9]_")]

int_box_office = data.frame()

countries = c()

for (file in int_box_files) {
  df = read.csv(paste("box_office_international/", file, sep=""))
  if (!"Territory" %in% colnames(df)) next
  # if X is in colnames in the first position, the first column names are shifted by 1. Instance in 2014_4_The Lego Movie.csv
  if (class(df$X) != "integer") {
    test = df[, -ncol(df)]
    colnames(test) = names(df)[-1]
    df = test
    
  }
  df$Maximum.Screens = as.integer(df$Maximum.Screens)
  df = df %>% drop_na()
  df = df %>% select(-c("Opening.Weekend", "Opening.Weekend.Screens", "Theatrical.Engagements", "Release.Date", "Report.Date"))
  if ("X" %in% colnames(df)) df = df %>% select(-c("X")) 
  df = df %>% rename(c("max_screens"="Maximum.Screens", "box_office"="Total.Box.Office"))
  df$box_office = as.integer(str_replace_all(df$box_office, pattern="[$,]", replacement=""))
  df$max_screens = as.integer(df$max_screens)
  df$filename = file
  int_box_office = rbind(int_box_office, df)
  
  # countries = c(countries, df$Territory)
}
# unique(countries)

# remove all instances where box office value is 0. It doesn't make sense
int_box_office = int_box_office %>% subset(box_office != 0)
# unique(int_box_office$Territory)

int_box_office$movie = str_extract(int_box_office$filename, pattern="(?<=_[0-9]_).+(?=\\.)")

box_office_merged = int_box_office %>% left_join(domestic_movies, by=c("movie" = "Movie"))

best_international = box_office_merged %>% group_by(movie) %>% slice_max(box_office) %>% arrange(filename)

# bi_grouped = best_international %>% group_by(Territory) %>% summarize(total=sum(box_office))

# -------------------------------------------------------------------
# get supporting cast for movies in China and UK
movies_in_both = int_box_office %>% subset(Territory == "China" | Territory == "United Kingdom") %>% 
  group_by(filename) %>% summarize(count = n()) %>% subset(count == 2)

cast_china_uk = cast %>% subset(filename %in% movies_in_both$filename) %>% subset(type == "Supporting Cast")

lead_cast_china_uk = cast %>% subset(filename %in% movies_in_both$filename) %>%
  subset(type == "Leading Cast" | type == "Lead Ensemble Members")

# --
unique_names = cast_china_uk %>% select(name) %>% unique()
all_names_split = str_replace(unique_names$name, pattern="[,].+|II", replacement="")
all_names_split = as.data.frame(all_names_split)
names(all_names_split) = c("name")

all_names_split = separate(all_names_split, name, into = c("firstName", "lastName"), sep=" ", extra="merge")
all_names_split$last_name = replace_na(all_names_split$last_name, "")
all_names_split$last_name = str_replace(all_names_split$last_name, pattern=".+[:space:]", replacement="")

all_names_split[is.na(all_names_split$lastName),]$lastName = ""

first_last = all_names_split %>% list()
payload = c("personalNames" = first_last)
payload_json = toJSON(payload)

response = POST(ethnicity_base, body=payload_json, add_headers("Accept" = "application/json", "Content-Type" = "application/json",
                                                               "X-API-KEY" = api_key))
parsed_eth_result = fromJSON(content(response, "text"))$personalNames

ethnicity_dataset = parsed_eth_result %>% select(c("firstName", "lastName", "raceEthnicity"))

names_df = as.data.frame(unique_names)
names(names_df) = c("name")

# we'll first left join the ethnicity dataset to the split name dataset just to ensure that they are in order
ethnicity_names = all_names_split %>% left_join(ethnicity_dataset, by=c("firstName"="firstName", "lastName"="lastName"))
ethnicity_names$fullName = unique_names$name
write.csv(ethnicity_names, file="supporting_cast_ethnicity.csv")


# ------------------------------------

all_names_json = cast_china_uk %>% select(name) %>% unique() %>% list()

payload = c("personalNames" = all_names_json)
payload_json = toJSON(payload)

response = POST(gender_base, body=payload_json, add_headers("Accept" = "application/json", "Content-Type" = "application/json",
                                                            "X-API-KEY" = api_key))
parsed_gender_result = fromJSON(content(response, "text"))$personalNames
gender_dataset = parsed_gender_result %>% select(c("name", "likelyGender"))

# no missing values

cast_bio = gender_dataset %>% left_join(ethnicity_names, by=c("name"="fullName"))
write.csv(cast_bio, file="supporting_cast_china_uk.csv")
# ---------------------------------------------

lead_cast_with_gender_ethnicity = read.csv("cast_with_gender_and_ethnicity.csv")
supporting_cast_uk_china = read.csv("supporting_cast_china_uk.csv")

all_cast_china_uk = cast %>% subset(filename %in% movies_in_both$filename) %>% subset(type == "Leading Cast" | type == "Lead Ensemble Members" | type == "Supporting Cast")

all_cuk = as.data.frame(all_cast_china_uk$name %>% unique())
colnames(all_cuk) = c("name")

all_bio = rbind(lead_cast_with_gender_ethnicity %>% 
  select(c("name", "likelyGender", "raceEthnicity")),
supporting_cast_uk_china %>% 
  select(c("name", "likelyGender", "raceEthnicity")))

all_bio = all_bio %>% distinct(name, .keep_all = TRUE)

leads_supporting_china_uk = all_cuk %>% left_join(all_bio, by=c("name"="name"))

sum(is.na(leads_supporting_china_uk))
leads_supporting_china_uk[is.na(leads_supporting_china_uk$raceEthnicity),]

all_cast_china_uk_bio = all_cast_china_uk %>% left_join(leads_supporting_china_uk, by=c("name"="name"))
all_cast_china_uk_bio = all_cast_china_uk_bio %>% mutate(raceEthnicity = ifelse(name == "Zendaya", "B_NL", raceEthnicity))
all_cast_china_uk_bio = all_cast_china_uk_bio %>% mutate(raceEthnicity = ifelse(name == "Awkwafina", "A", raceEthnicity))

sum(is.na(all_cast_china_uk_bio))

write.csv(file="leads_supporting_china_uk.csv", all_cast_china_uk_bio)
all_cast_china_uk_bio
# --------------------------------------------------------------

china_uk = int_box_office %>% subset(Territory == "China" | Territory == "United Kingdom") %>% 
  subset(filename %in% movies_in_both$filename)

avg_screens_china = china_uk %>% subset(Territory == "China" & max_screens != 0) %>% pull(max_screens) %>% mean() %>% as.integer()
avg_screens_uk = china_uk %>% subset(Territory == "United Kingdom" & max_screens != 0) %>% pull(max_screens) %>% mean() %>% as.integer()

china_uk = china_uk %>% mutate(
  norm = ifelse(Territory == "United Kingdom", box_office / 68920787,
    ifelse(Territory == "China", box_office / 1455390680, NA))
)

# ------------------------

ggplot(china_uk) +
  geom_histogram(aes(x = box_office, fill = Territory), alpha = 0.75, bins=30) +
  scale_fill_manual(values = c("indianred4", "dodgerblue4"),
                    labels = c("China", "UK"),
                    name = "Territory") +
  labs(x = "Box Office Earnings Per Capita",
       y = "Count of Movies",
       title = "Distribution of Box Office Earnings Per Capita in China vs UK") +
  theme_minimal()
# ----------------------

demography_china_uk = all_cast_china_uk_bio %>% group_by(filename, likelyGender, raceEthnicity) %>% 
  summarize(total=n()) %>% 
  pivot_wider(id_cols = c("filename"),
              names_from = c("raceEthnicity", "likelyGender"),
              values_from = total)
demography_china_uk[is.na(demography_china_uk)] = 0
demography_china_uk

# ----------------------------
demo_cuk = china_uk %>% left_join(demography_china_uk, by=c("filename"="filename"))
demo_cuk = demo_cuk %>% left_join(domestic_movies %>% select(c("Movie", "domestic_box_office")),
                       by=c("movie"="Movie"))
demo_cuk$diff = demo_cuk$box_office - demo_cuk$domestic_box_office

demo_cuk_agg = demo_cuk %>% mutate(white = W_NL_male + W_NL_female,
                  minorities = B_NL_male + HL_male + A_male +
                    B_NL_female + HL_female + A_female,
                    male = W_NL_male + B_NL_male + HL_male + A_male,
                    female = W_NL_female + B_NL_female + HL_female + A_female)

# --------------------------------------
domestic_leads_wide = lead_cast_with_gender_ethnicity %>% group_by(filename, likelyGender, raceEthnicity) %>% 
  summarize(total=n()) %>% 
  pivot_wider(id_cols = c("filename"),
              names_from = c("raceEthnicity", "likelyGender"),
              values_from = total)
domestic_leads_wide[is.na(domestic_leads_wide)] = 0

domestic_leads_wide$movie = str_extract(domestic_leads_wide$filename, pattern="(?<=_[0-9]_).+(?=\\.)")

domestic_data = domestic_leads_wide %>% left_join(domestic_movies %>% select(c("Movie", "domestic_box_office")), by=c("movie" = "Movie"))
domestic_data = domestic_data %>% mutate(white = W_NL_male + W_NL_female,
                                         minorities = B_NL_male + HL_male + A_male +
                                         B_NL_female + HL_female + A_female)

# -------------------------------------------

demography_leads_china_uk = lead_cast_china_uk %>% group_by(filename, likelyGender, raceEthnicity) %>% 
  summarize(total=n()) %>% 
  pivot_wider(id_cols = c("filename"),
              names_from = c("raceEthnicity", "likelyGender"),
              values_from = total)
demography_leads_china_uk[is.na(demography_leads_china_uk)] = 0
demography_leads_china_uk

demo_leads_agg = china_uk %>% left_join(demography_leads_china_uk, by=c("filename"="filename")) %>% 
  left_join(domestic_movies %>% select(Movie, domestic_box_office), by=c("movie"="Movie"))
demo_leads_agg = demo_leads_agg %>% mutate(white = W_NL_male + W_NL_female,
                                         minorities = B_NL_male + HL_male + A_male +
                                           B_NL_female + HL_female + A_female)
demo_leads_agg$diff = demo_leads_agg$box_office - demo_leads_agg$domestic_box_office
