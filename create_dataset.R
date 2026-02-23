library(tidyverse)
library(httr2)
library(rvest)


get_tables = function(url){
  # basic function to retrieve a table embedded in html from a url
  page = read_html(url)
  
  table_node = html_nodes(page, "table")
  
  table_content = html_table(table_node)
  
  return(table_content)
}


#2022 - 2025: https://commons.wikimedia.org/w/index.php?title=Data%3AWikipedia+statistics%2Fdata.tab&date-range-to=2024-01-01&tagfilter=&action=history
# each version is based on the 01.01 of that year
# 2026

url_2025 = "https://commons.wikimedia.org/w/index.php?title=Data:Wikipedia_statistics/data.tab&oldid=978320135"
url_2024 = "https://commons.wikimedia.org/w/index.php?title=Data:Wikipedia_statistics/data.tab&oldid=837708109"
url_2023 = "https://commons.wikimedia.org/w/index.php?title=Data:Wikipedia_statistics/data.tab&oldid=721932139"
url_2022 = "https://commons.wikimedia.org/w/index.php?title=Data:Wikipedia_statistics/data.tab&oldid=618297893"

urls_first_batch = c(url_2025, url_2024, url_2023, url_2022)
year = str_split(url_2025 %>%
                   substitute() %>%
                   deparse(),
                 pattern = "_")[[1]][2]

wiki_tbl_list = list()
year = 2025
for(url in urls_first_batch){
  
  # get table
  tbl = get_tables(url)[[1]]

  # filter to wikipedia data
  tbl = tbl %>%
    rowwise() %>%
    mutate(lang_code = str_split(site, "\\.")[[1]][1],
           type = str_split(site, "\\.")[[1]][2]) %>%
    filter(type == "wikipedia")
  
  # assign year
  tbl$year = year
  
  
  wiki_tbl_list[[as.character(year)]] = tbl
  year = year - 1
  
}


tbl_2022_2025 = bind_rows(wiki_tbl_list)

# 2007 - 2022: https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&action=history --> multiple tables that need to be merged

url_2021 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=20916592"
url_2020 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=19673461"
url_2019 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=18750621"
url_2018 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=17590233"
url_2017 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=16199125"
url_2016 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=15204715"
url_2015 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=10868650"
url_2014 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=6908984"
url_2013 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=4951480"
url_2012 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=3198344"
url_2011 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=2263676"
url_2010 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=1786773"
url_2009 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=1329067"
url_2008 = "https://meta.wikimedia.org/w/index.php?title=List_of_Wikipedias/Table&oldid=814294"


#### url 2021
make_all_character = function(tab){
  i = 0
  for (i in 1:length(tab)){
    tab[i][[1]] = tab[i][[1]] %>%
      mutate(across(everything(), as.character))

    i = i + 1
   
  }
  return(tab)
}


join_tabs = function(tabs){
  tabs = tabs %>%
      make_all_character()
  joint_df = bind_rows(tabs)
  return(joint_df)
}


urls_second_batch = c(url_2021, url_2020, url_2019, url_2018, url_2017, url_2016, url_2015, url_2014, url_2013, url_2012, url_2011, url_2010, url_2009, url_2008)

wiki_tbl_list = list()
year = 2021
for(url in urls_second_batch){
  
  # get table
  tbl = get_tables(url)

  # join tables
  tbl = join_tabs(tbl)
  
  # assign year
  tbl$year = year
  print(year)
  
  wiki_tbl_list[[as.character(year)]] = tbl
  year = year - 1
  
}


tbl_2008_2021 = bind_rows(wiki_tbl_list)

# join the two tables
tbl_2008_2021 = tbl_2008_2021 %>%
  select(Wiki, Articles, Edits, Admins, Users, `Active Users`, Files, year) %>%
  rename( "language_code" = Wiki,
          "articles" = Articles,
          "edits" = Edits,
          "admins" = Admins,
          "users" = Users,
          "active_users" = `Active Users`,
          "files" = Files) 

# remove doublet from 2011
tbl_2008_2021 = tbl_2008_2021 %>%
  group_by(language_code, year) %>%
  summarize(articles = first(articles),
            edits = first(edits),
            admins = first(admins),
            users = first(users),
            active_users = first(active_users),
            files = first(files)) %>% 
  ungroup()


full_tbl = tbl_2022_2025 %>%
  select(lang_code, articles, edits, admins, users, activeusers, files, year) %>%
  rename("language_code" = lang_code,
         "active_users" = activeusers) %>%
  rbind(tbl_2008_2021)

# turn into df
full_tbl = full_tbl %>%
  as.data.frame()

  
# inspect missing language codes
full_tbl %>%
    filter(is.na(language_code)) # missing language codes is equivalent to the grand table total --> simply remove

full_tbl = full_tbl %>%
  filter(!is.na(language_code))

# remove cases of aggregations

full_tbl = full_tbl %>%
  filter(!language_code %in% c("total", "totalclosed", "totalactive"))

#### add iso codes #####
iso = read_csv("data/raw/wiki_ling_div_language_data.csv")

# join isocode table with article data
tbl_iso = iso %>%
  select(language_code, ISO6393) %>%
  right_join(full_tbl , join_by(language_code))

# inspect cases lacking corresponding ISOcode
tbl_iso %>%
  filter(is.na(ISO6393)) %>%
  distinct(language_code, ISO6393) %>%
  head()

# assign to correct iso code
tbl_iso = tbl_iso %>%
  mutate(ISO6393 = case_when(language_code == "zh-min-nan" ~ "nan",
                             language_code == "zh-yue" ~ "yue",
                             language_code == "ku" ~ "ckb",
                             language_code == "als" ~ "gsw",
                             language_code == "bat-smg" ~ "sgs",
                             language_code == "zh-classical" ~ "lzh",
                             language_code == "fiu-vro" ~ "vro",
                             language_code == "kv" ~ "koi",
                             language_code == "roa-rup" ~ "rup",
                             language_code == "be-tarask" ~ "bel",
                             language_code == "simple" ~ "eng",
                             language_code == "map-bms" ~ "jav",
                             language_code == "roa-tara" ~ "nap",
                             language_code == "knc" ~ "knc",
                             language_code == "nup" ~ "nup",
                             language_code == "rki" ~ "rki",
                             language_code == "syl" ~ "syl",
                             language_code == "tok" ~ "tok",
                             language_code == "tokipona" ~ "tok",
                             language_code == "mo" ~ "ron",
                             language_code == "be-x-old" ~ "be",
                             language_code == "tlh" ~ "tlh",
                             TRUE ~ ISO6393))

# make columns numeric and aggregate cases with multiple wikipedias per isocode
tbl_iso = tbl_iso %>%
  select(!language_code) %>%
  mutate(across(.cols = !c(ISO6393, year), ~gsub(",", "", .x))) %>%
  mutate(across(.cols = !c(ISO6393, year), ~gsub(" ", "", .x))) %>%
  mutate(across(.cols = !c(ISO6393, year), as.numeric)) %>%
  group_by(ISO6393, year) %>%
  summarize(articles = sum(articles),
            edits = sum(edits),
            admins = sum(admins),
            users = sum(users),
            active_users = sum(active_users),
            files = sum(files)) %>% 
  ungroup()
  
# select only metrics associated with quality
tbl_iso = tbl_iso %>%
  select(ISO6393, year, articles, edits, admins)

# check NA
colSums(is.na(tbl_iso))


# create a composite index #####

# inspect correlations

cm = cor(tbl_iso %>%
           select(!c(ISO6393, year)))

print(cm) # edits and admins are most highly correlated

idx = tbl_iso %>% # z-scale measures, 1 unit = 1 sd --> average --> each dimension contributes relative to its magnitude compared to all other datapoints in the dimension
    mutate(admins_z = as.numeric(scale(admins)),
           articles_z = as.numeric(scale(articles)),
           edits_z = as.numeric(scale(edits))) %>%
    mutate(composite = (admins_z + articles_z + edits_z) / 3) %>%
    mutate(composite = (composite - min(composite)) / (max(composite) - min(composite))) %>% # min max scale --> bounded between 0 and 1
    select(ISO6393, year, admins, articles, edits, composite)

# inspect correlation
cm = cor(idx %>%
           select(!c(ISO6393, year)))
print(cm) # composite index is highly correlated with all dimensions, r > 0.88; edits has the most mechanical influence impact r = 0.95 because it is the most correlated with the others

# write
write_csv(idx, "data/processed/wikipedia_support_time_series.csv")

