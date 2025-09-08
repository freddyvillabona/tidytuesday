
  library(ggplot2)
  library(dplyr)
  library(hrbrthemes)
  library(viridis)
  library(readxl)
  library(patchwork) 

note <- read_excel("note.xlsx")

#################
not <- note %>%
  group_by(Semester, kk) %>% 
  summarise(mean = mean(Note), sum = sum(Note), n = n())

not <- not %>% 
  arrange(kk)

not$kk <- as.factor(not$kk)

lb <- c("HS2018", "FS2019", "HS2019",
        "FS2020", "HS2020", "FS2021",
        "HS2021")


###################
not2 <- note %>%
  group_by(Semester, kk) %>% 
  summarise(sum = sum(ECTS), n = n())

not2 <- not2 %>% 
  arrange(kk)
names(not2)[3] = "ECTS"

pal <- viridis_pal()(7)


###################
note <- read_excel("note.xlsx",5)
note$month <- as.Date(note$month)

