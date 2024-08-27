library(tidyverse)

tick <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\Tick_working_sheet.csv")
view(tick)

# Replace all NA values with 0 using 
Tick <- replace(tick, is.na(tick), 0)
str(Tick)

sum(is.na(Tick))  # Nice and clean
view(Tick)
