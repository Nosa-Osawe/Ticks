require(tidyverse)
require(readxl)

lice <- read_excel("C:\\Users\\DELL\\Documents\\Git in R\\Ticks\\Data\\Lice.xlsx",
                   sheet = "workingsheet")
view(lice)
