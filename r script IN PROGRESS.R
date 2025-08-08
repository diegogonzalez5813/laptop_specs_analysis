library(tidyverse)
#setting up datasets
laptop <- read_csv("data/laptop.csv")
view(laptop)
cpu <- read_csv("data/cpu.csv")
view(cpu)
gpu <- read_csv("data/gpu.csv")
view(gpu)
#standarizing cpu and gpu names in laptop.csv
##the functions and code were made with help from AI
###hrs of my learning,writing own code, and debugging, not in a copy and paste method
laptop_clean <- laptop |>
  mutate(
     cpu_block = str_extract(Processor_Name, "Intel Core i[3579]"), 
     generation = case_when(
       str_detect(Processor_Name, "([0-9]+)th") ~ str_extract(Processor_Name, "([0-9]{1,2})"),
       TRUE ~ NA_character_
    ),
    cleaned_cpu = paste(cpu_block, generation, sep = "-") |>
      str_trim())
    
#cleaning the cpu mark dataset for merge
cpu_clean_data <- cpu |>
  mutate(cpu_block = str_extract(`CPU Name`, "Intel Core i[3579]"), 
    generation = case_when(
      str_detect(`CPU Name`, "i[3579]-([0-9]{1,2})") ~ str_extract(`CPU Name`, "([0-9]{1})"),
      TRUE ~ NA_character_
    ),
    cleaned_cpu = paste(cpu_block, generation, sep = "-")
    )|>
  select(cleaned_cpu, `CPU Mark
`) |>
  distinct(cleaned_cpu, .keep_all = TRUE)
#merging finally
merged_data <- laptop_clean |>
  left_join(cpu_clean_data, by = "cleaned_cpu")
merged_data <-
  merged_data |>
  rename(cpu_mark = `CPU Mark
         `)
#max and weighing formulas!
#

max_scores <- list(
  cpu = 60600,
  ram = 500,
  hard_drive = 3
)

weights <- list(
  cpu = .7, 
  ram = .3, 
  harddrive = .18
)
final_data <- merged_data |>
  mutate(
    ram_score = case_when(
      RAM == "4 GB" ~ 100,
      RAM == "8 GB" ~ 200,
      RAM == "16 GB" ~ 300,
      RAM == "32 GB" ~ 400,
      RAM == "64 GB" ~ 500,
      TRUE ~ 0
    ),
    hard_drive_score = case_when(
      SSD == "1024 GB SSD Storage" ~ 1,
      SSD == "2048 GB SSD Storage" ~ 2,
      SSD == "512 GB SSD Storage" ~ 0.5,
      TRUE ~ 0),
cpu_norm = (cpu_mark / max_scores$cpu) * 100,
ram_norm = (ram_score / max_scores$ram) * 100,
cpu_weighted = cpu_norm * weights$cpu,
ram_weighted = ram_norm * weights$ram,
Performance_Grade = cpu_weighted + ram_weighted
)

library(ggplot2)  
ggplot(data = final_data, aes(x = Price, y = Performance_Grade)) + geom_point()
