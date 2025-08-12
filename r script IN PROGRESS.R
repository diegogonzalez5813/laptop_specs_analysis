library(tidyverse)
#setting up datasets
laptop <- read_csv("data/laptop.csv")
view(laptop)
cpu <- read_csv("data/cpu.csv")
view(cpu)
gpu <- read_csv("data/gpu.csv")
view(gpu)

##the functions and code were made with help from AI
###hrs of my learning,writing own code, and debugging, not in a copy and paste method

#cleaning laptop.csv dataset for merge

laptop_clean <- laptop |>
  mutate(
    cleaned_cpu = case_when(
      str_detect(Processor_Name, "Intel Core i[0-9].*\\(([4-9]{1})th Gen\\)") ~
        str_extract(Processor_Name, "Intel Core i[0-9]")|>
        paste0("-", str_extract(Processor_Name, "(?<=\\()([4-9]{1})")),
      
      TRUE ~ NA_character_
  ))
#cleaning the cpu mark dataset for merge

cpu_clean_data <- cpu |>
  rename(cpu_name = 'CPU Name') |>
  mutate(
    cleaned_cpu = case_when(
      str_detect(cpu_name, "Intel Core i[0-9]-([4-9]{1})") ~
        str_extract(cpu_name, "Intel Core i[0-9]-([4-9]{1})"),
      TRUE ~ NA_character_
    ) 
    )

#finding the mean of cpu_marks for generation models
average_cpu_scores <- cpu_clean_data |>
  group_by(cleaned_cpu) |>
  summarise(
    avg_cpu_mark = mean(`CPU Mark
`, na.rm = TRUE)
  )

#merging
merged_clean_data <- laptop_clean |>
  left_join(average_cpu_scores, by = "cleaned_cpu")

#removing rows with na from merging_clean_data
final_data <- merged_clean_data |>
  drop_na(cleaned_cpu)

#lets pause to celebrate :)

#max,norm, weighing, and performance grade formulas!
max_scores <- list(
  cpu = 21029.467,
  ram = 400,
  hard_drive = 60
)

weights <- list(
  cpu = .7, 
  ram = .3, 
  hard_drive = .18
)
fd_pf <- final_data |>
  mutate(
    ram_score = case_when(
      RAM == "4 GB" ~ 100,
      RAM == "8 GB" ~ 200,
      RAM == "16 GB" ~ 300,
      RAM == "32 GB" ~ 400,
      TRUE ~ 0
    ),
    hard_drive_score = case_when(
      SSD == "1024 GB SSD Storage" ~ 60,
      SSD == "512 GB SSD Storage" ~ 50,
      SSD == "256 GB SSD Storage" ~ 40,
      SSD == "128 GB SSD Storage" ~ 15,
      SSD == "64 GB SSD Storage" ~ 10,
      SSD == "16 GB SSD Storage" ~ 5,
      SSD == "8 GB SSD Storage" ~ 1,
      TRUE ~ 0),
cpu_norm = (avg_cpu_mark / max_scores$cpu) * 100,
ram_norm = (ram_score / max_scores$ram) * 100,
hard_drive_norm = (hard_drive_score / max_scores$hard_drive) * 100,

cpu_weighted = cpu_norm * weights$cpu,
ram_weighted = ram_norm * weights$ram,
hard_drive_weighted = hard_drive_norm * weights$hard_drive,
Performance_Grade = cpu_weighted + ram_weighted + hard_drive_weighted
)

#Adding USD price column from current price column (Indian Rupees)
fd_pf_usd <- fd_pf |>
  mutate(price_usd = Price/ 87.61)

#conclusive dataset with performance grade/price variable for a 
##price unit per dollar (punit_per_usd) variable
omega_data <- fd_pf_usd |>
  mutate(punit_per_usd = Performance_Grade/ price_usd )

#lets pause to celebrate :)

#graphing using omega_data

library(ggplot2) 
#scatterplot with all laptops and top ten highlighted
ggplot() +
  geom_point(data = omega_data, mapping = aes(x = price_usd, y = Performance_Grade, position = "jitter" )) +
  geom_point(data = t10_punit_per_usd, aes(x = price_usd, y = Performance_Grade, color = name_shrt, position = "jitter")) +
  labs(x = "Price ($)", y= "Performance Grade", title = "Price vs Performance Grade", subtitle = "Top Nine Highlighted", color = "Top Nine Laptop Names")


#scatterplot with all laptops color coded by CPU
ggplot(data = omega_data, mapping = aes(x = price_usd, y = Performance_Grade, color = cleaned_cpu, position = "jitter")) + geom_point() + labs(x = "Price (USD)", y  = "Performance Grade", title = "Price vs Performance Grade", subtitle = "Color Coded by CPU", color = "CPU Name") 

#top ten scatterplot
t10_punit_per_usd <- omega_data |>
  arrange(desc(punit_per_usd)) |>
  slice_head(n = 10) |>
  mutate(name_shrt = str_trunc(`Name`, width = 20))
ggplot(data = t10_punit_per_usd, aes(x = name_shrt, y = punit_per_usd)) + 
  geom_point() + labs(x = "Laptops", y = "Performance per USD", title = "Top Ten Laptops", subtitle = "Per Performance Grade / USD Price") + coord_flip()

