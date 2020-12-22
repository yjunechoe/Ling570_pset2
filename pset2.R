set.seed(570)

library(tidyverse)
library(furrr)
plan(multisession, workers = availableCores() - 1)

# Load data

data <- read_file(here::here("PS2", "mother.speech.txt")) %>% 
  str_split("W") %>% 
  `[[`(1) %>% 
  str_split("(P|S)") %>% 
  future_map(~ discard(.x, ~str_length(.) == 0))

# Create transition matrix

parse_transitions <- function(){
  
  transitions_list <- list()
  counter <- 0
  
  for (word_id in 1:length(data)) {
    word <- data[[word_id]]
    for (segment_id in 1:length(word)) {
      counter <- counter + 1
      if (segment_id == 1) {
        transitions_list[[counter]] <- list(before = "W", after = word[[segment_id]])
        if (length(word) > 1){
          counter <- counter + 1
          transitions_list[[counter]] <- list(before = word[[segment_id]], after = word[[segment_id + 1]])
        }
      } else if (segment_id == length(word)) {
        transitions_list[[counter]] <- list(before = word[[segment_id]], after = "W")
      } else {
        transitions_list[[counter]] <- list(before = word[[segment_id]], after = word[[segment_id + 1]])
      }
    }
  }
  
  transitions_list
}

transitions <- parse_transitions() %>% 
  transpose() %>% 
  as_tibble() %>% 
  mutate_all(flatten_chr)

segments <- sort(c("W", unique(flatten_chr(data))))

transitions_matrix <- crossing(after = segments, before = segments) %>% 
  left_join(count(transitions, after, before)) %>% 
  spread(before, n, fill = 0) %>% 
  select(-1) %>% 
  as.matrix()


# Algorithm

testing_data <- flatten_chr(data)

test <- function(){
  output <- list()
  cur_segment <- "W"
  
  for (i in 1:length(testing_data)){
    new_segment <- sample(segments, 1, prob = transitions_matrix[, cur_segment])
    output[[i]] <- new_segment
    cur_segment <- new_segment
  }
  
  output <- unique(str_split(paste(flatten_chr(output), collapse = ""), "W")[[1]])
  
  output
}

# Evaluation

gold <- data %>%
  map_chr(~paste(.x, collapse = "")) %>% 
  unique()

eval_algo <- function(output){
  correct <- intersect(output, gold)
  
  precision <- length(correct)/length(output)
  recall <- length(correct)/length(gold)
  F1 <- 2 * (precision * recall)/(precision + recall)
  
  tibble(
    Precision = precision,
    Recall = recall,
    F1 = F1
  )
}

eval_algo(test())