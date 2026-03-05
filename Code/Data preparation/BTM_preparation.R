############################################
#### Pairwise comparisons analysis 2025 ####
####    BTM dataset preparation         ####
############################################


setwd("~/Desktop/Pairwise comparisons_study")

### packages ####
library(tidyverse)
library(ggplot2)
library(dplyr)

### loading datasets ####

aut          <-     read.csv("Data/snippets/aut-snip.csv")
bmpn         <-     read.csv("Data/snippets/bmpn-snip.csv")
dass         <-     read.csv("Data/snippets/dass-snip.csv")
democr       <-     read.csv("Data/snippets/democr-snip.csv")
ders         <-     read.csv("Data/snippets/ders-snip.csv")
height_inv   <-     read.csv("Data/snippets/height_inv-snip.csv")


### BTM dataset creation ####


# here we take the order of the display and we leave in the column only the first displayed item (meaning
# that the number of items in column ending with _DO represents the item that was displayed on the left side for participants)
# numbers in this case are as follows from the _DO column = if the column hi_3_7_DO has value 7, it means that 7 was on the left
# and 3 was on the right side

split_and_record <- function(dataframe) {
  # Get column names
  column_names <- names(dataframe)
  # Iterate over every second column
  for (i in seq(2, length(column_names), by = 2)) {
    col_name <- column_names[i]
    dataframe[[col_name]] <- sapply(strsplit(as.character(dataframe[[col_name]]), "\\|"), function(x) x[1]) 
  }
  return(dataframe)
}

BTM_aut <- split_and_record(aut[,2:43])
BTM_aut[] <- lapply(BTM_aut, as.numeric)
BTM_bmpn <- split_and_record(bmpn[,2:21])
BTM_bmpn[] <- lapply(BTM_bmpn, as.numeric)
BTM_dass <- split_and_record(dass[,2:43])
BTM_dass[] <- lapply(BTM_dass, as.numeric)
BTM_democr <- split_and_record(democr[,2:21])
BTM_democr[] <- lapply(BTM_democr, as.numeric)
BTM_ders <- split_and_record(ders[,2:43])
BTM_ders[] <- lapply(BTM_ders, as.numeric)
BTM_height_inv <- split_and_record(height_inv[,2:57])
BTM_height_inv[] <- lapply(BTM_height_inv, as.numeric)


# now, although we have item´s display included, we want to have information binarized
# so we will recode it in a way, that if item with lower ID is on the left, we will
# get value 1 and if item with higher ID is on the left, we will get value 0

# Function to find unique values and recode them for split columns
unique_and_recode_every_second <- function(dataframe) {
  # Get column names
  column_names <- names(dataframe)
  # Iterate over every second column
  for (i in seq(2, length(column_names), by = 2)) {
    col_name <- column_names[i]
    # Find unique values excluding NA
    unique_values <- unique(dataframe[[col_name]], na.rm = TRUE)
    # If there are unique values
    if (length(unique_values) > 1) {
      # Recode values
      lower_value <- min(unique_values, na.rm = TRUE)
      dataframe[[col_name]] <- ifelse(dataframe[[col_name]] == lower_value,1, ifelse(is.na(dataframe[[col_name]]), NA, 0))
    }
  }
  return(dataframe)
}

BTM_aut <- unique_and_recode_every_second(BTM_aut)
BTM_bmpn <- unique_and_recode_every_second(BTM_bmpn)
BTM_dass <- unique_and_recode_every_second(BTM_dass)
BTM_democr <- unique_and_recode_every_second(BTM_democr)
BTM_ders <- unique_and_recode_every_second(BTM_ders)
BTM_height_inv <- unique_and_recode_every_second(BTM_height_inv)


# Here we are extracting complimentary columns for the BTM analysis
# we are creating new set of columns ending on _alt where we will include
# items which were not selected in the pairwise comparison 
# basically we will then consider the primary columns to be winner columns
# and _alt columns to be loser columns

opposite_value_columns <- function(data_frame) {
  
  num_cols1 <- ncol(data_frame)
  
  for (i in seq(1, num_cols1, by = 2)) {
    # Get the column name
    col_name <- names(data_frame)[i]
    
    # Extract the numbers from the column name
    numbers <- as.numeric(unlist(regmatches(col_name, gregexpr("[0-9]+", col_name))))
    
    # Ensure there are exactly 2 numbers in the column name
    if (length(numbers) != 2) {
      warning(paste("Column", col_name, "does not contain exactly 2 numbers in the name. Skipping."))
      next  # Skip to the next column
    }
    
    # Get the unique non-NA values in the column
    unique_values <- unique(data_frame[[col_name]][!is.na(data_frame[[col_name]])])
    
    # If there are no unique values in the column, assume it's the first number
    if (length(unique_values) == 0) {
      unique_values <- c(numbers[1])
    }
    
    # If the column has only one unique value, assume the opposite value is the other number
    if (length(unique_values) == 1) {
      unique_values <- c(unique_values, setdiff(numbers, unique_values))
    }
    
    # Determine the opposite value for each unique value
    opposite_values <- ifelse(unique_values == unique_values[1], unique_values[2], unique_values[1])
    
    # Replace each value in the column with its opposite value
    data_frame[[paste0(col_name, "_alt")]] <- ifelse(is.na(data_frame[[col_name]]), NA, opposite_values[match(data_frame[[col_name]], unique_values)])
  }
  
  return(data_frame)
}

BTM_aut        <- opposite_value_columns(BTM_aut)
BTM_bmpn       <- opposite_value_columns(BTM_bmpn)
BTM_dass       <- opposite_value_columns(BTM_dass)
BTM_democr     <- opposite_value_columns(BTM_democr)
BTM_ders       <- opposite_value_columns(BTM_ders)
BTM_height_inv <- opposite_value_columns(BTM_height_inv)



# rename columns by winners and losers (_alt)
# winners have wx columns, losers have lx columns and order has rx column
rename_cols <- function(dataframe) {
  
  num_cols1 <- ncol(dataframe)
  
  for (i in seq(1, num_cols1, by = 2)) {
    # Get the current column name
    current_col <- names(dataframe)[i]
    
    # Rename the column to "w"
    names(dataframe)[i] <- paste0("w", (i + 1) %/% 2)
  }
  for (i in seq(2, num_cols1, by = 2)) {
    # Get the current column name
    current_col <- names(dataframe)[i]
    
    # Rename the column to "r"
    names(dataframe)[i] <- paste0("r", (i + 1) %/% 2)
  }
  
  # Rename the last third of the columns with "l" prefix
  last_third_start <- ceiling(2 * num_cols1 / 3 + 1)
  for (i in last_third_start:num_cols1) {
    current_col <- names(dataframe)[i]
    names(dataframe)[i] <- paste0("l", i - last_third_start + 1)
  }
  return(dataframe)
}


BTM_aut        <- rename_cols(BTM_aut)
BTM_bmpn       <- rename_cols(BTM_bmpn)
BTM_dass       <- rename_cols(BTM_dass)
BTM_democr     <- rename_cols(BTM_democr)
BTM_ders       <- rename_cols(BTM_ders)
BTM_height_inv <- rename_cols(BTM_height_inv)

# connect winner columns into one and loser columns into one
connecting_columns <- function(dataframe) { 
  df <- dataframe %>%
    mutate(row_id = row_number()) %>%
    gather(key, value, -row_id) %>%
    select(-row_id)
  # Extract w, l and r columns
  df_w <- df %>%
    filter(grepl("^w", key)) %>%
    select(value) %>%
    rename(w = value)
  
  df_l <- df %>%
    filter(grepl("^l", key)) %>%
    select(value) %>%
    rename(l = value)
  
  df_r <- df %>%
    filter(grepl("^r", key)) %>%
    select(value) %>%
    rename(r = value)
  
  # Combine w and l columns
  result <- data.frame(w = df_w$w, l = df_l$l, r = df_r$r)
}

BTM_aut        <- connecting_columns(BTM_aut)
BTM_bmpn       <- connecting_columns(BTM_bmpn)
BTM_dass       <- connecting_columns(BTM_dass)
BTM_democr     <- connecting_columns(BTM_democr)
BTM_ders       <- connecting_columns(BTM_ders)
BTM_height_inv <- connecting_columns(BTM_height_inv)

### Transitivity assumptions ####

plot(cbind(table(BTM_aut$w), 
           table(BTM_aut$l)), xlab = "Winner", ylab = "Loser", main = "Winner-Loser comparison: AUT") + # we seem linear, nice!!!
  text(table(BTM_aut$w), table(BTM_aut$l),
      pos = 4, cex = 0.8)

plot(cbind(table(BTM_bmpn$w), 
           table(BTM_bmpn$l)), xlab = "Winner", ylab = "Loser", main = "Winner-Loser comparison: BMPN")+ # we seem linear, nice!!!
  text(table(BTM_bmpn$w), table(BTM_bmpn$l),
       pos = 4, cex = 0.8)

plot(cbind(table(BTM_dass$w), 
           table(BTM_dass$l)), xlab = "Winner", ylab = "Loser", main = "Winner-Loser comparison: DASS")+ # we seem linear, nice!!!
  text(table(BTM_dass$w), table(BTM_dass$l),
       pos = 4, cex = 0.8)

plot(cbind(table(BTM_democr$w), 
           table(BTM_democr$l)), xlab = "Winner", ylab = "Loser", main = "Winner-Loser comparison: DEM")+ # we seem linear, nice!!!
  text(table(BTM_democr$w), table(BTM_democr$l),
       pos = 4, cex = 0.8)

plot(cbind(table(BTM_ders$w), 
           table(BTM_ders$l)), xlab = "Winner", ylab = "Loser", main = "Winner-Loser comparison: DERS")+ # we seem linear, nice!!!
  text(table(BTM_ders$w), table(BTM_ders$l),
       pos = 4, cex = 0.8)

plot(cbind(table(BTM_height_inv$w), 
           table(BTM_height_inv$l)), xlab = "Winner", ylab = "Loser", main = "Winner-Loser comparison: HI")+ # we seem linear, nice!!!
  text(table(BTM_height_inv$w), table(BTM_height_inv$l),
       pos = 4, cex = 0.8)

# Transitivity check function
bt_matrix <- function(df, winner_col = "w", loser_col = "l") {
  df <- df[complete.cases(df[[winner_col]], df[[loser_col]]), ]
  # Compute total wins per item
  wins <- table(df[[winner_col]])
  losses <- table(df[[loser_col]])
  items <- sort(unique(c(names(wins), names(losses))))
  total_wins <- sapply(items, function(x) wins[x] %||% 0)  # %||% from rlang: if NA -> 0
  items <- items[order(total_wins, decreasing = TRUE)]      # sort by wins
  
  # Initialize matrix
  k <- length(items)
  mat <- matrix(NA, nrow = k, ncol = k)
  rownames(mat) <- colnames(mat) <- items
  
  # Fill matrix
  for(i in 1:k){
    for(j in 1:k){
      if(i == j) next
      wins_ij <- sum(df[[winner_col]] == items[i] & df[[loser_col]] == items[j])
      wins_ji <- sum(df[[winner_col]] == items[j] & df[[loser_col]] == items[i])
      total <- wins_ij + wins_ji
      if(total == 0) next
      if(i > j) mat[i,j] <- wins_ij / total
      if(i < j) mat[i,j] <- wins_ij / total
    }
  }
  return(mat)
}

# checking for transitivity issues in scales
bt_matrix(BTM_aut)
bt_matrix(BTM_ders)
bt_matrix(BTM_dass)
bt_matrix(BTM_democr) # welp, not ideal
bt_matrix(BTM_bmpn)
bt_matrix(BTM_height_inv)

### CODEBOOKS for BTM transformation ####

# 7-statement scales (ders, dass, aut)
ID <- c(01,02,03,04,05,06,07)
Item_names <- c("first","second","third","fourth","fifth","sixth","seventh")
Item <- c("V1", "V2", "V3","V4", "V5", "V6","V7")
code_seven <- data_frame(ID,Item, Item_names)


# 5-statement scales (dem, bmpn)
ID <- c(01,02,03,04,05)
Item_names <- c("first","second","third","fourth","fifth")
Item <- c("V1", "V2", "V3","V4", "V5")
code_fifth <- data_frame(ID,Item, Item_names)


# 8-statement heitght inventory
ID <- c(01,02,03,04,05,06,07,08)
Item_names <- c("men","women","reach","hints","view_obstruction","short_beds","bow_down", "low_doorpost")
Item <- c("V1", "V2", "V3","V4", "V5", "V6","V7", "V8")
code_hi <- data_frame(ID,Item, Item_names)





### preparation of data matrix for BTM
matrix_creation <- function(data, code){ 
  bt_mat <- matrix(0, nrow = nrow(data), ncol = nrow(code), dimnames = list(NULL, code$Item))
  bt_mat <- cbind(data, bt_mat)
  bt_mat <- as.data.frame(bt_mat)
}

bt_mat_aut        <- matrix_creation(BTM_aut, code_seven) # missing in 457, 1053, 1057 - IDs 33 a 37
bt_mat_bmpn       <- matrix_creation(BTM_bmpn, code_fifth)
bt_mat_dass       <- matrix_creation(BTM_dass, code_seven)
bt_mat_democr     <- matrix_creation(BTM_democr, code_fifth)
bt_mat_ders       <- matrix_creation(BTM_ders, code_seven)
bt_mat_height_inv <- matrix_creation(BTM_height_inv, code_hi)

# here we basically provide some variation using order of display - this order (which is supposed to be random) is binarized in column r. 1 means, that in paired comparison item with lower id was on the left side, 0 means that this item was on the right side 
# what we actually do is just switch the values in the matrix so that from this order variable r we get response variable r - relating to item that contains 1 in the matrix. 1 means that it won in the comparison, 0 means that it lost

# 7-statement scales (ders, dass, aut)
matrix_finalization_seven <- function(data) {
  for (i in 1:7) {
    # First, handle the case when data$r == 1
    if (any(data$r == 1 & data$w == i)) {
      data[data$r == 1 & data$w == i, 3 + i] <- 1
    }
    if (any(data$r == 1 & data$l == i)) {
      data[data$r == 1 & data$l == i, 3 + i] <- -1
    }
    
    # Then, handle the case when data$r == 0
    if (any(data$r == 0 & data$l == i)) {
      data[data$r == 0 & data$l == i, 3 + i] <- 1
    }
    if (any(data$r == 0 & data$w == i)) {
      data[data$r == 0 & data$w == i, 3 + i] <- -1
    }
  }
  return(data)
}

bt_mat_aut <- bt_mat_aut[complete.cases(bt_mat_aut$w),]# missing in 457, 1053, 1057 - IDs 33 a 37
bt_mat_aut        <- matrix_finalization_seven(bt_mat_aut)
bt_mat_dass       <- matrix_finalization_seven(bt_mat_dass)
bt_mat_ders       <- matrix_finalization_seven(bt_mat_ders)

# 5-statement scales (ders, dass, aut)
matrix_finalization_five <- function(data) {
  for (i in 1:5) {
    # First, handle the case when data$r == 1
    if (any(data$r == 1 & data$w == i)) {
      data[data$r == 1 & data$w == i, 3 + i] <- 1
    }
    if (any(data$r == 1 & data$l == i)) {
      data[data$r == 1 & data$l == i, 3 + i] <- -1
    }
    
    # Then, handle the case when data$r == 0
    if (any(data$r == 0 & data$l == i)) {
      data[data$r == 0 & data$l == i, 3 + i] <- 1
    }
    if (any(data$r == 0 & data$w == i)) {
      data[data$r == 0 & data$w == i, 3 + i] <- -1
    }
  }
  return(data)
}

bt_mat_bmpn       <- matrix_finalization_five(bt_mat_bmpn)
bt_mat_democr     <- matrix_finalization_five(bt_mat_democr)

# 8-statement HI 
matrix_finalization_hi <- function(data) {
  for (i in 1:8) {
    # First, handle the case when data$r == 1
    if (any(data$r == 1 & data$w == i)) {
      data[data$r == 1 & data$w == i, 3 + i] <- 1
    }
    if (any(data$r == 1 & data$l == i)) {
      data[data$r == 1 & data$l == i, 3 + i] <- -1
    }
    
    # Then, handle the case when data$r == 0
    if (any(data$r == 0 & data$l == i)) {
      data[data$r == 0 & data$l == i, 3 + i] <- 1
    }
    if (any(data$r == 0 & data$w == i)) {
      data[data$r == 0 & data$w == i, 3 + i] <- -1
    }
  }
  return(data)
}

bt_mat_height_inv <- bt_mat_height_inv[complete.cases(bt_mat_height_inv$w),]# NA in 577 ID 37
bt_mat_height_inv       <- matrix_finalization_hi(bt_mat_height_inv)


# I checked the operationalization and it looks alright, so we can proceed with
# errasing of w and l columns


# errasing w and l
bt_mat_aut        <- bt_mat_aut[,-c(1:2)]
bt_mat_bmpn       <- bt_mat_bmpn[,-c(1:2)]
bt_mat_dass       <- bt_mat_dass[,-c(1:2)]
bt_mat_democr     <- bt_mat_democr[,-c(1:2)]
bt_mat_ders       <- bt_mat_ders[,-c(1:2)]
bt_mat_height_inv <- bt_mat_height_inv[,-c(1:2)]


# In order to have one long dataset with LS, we need to make sure, we know where
# is what data used. We therefore create another variable called type
# we also add type id (which is 0 for everyone), and sex which is NA for everyone
# adding type column (0 = pairwise scale)
bt_mat_aut       <- bt_mat_aut %>%
  mutate(id_type = 0,
         id = 0,
         sex = NA,
         dataset_id = "AUT",
         collection = 1,
         item = "V1")
bt_mat_bmpn       <- bt_mat_bmpn %>%
  mutate(id_type = 0,
         id = 0,
         sex = NA,
         dataset_id = "BMPN",
         collection = 1,
         item = "V1")
bt_mat_dass       <- bt_mat_dass %>%
  mutate(id_type = 0,
         id = 0,
         sex = NA,
         dataset_id = "DASS",
         collection = 1,
         item = "V1")
bt_mat_democr       <- bt_mat_democr %>%
  mutate(id_type = 0,
         id = 0,
         sex = NA,
         dataset_id = "DEM",
         collection = 1,
         item = "V1")
bt_mat_ders       <- bt_mat_ders %>%
  mutate(id_type = 0,
         id = 0,
         sex = NA,
         dataset_id = "DERS",
         collection = 1,
         item = "V1")
bt_mat_height_inv <- bt_mat_height_inv %>%
  mutate(id_type = 0,
         id = 0,
         sex = NA,
         dataset_id = "HI",
         collection = 1,
         item = "V1")

# view(bt_mat_bmpn) checked if it is correctly done

# save all individual datasets for BTM analysis

# write.csv(bt_mat_aut, "Data/BTM/bt_aut.csv", row.names = FALSE)
# write.csv(bt_mat_bmpn, "Data/BTM/bt_bmpn.csv", row.names = FALSE)
# write.csv(bt_mat_dass, "Data/BTM/bt_dass.csv", row.names = FALSE)
# write.csv(bt_mat_democr, "Data/BTM/bt_dem.csv", row.names = FALSE)
# write.csv(bt_mat_ders, "Data/BTM/bt_ders.csv", row.names = FALSE)
# write.csv(bt_mat_height_inv, "Data/BTM/bt_hi.csv", row.names = FALSE)

