reley_distribution <- function(x, sigma) {
  if (x < 0) return (0);
  return (1 - exp(-0.5 * (x / sigma)^2));
}

inverse_reley_distribution <- function(p, sigma) {
  if (p < 0 || p > 1) return (-1);
  if (p == 1) return (-1);
  return (sigma * sqrt(-2 * log(1 - p)));
}

cutToIntervals <- function(arr, alphabet_power) {
  summ <- sum(arr^2)
  size <- length(arr)
  sigma <- sqrt(summ / (2 * size))
  interval_width <- size / alphabet_power
  interval <- matrix(NA, nrow = alphabet_power, ncol = 2)
  buffer <- arr[1]
  
  for (i in 1:alphabet_power) {
    a <- reley_distribution(buffer, sigma)
    b <- inverse_reley_distribution(1.0 / alphabet_power + a, sigma)
    
    interval[i, 1] <- buffer
    interval[i, 2] <- b
    buffer <- b
  }
  interval[alphabet_power, 2] <- arr[size]
  if(interval[alphabet_power, 1] > interval[alphabet_power, 2]){
    interval_width <- (interval[alphabet_power, 2] - interval[alphabet_power - 1, 1]) / 2
    interval[alphabet_power - 1, 2] <- interval[alphabet_power - 1, 1] + interval_width
    interval[alphabet_power, 1] <- interval[alphabet_power - 1, 2]
  }
  return (interval)
}

toCharArray <- function(default_array, matrix_interval, alphabet) {
  c_array <- vector(mode = "character", length = (length(default_array)))
  for (i in 1:length(default_array)) {
    for (j in 1:nrow(matrix_interval)) {
      if (default_array[i] >= matrix_interval[j, 1] && default_array[i] <= matrix_interval[j, 2]) {
        c_array[i] <- alphabet[j] 
        break
      }
    }
  }
  return(c_array)
}

makeResultMatrix <- function(c_array, alphabet) {
  alphabet_power <- length(alphabet)
  result_matrix <- matrix(0, nrow = alphabet_power, ncol = alphabet_power, dimnames = list(alphabet, alphabet))
  
  for (i in 1:length(c_array)) {
    current_index <- match(c_array[i], alphabet)
    next_index <- match(c_array[i + 1], alphabet)
    
    if (!is.na(current_index) && !is.na(next_index)) {
      result_matrix[current_index, next_index] <- result_matrix[current_index, next_index] + 1
    }
  }
  
  return(result_matrix)
}

SIZE <- 500
ALPHABER_POWER <- 26

if(SIZE > 0 && ALPHABER_POWER > 0){
  array <- sample(1:200, SIZE, replace = TRUE)
  print("default array:")
  print(array)

  alphabet <- LETTERS[1:ALPHABER_POWER]
  
  print("alphabet:")
  print(alphabet)
  
  sorted_array <- sort(array)
  
  print("sorted array:")
  print(sorted_array)
  
  intervals <- cutToIntervals(sorted_array, ALPHABER_POWER)
  print("intervals:")
  print(intervals)
  
  char_array <- toCharArray(array, intervals, alphabet)
  print("char array:")
  print(char_array)
  
  print(makeResultMatrix(char_array, alphabet))
}else{
  print("alphabet power or array size <= 0, calculating impossible.")
}


