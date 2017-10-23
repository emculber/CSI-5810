entropy <- function(values) {
  total <- 0
  entropy_total <- 0
  for(i in 1:length(values)) { 
    total <- total + values[i]
  }

  for(i in 1:length(values)) { 
    entropy_total <- entropy_total + ((-values[i]/total) * log2(values[i]/total))
  }
  if(is.nan(entropy_total)) {
    return(0)
  }
  return(entropy_total)
}

info <- function(...) {
  input_list <- list(...)
  total <- 0
  for(i in 1:length(input_list)) {
    input <- input_list[[i]]
    for(x in 1:length(input)) {
      total <- total + input[x]
    }
  }

  Gain <- 0
  for(i in 1:length(input_list)) {
    input <- input_list[[i]]
    local_total <- 0
    for(x in 1:length(input)) {
      local_total <- local_total + input[x]
    }
    Gain <- Gain + ((local_total/total) * entropy(input))
  }
  return(Gain)
}

print("===========================================================")
print("====================Example================================")
print("===========================================================")

Outlook <- c("Sunny", 
             "Sunny", 
             "Overcast", 
             "Rainy", 
             "Rainy", 
             "Rainy", 
             "Overcast", 
             "Sunny", 
             "Sunny", 
             "Rainy",
             "Sunny",
             "Overcast",
             "Overcast",
             "Rainy")

Temp <- c("Hot",
          "Hot",
          "Hot",
          "Mild",
          "Cool",
          "Cool",
          "Cool",
          "Mild",
          "Cool",
          "Mild",
          "Mild",
          "Mild",
          "Hot", 
          "Mild")

Humidity <- c("High",
              "High",
              "High",
              "High",
              "Normal",
              "Normal",
              "Normal",
              "High",
              "Normal",
              "Normal",
              "Normal",
              "High",
              "Normal",
              "High")

Windy <- c("False",
           "True", 
           "False",
           "False",
           "False",
           "True",
           "True",
           "False",
           "False",
           "False",
           "True",
           "True",
           "False",
           "True")

Play <- c("No",
          "No",
          "Yes",
          "Yes",
          "Yes",
          "No",
          "Yes",
          "No",
          "Yes",
          "Yes",
          "Yes",
          "Yes",
          "Yes",
          "No")

example <- data.frame(Outlook=Outlook, Temp=Temp, Humidity=Humidity, Windy=Windy, Play=Play)

Outlook_values <- unique(example[,1])

no_play_count <- nrow(example[example$Play == "No",])
yes_play_count <- nrow(example[example$Play == "Yes",])

# Outlook = Sunny
outlook_sunny <- example[example$Outlook == Outlook_values[1],]
outlook_sunny_no_play_count <- nrow(outlook_sunny[outlook_sunny$Play == "No",])
outlook_sunny_yes_play_count <- nrow(outlook_sunny[outlook_sunny$Play == "Yes",])

# entropy_sunny <- info(c(outlook_sunny_no_play_count, outlook_sunny_yes_play_count))

# Outlook = Overcast
outlook_overcast <- example[example$Outlook == Outlook_values[2],]
outlook_overcast_no_play_count <- nrow(outlook_overcast[outlook_overcast$Play == "No",])
outlook_overcast_yes_play_count <- nrow(outlook_overcast[outlook_overcast$Play == "Yes",])

# entropy_overcast <- info(c(outlook_overcast_no_play_count, outlook_overcast_yes_play_count))

# Outlook = Rainy
outlook_rainy <- example[example$Outlook == Outlook_values[3],]
outlook_rainy_no_play_count <- nrow(outlook_rainy[outlook_rainy$Play == "No",])
outlook_rainy_yes_play_count <- nrow(outlook_rainy[outlook_rainy$Play == "Yes",])

# entropy_rainy <- info(c(outlook_rainy_no_play_count, outlook_rainy_yes_play_count))

expected_outlook <- info(c(outlook_sunny_no_play_count, outlook_sunny_yes_play_count),
                 c(outlook_overcast_no_play_count, outlook_overcast_yes_play_count),
                 c(outlook_rainy_no_play_count, outlook_rainy_yes_play_count))

# Temp = Hot
temp_hot <- example[example$Temp == "Hot",]
temp_hot_no_play_count <- nrow(temp_hot[temp_hot$Play == "No",])
temp_hot_yes_play_count <- nrow(temp_hot[temp_hot$Play == "Yes",])
# Temp = Mild
temp_mild <- example[example$Temp == "Mild",]
temp_mild_no_play_count <- nrow(temp_mild[temp_mild$Play == "No",])
temp_mild_yes_play_count <- nrow(temp_mild[temp_mild$Play == "Yes",])
# Temp = Cool
temp_cool <- example[example$Temp == "Cool",]
temp_cool_no_play_count <- nrow(temp_cool[temp_cool$Play == "No",])
temp_cool_yes_play_count <- nrow(temp_cool[temp_cool$Play == "Yes",])

expected_temp <- info(c(temp_hot_no_play_count, temp_hot_yes_play_count),
                      c(temp_mild_no_play_count, temp_mild_yes_play_count),
                      c(temp_cool_no_play_count, temp_cool_yes_play_count))

# Humidity = High
humidity_high <- example[example$Humidity == "High",]
humidity_high_no_play_count <- nrow(humidity_high[humidity_high$Play == "No",])
humidity_high_yes_play_count <- nrow(humidity_high[humidity_high$Play == "Yes",])
# Humidity = Normal
humidity_normal <- example[example$Humidity == "Normal",]
humidity_normal_no_play_count <- nrow(humidity_normal[humidity_normal$Play == "No",])
humidity_normal_yes_play_count <- nrow(humidity_normal[humidity_normal$Play == "Yes",])

expected_humidity <- info(c(humidity_high_no_play_count, humidity_high_yes_play_count),
                          c(humidity_normal_no_play_count, humidity_normal_yes_play_count))

# Windy = True
windy_true <- example[example$Windy == "True",]
windy_true_no_play_count <- nrow(windy_true[windy_true$Play == "No",])
windy_true_yes_play_count <- nrow(windy_true[windy_true$Play == "Yes",])
# Windy = False
windy_false <- example[example$Windy == "False",]
windy_false_no_play_count <- nrow(windy_false[windy_false$Play == "No",])
windy_false_yes_play_count <- nrow(windy_false[windy_false$Play == "Yes",])

expected_windy <- info(c(windy_true_no_play_count, windy_true_yes_play_count),
                       c(windy_false_no_play_count, windy_false_yes_play_count))

gain_outlook <- info(c(yes_play_count, no_play_count)) - expected_outlook
gain_temp <- info(c(yes_play_count, no_play_count)) - expected_temp
gain_humidity <- info(c(yes_play_count, no_play_count)) - expected_humidity
gain_windy <- info(c(yes_play_count, no_play_count)) - expected_windy

print(sprintf("gain(Outlook)         = %s bits", gain_outlook))
print(sprintf("gain(Temperature)     = %s bits", gain_temp))
print(sprintf("gain(Humidity)        = %s bits", gain_humidity))
print(sprintf("gain(Windy)           = %s bits", gain_windy))

print("===========================================================")
print("====================Homework===============================")
print("===========================================================")


Color <- c("Red", "Blue", "Red", "Green", "Red", "Green")
Shape <- c("Square", "Square", "Round", "Square", "Round", "Square")
Size <- c("Big", "Big", "Small", "Small", "Big", "Big")
Class <- c("+", "+", "-", "-", "+", "-")

data <- data.frame(Color=Color, Shape=Shape, Size=Size, Class=Class)

negitive_count <- nrow(data[data$Class == "-",])
positive_count <- nrow(data[data$Class == "+",])

# Color = Red
color <- data[data$Color == "Red",]
color_red_negitive_count <- nrow(color[color$Class == "-",])
color_red_positive_count <- nrow(color[color$Class == "+",])

# Color = Blue
color <- data[data$Color == "Blue",]
color_blue_negitive_count <- nrow(color[color$Class == "-",])
color_blue_positive_count <- nrow(color[color$Class == "+",])

# Color = Green
color <- data[data$Color == "Green",]
color_green_negitive_count <- nrow(color[color$Class == "-",])
color_green_positive_count <- nrow(color[color$Class == "+",])

expected_color <- info(c(color_red_negitive_count, color_red_positive_count),
                       c(color_blue_negitive_count, color_blue_positive_count),
                       c(color_green_negitive_count, color_green_positive_count))

# Shape = Square
shape <- data[data$Shape == "Square",]
shape_square_negitive_count <- nrow(shape[shape$Class == "-",])
shape_square_positive_count <- nrow(shape[shape$Class == "+",])

# Shape = Round
shape <- data[data$Shape == "Round",]
shape_round_negitive_count <- nrow(shape[shape$Class == "-",])
shape_round_positive_count <- nrow(shape[shape$Class == "+",])

expected_shape <- info(c(shape_square_negitive_count, shape_square_positive_count),
                       c(shape_round_negitive_count, shape_round_positive_count))

# Size = Big
size <- data[data$Size == "Big",]
size_big_negitive_count <- nrow(size[size$Class == "-",])
size_big_positive_count <- nrow(size[size$Class == "+",])

# Size = Small
size <- data[data$Size == "Small",]
size_small_negitive_count <- nrow(size[size$Class == "-",])
size_small_positive_count <- nrow(size[size$Class == "+",])

expected_size <- info(c(size_big_negitive_count, size_big_positive_count),
                       c(size_small_negitive_count, size_small_positive_count))

gain_color <- info(c(positive_count, negitive_count)) - expected_color
gain_shape <- info(c(positive_count, negitive_count)) - expected_shape
gain_size <- info(c(positive_count, negitive_count)) - expected_size

print(sprintf("gain(Color)         = %s bits", gain_color))
print(sprintf("gain(Shape)         = %s bits", gain_shape))
print(sprintf("gain(Size)          = %s bits", gain_size))














