# ----
# Created : 2022-05-17
# Author  : Cox, Graham
# ----

# Load Libraries ----

library(tidyverse)
library(ggtext)
library(showtext)

# Set up fonts ---

# Set up fonts ----

font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "roboto", "Roboto")

showtext_auto()




# Load data from local file

df <- read_csv("data/GLB.Ts+dSST.csv",
               show_col_types = FALSE)

# Show data structure

glimpse(df)

# Transform data

df <- df %>%

  # Select only the year column and the J-D column
  # assumption is that J-D column is values for complete year
  # shown in column 1
  select(c(1, 14)) %>%

  # Rename columns
  rename(annual = `J-D`,
         year = Year) %>%

  # Ensure measurement is a numeric value
  mutate(annual = as.numeric(annual)) %>%

  # Remove NA values introduced in previous mutate
  drop_na() %>%

  # Esnure correct sort order for years
  arrange(year)

temp_by_year_plot <- df %>%

  # Plot year on x and annual value on y
  ggplot(aes(year, annual)) +

  # Add geom as a column without legend
  # set the colour to be as a factor of the value
  geom_col(show.legend = FALSE,
           aes(fill = factor(sign(annual)))) +

  # Add a horizontal line for zero mark on y axis
  geom_hline(yintercept = 0, colour = "white", size = 1) +

  # Add some test to describe plot

  labs(
    title = "Combined Land-Surface Air and Sea-Surface Water Temperature",
    subtitle = "Deviations from the corresponding 1951-1980 means.",
    caption = "Source\nGISS Surface Temperature Analysis (GISTEMP v4)\nhttps://data.giss.nasa.gov/gistemp/",
    x = NULL,
    y = "Deviation in mean temperature"

  ) +

  # Add manual colour scale
  scale_fill_manual(values = c("steelblue", "green", "tomato")) +

  # Set left and right margins for x axis
  scale_x_continuous(expand = c(0.01,0.01)) +

  # Set limits for y axis
  scale_y_continuous(limits = c(-1, 1)) +

  # Set theme
  theme_minimal(base_size = 14,
                base_family = "roboto") +
  theme(
    axis.line.x = element_line(colour = "grey40"),
    panel.grid = element_blank(),
    plot.title = element_textbox_simple(size = 28,
                                        family = "patua-one",
                                        colour = "steelblue",
                                        margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 18,
                                 lineheight = 1.1),
    plot.caption = element_text(size = 10,
                                hjust = 0)
  )


temp_by_year_plot
