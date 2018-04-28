radiant.data::visualize(
  diamonds, 
  xvar = "carat", 
  yvar = "price", 
  type = "scatter", 
  color = "clarity", 
  custom = TRUE
) +
  labs(
    title = "Diamond prices", 
    x = "Carats", 
    y = "Price ($)"
  )
