## ui for data menu in radiant
navbar_proj(
  do.call(
    navbarPage,
    c(
      "Radiant for R",
      getOption("radiant.nav_ui"),
      getOption("radiant.shared_ui"),
      help_menu("help_data_ui")
    )
  )
)
