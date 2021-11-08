## ui for data menu in radiant
navbar_proj(
  suppressWarnings(
    do.call(
      navbarPage,
      c(
        "Radiant",
        getOption("radiant.nav_ui"),
        getOption("radiant.shared_ui"),
        help_menu("help_data_ui")
      )
    )
  )
)
