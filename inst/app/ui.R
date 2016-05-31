## ui for data menu in radiant
do.call(navbarPage,
  c("Radiant", getOption("radiant.nav_ui"), getOption("radiant.shared_ui"),
    help_menu("help_data_ui"))
)
