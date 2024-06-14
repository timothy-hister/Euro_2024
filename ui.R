credentials <- data.frame(
  user = c("euro", "shinymanager"), # mandatory
  password = c("analytics", "12345"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

ui = page_sidebar(
  shinyjs::useShinyjs(),
  theme = bs_theme(bootswatch = "minty"),
  title = "Euro 2024",
  sidebar = list(
    img(src='logo.jpg'),
    virtualSelectInput("players", "Choose Your Players", choices = players$name, selected = players$name, multiple = T, width = "100%", dropboxWrapper = "body"),
    sliderTextInput("as_of_game", "Choose The Game Number", choices = 0:last_game, selected = last_game),
    #p("For the supremely dorky of you..."),
    pickerInput("teams", "Choose Your Teams", choices = all_teams, selected = all_teams, multiple = T, options = pickerOptions(container = "body"), width = "100%"),
    pickerInput("locations", "Choose Your Locations", choices = all_locations, selected = all_locations, multiple = T, options = pickerOptions(container = "body"), width = "100%")
    ),
  navset_card_underline(
    title = "",
    #nav_panel("Welcome", uiOutput("welcome")),
    nav_panel("Standings", reactableOutput("standings_tbl")),
    nav_panel("Fun Graph", girafeOutput("graph"))
  )
)
