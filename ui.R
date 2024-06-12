credentials = data.frame(
  user = c("euro2024baby", "shinymanager"), # mandatory
  password = c("welovehsiar", "12345"), # mandatory
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

ui = page_sidebar(
  tags$head(
    tags$style(HTML("
      strong {display:inline}"
  ))),
  theme = bs_theme(bootswatch = "minty"),
  title = "Euro 2024",
  sidebar = list(
    img(src='logo.jpg'),
    pickerInput("players", "Choose Your Players", choices = players$name, selected = players$name, multiple = T, options = pickerOptions(container = "body"), width = "100%"),
    numericInputIcon("as_of_game", "Choose The Game Number", value = last_game, min = 0, max = max(games$game_id)),
    p("For the supremely dorky of you..."),
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