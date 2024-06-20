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
  tags$script(
    "
    // Custom JavaScript function to navigate to the second page
    function goToSecondPage(tableId) {
      setTimeout(function() {
        $('#' + tableId + ' .rt-tbody .-page-2').click();
      }, 500); // Adjust the timeout as needed
    }
    "
  ),
  theme = bs_theme(bootswatch = "minty"),
  title = "Euro 2024",
  sidebar = list(
    img(src='logo.jpg'),
    virtualSelectInput("players", "Choose Your Players", choices = players$name, selected = players$name, multiple = T, width = "100%", dropboxWrapper = "body"),
    sliderTextInput("as_of_game", "Choose The Game Number", choices = 0:last_game, selected = last_game),
    virtualSelectInput("graph_player", "Choose Your Player to See", choices = players$name, selected = filter(players, name == sample(players$name, 1))$name, multiple = F, width = "100%", dropboxWrapper = "body"),
    #p("For the supremely dorky of you..."),
    pickerInput("teams", "Choose Your Teams", choices = all_teams, selected = all_teams, multiple = T, options = pickerOptions(container = "body"), width = "100%"),
    pickerInput("locations", "Choose Your Locations", choices = all_locations, selected = all_locations, multiple = T, options = pickerOptions(container = "body"), width = "100%")
    ),
  navset_card_underline(
    id = "navbar",
    title = "",
    #nav_panel("Prints", id = "Prints", textOutput("res_auth"), h5("Standings Table"), tableOutput("standings_tbl1")),
    nav_panel("Welcome!", fluidRow(column(width = 12, uiOutput("welcome")))),
    nav_panel("Standings", withSpinner(reactableOutput("standings"))),
    nav_panel("Fun Graph", withSpinner(girafeOutput("graph"))),
    nav_panel("Games", withSpinner(reactableOutput("games_tbl")))
  )
)

if (params$authenticate) ui = secure_app(ui)