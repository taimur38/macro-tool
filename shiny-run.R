options(warn = -1)
options(shiny.autoreload = TRUE)
# pkgload::load_all(".")
# macroTool()
shiny::runApp(port=7777, launch.browser=TRUE)

