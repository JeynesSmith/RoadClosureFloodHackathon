loadLeafletChannel <- function() {
  htmltools::tagList(
    htmltools::tags$script(src = "homemade_leaflet.js"),
    htmltools::tags$link(href = "my_styles.css", rel = "stylesheet")
  )
}

addReceiver <- function(x) {
  htmlwidgets::onRender(x, "function() { myMap = this; setView(); }")
}

send_to <- function(x, session) {
  session$sendCustomMessage("R_message", x)
}

# Example
# send_to(list(type = "lines", data = 123), session)
