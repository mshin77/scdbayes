# info-popup
# uniform ⓘ actionLink + modalDialog wrapper

# icon link; pair the id with an observeEvent that calls info_popup_modal()
info_popup_link <- function(id, title = "More info") {
  shiny::tags$a(
    href = "#",
    role = "button",
    title = title,
    `aria-label` = title,
    style = "color: #337ab7; font-size: 13px; cursor: pointer;",
    onclick = sprintf(
      "Shiny.setInputValue('%s', Math.random(), {priority: 'event'}); return false;",
      id
    ),
    # role=button: Space activates like a native button; Enter already fires click on a link
    onkeydown = "if (event.key === ' ' || event.key === 'Spacebar') { event.preventDefault(); this.click(); }",
    shiny::icon("info-circle")
  )
}

# Standard modal body
info_popup_modal <- function(title, body, size = "m") {
  body_tag <- if (inherits(body, "shiny.tag") || inherits(body, "shiny.tag.list")) {
    body
  } else {
    shiny::HTML(body)
  }
  # one modal is open at a time, so a fixed title id is unique
  shiny::tagAppendAttributes(
    shiny::modalDialog(
      title = shiny::tags$span(id = "info-popup-title", shiny::HTML(as.character(title))),
      shiny::div(style = "font-size: 15px; line-height: 1.6;", body_tag),
      size = size,
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ),
    role = "dialog", `aria-modal` = "true", `aria-labelledby` = "info-popup-title"
  )
}

