# This function is exported (i.e. copied) from the shiny package (v.1.0.0). It
# is only exported in order to call a modified function that allows radiobutton
# selections to be create with html IDs.
radioButtons2 <- function(inputId, label, choices, selected = NULL,
                         inline = FALSE, width = NULL) {

  # resolve names
  choices <- shiny:::choicesWithNames(choices)

  selected <- shiny::restoreInput(id = inputId, default = selected)

  # default value if it's not specified
  selected <- if (is.null(selected)) choices[[1]] else {
    shiny:::validateSelected(selected, choices, inputId)
  }
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")

  options <- generateOptions(inputId, choices, selected, inline, type = 'radio')

  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline)
    divClass <- paste(divClass, "shiny-input-container-inline")

  tags$div(id = inputId,
           style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
           class = divClass,
           shiny:::controlLabel(inputId, label),
           options
  )
}

# This is copied from R package shiny (v.1.0.0). it only differs in that the
# inputTag created includes an id value.
#
# generate options for radio buttons and checkbox groups (type = 'checkbox' or
# 'radio')
generateOptions <- function(inputId, choices, selected, inline, type = 'checkbox') {
  # generate a list of <input type=? [checked] />
  options <- mapply(choices, names(choices),
                    FUN = function(value, name) {
                      inputTag <- shiny::tags$input(
                        type = type, id = paste0(inputId, "_", value), name = inputId, value = value
                      )
                      if (value %in% selected)
                        inputTag$attribs$checked <- "checked"

                      # If inline, there's no wrapper div, and the label needs a class like
                      # checkbox-inline.
                      if (inline) {
                        shiny::tags$label(class = paste0(type, "-inline"), inputTag, shiny::tags$span(name))
                      } else {
                        shiny::tags$div(class = type,
                                        shiny::tags$label(inputTag, shiny::tags$span(name))
                        )
                      }
                    },
                    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  shiny::div(class = "shiny-options-group", options)
}

#This function is exported (i.e. copied) from the shiny package (v.1.0.0).
updateRadioButtons2 <- function(session, inputId, label = NULL, choices = NULL,
                               selected = NULL, inline = FALSE) {
  # you must select at least one radio button
  if (is.null(selected) && !is.null(choices)) selected <- choices[[1]]
  updateInputOptions(session, inputId, label, choices, selected, inline, type = 'radio')
}

#This function is exported (i.e. copied) from the shiny package (v.1.0.0).
updateInputOptions <- function(session, inputId, label = NULL, choices = NULL,
                               selected = NULL, inline = FALSE,
                               type = 'checkbox') {
  if (!is.null(choices))
    choices <- shiny:::choicesWithNames(choices)
  if (!is.null(selected))
    selected <- shiny:::validateSelected(selected, choices, session$ns(inputId))

  options <- if (!is.null(choices)) {
    format(shiny::tagList(
      generateOptions(session$ns(inputId), choices, selected, inline, type = type)
    ))
  }

  message <- shiny:::dropNulls(list(label = label, options = options, value = selected))

  session$sendInputMessage(inputId, message)
}
