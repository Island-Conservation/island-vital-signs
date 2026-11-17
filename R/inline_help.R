# UI helpers for help that sits where the question is asked
#
# The module help modal is a reference document: it is read once, if at all.
# These helpers carry the short version to the moment it is needed -- a term's
# definition beside the control that uses it, and a map's caveat beside the
# map. A popover is used rather than a modal because a modal covers the very
# control the reader is trying to set.

# A control's label with a "?" beside it that opens a short note underneath.
#
# The label is rendered here rather than by the input, so the button is a
# sibling of the label rather than nested inside it -- a <label> should not
# contain interactive elements of its own. Pass label = NULL to the input.
#
# input_id: the input the label belongs to
# text:     the label text
# help_id:  id of the matching help_note()
help_label <- function(input_id, text, help_id) {
  tags$div(
    class = "help-label-row",

    tags$label(`for` = input_id, class = "control-label", text),

    # .help-btn carries the look the module help buttons already use, so the
    # two never drift apart; .inline-help-btn is the JS hook and the inline fit.
    tags$button(
      type = "button",
      class = "help-btn inline-help-btn",
      `aria-expanded` = "false",
      `aria-controls` = help_id,
      `aria-label` = paste0("What does \"", text, "\" mean?"),
      icon("question-circle")
    )
  )
}

# The note a help_label() opens. Hidden until its button is clicked.
#
# help_id: must match the help_label() that opens it
# ...:     the note's content, a sentence or three
help_note <- function(help_id, ...) {
  tags$div(
    id = help_id,
    class = "inline-help-note",
    hidden = NA,
    ...
  )
}

# A short, always-visible line under a module title saying what the tab is for.
module_intro <- function(...) {
  tags$p(class = "module-intro", ...)
}

# A caveat that belongs beside a result rather than in the help modal: the
# thing a reader could otherwise carry away from the map having misread it.
result_note <- function(...) {
  tags$div(
    class = "result-note",
    tags$span(class = "result-note-label", "Reading this map:"),
    tags$span(...)
  )
}

# A www/ asset's path with its modified time attached, so a browser holding an
# old copy fetches the new one. Styling changes are otherwise easy to miss:
# the app looks unchanged and the cause is invisible from inside the app.
asset_url <- function(file) {
  path <- file.path("www", file)

  if (!file.exists(path)) {
    return(file)
  }

  paste0(file, "?v=", as.integer(as.numeric(file.mtime(path))))
}
