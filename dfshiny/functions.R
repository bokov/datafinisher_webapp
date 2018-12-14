sortableList <- function (inputId, label, items
                       ,as_source = FALSE, connect = NULL
                       ,item_class = c("default", "primary", "success", "info", 
                                      "warning", "danger")
                       ,placeholder = NULL, width = "100%", 
                       ...) 
{
  if (is.null(connect)) {
    connect <- "false"
  }
  else {
    connect <- paste0("#", connect, collapse = ", ")
  }
  item_class <- sprintf("btn btn-%s", match.arg(item_class))
  if (length(items) == 0 || !is.list(items)) {
    item_tags <- list()
  } else {
    item_values <- names(items);
    if(length(item_values)==0) item_values <- make.names(items);
    item_html <- items;
    item_tags <- lapply(1:length(item_values), function(i) {
      tag <- shiny::tags$div(item_html[i], `data-value` = item_values[i], 
                             class = item_class, style = "width: 80%; text-align: justify; margin: 1px")
      if (as_source) {
        options <- list(connectToSortable = connect, 
                        helper = "clone", cancel = "")
        tag <- jqui_draggable(tag, options = options)
      }
      return(tag)
    })
  }
  style <- "";
  #style <- "font-size: 0px; height: 600px;" #min-height: 200px;"
  # style <- sprintf("width: %s; font-size: 0px; min-height: 25px;", 
  #                  shiny::validateCssUnit(width))
  container <- shiny::tagSetChildren(shiny::tags$div(id = inputId, 
                                                     style = style, ...), list = item_tags)
  if (!as_source) {
    cb <- "function(e, ui){if(!$(e.target).children().length)$(e.target).empty();}"
    func <- "function(event, ui){\n      return $(event.target).children().map(function(i, e){\n        return $(e).attr(\"data-value\");\n      }).get();\n    }"
    options <- list(connectWith = connect, 
                    axis='y',containment='parent',remove = htmlwidgets::JS(cb), 
                    shiny = list(order = list(sortcreate = htmlwidgets::JS(func), 
                                              sortupdate = htmlwidgets::JS(func))))
    container <- jqui_sortable(container, options = options)
  }
  if (!is.null(placeholder)) {
    css <- "#%s:empty:before{content: \"%s\"; font-size: 14px; opacity: 0.5;}"
    placeholder <- shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(sprintf(css, 
                                                                                           inputId, placeholder)))))
  }
  shiny::tagList(placeholder, shiny::tags$label(label, `for` = inputId), 
                 container)
}
