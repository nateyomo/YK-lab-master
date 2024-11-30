gt_oian <- function(identifier_col, filter_patterns, display_columns, table_title, order_by = NULL) {
  # Load the necessary libraries
  library(readxl)
  library(tidytable)
  library(gt)
  library(here)

  # Define constants for the Excel file and sheet name
  excel_file <- here("yk_tables.xlsx")
  sheet_name <- "OIAN"
  
  # # Helper function to format cell content with markdown
  # format_content <- function(text_col, link_col, citation_col) {
  #   if (n_distinct(text_col) > 0) {
  #     texts <- text_col[text_col != "" & !is.na(text_col)]
  #     links <- link_col[text_col != "" & !is.na(text_col)]
  #     citations <- citation_col[text_col != "" & !is.na(text_col)]
      
  #     if (length(texts) > 1) {
  #       paste0(paste0("* [", texts, "](", links, ") ", citations), collapse = "\n \n")
  #     } else if (length(texts) == 1) {
  #       paste0("[", first(texts), "](", first(links), ") ", first(citations))
  #     } else {""}
  #   } else {""}
  # }

format_content <- function(text_col, link_col, citation_col) {
  texts <- text_col[text_col != "" & !is.na(text_col)]
  links <- link_col[text_col != "" & !is.na(text_col)]
  citations <- citation_col[text_col != "" & !is.na(text_col)]
  
  if (length(texts) > 1) {
    paste0("* [", texts, "](", links, ") ", citations, collapse = "\n \n")
  } else if (length(texts) == 1) {
    paste0("* [", texts[1], "](", links[1], ") ", citations[1], collapse = "\n \n")
  } else {
    ""
  }
}

  format_action_content <- function(action_title_col, action_text_col, action_link_col, action_citation_col) {
    action_title_col[is.na(action_title_col) | action_title_col == ""] <- "<No Title>"
    action_list <- split(action_text_col, action_title_col, drop = FALSE)
    link_list <- split(action_link_col, action_title_col, drop = FALSE)
    citation_list <- split(action_citation_col, action_title_col, drop = FALSE)

    formatted_items <- lapply(names(action_list), function(title) {
        items <- action_list[[title]]
        links <- link_list[[title]]
        citations <- citation_list[[title]]
        valid_items <- items[items != "" & !is.na(items)]
        valid_links <- links[items != "" & !is.na(items)]
        valid_citations <- citations[items != "" & !is.na(items)]

        if (length(valid_items) > 0) {
            formatted_items <- paste0(
                "* [", valid_items, "](", valid_links, ") ", valid_citations, collapse = "\n"
            )
            if (title == "<No Title>") {
                return(formatted_items)  # Return formatted items without any bold title
            } else {
                return(paste0("**", title, "**\n\n", formatted_items))
            }
        } else {
            return("")  # Return empty string if there are no valid items
        }
    })
    return(paste(formatted_items[formatted_items != ""], collapse = "\n\n"))
}

  format_root_content <- function(root_text, root_link, root_citation, root_rate) {
    valid_rows <- root_text != "" & !is.na(root_text)
    if (any(valid_rows)) {
        roots <- root_text[valid_rows]
        links <- root_link[valid_rows]
        citations <- root_citation[valid_rows]
        rates <- root_rate[valid_rows]
        if (length(unique(rates)) == 1 && length(unique(citations)) == 1) {
            paste0("[", first(roots), "](", first(links), ") - [", last(roots), "](", last(links), ") ", first(citations))
        } else {
            paste0(paste0("* [", roots, "](", links, ") ", citations), collapse = "\n \n")
        }
    } else {
        return("")  # Return an empty string if no valid rows
    }
}

  tryCatch({
    # Read the Excel file with specified sheet
    oian_data <- suppressMessages(read_excel(excel_file, sheet = sheet_name))

    # Filter for rows that match the filter patterns
    initial_filtered_data <- oian_data %>%
      filter(grepl(filter_patterns, .data[[identifier_col]], ignore.case = TRUE))
  
    muscle_identifiers <- unique(initial_filtered_data$muscle_identifier)

    combined_data <- oian_data %>%
      filter(muscle_identifier %in% muscle_identifiers) %>%
      group_by(muscle_identifier) %>%
      summarize(
        name_final = if(n_distinct(name_text) > 0) paste0("[", first(name_text), "](", first(name_link), ")") else "",
        origin_final = format_content(origin_text, origin_link, origin_citation),
        insertion_final = format_content(insertion_text, insertion_link, insertion_citation),
        innervation_final = {
            if (all(is.na(innervation_text), is.na(innervation_link), is.na(innervation_citation))) {
                ""
            } else {
                formatted_innervation <- format_content(innervation_text, innervation_link, innervation_citation)
                additional_info <- format_root_content(root_text, root_link, root_citation, root_rate)
                
                if (nchar(formatted_innervation) > 0 && nchar(additional_info) > 0) {
                    paste0(formatted_innervation, "\n\n", additional_info)
                } else {
                    formatted_innervation
                }
            }
        },
        action_final = format_action_content(action_title, action_text, action_link, action_citation),
        fiber_proportion_final = format_content(fiber_proportion_text, fiber_proportion_link, fiber_proportion_citation),
        lever_final = format_content(lever_text, lever_link, lever_citation)
      ) %>%
      ungroup();

    if (!is.null(order_by) && order_by %in% names(combined_data)) {
      combined_data <- combined_data %>% arrange(across(all_of(order_by)))
    }
    
    column_labels <- list(
      name_final = "Muscle",
      origin_final = "Origin",
      insertion_final = "Insertion",
      innervation_final = "Innervation",
      action_final = "Action",
      fiber_proportion_final = "Fiber",
      lever_final = "Lever"
    )
    
    gt_oian_table <- combined_data %>%
      select(all_of(display_columns)) %>%
      gt() %>%
      fmt_markdown(columns = everything()) %>%
      cols_label(!!!setNames(column_labels[display_columns], display_columns)) %>%
      tab_header(title = table_title) %>%
      tab_style(
        style = cell_text(size = px(12)),
        locations = cells_body(columns = everything())
      ) %>%
      tab_style(
        style = cell_text(size = px(14), weight = "bold"),
        locations = cells_column_labels(columns = everything())
      )
  
    return(gt_oian_table)
  
  }, error = function(e) {
    cat("An error occurred:\n", e$message, "\n")
  })
}
