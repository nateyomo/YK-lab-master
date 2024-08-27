gt_oian <- function(identifier_col, filter_patterns, display_columns, table_title, order_by = NULL) {
  # Load the necessary libraries
  library(readxl)
  library(dplyr)
  library(gt)
  library(here)
  
  # Define constants for the Excel file and sheet name
  excel_file <- here("yk_tables.xlsx")
  sheet_name <- "OIAN"
  
  # Helper function to format cell content with markdown
  format_content <- function(text_col, link_col, citation_col) {
    if (n_distinct(text_col) > 0) {
      texts <- text_col[text_col != ""]
      if (length(texts) > 1) {
        paste0(paste0("* [", texts, "](", link_col[text_col != ""], ") ", citation_col[text_col != ""]), collapse = "\n \n")
      } else {
        paste0("[", first(text_col), "](", first(link_col), ") ", first(citation_col))
      }
    } else {
      ""
    }
  }
  
  # Helper function to format root content with conditional logic
  format_root_content <- function(text_col, link_col, citation_col, rate_col) {
    if (n_distinct(text_col) > 0) {
      roots <- text_col[text_col != ""]
      links <- link_col[text_col != ""]
      citations <- citation_col[text_col != ""]
      rates <- rate_col[text_col != ""]
      
      if (length(unique(rates)) == 1 && length(unique(citations)) == 1) {
        paste0("[", first(roots), "](", first(links), ") - [", last(roots), "](", last(links), ") ", first(citations))
      } else {
        paste0(paste0("* [", roots, "](", links, ") ", citations), collapse = "\n \n")
      }
    } else {
      ""
    }
  }
  
  # Helper function to format action content grouped by action_title
  format_action_content <- function(action_title_col, action_text_col, action_link_col, action_citation_col) {
    if (n_distinct(action_title_col) > 0) {
      # Create a named list to store action items by action_title
      action_list <- split(action_text_col, action_title_col)
      link_list <- split(action_link_col, action_title_col)
      citation_list <- split(action_citation_col, action_title_col)
      
      formatted_items <- lapply(names(action_list), function(title) {
        items <- action_list[[title]]
        links <- link_list[[title]]
        citations <- citation_list[[title]]
        
        # Combine items with links and citations, formatted as bullet points
        formatted_items <- paste0(
          "* [", items, "](", links, ") ", citations, collapse = "\n"
        )
        
        # Format action_title in bold and add line break before items
        paste0("**", title, "**\n\n", formatted_items)
      })
      
      paste0(formatted_items, collapse = "\n\n")
    } else {
      ""
    }
  }

  tryCatch({
    # Read the Excel file with specified sheet
    oian_data <- read_excel(excel_file, sheet = sheet_name)
  
    # Replace NA with empty strings in the original data
    oian_data[is.na(oian_data)] <- ""
  
    # Filter for rows that match the filter patterns
    initial_filtered_data <- oian_data %>%
      filter(grepl(filter_patterns, .data[[identifier_col]], ignore.case = TRUE))
  
    # Get the unique muscle_identifiers from the filtered rows
    muscle_identifiers <- unique(initial_filtered_data$muscle_identifier)
  
    # Filter the data to include all rows with these muscle_identifiers
    combined_data <- oian_data %>%
      filter(muscle_identifier %in% muscle_identifiers) %>%
      group_by(muscle_identifier) %>%
      summarize(
        name_final = if (n_distinct(name_text) > 0) paste0("[", first(name_text), "](", first(name_link), ")") else "",
        origin_final = format_content(origin_text, origin_link, origin_citation),
        insertion_final = format_content(insertion_text, insertion_link, insertion_citation),
        innervation_final = paste0(
          format_content(innervation_text, innervation_link, innervation_citation), 
          "\n \n", 
          format_root_content(root_text, root_link, root_citation, root_rate)
        ),
        action_final = format_action_content(action_title, action_text, action_link, action_citation),
        fiber_proportion_final = format_content(fiber_proportion_text, fiber_proportion_link, fiber_proportion_citation),
        lever_final = format_content(lever_text, lever_link, lever_citation)
      ) %>%
      ungroup()
    
    # Order the data if order_by is specified
    if (!is.null(order_by) && order_by %in% names(combined_data)) {
      combined_data <- combined_data %>% arrange(across(all_of(order_by)))
    }
    
    # Define the column labels
    column_labels <- list(
      name_final = "Muscle",
      origin_final = "Origin",
      insertion_final = "Insertion",
      innervation_final = "Innervation",
      action_final = "Action",
      fiber_proportion_final = "Fiber",
      lever_final = "Lever"
    )
    
    # Create the gt table with smaller font sizes
    gt_oian_table <- combined_data %>%
      select(all_of(display_columns)) %>%
      gt() %>%
      fmt_markdown(columns = all_of(display_columns)) %>%
      cols_label(!!!setNames(column_labels[display_columns], display_columns)) %>%
      tab_header(title = table_title) %>%
      tab_style(
        style = cell_text(size = px(12)), # Adjust the font size here
        locations = cells_body(columns = everything())
      ) %>%
      tab_style(
        style = cell_text(size = px(14), weight = "bold"), # Adjust the font size for headers
        locations = cells_column_labels(columns = everything())
      )
  
    # Print the gt table
    gt_oian_table
  
  }, error = function(e) {
    cat("An error occurred:\n", e$message, "\n")
  })
}
