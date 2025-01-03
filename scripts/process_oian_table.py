import pandas as pd
import yaml
from pathlib import Path
from pyprojroot import here
from IPython.display import display, HTML

def load_yaml(yaml_path):
    """Load data from a YAML file."""
    try:
        with open(yaml_path, 'r') as file:
            return yaml.safe_load(file)
    except FileNotFoundError:
        print(f"Error: YAML file not found at {yaml_path}")
        return {}
    except yaml.YAMLError as e:
        print(f"Error parsing YAML file: {e}")
        return {}


def get_link(variable_name, link_type):
    """
    Generate a hyperlink based on a variable name and link type.

    Args:
        variable_name (str): The identifier in the YAML file.
        link_type (str): The type of link to retrieve (e.g., 'path', 'origin').

    Returns:
        str: The full hyperlink.
    """
    base_dir = here()

    yaml_path = base_dir / "_variables.yml"
    variable_data = load_yaml(yaml_path)

    link = variable_data.get(variable_name, {}).get(link_type, '')
    if not link:
        print(f"Warning: No '{link_type}' link found for variable '{variable_name}' in YAML data.")
    
    base_url = "https://yomokerst.com"
    full_link = f"{base_url}{link}"
    return full_link

def process_muscle_data(df, filter_column=None, filter_identifiers=None):
    """
    Process muscle data with optional filtering and inclusion of all rows matching unique muscle_identifiers.

    Parameters:
    df (pd.DataFrame): The input DataFrame containing muscle data.
    filter_column (str): The column name to apply filtering on.
    filter_identifiers (list): A list of identifiers to filter the DataFrame.

    Returns:
    pd.DataFrame: Processed DataFrame with relevant muscle data.
    """
    # Step 1: Filter the DataFrame by filter_column and filter_identifiers
    if filter_column and filter_identifiers:
        # Initial filtering based on filter_column and filter_identifiers
        def row_matches(value):
            if pd.notna(value):
                # Split the cell into a list of identifiers, trim spaces, and check for matches
                cell_identifiers = [identifier.strip() for identifier in value.split(',')]
                return any(identifier in cell_identifiers for identifier in filter_identifiers)
            return False

        # Apply the `row_matches` function to the filter_column
        filtered_df = df[df[filter_column].apply(row_matches)]

        
        # Get all unique muscle_identifier values from the filtered DataFrame
        unique_muscle_identifiers = filtered_df['muscle_identifier'].unique()
        
        # Include all rows that match the identified unique muscle_identifiers
        df = df[df['muscle_identifier'].isin(unique_muscle_identifiers)]
    
    # Step 2: Process the DataFrame (example: grouping or extracting unique YAML strings)
    yaml_identifiers = df.groupby('muscle_identifier')['variable_yaml'].first()
    
    # Step 3: Optional - further processing or returning the modified DataFrame
    return df, yaml_identifiers

def format_origin(muscle_identifier, yaml_identifier, df):
    """Format the origin column for a specific muscle identifier."""
    filtered_rows = df[df['muscle_identifier'] == muscle_identifier]
    links = []
    for _, row in filtered_rows.iterrows():
        origin_text = row.get('origin_text', '')
        if pd.notna(origin_text) and origin_text and yaml_identifier:
            link = get_link(yaml_identifier, "origin")
            links.append(f'<a href="{link}">{origin_text}</a>')
    return '<br>'.join(links) if links else ''

def format_insertion(muscle_identifier, yaml_identifier, df):
    """Format the insertion column for a specific muscle identifier."""
    filtered_rows = df[df['muscle_identifier'] == muscle_identifier]
    links = []
    for _, row in filtered_rows.iterrows():
        insertion_text = row.get('insertion_text', '')
        if pd.notna(insertion_text) and insertion_text and yaml_identifier:
            link = get_link(yaml_identifier, "insertion")
            links.append(f'<a href="{link}">{insertion_text}</a>')
    return '<br>'.join(links) if links else ''

def format_innervation(muscle_identifier, yaml_identifier, df):
    """Format the innervation column for a specific muscle identifier, including first and last non-empty root_text."""
    filtered_rows = df[df['muscle_identifier'] == muscle_identifier]
    links = []
    root_texts = []

    for _, row in filtered_rows.iterrows():
        # Handle innervation_text
        innervation_text = row.get('innervation_text', '')
        if pd.notna(innervation_text) and innervation_text and yaml_identifier:
            link = get_link(yaml_identifier, "innervation")
            links.append(f'<a href="{link}">{innervation_text}</a>')
        
        # Handle root_text
        root_text = row.get('root_text', '')
        if pd.notna(root_text) and root_text:
            root_texts.append(root_text)

    # Add first and last root_text as a single line, if available
    if root_texts:
        root_summary = f"{root_texts[0]} - {root_texts[-1]}" if len(root_texts) > 1 else root_texts[0]
        links.append(root_summary)

    return '<br>'.join(links) if links else ''



def format_action(muscle_identifier, yaml_identifier, df):
    """Format the action column for a specific muscle identifier."""
    filtered_rows = df[df['muscle_identifier'] == muscle_identifier]
    action_groups = {}
    for _, row in filtered_rows.iterrows():
        action_title = row.get('action_title', None)
        action_text = row.get('action_text', '')
        if pd.notna(action_text) and action_text:
            action_link = get_link(yaml_identifier, "action")
            if pd.notna(action_title) and action_title:
                if action_title not in action_groups:
                    action_groups[action_title] = []
                action_groups[action_title].append(f'<a href="{action_link}">{action_text}</a>')
            else:
                if 'Unnamed' not in action_groups:
                    action_groups['Unnamed'] = []
                action_groups['Unnamed'].append(f'<a href="{action_link}">{action_text}</a>')

    formatted_actions = []
    for title, texts in action_groups.items():
        if title == 'Unnamed':
            formatted_actions.append('<br>'.join(texts))
        else:
            formatted_actions.append(f'<strong>{title}</strong>: {", ".join(texts)}')

    return '<br>'.join(formatted_actions)

def process_oian_table(filter_column=None, filter_identifiers=None):
    """
    Process an Excel file to create an HTML table of muscle OIAN data.

    Args:
        filter_column (str): The column to filter the data by.
        filter_identifiers (list): The values to include in the filter.

    Returns:
        str: An HTML table representation of the processed data.
    """
    base_dir = here()
    excel_path = base_dir / "yk_tables.xlsx"
    
    if excel_path.exists():
        try:
            # Step 1: Read the Excel file into a DataFrame
            df = pd.read_excel(excel_path, sheet_name='OIAN', header=0)

            # Step 2: Use process_muscle_data to filter and get YAML identifiers
            df, yaml_identifiers = process_muscle_data(df, filter_column=filter_column, filter_identifiers=filter_identifiers)

            # Step 3: Format and hyperlink muscle name
            df['name_final'] = df.apply(
                lambda row: f'<a href="{get_link(row["variable_yaml"], "path")}">{row["name_text"]}</a>' 
                if pd.notna(row["name_text"]) and row["name_text"] and row["variable_yaml"] else '', 
                axis=1
            )

            # Step 4: Format columns
            df['origin_final'] = df.apply(
                lambda row: format_origin(row['muscle_identifier'], yaml_identifiers.get(row['muscle_identifier'], None), df), axis=1
            )
            df['insertion_final'] = df.apply(
                lambda row: format_insertion(row['muscle_identifier'], yaml_identifiers.get(row['muscle_identifier'], None), df), axis=1
            )
            df['innervation_final'] = df.apply(
                lambda row: format_innervation(row['muscle_identifier'], yaml_identifiers.get(row['muscle_identifier'], None), df), axis=1
            )
            df['action_final'] = df.apply(
                lambda row: format_action(row['muscle_identifier'], yaml_identifiers.get(row['muscle_identifier'], None), df), axis=1
            )

            # Step 5: Aggregate results to ensure only one row per muscle_identifier
            aggregated_df = df.groupby('muscle_identifier').agg({
                'name_final': 'first',  # Get the first valid name_final
                'origin_final': 'first',  # You can also use a custom aggregation function if needed
                'insertion_final': 'first',
                'innervation_final': 'first',
                'action_final': 'first'
            }).reset_index()

            # Step 6: Rename the columns
            aggregated_df.rename(columns={
                'name_final': 'Muscle',
                'origin_final': 'Origin',
                'insertion_final': 'Insertion',
                'innervation_final': 'Innervation',
                'action_final': 'Action'
            }, inplace=True)

            # Step 7: Create the final HTML table
            oian_table = aggregated_df[['Muscle', 'Origin', 'Insertion', 'Innervation', 'Action']].to_html(
                classes='oian-table', index=False, escape=False
            )
            return oian_table
        except Exception as e:
            print(f"Error processing the Excel file: {e}")
            return None
    else:
        print("Excel file does not exist. Please check the path.")
        return None


def process_functional_unit_table(filter_column=None, filter_identifiers=None):
    """
    Process an Excel file to create an HTML table of muscle OIAN data.

    Args:
        filter_column (str): The column to filter the data by.
        filter_identifiers (list): The values to include in the filter.

    Returns:
        str: An HTML table representation of the processed data.
    """
    base_dir = here()
    excel_path = base_dir / "yk_tables.xlsx"
    
    if excel_path.exists():
        try:
            # Step 1: Read the Excel file into a DataFrame
            df = pd.read_excel(excel_path, sheet_name='OIAN', header=0)

            # Step 2: Use process_muscle_data to filter and get YAML identifiers
            df, yaml_identifiers = process_muscle_data(df, filter_column=filter_column, filter_identifiers=filter_identifiers)

            # Step 3: Format and hyperlink muscle name
            df['name_final'] = df.apply(
                lambda row: f'<a href="{get_link(row["variable_yaml"], "path")}">{row["name_text"]}</a>' 
                if pd.notna(row["name_text"]) and row["name_text"] and row["variable_yaml"] else '', 
                axis=1
            )

            # Step 4: Format columns
            df['synergist_final'] = df.apply(
                lambda row: format_origin(row['muscle_identifier'], yaml_identifiers.get(row['muscle_identifier'], None), df), axis=1
            )
            df['antagonist_final'] = df.apply(
                lambda row: format_insertion(row['muscle_identifier'], yaml_identifiers.get(row['muscle_identifier'], None), df), axis=1
            )

            # Step 6: Rename the columns
            aggregated_df.rename(columns={
                'functional_unit_action_text': 'Action',
                'synergist_final': 'Synergists',
                'antagonist_final': 'Antagonists'
            }, inplace=True)

            # Step 7: Create the final HTML table
            functional_unit_table = aggregated_df[['Muscle', 'Origin', 'Insertion', 'Innervation', 'Action']].to_html(
                classes='oian-table', index=False, escape=False
            )
            return functional_unit_table
        except Exception as e:
            print(f"Error processing the Excel file: {e}")
            return None
    else:
        print("Excel file does not exist. Please check the path.")
        return None



if __name__ == "__main__":
    # Example usage
    filter_column = 'muscle_identifier'  # Column to filter by
    filter_identifiers = ['gluteus_maximus', 'biceps_brachii']  # Example identifiers to filter
    oian_html_table = process_oian_table(filter_column, filter_identifiers)
    
    # Display the HTML table
    if oian_html_table:
        display(HTML(oian_html_table))
