library(tidyverse)

library(readxl)
snapper_file = "data/red_snapper_smry.xlsx"
excel_sheets(snapper_file)


read_excel(snapper_file, "Sheet1") 
# This doesn't work...
# Duplicated column names have had "...j" appended, where j is the column number
# THe first row is another set of column names

# Let's read it in with the first row skipped
snapper_messy = read_excel(snapper_file, "Sheet1", skip = 1) 
snapper_messy

# Now lets figure out how to get the main titles back in...
snapper_messy_titles = read_excel(snapper_file, "Sheet1", n_max = 1)
snapper_messy_titles
# We're going to have to combine these somehow


snapper_messy_long = snapper_messy %>% 
  # Remove the totally empty columns
  select_if(is.numeric) %>% # fully NA columns are considered logical
  pivot_longer(-1, names_to = c("type", "col_num"), 
               names_sep = "[\\.]{3}", # This is a regular expression representing "..."
               names_ptypes = list(type = character(), col_num = integer()),
               values_to = "value",
               values_drop_na = TRUE)
snapper_messy_long

snapper_messy_titles_long = snapper_messy_titles %>% 
  # Remove first column and NA columns
  # Note that the non-na columns are characters, not numeric in this case
  select(-...1) %>% select_if(is.character) %>% 
  pivot_longer(everything(), 
             names_to = "title", values_to = "type")
snapper_messy_titles_long

# So we should be able to match the column positions corresponding to N; 
# we just need to reshape the data again to put it on the right rows
# first, we need to put a unique identifier 

snapper_messy_titles_long %>% 
  mutate(id = cumsum(type == "% mature"))
  # cumsum is the cumulative sum; it adds the value to previous values
  # e.g., cumsum(1:4)
  # try changing cumsum to as.numeric to see what's being added

title_key = snapper_messy_titles_long %>% 
  mutate(id = cumsum(type == "% mature")) %>% 
  # Now let's rename type to more accurately show the type
  mutate(type = if_else(type == "N", "col_num", "title")) %>% 
  pivot_wider(names_from = type, values_from = title) %>% 
# Remove the "..." from the front of col_num and convert it into an integer
  separate(col_num, into = c(NA,"col_num"), sep = "[\\.]{3}", convert = TRUE)

title_key

# Merge the two columns together by matching up col_num
# problem: we only have col_nums, for N, not for % mature
# Let's fix that first
joined_snapper_messy = 
  bind_rows(title_key, # duplicate the data frame, but shift col_num by 1
            title_key %>% mutate(col_num = col_num - 1)) %>% 
  left_join( # merge this data with snapper_messy long
    snapper_messy_long, by = "col_num")
joined_snapper_messy

# Now that everything is together, finish tidying it
# Note, the title column is really 3 columns: State, Year, and Paper
snapper_tidy = joined_snapper_messy %>% 
  select(-col_num) %>% # no longer needed
  pivot_wider(names_from = type, values_from = value) %>% # split N and % mature
  select(-id) %>% # no longer needed
  # This separates a column by matching regular expression patterns
  extract(title, into = c("State", "Year", "Paper"),
          regex = paste0( # I've separated out the regex statement to make it easier to understand
            "(..) ",    # State; 2 characters followed by space
            "(....) ",  # Year, 4 characters, followed by space
            "\\(from ", # "(from" is detected and ignored (no in parentheses)
            "(.+)\\)"   # Paper has at least one character, followed by ")"
          )) %>%  # Note we have to escape the literal parentheses
  rename(Size = `size (mm FL)`, Percent_mature = `% mature`) %>% 
  mutate(Percent_mature = Percent_mature / 100)
snapper_tidy  


# DELETE THIS PART BEFORE CLASS, BUT KEEP FOR A LATER BLOG POST OR SOMETHING


# Functions to automatically handle this
repeat_names = function(x) {
  nm = names(x)
  unnamed = function(.nm) str_detect(.nm, "^[\\.]{3}[0-9]") 
  while(any(unnamed(nm))) {
    nm[unnamed(nm)] <- lag(nm, default = "")[unnamed(nm)]
  }
    # browser()
  x %>% set_names(nm)
}
rows_to_colnames = function(data, n_rows = 1, level_sep = ">>") {
  # browser()
  name_list = data %>% slice(seq_len(n_rows)) %>% repeat_names %>% 
    imap_chr(~paste(.y, paste(.x, collapse = level_sep), sep = level_sep)) 
  data %>% slice(-seq_len(n_rows)) %>%
    set_names(name_list) %>% 
    map_if(is.character, parse_guess) %>% bind_cols
}

read_excel(snapper_file, "Sheet1") %>% 
  rows_to_colnames(n_rows = 1) %>% 
  select_if(~!all(is.na(.x))) %>%
  rename(size = `>>size (mm FL)`) %>% 
  filter(!is.na(size)) %>% 
  pivot_longer(-size, names_to = c("State", "Year", "Paper", ".value"),
               names_pattern = paste0("([A-Z]{2}) ", # state
                                      "([0-9]{4}) ", # year
                                      "\\(from ", # junk
                                      "(.+)", # Paper
                                      "\\)>>", # junk
                                      "(.+)" #value
                                      ),
               values_drop_na = TRUE) %>% 
  rename(percent_mature = `% mature`)
  
