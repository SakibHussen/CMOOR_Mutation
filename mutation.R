options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("shiny")
library(shiny)
library(shinyjs)
library(rsconnect)


# rsconnect::writeManifest( # Commented out as this is for deployment, not app logic
#   appDir = ".",
#   appPrimaryDoc = "app.R",
#   appFiles = NULL
# )

# Define the genetic code (mRNA codons to amino acids)
genetic_code <- list(
  "UUU" = "Phe", "UUC" = "Phe", "UUA" = "Leu", "UUG" = "Leu",
  "CUU" = "Leu", "CUC" = "Leu", "CUA" = "Leu", "CUG" = "Leu",
  "AUU" = "Ile", "AUC" = "Ile", "AUA" = "Ile", "AUG" = "Met", # AUG is also Start
  "GUU" = "Val", "GUC" = "Val", "GUA" = "Val", "GUG" = "Val",
  "UCU" = "Ser", "UCC" = "Ser", "UCA" = "Ser", "UCG" = "Ser",
  "CCU" = "Pro", "CCC" = "Pro", "CCA" = "Pro", "CCG" = "Pro",
  "ACU" = "Thr", "ACC" = "Thr", "ACA" = "Thr", "ACG" = "Thr",
  "GCU" = "Ala", "GCC" = "Ala", "GCA" = "Ala", "GCG" = "Ala",
  "UAU" = "Tyr", "UAC" = "Tyr", "UAA" = "STOP", "UAG" = "STOP",
  "CAU" = "His", "CAC" = "His", "CAA" = "Gln", "CAG" = "Gln",
  "AAU" = "Asn", "AAC" = "Asn", "AAA" = "Lys", "AAG" = "Lys",
  "GAU" = "Asp", "GAC" = "Asp", "GAA" = "Glu", "GAG" = "Glu",
  "UGU" = "Cys", "UGC" = "Cys", "UGA" = "STOP", "UGG" = "Trp",
  "CGU" = "Arg", "CGC" = "Arg", "CGA" = "Arg", "CGG" = "Arg",
  "AGU" = "Ser", "AGC" = "Ser", "AGA" = "Arg", "AGG" = "Arg",
  "GGU" = "Gly", "GGC" = "Gly", "GGA" = "Gly", "GGG" = "Gly"
)

# List of valid 3-letter amino acid codes
valid_amino_acid_codes <- toupper(unlist(unname(genetic_code)))
valid_amino_acid_codes <- unique(valid_amino_acid_codes[valid_amino_acid_codes != "STOP"])
valid_amino_acid_codes <- c(valid_amino_acid_codes, "STOP")

# --- Amino acid color mapping ---
amino_acid_properties <- list(
  "ALA" = list(category = "Aliphatic", color = "#FF0000"),
  "GLY" = list(category = "Aliphatic", color = "#FF0000"),
  "ILE" = list(category = "Aliphatic", color = "#FF0000"),
  "LEU" = list(category = "Aliphatic", color = "#FF0000"),
  "PRO" = list(category = "Aliphatic", color = "#FF0000"),
  "VAL" = list(category = "Aliphatic", color = "#FF0000"),
  "PHE" = list(category = "Aromatic", color = "#99CC66"),
  "TRP" = list(category = "Aromatic", color = "#99CC66"),
  "TYR" = list(category = "Aromatic", color = "#99CC66"),
  "ASP" = list(category = "Acidic", color = "#FF6F00"),
  "GLU" = list(category = "Acidic", color = "#FF6F00"),
  "ARG" = list(category = "Basic", color = "#00B7EB"),
  "HIS" = list(category = "Basic", color = "#00B7EB"),
  "LYS" = list(category = "Basic", color = "#00B7EB"),
  "SER" = list(category = "Hydroxylic", color = "#FFB6D5"),
  "THR" = list(category = "Hydroxylic", color = "#FFB6D5"),
  "CYS" = list(category = "Sulfur-containing", color = "#FFD700"),
  "MET" = list(category = "Sulfur-containing", color = "#FFD700"),
  "ASN" = list(category = "Amidic", color = "#003f88"),
  "GLN" = list(category = "Amidic", color = "#003f88"),
  "STOP" = list(category = "Stop Codon", color = "#696969")
)

# Function to validate DNA sequence
validate_dna <- function(dna) {
  dna <- toupper(gsub(" ", "", dna))
  if (!grepl("^[ATCG]*$", dna)) {
    return("Error: DNA sequence must contain only A, T, C, G")
  }
  return(dna)
}

# Color mapping for amino acids (3-letter codes) based on codon chart colors
# CSS Class Suggestions for future use:
# .aa-basic { color: #00B7EB; }
# .aa-amidic { color: #003f88; }
# .aa-acidic { color: #FF6F00; }
# .aa-sulfur { color: #FFD700; }
amino_acid_properties <- list(
  # Aliphatic (Red)
  "ALA" = list(category = "Aliphatic", color = "#FF0000"),
  "GLY" = list(category = "Aliphatic", color = "#FF0000"),
  "ILE" = list(category = "Aliphatic", color = "#FF0000"),
  "LEU" = list(category = "Aliphatic", color = "#FF0000"),
  "PRO" = list(category = "Aliphatic", color = "#FF0000"),
  "VAL" = list(category = "Aliphatic", color = "#FF0000"),
  
  # Aromatic (Green)
  "PHE" = list(category = "Aromatic", color = "#99CC66"),
  "TRP" = list(category = "Aromatic", color = "#99CC66"),
  "TYR" = list(category = "Aromatic", color = "#99CC66"),
  
  # Acidic (Deep Orange)
  "ASP" = list(category = "Acidic", color = "#FF6F00"),
  "GLU" = list(category = "Acidic", color = "#FF6F00"),
  
  # Basic (Punchy Cyan Blue)
  "ARG" = list(category = "Basic", color = "#00B7EB"),
  "HIS" = list(category = "Basic", color = "#00B7EB"),
  "LYS" = list(category = "Basic", color = "#00B7EB"),
  
  # Hydroxylic (Pink)
  "SER" = list(category = "Hydroxylic", color = "#FFB6D5"),
  "THR" = list(category = "Hydroxylic", color = "#FFB6D5"),
  
  # Sulfur-containing (Vivid Yellow/Gold)
  "CYS" = list(category = "Sulfur-containing", color = "#FFD700"),
  "MET" = list(category = "Sulfur-containing", color = "#FFD700"),
  
  # Amidic (Dark Blue)
  "ASN" = list(category = "Amidic", color = "#003f88"),
  "GLN" = list(category = "Amidic", color = "#003f88"),
  
  # Special case for STOP codon
  "STOP" = list(category = "Stop Codon", color = "#696969")
)

# Function to format amino acid sequence with colors based on properties
format_amino_acids_with_color <- function(amino_acid_string) {
  if (is.null(amino_acid_string) || nchar(amino_acid_string) == 0) {
    return(NULL) # Return NULL for empty, so it doesn't render empty spans
  }
  
  amino_acid_string <- toupper(gsub(" ", "", amino_acid_string))
  amino_acids <- strsplit(amino_acid_string, "(?<=\\G...)", perl = TRUE)[[1]]
  
  formatted_html_elements <- lapply(amino_acids, function(aa_code) {
    props <- amino_acid_properties[[aa_code]]
    if (is.null(props)) {
      return(tags$span(class = "colored-amino-acid", style = "color: #333333; font-weight: bold;", aa_code)) # Add bold
    } else {
      return(tags$span(class = "colored-amino-acid", style = paste0("color: ", props$color, "; font-weight: bold;"), aa_code)) # Add bold
    }
  })
  
  # Simply combine the individual span elements. CSS will handle spacing.
  # Use HTML() to ensure the list of tags is rendered as HTML, rather than R's default list printing.
  # You might need to flatten the list if it's deeply nested, but `tagList` usually handles this.
  tagList(formatted_html_elements)
}

# Function to transcribe DNA to RNA
transcribe_dna <- function(dna) {
  dna <- toupper(gsub(" ", "", dna))
  rna <- chartr("ATCG", "UAGC", dna)
  return(rna)
}

# Function to validate RNA sequence (no longer user input, but still needs format check)
validate_rna_format <- function(rna_seq) {
  rna_seq <- toupper(gsub(" ", "", rna_seq))
  if (!grepl("^[AUCG]*$", rna_seq)) {
    return("Error: RNA sequence contains invalid characters. Only A, U, C, G are allowed.")
  }
  if (nchar(rna_seq) %% 3 != 0) {
    return("Error: RNA sequence length must be a multiple of 3 for translation.")
  }
  return(rna_seq)
}

# Function to translate RNA to Amino Acids
translate_rna <- function(rna_seq) {
  rna_seq <- toupper(gsub(" ", "", rna_seq))
  if (nchar(rna_seq) %% 3 != 0) {
    return("Error: RNA sequence length must be a multiple of 3 for translation.")
  }
  codons <- strsplit(rna_seq, "(?<=\\G...)", perl = TRUE)[[1]]
  amino_acids <- sapply(codons, function(codon) {
    aa <- genetic_code[[codon]]
    if (is.null(aa)) {
      return("Invalid Codon") 
    }
    return(aa)
  })
  return(amino_acids)
}

# Function to validate Amino Acid sequence entered by user
validate_amino_acids_input <- function(expected_rna_seq, user_amino_acid_input) {
  user_amino_acid_input <- toupper(gsub(" ", "", user_amino_acid_input))
  
  # Break user input into 3-letter pieces
  input_aas_chunks <- strsplit(user_amino_acid_input, "(?<=\\G...)", perl = TRUE)[[1]]
  
  # Check that 3-letter pieces are all valid amino acid codes
  if (any(!input_aas_chunks %in% valid_amino_acid_codes)) {
    invalid_codes <- input_aas_chunks[!input_aas_chunks %in% valid_amino_acid_codes]
    # Add spaces between codes for clearer error message
    invalid_codes_str <- paste(invalid_codes, collapse = ", ")
    return(paste("Error: Invalid amino acid code(s) entered:", invalid_codes_str, ". Please use standard 3-letter codes (e.g., MET, TRP, STOP)."))
  }
  
  expected_amino_acids_array <- translate_rna(expected_rna_seq)
  
  # Handle potential errors from translate_rna
  if (grepl("^Error:", expected_amino_acids_array[1])) {
    return(expected_amino_acids_array[1])
  }
  
  expected_amino_acids_string <- paste(expected_amino_acids_array, collapse = "")
  expected_amino_acids_string <- toupper(expected_amino_acids_string)
  
  # Check that the length of the amino acid sequence is correct
  if (nchar(user_amino_acid_input) != nchar(expected_amino_acids_string)) {
    return("Error: Amino acid sequence length is incorrect.")
  }
  
  # Check that the amino acid sequence is a match based the amino acid codon chart
  if (user_amino_acid_input != expected_amino_acids_string) {
    return("Error: Amino acid sequence does not match the translated RNA sequence.")
  }
  
  return(user_amino_acid_input)
}

# Function to format DNA/RNA sequence for display (just adds spaces)
format_sequence <- function(seq_string, chunk_size = 3) {
  seq_string <- as.character(seq_string) 
  # Check if the sequence is empty to avoid errors with substring
  if (nchar(seq_string) == 0) {
    return("")
  }
  # Add spaces between chunks but limit the total length
  formatted <- paste(substring(seq_string, seq(1, nchar(seq_string), chunk_size), seq(chunk_size, nchar(seq_string), chunk_size)), collapse = " ")
  # If the formatted string is too long, truncate it
  if (nchar(formatted) > 100) {
    formatted <- substr(formatted, 1, 100)
    formatted <- paste0(formatted, "...")
  }
  return(formatted)
}

# Function to generate random DNA sequence
generate_dna <- function(length = 15) {
  # Always use 15 nucleotides for consistency
  length <- 15
  
  # Function to check if a DNA sequence would create stop codons when transcribed
  has_stop_codons <- function(dna_seq) {
    # Check for stop codons: UAA, UAG, UGA
    # These correspond to DNA sequences: TAA, TAG, TGA
    stop_patterns <- c("TAA", "TAG", "TGA")
    
    for (pattern in stop_patterns) {
      if (grepl(pattern, dna_seq)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Generate DNA sequence and check for stop codons
  max_attempts <- 500  # Increased attempts for better safety
  for (attempt in 1:max_attempts) {
    # Use only safe bases to avoid stop codons
    safe_bases <- c("A", "C", "G")  # Avoid T completely to prevent stop codons
    dna_seq <- paste(sample(safe_bases, length, replace = TRUE), collapse = "")
    
    # Double-check by transcribing and verifying no stop codons in RNA
    rna_seq <- chartr("ATCG", "UAGC", dna_seq)
    if (!grepl("UAA|UAG|UGA", rna_seq)) {
      return(dna_seq)
    }
  }
  
  # If we still can't find a safe sequence, generate a completely safe one
  safe_bases <- c("A", "C", "G")
  return(paste(sample(safe_bases, length, replace = TRUE), collapse = ""))
}

# --- DNA Sequence Validation ---
validate_dna_sequence <- function(dna) {
  dna <- toupper(gsub(" ", "", dna))
  if (!grepl("^[ATCG]+$", dna)) {
    return(list(valid = FALSE, error = "DNA sequence must contain only A, T, C, G."))
  }
  if (nchar(dna) %% 3 != 0) {
    return(list(valid = FALSE, error = "DNA sequence length must be a multiple of 3."))
  }
  list(valid = TRUE, dna = dna)
}

# --- Update Sequences Based on Entered DNA ---
# Returns a list with DNA, RNA, and amino acid sequence (or error)
update_sequences_from_dna <- function(dna) {
  val <- validate_dna_sequence(dna)
  if (!val$valid) {
    return(list(error = val$error))
  }
  dna <- val$dna
  rna <- transcribe_dna(dna)
  aa <- translate_rna(rna)
  list(dna = dna, rna = rna, amino_acids = aa)
}

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  titlePanel("🌟 Central Dogma Explorer"),
  tags$style(HTML("
    body {
      background-color: #f0f4f8;
      font-family: 'Inter', sans-serif;
      color: #333;
      overflow-x: hidden;
      margin: 0;
      padding: 0;
    }
    .title-panel {
      text-align: center;
      color: #2c3e50;
      font-size: 28px;
      margin-bottom: 20px;
    }
    .intro-text {
      text-align: center;
      font-size: 16px;
      color: #666;
      margin-bottom: 30px;
      background-color: #e6f0fa;
      padding: 15px;
      border-radius: 8px;
    }
    .input-section {
      background-color: #ffffff;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0 4px 10px rgba(0,0,0,0.1);
      margin-bottom: 20px;
      width: 100%;
      box-sizing: border-box;
    }
    .input-field {
      margin-bottom: 15px;
    }
    .input-field label {
      font-weight: bold;
      font-size: 18px;
      color: #2c3e50;
      display: block;
      margin-bottom: 5px;
    }
    .input-field input {
      font-family: 'Courier New', monospace !important;
      font-size: 22px !important;
      padding: 12px !important;
      border-radius: 8px !important;
      border: 2px solid #ccc !important;
      width: 100% !important;
      height: 60px !important;
      box-sizing: border-box !important;
      overflow-x: hidden !important;
      white-space: normal !important;
      word-wrap: break-word !important;
      word-break: break-all !important;
      min-width: 0 !important;
      max-width: none !important;
    }
    .input-field input:focus {
      border-color: #007bff !important;
      outline: none !important;
    }
    .valid {
      border-color: #28a745 !important;
    }
    .invalid {
      border-color: #dc3545 !important;
    }
    .button-section {
      text-align: center;
      margin: 20px 0;
    }
    .btn-action {
      font-size: 18px;
      padding: 12px 24px;
      border-radius: 5px;
      margin: 0 10px 10px 0;
      transition: background-color 0.3s ease-in-out, transform 0.2s ease-in-out;
      box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      border: none;
    }
    .btn-process {
      background-color: #007bff;
      color: white;
    }
    .btn-process:hover {
      background-color: #0056b3;
    }
    .btn-refresh {
      background-color: #28a745;
      color: white;
    }
    .btn-refresh:hover {
      background-color: #218838;
    }
    .results-section {
      background-color: #ffffff;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0 4px 10px rgba(0,0,0,0.1);
      width: 100%;
      box-sizing: border-box;
    }
    .sequence-label {
      font-weight: bold;
      font-size: 20px;
      color: #2c3e50;
      margin-top: 15px;
      margin-bottom: 8px;
    }
    .sequence-text {
      font-family: 'Courier New', monospace;
      font-size: 20px;
      line-height: 1.3;
      background-color: #f8f9fa;
      padding: 15px;
      border-radius: 5px;
      border: 1px solid #ddd;
      min-height: 60px;
      word-wrap: break-word;
      word-break: break-all;
      white-space: normal;
      width: 100%;
      box-sizing: border-box;
      overflow: hidden;
    }
    .sequence-text span.colored-amino-acid {
      margin-right: 10px;
      margin-bottom: 6px;
      font-family: 'Courier New', monospace;
      font-size: 20px;
      font-weight: bold;
      display: inline-block;
      min-width: 50px;
      text-align: center;
      padding: 4px 8px;
      border-radius: 4px;
      background-color: #ffffff;
    }
    .error-message {
      color: #dc3545;
      font-size: 18px;
      margin-top: 10px;
      font-weight: bold;
    }
    .success-message {
      color: #28a745;
      font-size: 18px;
      margin-top: 10px;
      font-weight: bold;
    }
    @media (max-width: 1200px) {
      .input-field input {
        font-size: 20px !important;
      }
      .sequence-text {
        font-size: 18px;
      }
      .sequence-text span.colored-amino-acid {
        font-size: 18px;
      }
    }
    @media (max-width: 768px) {
      .input-field input {
        font-size: 18px !important;
        height: 55px !important;
      }
      .sequence-text {
        font-size: 16px;
        padding: 12px;
      }
      .sequence-text span.colored-amino-acid {
        font-size: 16px;
        margin-right: 8px;
        min-width: 45px;
      }
      .btn-action {
        width: 100%;
        margin: 0 0 10px 0;
      }
    }
  ")),
  div(class = "intro-text",
      "Welcome, students! 🎓 Observe the generated DNA and RNA sequences. Your task is to enter the correct amino acid sequence by translating the RNA. Let's explore the Central Dogma! 🧬"
  ),
  # Side-by-side layout for input fields (left) and codon chart (right)
  fluidRow(
    # Left column: Input fields and buttons
    column(6, style = "padding-right: 30px; min-width: 320px; max-width: 600px;",
           div(class = "input-section",
               div(class = "input-field",
                   tags$label("DNA Template Sequence:"),
                   textInput("dna_display", NULL, value = "TACACCGAAGGCTAA")
               ),
               div(class = "input-field",
                   tags$label("RNA Sequence:"),
                   textInput("rna_display", NULL, value = "AUGUGGCUUCCGAUU")
               ),
               div(class = "input-field",
                   tags$label("Correct Amino Acid Sequence:"),
                   uiOutput("correct_amino_acid_sequence")
               ),
               div(class = "button-section",
                   # Removed Check Translation button as per user request
                   actionButton("refresh", "Refresh All", class = "btn-action btn-refresh")
               )
           )
    ),
    # Right column: Mutation input and results
    column(6, style = "padding-left: 30px; min-width: 320px; max-width: 500px;",
           div(class = "input-section",
               div(class = "input-field",
                   tags$label("Select Mutation Type:"),
                   selectInput("mutation_type", NULL, choices = c("Insertion", "Deletion"))
               ),
               div(class = "input-field",
                   tags$label("Specify Mutation Position (1-based index):"),
                   numericInput("mutation_position", NULL, value = 1, min = 1, max = 15, step = 1)
               ),
               conditionalPanel(
                 condition = "input.mutation_type == 'Insertion'",
                 div(class = "input-field",
                     tags$label("Base to Insert (A, T, G, C):"),
                     textInput("insertion_base", NULL, value = "A")
                 )
               ),
               div(class = "input-field",
                   tags$label("Select Mutation Classification:"),
                   selectInput("student_mutation_classification", NULL,
                               choices = c("Silent", "Missense", "Nonsense", "Frameshift"))
               ),
               div(class = "button-section",
                   actionButton("submit_mutation", "Submit Mutation", class = "btn-action btn-process")
               )
           )
       )
   ),
   # Mutation results section
   fluidRow(
     column(12,
            uiOutput("mutation_results"),
            textOutput("mutation_error")
     )
   )
)

# Server Definition
server <- function(input, output, session) {
  # Initial DNA and RNA sequences
  initial_dna <- reactiveVal("TACACCGAAGGCTAA")
  initial_rna <- reactiveVal("AUGUGGCUUCCGAUU") 
  
  # Mutation results reactive values
  mutation_results <- reactiveVal(NULL)
  
  # Update display inputs on initial load
  observe({
    updateTextInput(session, "dna_display", value = format_sequence(initial_dna(), chunk_size = 3))
    updateTextInput(session, "rna_display", value = format_sequence(initial_rna(), chunk_size = 3))
    
    # Update position input max value based on current DNA length
    updateNumericInput(session, "mutation_position", max = nchar(initial_dna()))
  })
  
  # Disable DNA and RNA display inputs to make them readonly
  observe({
    shinyjs::disable("dna_display")
    shinyjs::disable("rna_display")
  })
  
  # Removed translation check observer as per user request
  # Show amino acid sequence automatically on refresh or initial load
  observe({
    current_dna <- initial_dna() 
    current_rna <- initial_rna() 
    
    # Render the correct amino acid sequence with color
    correct_amino_acids_array <- translate_rna(current_rna)
    correct_amino_acids_string <- toupper(paste(correct_amino_acids_array, collapse = ""))
    output$correct_amino_acid_sequence <- renderUI({
      div(class = "sequence-text", format_amino_acids_with_color(correct_amino_acids_string))
    })
  })
  
  # Remove validation and input related to amino_acid_input
  observe({
    output$error_message <- renderText("")
  })
  
  # Handle refresh
  observeEvent(input$refresh, {
    # Always generate a 15-nucleotide DNA sequence for consistency
    new_dna <- generate_dna(length = 15)
    
    # Transcribe the new DNA to get the corresponding RNA
    new_rna <- transcribe_dna(new_dna)
    
    # Double-check that no stop codons are present in the RNA
    if (grepl("UAA|UAG|UGA", new_rna)) {
      # If stop codons found, regenerate the sequence
      for (attempt in 1:10) {
        new_dna <- generate_dna(length = 15)
        new_rna <- transcribe_dna(new_dna)
        if (!grepl("UAA|UAG|UGA", new_rna)) {
          break
        }
      }
      # If still has stop codons, use a completely safe sequence
      if (grepl("UAA|UAG|UGA", new_rna)) {
        safe_bases <- c("A", "C", "G")
        new_dna <- paste(sample(safe_bases, 15, replace = TRUE), collapse = "")
        new_rna <- transcribe_dna(new_dna)
      }
    }
    
    # Update reactive values
    initial_dna(new_dna)
    initial_rna(new_rna)
    
    # Update display inputs
    updateTextInput(session, "dna_display", value = format_sequence(new_dna, chunk_size = 3)) # Format new DNA
    updateTextInput(session, "rna_display", value = format_sequence(new_rna, chunk_size = 3)) # Format new RNA
    updateTextInput(session, "amino_acid_input", value = "")
    output$error_message <- renderText("")
    output$success_message <- renderText("")
    output$sequence_results <- renderUI(NULL)
    
    # Clear mutation results
    mutation_results(NULL)
  })
  
  # Handle mutation submission
  observeEvent(input$submit_mutation, {
    # Get current sequences
    original_dna <- initial_dna()
    original_rna <- initial_rna()
    
    # Get mutation parameters
    mutation_type <- input$mutation_type
    position <- input$mutation_position
    student_classification <- input$student_mutation_classification
    
    # Validate position
    if (position < 1 || position > nchar(original_dna)) {
      output$mutation_error <- renderText("Error: Position must be between 1 and the length of the DNA sequence.")
      return()
    }
    
    # Apply mutation based on type
    if (mutation_type == "Insertion") {
      insertion_base <- toupper(input$insertion_base)
      if (!insertion_base %in% c("A", "T", "C", "G")) {
        output$mutation_error <- renderText("Error: Insertion base must be A, T, C, or G.")
        return()
      }
      
      # Apply insertion
      mutated_dna <- paste0(
        substr(original_dna, 1, position - 1),
        insertion_base,
        substr(original_dna, position, nchar(original_dna))
      )
      
      # For insertions, we need to ensure the sequence is a multiple of 3
      # Remove any remaining bases that don't make complete triplets
      remaining_length <- nchar(mutated_dna)
      if (remaining_length %% 3 != 0) {
        # Remove the last 1 or 2 bases to make it a multiple of 3
        complete_triplets_length <- floor(remaining_length / 3) * 3
        mutated_dna <- substr(mutated_dna, 1, complete_triplets_length)
      }
      
    } else if (mutation_type == "Deletion") {
      # Apply deletion
      mutated_dna <- paste0(
        substr(original_dna, 1, position - 1),
        substr(original_dna, position + 1, nchar(original_dna))
      )
      
      # For deletions, we need to ensure the sequence is a multiple of 3
      # Remove any remaining bases that don't make complete triplets
      remaining_length <- nchar(mutated_dna)
      if (remaining_length %% 3 != 0) {
        # Remove the last 1 or 2 bases to make it a multiple of 3
        complete_triplets_length <- floor(remaining_length / 3) * 3
        mutated_dna <- substr(mutated_dna, 1, complete_triplets_length)
      }
    }
    
    # Transcribe mutated DNA to RNA
    mutated_rna <- transcribe_dna(mutated_dna)
    
    # Check if RNA length is still a multiple of 3
    if (nchar(mutated_rna) %% 3 != 0) {
      output$mutation_error <- renderText("Error: Mutation results in RNA sequence that is not a multiple of 3. Please try a different position.")
      return()
    }
    
    # Translate to amino acids
    original_aa <- translate_rna(original_rna)
    mutated_aa <- translate_rna(mutated_rna)
    
    # Check for translation errors
    if (is.character(original_aa) && grepl("^Error:", original_aa[1])) {
      output$mutation_error <- renderText(paste("Translation error:", original_aa[1]))
      return()
    }
    
    if (is.character(mutated_aa) && grepl("^Error:", mutated_aa[1])) {
      output$mutation_error <- renderText(paste("Translation error:", mutated_aa[1]))
      return()
    }
    
    # Classify the mutation
    actual_classification <- classify_mutation(original_aa, mutated_aa, mutation_type, position)
    
    # Check if student classification is correct
    is_correct <- student_classification == actual_classification
    
    # Store results
    results <- list(
      original_dna = original_dna,
      mutated_dna = mutated_dna,
      original_rna = original_rna,
      mutated_rna = mutated_rna,
      original_aa = original_aa,
      mutated_aa = mutated_aa,
      mutation_type = mutation_type,
      position = position,
      student_classification = student_classification,
      actual_classification = actual_classification,
      is_correct = is_correct
    )
    
    mutation_results(results)
  })
  
  # Display mutation results
  output$mutation_results <- renderUI({
    results <- mutation_results()
    if (is.null(results)) return(NULL)
    
    div(class = "results-section",
        h3("Mutation Results"),
        
        # Mutated DNA
        div(class = "sequence-label", "Mutated DNA:"),
        div(class = "sequence-text", format_sequence(results$mutated_dna)),
        
        # Mutated RNA
        div(class = "sequence-label", "Mutated RNA:"),
        div(class = "sequence-text", format_sequence(results$mutated_rna)),
        
        # Mutated Amino Acids
        div(class = "sequence-label", "Mutated Amino Acids:"),
        div(class = "sequence-text", format_amino_acids_with_color(paste(results$mutated_aa, collapse = ""))),
        
        # Mutation details
        div(class = "sequence-label", "Mutation Details:"),
        div(class = "sequence-text", 
            paste("Type:", results$mutation_type, "at position", results$position),
            br(),
            if(results$mutation_type == "Insertion") {
              paste("Base inserted:", toupper(input$insertion_base))
            } else {
              "Base deleted from original position"
            },
            br(),
            br(),
            tags$small(style = "color: #666; font-style: italic;",
                      "Note: Sequences are limited to 15 nucleotides for display. In real biology, mutations can create longer/shorter sequences.")
        ),
        
        # Classification results
        div(class = "sequence-label", "Classification:"),
        div(class = "sequence-text",
            paste("Your answer:", results$student_classification),
            br(),
            paste("Correct answer:", results$actual_classification),
            br(),
            if(results$is_correct) {
              div(class = "success-message", "✅ Correct!")
            } else {
              div(class = "error-message", "❌ Incorrect!")
            },
            br(),
            br(),
            tags$small(style = "color: #666;",
                      if(results$actual_classification == "Frameshift") {
                        "Frameshift: Insertion/deletion of 1 base shifts the reading frame, changing all amino acids after the mutation point."
                      } else {
                        paste("This mutation changes the amino acid sequence without shifting the reading frame.")
                      }
            )
        )
    )
  })
  
  # Error message for mutations
  output$mutation_error <- renderText("")
}

# Function to classify mutations
classify_mutation <- function(original_aa, mutated_aa, mutation_type, position) {
  # Handle insertions and deletions
  if (mutation_type == "Insertion" || mutation_type == "Deletion") {
    # Check if the mutation causes a frameshift
    # Frameshift occurs when the number of bases added/removed is NOT a multiple of 3
    if (mutation_type == "Insertion") {
      # Insertion of 1 base causes frameshift
      return("Frameshift")
    } else {
      # Deletion of 1 base causes frameshift  
      return("Frameshift")
    }
  }
  
  # For substitutions (not implemented in current UI but keeping logic)
  if (length(original_aa) != length(mutated_aa)) {
    return("Frameshift")
  }
  
  # Find the affected codon position
  codon_position <- ceiling(position / 3)
  
  if (codon_position > length(original_aa) || codon_position > length(mutated_aa)) {
    return("Frameshift")
  }
  
  original_codon_aa <- original_aa[codon_position]
  mutated_codon_aa <- mutated_aa[codon_position]
  
  # Classify based on amino acid change
  if (original_codon_aa == mutated_codon_aa) {
    return("Silent")
  } else if (mutated_codon_aa == "STOP") {
    return("Nonsense")
  } else {
    return("Missense")
  }
}

# Run the app
shinyApp(ui = ui, server = server)