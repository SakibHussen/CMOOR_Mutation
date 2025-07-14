

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
  amino_acids <- c()
  for (codon in codons) {
    aa <- genetic_code[[codon]]
    if (is.null(aa)) {
      aa <- "Invalid Codon"
    }
    amino_acids <- c(amino_acids, aa)
    if (aa == "STOP") break
  }
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

# --- UI Definition ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("🌟 Mutation Simulation Explorer"),
  tags$style(HTML("
    body { background-color: #f0f4f8; font-family: 'Inter', sans-serif; color: #333; }
    .input-section { background-color: #fff; padding: 20px; border-radius: 10px; box-shadow: 0 4px 10px rgba(0,0,0,0.1); margin-bottom: 20px; }
    .sequence-label { font-weight: bold; font-size: 20px; color: #2c3e50; margin-top: 15px; margin-bottom: 8px; }
    .sequence-text {
      font-family: 'Courier New', monospace;
      font-size: 28px;
      background-color: #f8f9fa;
      padding: 15px;
      border-radius: 5px;
      border: 1px solid #ddd;
      min-height: 70px;
      max-height: 70px;
      word-wrap: normal;
      word-break: normal;
      white-space: nowrap;
      width: 100%;
      box-sizing: border-box;
      overflow-x: auto;
      overflow-y: hidden;
      height: 70px;
      display: flex;
      align-items: center;
      margin-bottom: 0;
    }
    .sequence-text span.colored-amino-acid { margin-right: 10px; font-family: 'Courier New', monospace; font-size: 28px; font-weight: bold; display: inline-block; min-width: 50px; text-align: center; padding: 4px 8px; border-radius: 4px; background-color: #fff; }
    .student-section { background: #fff3e0; border-radius: 10px; padding: 18px; margin-top: 20px; border: 1.5px dashed #ff9800; }
    .student-section label { color: #d84315; font-weight: bold; }
    .btn-refresh { background-color: #28a745; color: white; font-size: 18px; padding: 12px 24px; border-radius: 5px; border: none; margin: 0 10px 10px 0; }
    .btn-refresh:hover { background-color: #218838; }
    .btn-mutate { background-color: #1976d2; color: white; font-size: 18px; padding: 12px 24px; border-radius: 5px; border: none; margin: 20px 0 0 0; display: block; width: 220px; margin-left: auto; margin-right: auto; }
    .btn-mutate:hover { background-color: #0d47a1; }
    .btn-submit { background-color: #1976d2; color: white; font-size: 18px; padding: 12px 24px; border-radius: 5px; border: none; margin: 20px 0 0 0; display: block; width: 220px; margin-left: auto; margin-right: auto; }
    .btn-submit:hover { background-color: #0d47a1; }
    .error-message { color: #dc3545; font-size: 18px; margin-top: 10px; font-weight: bold; }
    .success-message { color: #28a745; font-size: 18px; margin-top: 10px; font-weight: bold; }
    .large-input {
      width: 100% !important;
      height: 70px !important;
      min-height: 70px !important;
      padding: 0 !important;
      display: flex;
      align-items: center;
    }
    .large-input input {
      font-size: 28px !important;
      height: 70px !important;
      min-height: 70px !important;
      width: 100% !important;
      box-sizing: border-box !important;
      padding: 16px !important;
      margin: 0 !important;
      display: block;
    }
  ")),
  fluidRow(
    # Static (left)
    column(4, style = "padding: 0 20px;",
           div(class = "input-section",
               div(class = "sequence-label", "DNA Template Sequence:"),
               div(class = "sequence-text", textOutput("static_dna")),
               div(class = "sequence-label", "RNA Sequence:"),
               div(class = "sequence-text", textOutput("static_rna")),
               div(class = "sequence-label", "Correct Amino Acid Sequence:"),
               div(class = "sequence-text", uiOutput("static_aa"))
           )
    ),
    # Codon chart (center)
    column(4, style = "padding: 0 20px; display: flex; flex-direction: column; align-items: center; justify-content: center;",
           div(class = "input-section", style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
               img(
                 src = "codon_chart.png",
                 style = "width: 100%; max-width: 450px; height: auto; border-radius: 10px; box-shadow: 0 4px 10px rgba(0,0,0,0.1); margin-bottom: 10px;"
               ),
               div(
                 style = "text-align: center; color: #666; font-size: 14px; margin-top: 10px;",
                 "Codon chart designed by Dr. Sayumi York & Dr. John Finnerty.",
                 tags$br(),
                 "App coded by Sakib Hussen."
               )
           )
    ),
    # Dynamic (right)
    column(4, style = "padding: 0 20px;",
           div(class = "input-section",
               div(class = "sequence-label", "Mutated DNA:"),
               div(class = "large-input", textInput("mutated_dna", NULL, value = "", width = "100%", placeholder = "Enter mutated DNA")),
               actionButton("mutate_btn", "Mutate", class = "btn-mutate"),
               div(class = "sequence-label", "Mutated RNA:"),
               div(class = "sequence-text", textOutput("mutated_rna")),
               div(class = "sequence-label", "Mutated Amino Acids:"),
               div(class = "sequence-text", uiOutput("mutated_aa_colored"))
           )
    )
  ),
  div(class = "button-section", style = "text-align:center; margin: 20px 0;",
      actionButton("refresh", "Refresh All", class = "btn-refresh")
  ),
  div(class = "student-section",
      div(style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
          span("This is a"),
          selectInput("student_type", NULL, choices = c("Missense", "Nonsense", "Silent", "Frameshift"), width = "140px"),
          span("mutation, because"),
          textInput("student_reason", NULL, width = "200px", placeholder = "your explanation"),
          span(". The change in the final form and function of the polypeptide is expected to be"),
          selectInput("student_effect", NULL, choices = c("large", "unknown", "none"), width = "90px"),
          span(".")
      )
  ),
  actionButton("submit_check", "Submit", class = "btn-submit"),
  uiOutput("error_message"),
  uiOutput("check_feedback")
)

# --- Server Definition ---
server <- function(input, output, session) {
  # --- Static (original) sequence state ---
  static_dna <- reactiveVal(generate_dna(15))
  static_rna <- reactiveVal("")
  static_aa <- reactiveVal("")
  
  # On refresh, generate new static DNA and reset mutated fields
  observeEvent(input$refresh, {
    new_dna <- generate_dna(15)
    new_rna <- transcribe_dna(new_dna)
    new_aa <- translate_rna(new_rna)
    static_dna(new_dna)
    static_rna(new_rna)
    static_aa(new_aa)
    updateTextInput(session, "mutated_dna", value = format_sequence(new_dna, 3))
    output$error_message <- renderUI("")
    output$check_feedback <- renderUI("")
    output$mutated_rna <- renderText({ format_sequence(new_rna, 3) })
    output$mutated_aa_colored <- renderUI({
      aa_str <- paste(new_aa, collapse = " ")
      format_amino_acids_with_color(aa_str)
    })
  }, ignoreInit = TRUE)
  
  # On app start, initialize static and mutated sequences
  observe({
    new_dna <- static_dna()
    new_rna <- transcribe_dna(new_dna)
    new_aa <- translate_rna(new_rna)
    static_rna(new_rna)
    static_aa(new_aa)
    updateTextInput(session, "mutated_dna", value = format_sequence(new_dna, 3))
    output$check_feedback <- renderUI("")
    output$mutated_rna <- renderText({ format_sequence(new_rna, 3) })
    output$mutated_aa_colored <- renderUI({
      aa_str <- paste(new_aa, collapse = " ")
      format_amino_acids_with_color(aa_str)
    })
  })
  
  # Display static panel
  output$static_dna <- renderText({ format_sequence(static_dna(), 3) })
  output$static_rna <- renderText({ format_sequence(static_rna(), 3) })
  output$static_aa <- renderUI({ format_amino_acids_with_color(paste(static_aa(), collapse = " ")) })
  
  # Mutate button logic: update RNA and AA from DNA
  observeEvent(input$mutate_btn, {
    dna <- toupper(gsub(" ", "", input$mutated_dna))
    # Truncate DNA to the largest multiple of 3 from the left
    n <- nchar(dna)
    n3 <- n - (n %% 3)
    if (n3 > 0) {
      dna_proc <- substr(dna, 1, n3)
    } else {
      dna_proc <- ""
    }
    rna <- transcribe_dna(dna_proc)
    aa <- translate_rna(rna)
    # Update the DNA input box to show formatted triplets
    updateTextInput(session, "mutated_dna", value = format_sequence(dna_proc, 3))
    output$mutated_rna <- renderText({ format_sequence(rna, 3) })
    output$mutated_aa_colored <- renderUI({
      aa_str <- paste(aa, collapse = " ")
      format_amino_acids_with_color(aa_str)
    })
  })
  
  # Validation and error handling for mutated DNA only
  output$error_message <- renderUI({
    msgs <- c()
    # Mutated DNA validation
    dna <- toupper(gsub(" ", "", input$mutated_dna))
    if (nchar(dna) > 0 && !grepl("^[ATCG]+$", dna)) {
      msgs <- c(msgs, "Mutated DNA must contain only A, T, C, G.")
    }
    if (nchar(dna) > 0 && (nchar(dna) < 12 || nchar(dna) > 15)) {
      msgs <- c(msgs, "Mutated DNA must be 12-15 nucleotides.")
    }
    if (length(msgs) > 0) {
      HTML(paste(msgs, collapse = "<br>"))
    } else {
      ""
    }
  })
  
  # Check/Submit logic
  observeEvent(input$submit_check, {
    # Get correct values
    student_dna <- toupper(gsub(" ", "", input$mutated_dna))
    rna <- transcribe_dna(student_dna)
    aa <- translate_rna(rna)
    student_rna <- rna
    student_aa <- paste(aa, collapse = " ")
    
    # 1. Check if student's RNA is a correct transcription of their DNA (always true now)
    rna_ok <- TRUE
    # 2. Check if student's AA is a correct translation of their RNA (always true now)
    aa_ok <- TRUE
    # 3. DNA check: just validate format
    dna_ok <- (nchar(student_dna) %% 3 == 0 && grepl("^[ATCG]+$", student_dna))
    
    # 4. Mutation type and effect classification
    original_aa <- static_aa()
    mutated_aa <- aa
    classify_mutation <- function(original_aa, mutated_aa) {
      if (length(original_aa) != length(mutated_aa)) return("Frameshift")
      diff_idx <- which(original_aa != mutated_aa)
      if (length(diff_idx) == 0) return("Silent")
      if (any(mutated_aa == "STOP" & seq_along(mutated_aa) <= length(original_aa))) return("Nonsense")
      return("Missense")
    }
    correct_type <- classify_mutation(original_aa, mutated_aa)
    student_type <- input$student_type
    type_ok <- (student_type == correct_type)
    
    # Effect rule: Frameshift/Nonsense = large, Missense = unknown, Silent = none
    correct_effect <- switch(correct_type,
                             "Frameshift" = "large",
                             "Nonsense" = "large",
                             "Missense" = "unknown",
                             "Silent" = "none",
                             "none"
    )
    student_effect <- input$student_effect
    effect_ok <- (student_effect == correct_effect)
    
    output$check_feedback <- renderUI({
      div(
        if (dna_ok) div(class = "success-message", "Mutated DNA: Correct!") else div(class = "error-message", "Mutated DNA: Check your answer."),
        if (rna_ok) div(class = "success-message", "Mutated RNA: Correct!") else div(class = "error-message", "Mutated RNA: Check your answer."),
        if (aa_ok) div(class = "success-message", "Mutated Amino Acids: Correct!") else div(class = "error-message", "Mutated Amino Acids: Check your answer."),
        if (type_ok) div(class = "success-message", paste0("Mutation Type: Correct! (", correct_type, ")")) else div(class = "error-message", paste0("Mutation Type: Check your answer. (Correct: ", correct_type, ")")),
        if (effect_ok) div(class = "success-message", paste0("Effect: Correct! (", correct_effect, ")")) else div(class = "error-message", paste0("Effect: Check your answer. (Correct: ", correct_effect, ")"))
      )
    })
  })
}

# --- STOP codon display fix ---
format_amino_acids_with_color <- function(amino_acid_string) {
  if (is.null(amino_acid_string) || nchar(amino_acid_string) == 0) {
    return(NULL)
  }
  amino_acid_string <- toupper(gsub("[ ]+", " ", trimws(amino_acid_string)))
  amino_acids <- unlist(strsplit(amino_acid_string, " "))
  formatted_html_elements <- lapply(amino_acids, function(aa_code) {
    props <- amino_acid_properties[[aa_code]]
    if (is.null(props)) {
      return(tags$span(class = "colored-amino-acid", style = "color: #333333; font-weight: bold;", aa_code))
    } else {
      return(tags$span(class = "colored-amino-acid", style = paste0("color: ", props$color, "; font-weight: bold;"), aa_code))
    }
  })
  tagList(formatted_html_elements)
}


shinyApp(ui = ui, server = server)