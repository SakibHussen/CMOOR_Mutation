# Mutation Simulation Shiny App

This app helps you learn about DNA mutations and their effects on RNA and proteins. It is designed for students to explore how changes in DNA can affect the resulting amino acid sequence.

## How It Works
- **Enter a DNA Sequence:** Type or paste a DNA sequence (using A, T, C, G) and click "Enter". The app will show the corresponding RNA and colored amino acid sequence.
- **Simulate a Mutation:**
  - Choose a mutation type: **Insertion** or **Deletion**.
  - Select the position in the DNA sequence where the mutation will happen.
  - If you choose Insertion, pick which base (A, T, C, or G) to insert.
- **Predict the Mutation Effect:**
  - After applying the mutation, select what type of mutation you think it is: Silent, Missense, Nonsense, or Frameshift.
- **View Results:**
  - The app will show the original and mutated DNA, RNA, and amino acid sequences.
  - It will display the mutation type detected by the program, the position, the amino acid change, and whether your answer was correct.

## Features
- DNA, RNA, and amino acid sequences are shown clearly.
- Amino acids are colored for easy visualization.
- All logic and interface are in the `mutation.R` file.
- No extra files or charts are needed.

## How to Run
1. Open `mutation.R` in RStudio or your R environment.
2. Make sure you have the `shiny` package installed (`install.packages("shiny")`).
3. Click "Run App" or use `shiny::runApp('mutation.R')`.

Enjoy exploring DNA mutations! 