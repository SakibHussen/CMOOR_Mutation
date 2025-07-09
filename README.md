# CMOOR_mutation Shiny App

## Running the App

To run the Shiny app, follow these steps:

1. Open an R console or RStudio.
2. Set your working directory to the `CMOOR_mutation` folder where `mutation.R` and the `www` folder are located.
3. Run the app with the command:

```R
shiny::runApp()
```

Alternatively, from the terminal, you can run:

```bash
R -e "shiny::runApp('CMOOR_mutation')"
```

This will launch the app in your default web browser.

## Image Display

The app uses an image `codon_chart.png` located in the `www` folder. Ensure this folder and image exist in the `CMOOR_mutation` directory for the image to display correctly.

If the image does not display, try clearing your browser cache or opening the image file directly to verify accessibility.
