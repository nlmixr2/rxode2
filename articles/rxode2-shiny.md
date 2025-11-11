# rxode2 and Shiny

## Facilities for generating R shiny applications

An example of creating an R [shiny
application](http://shiny.rstudio.com) to interactively explore
responses of various complex dosing regimens is available at
<http://qsp.engr.uga.edu:3838/rxode2/RegimenSimulator>. Shiny
applications like this one may be programmatically created with the
experimental function
[`genShinyApp.template()`](https://nlmixr2.github.io/rxode2/reference/genShinyApp.template.md).

The above application includes widgets for varying the dose, dosing
regimen, dose cycle, and number of cycles.

    genShinyApp.template(appDir = "shinyExample", verbose=TRUE)

    library(shiny)
    runApp("shinyExample")

[Click here to go to the Shiny
App](http://qsp.engr.uga.edu:3838/rxode2/RegimenSimulator)

## Exploring parameter fits graphically using shiny

An rxode2 object can be explored with `rxShiny(obj)`.
[`rxShiny()`](https://nlmixr2.github.io/rxode2/reference/rxShiny.md)
will also allow you to try new models to see how they behave.
