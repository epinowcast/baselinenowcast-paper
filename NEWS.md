# baselinenowcastpaper 0.0.0.1000

-   Add the necessary intermediate targets and figure targets for the Hub validation case study.
-   Set up estimating and applying dispersion to 7 day rolling sum of hospital admissions for Covid case study and compare nowcasts.
-   Generate a configuration file to generate the model permuations applied to the Covid case study.
-   Load in the KIT simple nowcasts for each nowcast date in the Covid case study and combine with the correspoding model permutation.
-   Modularise the pipeline to avoid using the wrapper functions and instead generating each component of the `baselinenowcast` pipeline.
-   First draft of supplement with mathematical model.
-   Add functionality to fit the "base" scenario for to the German Nowcast Hub data for a few nowcast dates and age-groups.
-   Update `baselinenowcast` function names to reflect updated functions.
-   Adjust running of `baselinenowcast` pipeline to be able to bind together nowcasts from independent runs for each weekday.
-   Set up scoring of quantiles for each nowcast.
-   Generate quantiles for each nowcast and create plots to overlay nowcasts and final data.
-   Set-up a generation of a single nowcast using targets.
-   Added pipeline outline.
-   Added repository skeleton.
