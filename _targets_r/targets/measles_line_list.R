tar_target(measles_line_list, {
  read.delim("https://raw.githubusercontent.com/kassteele/Nowcasting/refs/heads/master/data/measles_NL_2013_2014.dat", sep = "") |>
    mutate(
      reference_date = ymd(onset.date),
      report_date = ymd(report.date)
    ) |>
    select(reference_date, report_date)
})
