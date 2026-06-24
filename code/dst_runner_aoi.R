library(rmarkdown)
render(
  input = "code/dst_report_aoi.Rmd",       # The name of your saved RMarkdown file
  output_file = "dst_report.html",        # Your desired output filename
  output_dir = "reports"             # The target folder
)
