% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_and_write_excel.R
\name{write_formatted_excel}
\alias{write_formatted_excel}
\title{This function provide formatted excel file}
\usage{
write_formatted_excel(
  write_list,
  output_path,
  cols_for_color = NULL,
  header_front_size = 12,
  header_front_color = "#FFFFFF",
  header_fill_color = "darkblue",
  header_front = "Aptos",
  body_front = "Aptos",
  body_front_size = 11
)
}
\arguments{
\item{write_list}{a list file (can be compile with single or multiple dataframe)}

\item{output_path}{Path for output file}

\item{cols_for_color}{Column name in the dataframe which should be use for colorizing the cell. The default is null.}

\item{header_front_size}{Header front size (default is 12)}

\item{header_front_color}{hexcode for header front color (default is white)}

\item{header_fill_color}{hexcode for header fill color (default is Red)}

\item{header_front}{Define the name of the front for header (default is Arial Narrow)}

\item{body_front}{Define the name of the front for body (default is Arial Narrow)}

\item{body_front_size}{Body front size (default is 11)}
}
\value{
Nicely formatted excel file
}
\description{
This function provide formatted excel file
}
