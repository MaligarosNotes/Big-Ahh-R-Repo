# Assignment analysis helpers


load_assignment_data <- function(path = "data/raw/students.csv") {
  if (!file.exists(path)) {
    stop("Data file not found: ", path)
  }
  read.csv(path)
}

add_exam_average <- function(df, exam1 = "exam1", exam2 = "exam2") {
  if (!all(c(exam1, exam2) %in% names(df))) {
    stop("Required columns missing: ", exam1, ", ", exam2)
  }
  df$average <- (df[[exam1]] + df[[exam2]]) / 2
  df
}

install.packages(table)

CAPM_S26$Otex_Percent <- lag(CAPM_S26$OTEX, 1) - CAPM_S26$OTEX 

CAPM_S26
shift(CAPM_S26$OTEX, -1) 

??lag
