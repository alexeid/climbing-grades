
convert.to.uiaa.factor <- function(grades) {

  uiaa <- c("1-", "1", "1+", "2-", "2", "2+", "3-", "3", "3+", "4-", "4", "4+", "5-", "5", "5+", "6-", "6", "6+", "7-", "7", "7+", "8-", "8", "8+", "9-", "9", "9+", "10-", "10", "10+", "11-", "11", "11+", "12-", "12", "12+")
  
  factor(grades, ordered = TRUE, levels = uiaa)
}

convert.to.ordered.factor <- function(grades, grade.type) {
  
  if (grade.type == "UIAA") {
    convert.to.uiaa.factor(grades);
  } else if (grade.type == "AU") {
    as.numeric(as.character(grades))
  } else {
    grades
  }

}