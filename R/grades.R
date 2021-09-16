
convert.to.uiaa.factor <- function(grades) {

  uiaa <- as.vector(sapply(1:12, FUN=function(x) paste0(x, c("-", "", "+"))))
  
  factor(grades, ordered = TRUE, levels = uiaa)
}

convert.to.french.sport.factor <- function(grades) {
  
  french.sport <- as.vector(sapply(1:9, FUN=function(x) paste0(x, c("a", "a+", "b", "b+", "c", "c+"))))
  
  factor(grades, ordered = TRUE, levels = french.sport)
}

convert.to.vermin.factor <- function(grades) {
  
  # hack made V0+ below V0 in order since V0+ almost never used. min grade is V0 for most analyses
  
  vermin <- paste0("V", c("B-","B","B+","0-","0+", 0:17))
  
  factor(grades, ordered = TRUE, levels = vermin)
}

convert.to.ordered.factor <- function(grades, grade.type) {
  
  if (grade.type == "UIAA") {
    convert.to.uiaa.factor(grades);
  } else if (grade.type == "AU") {
    convert.ewbank.grades(as.character(grades))
  } else if (grade.type == "BLDV") {
    convert.to.vermin.factor(grades)
  } else if (grade.type == "FR") {
    convert.to.french.sport.factor(grades)
  } else {
    grades
  }

}

convert.ewbank.grades <- function(grades) {
  
  sapply(grades, FUN=from.to.ewbank.grade)  
}

from.to.ewbank.grade <- function(grade) {
  
  fromto <- str_split(grade, "/| to ", simplify=T);
  return (mean(as.numeric(fromto)))
}