library(globals)

message("findGlobals() with formula ...")

g <- findGlobals(. ~ x + y : z, substitute = TRUE)
print(g)
assert_identical_sets(g, c("~", ".", "+", "x", ":", "y", "z"))

g <- findGlobals(map(1L, ~ typeof(.x)), substitute = TRUE)
print(g)
assert_identical_sets(g, c("map", "~", "typeof", ".x"))


message("- findGlobals() with NULL in the formula ...")
## BUG: https://github.com/HenrikBengtsson/globals/issues/59
for (substitute in c(TRUE, FALSE)) {
  message("- substitute = ", substitute)
  
  g <- findGlobals(. ~ NULL, substitute = substitute)
  print(g)
  assert_identical_sets(g, c(".", "~"))

  g <- findGlobals(NULL ~ NULL, substitute = substitute)
  print(g)
  assert_identical_sets(g, c("~"))

  g <- findGlobals(~ NULL, substitute = substitute)
  print(g)
  assert_identical_sets(g, c("~"))

  g <- findGlobals(NULL ~ ., substitute = substitute)
  print(g)
  assert_identical_sets(g, c("~", "."))
}

# ## substitute=FALSE
# Browse[2]> str(expr)
#  language ~NULL
# 
# ## substitute=TRUE
# Browse[2]> str(expr)
# Class 'formula'  language ~NULL
#   ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 


message("- findGlobals() with ellipsis in formulas ...")
## BUG: https://github.com/HenrikBengtsson/globals/issues/62

g <- findGlobals(list(..., ..3) ~ list(., .x, ..., ..1, ..2))
print(g)
assert_identical_sets(g, c("~", "list", "...", "..3", ".", ".x", "..1", "..2"))

message("- findGlobals() with NULL in formulas ...")
## BUG: https://github.com/HenrikBengtsson/globals/issues/64

env <- new.env(parent = globalenv())
env$`~` <- function(...) "OVERRIDE!"

x <- ~ NULL
g <- eval(quote(findGlobals(x)), env)
assert_identical_sets(g, "~")

x <- list(~ NULL)
g <- eval(quote(findGlobals(x)), env)
assert_identical_sets(g, "~")

x <- list(NULL ~ NULL)
g <- eval(quote(findGlobals(x)), env)
assert_identical_sets(g, "~")

x <- list(NULL ~ b)
g <- eval(quote(findGlobals(x)), env)
assert_identical_sets(g, c("~", "b"))


message("findGlobals() with formula ... DONE")


message("globalsOf() with formula ...")

foo <- function(x) {
  map(1L, ~ typeof(x + .x))
}

g <- globalsOf(foo(1L), substitute = TRUE, mustExist = FALSE)
str(g)
assert_identical_sets(names(g), c("foo", "map", "{", "~", "typeof", "+", "x", ".x"))

message("globalsOf() with formula ... DONE")

