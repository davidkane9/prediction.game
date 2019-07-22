#' Play the prediction game.
#'
#' @param n The number of times to play the game.
#' @param guess_1 A number.
#' @param guess_2 A number.
#' @param formula A formula to apply to data. Must begin with ~ and may include
#'   references to data that are in the global workspace.
#'
#' @return A tibble with the results of the game.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom purrr map_dbl
#' @importFrom purrr rerun
#' @importFrom rlang is_scalar_double
#'
#' @export
#'
#' @examples
#' z <- play(n = 5, guess_1 = 3, guess_2 = 7, ~ sample(x = 1:10, size = 1))
#' z
#' 
#' z <- play(n = 500, guess_1 = 20.09, guess_2 = 20.07, 
#'           ~ mean(sample(mtcars$mpg, size = 10, replace = FALSE)))
#' table(z$winner)

# I am not even sure I understand this second example! I would have guessed that
# the best guess is 20.09, which is the result of mean(mtcars$mpg) would win.
# But not quite! A lower number is better because there is a bit of a right skew
# to mpg, resulting in the median being lower than the mean. But the simple
# median is not the best guess either. Best is a number somewhere in between the
# mean and the median. Tricky!

# Next step. Need a function, show(), which takes, via %>%, the result of play()
# and creates a cool animation using d3rain.

# Issue 1: Need to be able to run contests in which the formula does not produce
# a single number. Or do we? Hmmm. 

# Issue 2: No longer hard-code the evaluation function. Right now, we use the
# absolute value of the distance to evaluate the winner. But what if we wanted
# the squared distance? Or is this a bit of a trick question since the winner in
# both metrics is the same . . .

# Big question: What if, instead of declaring a winner each draw, we want to sum
# up all the errors (absolute or squared or cubed or whatever) and then use that
# sum to determine the winner? This would suggest that, within an individual
# contest, we might compare our two guesses against a draw of 10 or 100 or
# whatever answers from the formula.

# Of course, we might still want to do that n times . . .

# Bigger (?) question: Do we need for guesses to be formulas? Not today.
# Calculate your guess however you want, using whatever statistical tricks you
# want. But, at that point, your guess is just as number, as is mine, and we are
# going to see who wins. Any randomness comes from the formula. 

# I guess that this is, implicitly (?), a competition over sort-of bootstraped
# samples . . .

play <- function(n, guess_1, guess_2, formula){
  
  # Need more argument error checking and at least a few test cases.
  
  stopifnot(is_scalar_double(n))
  
  # Maybe this should be a Shiny app? Would then need to pre-load all the data
  # sets we care about.
  
  # We should not need to calculate an empty tibble at the start. Instead, we
  # should be using map_df() (or something like it) which will produce a tibble
  # automatically.
  
  x <- tibble::tibble(.rows = n) %>% 
    tibble::add_column("guess_1" = NA_real_, 
                       "guess_2" = NA_real_, 
                       "answer" = NA_real_)
  
  x <- x %>% 
    mutate(guess_1 = {{guess_1}},
           guess_2 = {{guess_2}}) %>% 
  
    # This won't work if you don't pass in the FUN as starting with a ~. In
    # other words, it needs to be passed in as a formula so that it won't
    # immediately be evaluated.
    
    mutate(answer = map_dbl(.x = 1:{{n}}, .f = formula)) %>% 
    
    mutate(winner = case_when(
       abs(guess_1 - answer) <  abs(guess_2 - answer) ~ "guess_1",
       abs(guess_1 - answer) >  abs(guess_2 - answer) ~ "guess_2",
       abs(guess_1 - answer) == abs(guess_2 - answer) ~ "tie"))
  
  
  # Output will be a tibble with n rows, one for each run of the experiment.
  # Plan is to pass that tibble on to a new function, using nice Tidyverse
  # chaining, which will display the result in some pleasing fashion. Check out
  # the d3rain package. Maybe the argument will be called show()? A dual column
  # histogram that fills over time would be cool!
  
  # Do we really need entire columns with guess_1 and guess_2 repeated n times?
  # Probably not!
  
  invisible(x)
}
