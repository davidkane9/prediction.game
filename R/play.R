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
#' z <- play(n = 500, guess_1 = 20.09, guess_2 = 21, 
#'           ~ mean(sample(mtcars$mpg, size = 10, replace = FALSE)))
#' table(z$winner)


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
