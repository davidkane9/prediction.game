#' Play the prediction game.
#' 
#' @param data A numeric vector.
#' @param n The number of times to play the game.
#' @param guess_1 A number.
#' @param guess_2 A number.
#' @param FUN A function to apply to data.
#' @param ... Optional arguments to FUN.
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
#' play(data = 1:10, n = 5, 3, 7, sample, size = 1)

# Big picture: I want to pass in functions like "median(sample(data$x, 5,
# replace = FALSE))". Can't figure out how! Happy to have weird syntax for
# accessing the data which is passed in. Or happy to hack this by just assuming
# that the data exists in the environment and is not actually passed in.

# Here are some notes on our current problem. First, consider some quotes from
# Advanced R:

# "Each time a function is called, a new environment is created to host
# execution. This is called the execution environment, and its parent is the
# function environment."

# "An execution environment is usually ephemeral; once the function has
# completed, the environment will be garbage collected."

# "This happens because every time a function is called a new environment is
# created to host its execution. This means that a function has no way to tell
# what happened the last time it was run; each invocation is completely
# independent."

# The issue seems to be that, when map_dbl() and other functions are executed
# within another function, we don't get the natural change in the random seed
# which we would expect. This works great:

# map_dbl(.x = 1:5, .f = ~ runif(1))
# [1] 0.4758260 0.1840778 0.4199492 0.5779564 0.8983577

# This does not!

# my_func <- function(FUN){map_dbl(.x = 1:5, .f = ~ FUN)}
# my_func(runif(1))
# [1] 0.7555603 0.7555603 0.7555603 0.7555603 0.7555603

# Argg! That is no good. It is as if the environment which is created for each
# call to runif() within the map_dbl() gets deleted before R "knows" to evolve
# the random seed.

# Note that even using rerun --- as we do in the code below --- does not work by
# itself.

# my_func <- function(FUN){unlist(rerun(.n = 5,  FUN))}
# > my_func(runif(1))
# [1] 0.7477465 0.7477465 0.7477465 0.7477465 0.7477465

# So it seems like FUN(data, ...) --- as in the code below --- is key. My guess
# is that the presence of the data argument forces R to "notify" itself that it
# is making a call --- to sample() in the case of the current code --- which then
# causes the random seed to evolve, which leads to 5 different answers.

play <- function(data, n, guess_1, guess_2, FUN, ...){
  
  # Need more argument error checking and at least a few test cases.
  
  stopifnot(is_scalar_double(n))
  
  # The biggest problem we face is a desire to allow the user to specify complex
  # functions like "median(sample(data$x, 5, replace = FALSE))". This is tricky
  # for two reasons.
  
  # First, how do we deal with the specific names of the variables we are
  # working with? That is, data should be a tibble with x and y and whatever.
  # But, right now, we are just passing in the name of the tibble. We don't
  # mention the variable names. The function (FUN) argument is just something
  # like sample. But we don't know which column in "data" the function "sample"
  # should be applied to. For now, only allow you to pass in a vector. Must be a
  # better way!
  
  # Second, how to handle complex functions, especially ones that take
  # arguments. Right now, we use ... which works fine in these simple cases. But
  # I am not sure that this scales to more complex situations. In particular, I
  # would like FUN to be able to be a chain of pipes . . .
  
  # For now, as long as data is a vector and FUN works on vectors, everything
  # should work as advertized. Then, for Tidyverse standards, data (since it is
  # a vector) should be renamed x.

  # Next version, data should be (or at least allowed to be) a tibble and then
  # this won't work (unless we check for the two cases explicitly). And that is
  # OK! Probably no need to allow someone to pass in a vector. Or maybe they can
  # pass in anything which works in their function . . .
  
  # Or maybe we should not have a separate data argument. Instead, we know that
  # the data will exist in the environment when they run play(). Will play()
  # find it if we allow them to pass in a full expression like median(data$x)?
  # That would be hacky, but, perhaps, easy. 
  
  # Alas, although it "works" in that the function can find that data, it seems
  # like, each time it does, the random seed gets reset (because you are
  # "jumping" out of the functions environment?) and, so, you get the same
  # sample each time. So, I think we have to pass the data in somehow.
  
  # Or, actually, this issue of no-randomness seems to be caused by something
  # other than the passing in of data with the function call.
  
  # Something like map_dbl(1:{{n}}, ~ FUN) seens to work OK, but we need a way
  # for FUN to refer to stuff in the local environment only, except for the
  # no-randomness.
  
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
    
    # This works, but I am not convinced it is best. First, should I be using
    # flatten() instead of unlist. Second, rerun() is listed as being in the
    # "questioning lifecycle stage." Why use something that R is not committed
    # to?
    
    # I want something like map_dbl(1:{{n}}, ~ FUN({{data}}, ...)) to work in
    # the next line. But I can't quite figure out why . . .
    
    mutate(answer = unlist(rerun(.n = {{n}}, FUN(data, ...)))) %>% 
    
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
  
  x
}
