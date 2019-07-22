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

# Big picture: I want to pass in functions like "median(sample(tbl$x, 5, replace
# = FALSE))" where the tibble tbl could also be passed in or could, instead,
# just be floating around in the global environment.

# The problem, as Cassandra points out, is that R evaluates the arguments to
# functions immediately. So, right away, that function is run and the answer,
# 16, is produced. Then, 16 is copied 1,000 times when we try to run our
# experiment 1,000 times.

# We are able to avoid this currently because, instead of passing in the full
# function call, we pass in the name of the function and the arguments
# separately. So, R can't evaluate them until we are in the middle of running
# the 1,000 experiments, when we combine FUN(data) and so on. This is not a bad
# approach, but it also is not as flexible as I want it to be. (But continuing
# on this path may be the best approach.)

# The other approach is to delay the evaluation of the full call somehow. I
# don't want it to run until we are within map_dbl(). But I can't figure out
# how.

# Not sure if this is helpful:

# Quote from here: https://adv-r.hadley.nz/functionals.html#passing-arguments

# Note there’s a subtle difference between placing extra arguments inside an
# anonymous function compared with passing them to map(). Putting them in an
# anonymous function means that they will be evaluated every time f() is
# executed, not just once when you call map(). This is easiest to see if we
# make the additional argument random:
#
# plus <- function(x, y) x + y
#
# x <- c(0, 0, 0, 0) map_dbl(x, plus, runif(1)) 
# > [1] 0.0625 0.0625 0.0625
# 0.0625 map_dbl(x, ~ plus(.x, runif(1))) 
# > [1] 0.903 0.132 0.629 0.945

# That is fine. But, again, once I try to put this in a function with a FUN
# argument and then pass in runif(1), it does not work.

# Maybe something like:

# my.call <- call("sample", x = c(1:10), size = 1); eval(my.call); eval(my.call)

# is what we need. Use call() and the ... arguments to build a function call.
# This is held fixed until we get down to the map_dbl() function and, at that
# point, it is run 1,000 times. But I can't get this to work within map_dbl! I
# can, however, get it to work like: replicate(n = 2, eval(my.call))

play <- function(data, n, guess_1, guess_2, FUN, ...){
  
  # Need more argument error checking and at least a few test cases.
  
  stopifnot(is_scalar_double(n))
  
  # The biggest problem we face is a desire to allow the user to specify complex
  # functions like "median(sample(data$x, 5, replace = FALSE))". This is tricky
  # for two reasons.
  
  # First, how do we deal with the specific names of the variables we are
  # working with? That is, data should be a tibble with x and y and whatever.
  # But, right now, we are just passing in the name of the tibble (or, actually,
  # vector right now). We don't mention the variable names. The function (FUN)
  # argument is just something like sample. But we don't know which column in
  # "data" the function "sample" should be applied to. For now, only allow you
  # to pass in a vector. Must be a better way!
  
  # Second, how to handle complex functions, especially ones that take
  # arguments. Right now, we use ... which works fine in these simple cases. But
  # I am not sure that this scales to more complex situations. In particular, I
  # would like FUN to be able to be a chain of pipes . . .
  
  # For now, as long as data is a vector and FUN works on vectors, everything
  # should work as advertised. Then, for Tidyverse standards, data (since it is
  # a vector) should be renamed x.

  # Next version, data should be (or at least allowed to be) a tibble and then
  # this won't work (unless we check for the two cases explicitly). And that is
  # OK! Probably no need to allow someone to pass in a vector. Or maybe they can
  # pass in anything which works in their function . . .
  
  # Perhaps a reasonable next step would be to pass in two arguments, a tibble
  # tb and a variable x. Then, FUN will act on that vector. 
  
  # Or maybe we should not even have a separate data argument. Instead, we know
  # that the data will exist in the environment when they run play(). Will
  # play() find it if we allow them to pass in a full expression like
  # median(data$x)? That would be hacky, but, perhaps, easy.
  
  # But then we would still have the problem of R wanting to evaluate 
  
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
    
    # Maybe call() is what I need? Treat the FUN which is passed in (and the
    # dot, dot, dot arguments) as something which needs to be turned into a call
    # object. Then, we can evaluate that call object within rerun or map_dbl or
    # whatever.
  
    # I think that this section is key:
  
    # https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
    
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
