# Title: Binomial Functions
# Inputs: check_functions.R, auxillary_functions.R
# Outputs: bin_choose(), bin_probability(), bin_distribution(), bin_cumulative()
# Description: This scripts writes all of the main functions for our binomial package.
# This means that these functions are able to be called by the the user. This includes Binomial Choose, Binomial Probability, Binomial Distribution, and Binomial Cumulative.

#' @title Binomial Choose
#' @description The number of combinations in which k successes can occur in n trials.
#' @param k number of successes (can be in vector)
#' @param n number of trials
#' @return total number of choose combinations (can be in vector)
#' @export
#' @examples
#' # 5 choose 2
#' bin_choose(n = 5, k = 2)
#'
#' # 5 choose 0
#' bin_choose(5, 0)
#'
#' # 5 choose c(1,2,3) vector form
#' bin_choose(5, 1:3)
#'
bin_choose <- function(n, k){
  if(!is.numeric(k)){
    stop("invalid k value - must be numeric")
  }
  if(sum(k == floor(k)) != length(k)){
    stop("invalid k value - must be whole number")
  }
  if(!is.numeric(n)){
    stop("invalid n value - must be numeric")
  }
  if(sum(n== floor(n)) != length(n)){
    stop("invalid n value - must be whole number")
  }
  if(n < k ){
    stop("n cannot be less than k")
  }
  choose = factorial(n)/ (factorial(k)*(factorial(n - k)))
  return(choose)
}


#' @title Binomial Probability
#' @description The probability of a certain outcome of a binomial distribution given number of trials and successes.
#' @param success number of successes (can be in vector)
#' @param trials number of trials
#' @param prob probability of event
#' @return the probability of the binomial distribution (can be vector)
#' @export
#' @examples
#' # probability of getting 2 successes in 5 trials
#  # (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' # probabilities of getting 2 or less successes in 5 trials
#' # (assuming prob of success = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#'
#' # 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = 0.45)
#'
bin_probability <- function(success, trials, prob){
  check_prob(prob)
  check_trials(trials)
  check_success(success, trials)
  p = choose(trials, success) * (prob^success) * (1-prob)^(trials - success)
  return(p)
}

#' @title Binomial Distribution
#' @description A main function including a dataframe of successes and probabilitis with two classes c(bindis, data.frame), can be used with plot().
#' @param trials number of trials
#' @param prob probability of event
#' @return dataframe of class c(bindis, data.frame)
#' @export
#' @examples
#' # binomial probability distribution with 5 trials and prob = 0.5
#' bin_distribution(trials = 5, prob = 0.5)
#'
#' # plot of binomial distribution with 5 trials and prob = 0.5
#' plot(bin_distribution(trials = 5, prob = 0.5))
#'
bin_distribution <- function(trials, prob){
  df = data.frame(success = 0:trials, probability = bin_probability(0:trials, trials, prob))
  class(df) <- c("bindis", "data.frame")
  return(df)
}

# plot method: plot.bindis
#' @export
plot.bindis <- function(x,...){
  plotter<-
    ggplot2::ggplot(data=x, ggplot2::aes(x = success, y=probability)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::xlab("Successes") +
    ggplot2::ylab("Probability") +
    ggplot2::scale_x_discrete(limits = x$success) +
    ggplot2::ggtitle("Binomial Distribution Plot")
  return(plotter)
  invisible(x)
}


#' @title Binomial Cumulative
#' @description A main function of a cumulative distribution of a binomial function, from 0 to number of trials, can be used with plot().
#' @param trials number of trials
#' @param prob probability of event
#' @return dataframe with sucesses in the first column, probability in the second column, and cumulative in the third column of class c(cinbcum, data.frame)
#' @export
#' @examples
#' # binomial cumulative distribution with 5 trials and prob = 0.5
#' z = bin_cumulative(trials = 5, prob = 0.5)
#'
#' # binomial cumulative distribution with 20 trials and prob = 0.3
#' bin_cumulative(trials = 20, prob = 0.3)
#'
#' # plot
#' plot(z)
#'
bin_cumulative<-function(trials, prob){
  df = data.frame(success = 0:trials,
                  probability = bin_probability(0:trials, trials, prob),
                  cumulative = cumsum(bin_probability(0:trials, trials, prob)))
  class(df) <-  c("bincum", "data.frame")
  return(df)
}

# plot method: plot.bincum
#' @export
plot.bincum <- function(x,...){
  plotter =
    ggplot2::ggplot(data=x, ggplot2::aes(x = success, y=cumulative)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("Successes") +
    ggplot2::ylab("Probability") +
    ggplot2::scale_x_discrete(limits = x$success) +
    ggplot2::ggtitle("Cumulative Binomial Distribution Plot")
  return(plotter)
  invisible(x)
}

#' @title Binomial Variable
#' @description A Binomial Random Variable, can be used with print() and summary()
#' @param trials number of trials
#' @param prob probability of event
#' @return Returns an object of class "binvar", that is, a binomial random variable object.
#' @export
#' @examples
#' # default
#' k <- bin_variable
#'
#' # print
#' k
#'
#' # summary
#' k.summary <- summary(k)
#'
#' # print summary
#' k.summary
#'
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  output = list(trials = trials, prob = prob)
  class(output) <- c("binvar")
  return(output)
}

# summary method: print.binvar
#' @export
print.binvar <- function(x,...){
  cat(paste("\"Binomial variable\" \n\nParameters \n- number of trials:",
            x$trials,
            "\n- prob of success:",
            x$prob))
  invisible(x)
}

# summary method: summary.binvar
#' @export
summary.binvar <- function(x,...){
  output.summary <-
    list(trials  = x$trials,
         prob = x$prob,
         mean = aux_mean(x$trials, x$prob),
         variance = aux_variance(x$trials, x$prob),
         mode = aux_mode(x$trials, x$prob),
         skewness = aux_skewness(x$trials, x$prob),
         kurtosis = aux_kurtosis(x$trials, x$prob)
    )
  class(output.summary) <- c("summary.binvar")
  return(output.summary)
  invisible(x)
}

# print method: print.summary.binvar
#' @export
print.summary.binvar <- function(x,...){
  cat(paste("\"Summary Binomial\" \n\nParameters \n- number of trials:", x$trials,
            "\n- prob of success:", x$prob,
            "\n\nMeasures \n- mean:", x$mean,
            "\n- variance:", x$var,
            "\n- mode:", x$mode,
            "\n- skewness:", round(x$skewness, 7),
            "\n- kurtosis:", round(x$kurtosis,7))
  )
  invisible(x)
}

#' @title Binomial Mean
#' @description The average value of a binomial distribution given trials and prob
#' @param trials number of trials
#' @param prob probability of event
#' @return returns a numeric mean of the binomial distribution
#' @export
#' @examples
#' # mean of a binomial with 10 trials and a prob of 0.3
#' bin_mean(10, 0.3)
#'
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title Binomial variance
#' @description The variance of a binomial distribution given trials and prob
#' @param trials number of trials
#' @param prob probability of event
#' @return returns a numeric variance of the binomial distribution
#' @export
#' @examples
#' # mean of a binomial with 10 trials and a prob of 0.3
#' bin_variance(10, 0.3)
#'
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title Binomial mode
#' @description The mode of a binomial ddistribution given trials and prob
#' @param trials number of trials
#' @param prob probability of event
#' @return returns a numeric mode of the binomial distribution
#' @export
#' @examples
#' # mode of a binomial with 10 trials and a prob of 0.3
#' bin_mode(10, 0.3)
#'
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title Binomial skewness
#' @description The skewness of a binomial distribution given trials and prob
#' @param trials number of trials
#' @param prob probability of event
#' @return returns a numeric skewness of the binomial distribution
#' @export
#' @examples
#' # skewness of a binomial with 10 trials and a prob of 0.3
#' bin_skewness(10, 0.3)
#'
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title Binomial kurtosis
#' @description The kurtosis of a binomial distribution given trials and prob
#' @param trials number of trials
#' @param prob probability of event
#' @return returns a numeric kurtosis of the binomial distribution
#' @export
#' @examples
#' # kurtosis of a binomial with 10 trials and a prob of 0.3
#' bin_kurtosis(10, 0.3)
#'
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}

