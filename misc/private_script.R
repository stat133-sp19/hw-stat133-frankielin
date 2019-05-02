# private function: check_prob()
check_prob <- function(prob){
  if(!is.numeric(prob)){
    stop("invalid prob value - must be numeric")
  } else if (prob> 1 | prob < 0){
    stop("prob must be between 0 and 1")
  }
  return(TRUE)
}

# private function: check_trials()
check_trials <- function(trials){
  if(!is.numeric(trials)){
    stop("invalid trials value - must be numeric")
  } else if(trials != floor(trials)){
    stop("invalid trials value - must be whole number")
  } else if(trials < 0){
    stop("invalid trials value- cannot be negative")
  }
  return(TRUE)
}

#private function: check_success()
check_success <- function(success, trials){
  if(!is.numeric(success)){
    stop("invalid success value - must be numeric")
  }
  else if(sum(success == floor(success)) != length(success)){
    stop("invalid success value - must be whole number")
  }
  check_trials(trials)
  out <- (success >= 0 & success <= trials)
  if(sum(out) != length(success)){
    stop("'success cannot be greater than trials")
  }
  return(TRUE)
}

#private function: aux_mean()
aux_mean <- function(trials, prob){
  return(trials * prob)
}

#private function: aux_variance()
aux_variance <- function(trials, prob){
  return(prob*(1-prob)*trials)
}

#private function: aux_mode()
aux_mode <- function(trials, prob){
  if(prob == 0){
    return(0)
  }
  if(prob == 1){
    return(trials)
  }
  pre_m = trials*prob + prob
  if (pre_m %% 1 == 0) {
    return(c(floor(pre_m), floor(pre_m)-1))
  }
  return(floor(pre_m))
}

#private function: aux_skew()
aux_skewness <- function(trials, prob){
  s = (1-2*prob)/sqrt(aux_variance(trials, prob))
  return(s)
}

#private function: aux_kurtosis
aux_kurtosis <- function(trials, prob){
  k = (1 - 6*prob*(1 - prob))/(aux_variance(trials,prob))
  return(k)
}


#' @title Binomial Choose
#' @description the number of combinations in which k successes can occur in n trials
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
  check_success(k, n)
  choose = factorial(n)/ (factorial(k)*(factorial(n - k)))
  return(choose)
}


#' @title Binomial Probability
#' @description the probability of a certain binomial outcome
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
#' @description A main function including a dataframe of successes and probability with two classes c(bindis, data.frame)
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
  require(ggplot2, quietly = TRUE)
  plotter<-
    ggplot(data=x, aes(x = success, y=probability)) +
    geom_bar(stat="identity") +
    xlab("Successes") +
    ylab("Probability") +
    scale_x_discrete(limits = x$success) +
    ggtitle("Binomial Distribution Plot")
  return(plotter)
  invisible(x)
}


#' @title Binomial Cumulative
#' @description A main function of a cumulative distribution of a binomial function
#' @param trials number of trials
#' @param prob probability of event
#' @return dataframe with sucesses in the first column, probability in the second column,and cumulative in the third column of class c(cinbcum, data.frame)
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
  require(ggplot2, quietly = TRUE)
  plotter =
    ggplot(data=x, aes(x = success, y=cumulative)) +
    geom_point() +
    geom_line() +
    xlab("Successes") +
    ylab("Probability") +
    scale_x_discrete(limits = x$success) +
    ggtitle("Cumulative Binomial Distribution Plot")
  return(plotter)
  invisible(x)
}

#' @title Binomial Variable
#' @description A Binomial Random Variable
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
#' @description The average value that a binomial produces
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
#' @description The variance of a binomial dsitribution
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
#' @description The mode of a binomial dsitribution
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
#' @description The skewness of a binomial dsitribution
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
#' @description The kurtosis of a binomial dsitribution
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

