#' Rasta package
#'
#' The Rasta package calculates the time until the next 420 and cleans your Environment and Console.
#'
#' @param
#' @return
#' @export



Rasta <- function(language = "English") {

  remove(list = ls(name = globalenv()), envir = .GlobalEnv)
  cat("\014")

  timezone = Sys.timezone()

  current_systime = lubridate::ymd_hms(lubridate::now(), tz = timezone)

  this_year_420 = (paste(year(current_systime), "-04-20 16:20:00", timezone))
  this_year_420 = lubridate::ymd_hms(this_year_420)
  this_year_420 = lubridate::force_tz(this_year_420, timezone)

  next_year_420 = (paste(year(current_systime)+1, "-04-20 16:20:00", timezone))
  next_year_420 = lubridate::ymd_hms(next_year_420)
  next_year_420 = lubridate::force_tz(next_year_420, timezone)

  if (current_systime > this_year_420) {
    time_to_next_420 = next_year_420-current_systime
    difference <- lubridate::interval(lubridate::ymd_hms(current_systime), lubridate::ymd_hms(next_year_420))
    difference = (as.period(difference, unit = "day"))
  } else if (current_systime < this_year_420) {
    time_to_next_420 = this_year_420-current_systime
    difference <- lubridate::interval(lubridate::ymd_hms(current_systime), lubridate::ymd_hms(this_year_420))
    difference = (as.period(difference, unit = "day"))
  }


  if (language == "English") {
  print(paste("There is", day(difference), "days,", hour(difference), "hours and", minute(difference), "minutes left until next 420.",
              "Your Environment and Console have been cleaned – happy coding! If you haven't reached Ballmer's Peak yet, you know how to get there..."))
  } else if (language == "German") {
    print(paste("Es geht noch", day(difference), "Tage", hour(difference), "Stunden und", minute(difference), "Minuten bis zum nächsten 420.",
                "Dein Environment und die Konsole wurden geleert – fröhliches Coden! Falls Du den Ballmer's Peak noch nicht erreicht hast, Du weiss wie Du dorthin gelangst..."))
  } else if (language == "Swiss German") {
    print(paste("Es gaht no", day(difference), "Täg,", hour(difference), "Stund und", minute(difference), "Minute bis zum nöchste 420.",
                "Dis Environment und d Konsole sind gleert worde – fröhlichs Rassssstacoding! Falls de Ballmer's Peak nonig erreicht hesch, Du weisch wie ihn erreiche chasch..."))
  }
}


