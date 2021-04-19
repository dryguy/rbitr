## code to prepare `tag_names` dataset goes here

lichess_tag_roster <- c("BlackElo", "BlackRatingDiff", "BlackTitle", "ECO",
                        "Opening", "Termination", "TimeControl", "UTCDate",
                        "UTCTime", "WhiteElo", "WhiteRatingDiff", "WhiteTitle")
seven_tag_roster <- c("Event", "Site", "Date", "Round", "White", "Black",
                      "Result")

usethis::use_data(lichess_tag_roster, seven_tag_roster, overwrite = TRUE)
