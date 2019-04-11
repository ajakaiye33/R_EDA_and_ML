#Exercise:1 creating dataframes

scored_point <- c(24,17,24,20)
score_allowed <- c(27,24,13,17)

games <- data.frame(scored_point, score_allowed)

games$diff <- games$scored_point - games$score_allowed

games$won <- games$scored_point > games$score_allowed

opp_names <- c("Broncos","Bears","Cowboys","Cardinals")

rownames(games) <- opp_names
games
