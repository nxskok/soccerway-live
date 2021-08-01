# remove blanks
deblank <- function(x) {
  x %>% 
    str_replace_all("\n", "") %>% 
    str_replace_all(" +", "")
}

# get scorers
get_scorers <- function(html) {
  html %>% 
    html_nodes("span.scorer") %>% 
    html_nodes("a") %>% 
    html_text() %>% 
    enframe() %>% 
    mutate(has_rtn = str_detect(value, "\n")) %>% 
    filter(has_rtn) %>%
    mutate(value = str_replace_all(value, "\n", "")) %>% 
    select(scorer = value)
}

# get minutes of goals
get_minutes <- function(html) {
  html %>% 
    html_nodes("span.minute") %>% 
    html_text() %>% 
    deblank() -> m
  # print(m)
  m
}

# get score after each goal
get_scores <- function(html) {
  html %>% html_nodes("span.score") %>% 
    html_text()
}

# make tibble of scoring history
make_story <- function(html) {
  v1 <- get_scorers(html)
  print(v1)
  v2 <- get_minutes(html)
  v3 <- get_scores(html)
  n <- length(v2)
  if (n==0) {
    n <- 1
    v2[1] <- "xx"
  }
  v3 <- v3[1:n]
  v1 %>% 
    slice(1:n) %>% 
    mutate(mins = v2,
           score = v3) %>% 
    select(mins, score, scorer)
}

# get relevant part of webpage - this is input to four functions above
get_scorer_info <- function(html) {
  html %>% html_nodes("ul.scorer-info") %>% 
    html_nodes("li") 
}

# get teams in match
get_teams <- function(html) {
  html %>% html_nodes("a.team-title") %>% html_text()
}

# get ids of teams in match

get_team_ids <- function(html) {
  html %>% 
    html_nodes("a.team-title") %>% 
    html_attr("href") %>% 
    enframe(name = NULL) %>% 
    extract(value, into = "id", regex = "/(\\d+)/$", convert = TRUE) %>% 
    pull(id)
}

# get score of match

get_score <- function(html) {
  html %>% html_nodes("h3.thick.scoretime") %>% 
    html_text2() %>% 
    str_extract("[0-9]+ - [0-9]+") -> score
  score <- ifelse(is.na(score), "", score)
  score <- ifelse(length(score) == 0, "", score)
  score
}

# get match status
get_match_status <- function(html) {
  html %>% html_nodes("span.match-state") %>% 
    html_text() -> x
  html %>%
    html_nodes("span") -> spans
  spans %>% 
    html_attr("class") -> classes
  spans %>% 
    html_text() -> texts
  tibble(class = classes, text = texts) %>% 
    filter(class == "game-minute") %>% 
    pull(text) -> y
  x %>% .[[1]] -> z
  ifelse(z == "", y, z)
}

# get the html that contains the match info - input to functions above
get_match_all_info <- function(html) {
  html %>% html_nodes("div.match-info") 
}

possibly_read_html <- possibly(read_html, otherwise = NULL)

# get html of match
get_match_html <- function(match_id) {
  base <- "https://uk.soccerway.com/matches/2021/03/09/england/championship/blackburn-rovers-football-club/swansea-city-afc/"
  my_url <- str_c(base, match_id, "/")
  # return(my_url)
  Sys.sleep(1) 
  possibly_read_html(my_url)
}

# get all the info
get_all_info <- function(match_id) {
  # dan't call this if match already done
  html <- get_match_html(match_id)
  if (!is.null(html)) {
    h1 <- get_scorer_info(html)
    score <- get_score(html)
    d <- make_story(h1)
    h <- get_match_all_info(html)
    teams <- get_teams(h)
    team_ids <- get_team_ids(h)
    status <- get_match_status(h)
  } else {
    score <- ""
    d <- tibble(x = numeric())
  }
  # return(list(h1=h1, d=d, h=h, teams=teams, status=status))
  # pb$tick()
  if (nrow(d)==0)  {
    d <- tibble(mins = "-'", score = score, scorer = "")
  }
  if (!exists("teams") | length(teams) < 2) { stop(glue::glue("In get_all_info match {match_id}, teams fails")) }
  d %>% mutate(
    t1 =  teams[1],
    t2 = teams[2],
    id1 = team_ids[1],
    id2 = team_ids[2],
    status = status
  ) %>% 
    select(t1, t2, status, everything())
}

# take a dataframe with column score, retrieve last score
last_score <- function(d) {
  if (nrow(d)==0) return("0-0")
  d %>% slice(nrow(.)) %>% pull(score)
}

status_of <- function(d) {
  d %>% slice(1) %>% pull(status)
}

add_last_status <- function(d) {
  d %>% 
    rowwise() %>% 
    mutate(last = last_score(info),
           stat = status_of(info))
}

snapshot <- function(matches) {
  # call get_all_info unless game known to be done
  tibble(matches) %>% 
    rowwise() %>% 
    mutate(info = list(get_all_info(matches))) %>% 
    add_last_status() 
}

unroll <- function(d, col) {
  d %>% unnest({{col}}) %>% select(matches, t1:scorer)
}

compare_scores <- function(d1, d2) {
  d1 %>% right_join(d2, by = "matches") %>% 
    mutate(reason = case_when(
      stat.y == "FT" & stat.x != "FT" ~ "status",
      stat.y == "AET" & stat.x != "AET" ~ "status",
      last.y != last.x ~ "goal",
      is.na(stat.x)    ~ "new",
      TRUE             ~ "no"
    )) %>% 
    filter(reason != "no") -> dd
  if (nrow(dd) == 0) return(dd)
  dd %>%  unnest(info.y) 
  # %>% 
    # select(matches, t1:scorer)
}

update_scores <- function(current, matches, ranks) {
  previous <- current
  current <- snapshot(matches)
  # print(previous)
  if (!is.list(previous)) {
    current %>% unroll(info) -> d
  } else {
    d <- compare_scores(previous, current)
  }
  # add ranks and remove dones
  # dones %>% select(matches) -> dones2
  if (nrow(d)>0) {
    d %>% 
      # anti_join(dones2) %>% 
      left_join(ranks, by = c("id1" = "sw_id")) %>% 
      left_join(ranks, by = c("id2" = "sw_id")) %>% 
      mutate(league = ifelse(league.x == league.y, league.x, str_c(league.x, " - ", league.y))) %>% 
      select(match = matches, league, t1, r1 = rank.x, t2, 
             r2 = rank.y, status, mins, score, scorer) -> d
  }
  list(current, d)
}

get_match_list <- function(last_get, get_all = FALSE, extra_leagues = numeric(0)) {
  games <- read_rds("~/Documents/r-projects/scoresway/rds/games.rds")
  if (!get_all) {
    read_rds("~/Documents/r-projects/ratings/league_ids.rds") %>% unnest(id) %>% 
      pull(id) -> league_id
  }
  league_id <- c(league_id, extra_leagues)
  # league_id
  games %>% 
    filter(get_all | comp %in% league_id) %>% 
    filter(between(time_stamp, last - dhours(2.5), now())) %>% 
    select(match, country, comp, comp_name, t1_name, t2_name, time_stamp) -> games1
  games1 %>% arrange(comp) %>%  pull(match) -> matches
  # msg <- glue::glue("There are {length(matches)} matches.")
  # message(msg)
  matches
}

get_matches_2 <- function(current, matches) {
  if (is.null(nrow(current))) {
    matches2 <- matches
  } else {
    current %>% filter(str_detect(stat, "FT")) -> dones
    # dones
    tibble(matches) %>% anti_join(dones) %>%
      pull(matches) -> matches2
    current %>% anti_join(dones) -> current2
  }
  list(current2, matches2, dones)
}