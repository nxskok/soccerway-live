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
    deblank()
}

# get score after each goal
get_scores <- function(html) {
  html %>% html_nodes("span.score") %>% 
    html_text()
}

# make tibble of scoring history
make_story <- function(html) {
  get_scorers(html) %>% 
    mutate(mins = get_minutes(html),
           score = get_scores(html)) %>% 
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

# get match status
get_match_status <- function(html) {
  html %>% html_nodes("span.match-state") %>% 
    html_text() %>% .[[1]]
}

# get the html that contains the match info - input to functions above
get_match_all_info <- function(html) {
  html %>% html_nodes("div.match-info") 
}

# get html of match
get_match_html <- function(match_id) {
  base <- "https://uk.soccerway.com/matches/2021/03/07/italy/serie-a/fc-crotone/torino-fc/"
  my_url <- str_c(base, match_id, "/")
  read_html(my_url)
}

# get all the info
get_all_info <- function(match_id) {
  html <- get_match_html(match_id)
  h <- get_scorer_info(html)
  d <- make_story(h)
  h <- get_match_all_info(html)
  teams <- get_teams(h)
  status <- get_match_status(h)
  if (nrow(d)==0)  {
    d <- tibble(mins = "0'", score = "0 - 0", scorer = "")
  }
  d %>% mutate(
    t1 = teams[1],
    t2 = teams[2],
    status = status
  ) %>% 
    select(t1, t2, status, everything())
}

# take a dataframe with column score, retrieve last score
last_score <- function(d) {
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
  tibble(matches) %>% 
    rowwise() %>% 
    mutate(info = list(get_all_info(matches))) %>% 
    add_last_status() 
}

unroll <- function(d, col) {
  d %>% unnest({{col}})
}

compare_scores <- function(d1, d2) {
  d1 %>% full_join(d2, by = "matches") %>% 
    filter(last.y != last.x | stat.y != stat.x | is.na(stat.x)) -> dd
  if (nrow(dd) == 0) return(dd)
  dd %>%  unnest(info.y) %>% 
    select(t1:scorer)
}

update_scores <- function(current, matches) {
  previous <- current
  current <- snapshot(matches)
  print(previous)
  if (!is.list(previous)) {
    current %>% unroll(info) -> d
  } else {
    d <- compare_scores(previous, current)
  }
  list(current, d)
}
