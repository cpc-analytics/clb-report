# Checks for the ingest step on made-up data: the allowlist, the member definition and the
# column guard. Run from the project root: Rscript tests/test-ingest.R
suppressMessages({library(dplyr); library(tidyr); library(tibble)})
source(here::here("scripts", "ingest-data.R"))
txt <- file.path(Sys.getenv("TMPDIR"), "raw-synth.txt")
writeLines(c(
 "id,username,email,name,group_names,created_at,last_seen_at,last_posted_at,time_read,topics_entered,posts_read_count,likes_given,topic_count,post_count,likes_received,avatar",
 "1,user_a,a@WHO.int,Name A,trust_level_1;CollabSecretariat,2025-01-02 10:00:00,2026-06-29 08:00:00,,120,3,4,5,1,2,3,x",
 "2,user_b,b@mail.example.org,Name B,trust_level_0,2026-05-01 10:00:00,2026-06-01 08:00:00,2026-06-02 08:00:00,60,1,1,1,0,0,0,y",
 "3,discobot,discobot_email,,,2020-01-01 00:00:00,2026-06-01 00:00:00,,0,0,0,0,0,0,0,z",
 "4,user_c,no_email,,,2021-01-01 00:00:00,2026-06-01 00:00:00,,0,0,0,0,0,0,0,z",
 "5,user_d,d@afro.who.int,Name D,,2024-01-01 00:00:00,2026-06-30 00:00:00,,5,0,0,0,0,0,0,w",
 "6,user_e,e@notwho.int,Name E,,2024-01-01 00:00:00,,,5,0,0,0,0,0,0,w"), txt)
raw <- readr::read_csv(txt, col_select = any_of(weekly_required), show_col_types = FALSE)
stopifnot(!"name" %in% names(raw), !"avatar" %in% names(raw))   # never even loaded
clean <- minimize_members(raw)
stopifnot(nrow(clean) == 4, identical(clean$email_domain, c("who.int","example.org","afro.who.int","notwho.int")),
          identical(clean$is_who, c(TRUE,FALSE,TRUE,FALSE)), !any(c("email","username") %in% names(clean)))
out <- file.path(Sys.getenv("TMPDIR"), "clean-synth.txt"); readr::write_csv(clean, out)
back <- readr::read_csv(out, show_col_types = FALSE)
stopifnot(inherits(back$last_seen_date, "Date"), is.logical(back$is_who))
r <- tryCatch(stop_if_identifiers(tibble(email = "x")), error = function(e) "blocked"); stopifnot(r == "blocked")
# survey + geo
sv <- tibble(a = c("h", "1", "2"), b = c("h","x","y"), c = c("h","1","2"), d=c("h","1","2"), e=c("h","1","2"), f=c("h","en","fr"), g=c("Sat","Very satisfied","Neutral"),
             h = c("Rec","Very likely","Neutral"), i=c("n","Yes","No"), j=c("a","Yes","No"), k=c("e","Yes","No"), l=c("ac","Yes","No"), m=c("kn","Yes","No"), n=c("in","Yes","No"))
s <- minimize_survey(sv, cfg$survey_rounds[[2]]); stopifnot(names(s)[1] == "Language", nrow(s) == 2)
stopifnot(identical(names(minimize_geo(tibble(ip = "1.2.3.4", country = "X"))), "country"))
# refuses unexpected columns on read
r <- tryCatch(stop_if_unexpected(tibble(id = 1, user_email = "x"), cfg$clean_weekly_columns), error = function(e) "blocked"); stopifnot(r == "blocked")
# a shifted survey layout is caught
shifted <- tibble(Language = "x", Satisfaction = rep("free text", 5), Recommended = "Very likely", Network = "Yes", Analysis = "Yes",
                  Events = "Yes", Access = "Yes", Knowledge = "Yes", Interaction = "Yes")
r <- tryCatch(check_survey_answers(shifted, cfg$survey_rounds[[2]]), error = function(e) "blocked"); stopifnot(r == "blocked")
# a re-exported raw file with the same name is picked up again
raw_f <- tempfile(); clean_f <- tempfile(); writeLines("a", clean_f); Sys.setFileTime(clean_f, Sys.time() - 100); writeLines("b", raw_f)
stopifnot(needs_ingest(raw_f, clean_f), !needs_ingest(clean_f, raw_f), needs_ingest(clean_f, raw_f, force = TRUE), needs_ingest(raw_f, tempfile()))
cat("INGEST OK\n")
