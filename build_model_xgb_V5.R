
rm(list = ls())
gc()



library(data.table)
library(xgboost)
library(dplyr)
library(stringdist)



setwd("~/Documents/StudyWork/Kaggle/MusicRecommendation/CODES")
source("lib.R")



testing = F
from_scratch = F



if(from_scratch){
  train <- fread("../DATA/train.csv")
  
  
  
  ## KNOWN/BASIC members information -----------------------------
  members <- fread("../DATA/members.csv")
  members[, registration_init_time := as.Date(strptime(registration_init_time, format = "%Y%m%d"))]
  members[, registration_year := as.numeric(year(registration_init_time))]
  members[, registration_month := as.integer(month(registration_init_time))]
  
  members[, expiration_date := as.Date(strptime(expiration_date, format = "%Y%m%d"))]
  members[, expiration_year := as.numeric(year(expiration_date))]
  members[, expiration_month := as.integer(month(expiration_date))]
  
  members[bd<0 | bd > 150, bd := NA]
  members[, bd := as.numeric(bd)]
  members[, member_since := as.numeric(expiration_date - registration_init_time)]
  members[member_since <=0, member_since := NA]
  
  members_cols = c("msno", "city", "bd", "gender", "registered_via", "registration_year", "registration_month", "expiration_year", "expiration_month", "member_since")
  train <- merge(train, members[, members_cols, with = F], by = "msno", all.x = T)
  
  save(members, members_cols, file = "../DERIVED/members.Rdata")
  rm(members)
  gc()
  
  
  
  ## KNOWN/BASIC songs/artist information---------------------------------
  songs <- fread("../DATA/songs.csv", encoding = "UTF-8")
  songs[, language := as.integer(language)]
  
  # song features
  songs[, num_genres := unlist(lapply(genre_ids, function(x) {
    a = sum(gregexpr("[\\|]",x)[[1]]>0) + 1
    if(x==""){
      a = 0
    }
    return(a)
  }))]
  songs[, num_artists :=  unlist(lapply(artist_name, function(x) {
    a = sum(gregexpr("[\\||and|feat|,|&]",x)[[1]]>0) + 1
    if(x==""){
      a = 0
    }
    return(a)
  }))]
  songs[, num_composers :=  unlist(lapply(composer, function(x) {
    a = sum(gregexpr("[\\||/|;|,|\\]",x)[[1]]>0) + 1
    if(x==""){
      a = 0
    }
    return(a)
  }))]
  songs[, num_lyricists :=  unlist(lapply(lyricist, function(x) {
    a = sum(gregexpr("[\\||/|;|,|\\]",x)[[1]]>0) + 1
    if(x==""){
      a = 0
    }
    return(a)
  }))]
  
  songs <- get.similarty(songs, "artist_name","composer")
  songs <- get.similarty(songs, "artist_name","lyricist")
  songs <- get.similarty(songs, "lyricist","composer")
  
  # artist features
  artists_lang <- songs[, list(num_tracks_language_artist = .N), by = c("artist_name","language")]
  artists <- songs[, list(mean_song_length_artist = mean(song_length), num_tracks_artist = .N, num_languages_artist = length(unique(language))), by = "artist_name"]
  
  songs <- merge(songs, artists_lang, by = c("artist_name","language"), all.x = T)
  songs <- merge(songs, artists, by = "artist_name", all.x = T)
  songs[, ratio_mean_song_length_artist := song_length/mean_song_length_artist]
  
  songs_cols = c("song_id", "song_length", "genre_ids", "artist_name", "composer", "lyricist", "language",
                 "sim_artist_name_composer", "sim_artist_name_lyricist", "sim_lyricist_composer",
                 "num_genres", "num_artists", "num_composers", "num_lyricists", "num_tracks_language_artist",
                 "mean_song_length_artist", "num_tracks_artist", "num_languages_artist", "ratio_mean_song_length_artist")
  train <- merge(train, songs[, songs_cols, with = F], by = "song_id", all.x = T)
  
  save(songs, songs_cols, file = "../DERIVED/songs.Rdata")
  rm(songs, artists, artists_lang)
  gc()
  
  
  
  ## merge song_extra data---------------------------------
  songs_extra <- fread("../DATA/song_extra_info.csv")
  songs_extra[, country := substr(isrc, start = 1, stop = 2)]
  songs_extra[, issuer := substr(isrc, start = 3, stop = 5)]
  songs_extra[, year_issued := as.integer(substr(isrc, start = 6, stop = 7))]
  songs_extra[year_issued>17, year_issued := 1900+year_issued]
  songs_extra[year_issued<=17, year_issued := 2000+year_issued]
  
  songs_extra_cols = c("song_id", "country", "issuer", "year_issued")
  train <- merge(train, songs_extra[, songs_extra_cols, with = F], by = "song_id", all.x = T)
  
  save(songs_extra, songs_extra_cols, file = "../DERIVED/songs_extra.Rdata")
  rm(songs_extra)
  gc()
  
  
  
  save(train, file = "../DERIVED/train_basic_features.Rdata")
  
}else{
  load("../DERIVED/train_basic_features.Rdata")
}



## Song/Artist' Engagement---------------------------------
s1 = get.CV.stat.v2(df = copy(train[,c("song_id"), with = F]), nfold = 1, var1 = c("song_id"), var2 = NULL, thr = NULL, func = NULL)
s2 = get.CV.stat.v2(df = copy(train[,c("artist_name"), with = F]), nfold = 1, var1 = c("artist_name"), var2 = NULL, thr = NULL, func = NULL)
s3 = get.CV.stat.v2(df = copy(train[,c("lyricist"), with = F]), nfold = 1, var1 = c("lyricist"), var2 = NULL, thr = NULL, func = NULL)
s4 = get.CV.stat.v2(df = copy(train[,c("composer"), with = F]), nfold = 1, var1 = c("composer"), var2 = NULL, thr = NULL, func = NULL)
s5 = get.CV.stat.v2(df = copy(train[,c("genre_ids"), with = F]), nfold = 1, var1 = c("genre_ids"), var2 = NULL, thr = NULL, func = NULL)

setnames(s1, "m", "song_plays")
setnames(s2, "m", "artist_plays")
setnames(s3, "m", "lyricist_plays")
setnames(s4, "m", "composer_plays")
setnames(s5, "m", "genre_ids_plays")
train <- merge(train, s1, by = "song_id", all.x = T)
train <- merge(train, s2, by = "artist_name", all.x = T)
train <- merge(train, s3, by = "lyricist", all.x = T)
train <- merge(train, s4, by = "composer", all.x = T)
train <- merge(train, s5, by = "genre_ids", all.x = T)
rm(s1, s2, s3, s4, s5)
gc()



## Members' Engagement---------------------------------
m1 = get.CV.stat.v2(df = copy(train[,c("msno"), with = F]), nfold = 1, var1 = c("msno"), var2 = NULL, thr = NULL, func = NULL)
m2 = get.CV.stat.v2(df = copy(train[,c("msno","artist_name"), with = F]), nfold = 1, var1 = c("msno","artist_name"), var2 = NULL, thr = NULL, func = NULL)
m3 = get.CV.stat.v2(df = copy(train[,c("msno","source_type"), with = F]), nfold = 1, var1 = c("msno","source_type"), var2 = NULL, thr = NULL, func = NULL)
m4 = get.CV.stat.v2(df = copy(train[,c("msno","language"), with = F]), nfold = 1, var1 = c("msno","language"), var2 = NULL, thr = NULL, func = NULL)
m5 = get.CV.stat.v2(df = copy(train[,c("msno","song_length"), with = F]), nfold = 1, var1 = c("msno"), var2 = "song_length", thr = NULL, func = function(x) return(mean(x)), return_count = F)
setnames(m1, "m", "member_song_plays")
setnames(m2, "m", "member_artist_plays")
setnames(m3, "m", "member_source_type_plays")
setnames(m4, "m", "member_language_plays")
setnames(m5, "m", "member_mean_song_length")
train <- merge(train, m1, by = "msno", all.x = T)
train <- merge(train, m2, by = c("msno","artist_name"), all.x = T)
train <- merge(train, m3, by = c("msno","source_type"), all.x = T)
train <- merge(train, m4, by = c("msno","language"), all.x = T)
train <- merge(train, m5, by = c("msno"), all.x = T)
train[, ratio_member_mean_song_length := song_length/member_mean_song_length]
rm(m1, m2, m3, m4, m5)
gc()



# ## Members' Replay Habits -------------------------------
train[, ID := 1:.N]
m1 = get.CV.stat.v2(df = copy(train[,c("ID","msno","source_type","target"), with = F]), nfold = 5, var1 = c("msno","source_type"), var2 = "target", thr = 30, func = function(x) return(mean(x)), return_count = F)
m2 = get.CV.stat.v2(df = copy(train[,c("ID","msno","artist_name","target"), with = F]), nfold = 5, var1 = c("msno","artist_name"), var2 = "target", thr = 30, func = function(x) return(mean(x)), return_count = F)
m3 = get.CV.stat.v2(df = copy(train[,c("ID","msno","genre_ids","target"), with = F]), nfold = 5, var1 = c("msno","genre_ids"), var2 = "target", thr = 30, func = function(x) return(mean(x)), return_count = F)
m4 = get.CV.stat.v2(df = copy(train[,c("ID","msno","language","target"), with = F]), nfold = 5, var1 = c("msno","language"), var2 = "target", thr = 30, func = function(x) return(mean(x)), return_count = F)
m5 = get.CV.stat.v2(df = copy(train[,c("ID","msno","language","source_type","target"), with = F]), nfold = 5, var1 = c("msno","language","source_type"), var2 = "target", thr = 20, func = function(x) return(mean(x)), return_count = F)
m6 = get.CV.stat.v2(df = copy(train[,c("ID","msno","language","source_system_tab","target"), with = F]), nfold = 5, var1 = c("msno","language","source_system_tab"), var2 = "target", thr = 20, func = function(x) return(mean(x)), return_count = F)
setnames(m1, "m", "member_source_type_replay_prob")
setnames(m2, "m", "member_artist_replay_prob")
setnames(m3, "m", "member_genre_ids_prob")
setnames(m4, "m", "member_language_prob")
setnames(m5, "m", "member_language_source_type_prob")
setnames(m6, "m", "member_language_source_system_tab_prob")
train <- merge(train, m1, by = "ID", all.x = T)
train <- merge(train, m2, by = c("ID"), all.x = T)
train <- merge(train, m3, by = "ID", all.x = T)
train <- merge(train, m4, by = c("ID"), all.x = T)
train <- merge(train, m5, by = c("ID"), all.x = T)
train <- merge(train, m6, by = c("ID"), all.x = T)
train[, ID := NULL]
rm(m1, m2)
gc()




## CV statistc of replay prob for categorical variables with too many levels ------------------------
cols_fac <- c("song_id", "msno", "genre_ids", "artist_name", "composer", "lyricist")
thresholds = c(30, 50, 30, 30, 30, 30)
train[, ID := 1:.N]
for(c in cols_fac){
  
  print(c)
  x <- get.CV.stat.v2(copy(train[,c("ID",c,"target"), with = F]), nfold = 5, var1 = c, var2 = "target", thr = thresholds[cols_fac==c], func = function(x){return(mean(x))}, return_count = F)
  train <- merge(train, x, by = "ID", all.x = T)
  train[, (c) := NULL]
  setnames(train, "m", c)
  rm(x)
  gc()
  
}
train[, ID := NULL]



if(!testing){
  #save(train, file = "../DERIVED/train.Rdata")
  write.csv(train, file = "../DERIVED/train.csv", row.names = F)
  
}



## get final list of features - cols_select --------------------------
cols_reject = c("ID", "target")
cols_select= colnames(train)[!(colnames(train) %in% cols_reject)]



## separate independent and dependent variables -------------------------
x_train <- subset(train, select = cols_select)
y_train <- train$target
rm(train)
gc()



## create a 70-30 train-validation split -------------------------------------------------------------------
set.seed(12345)
if(testing){
  indx <- sample(1:nrow(x_train), 0.5*nrow(x_train), replace = F)
  x_train <- x_train[-indx,]
  y_train <- y_train[-indx]
}
set.seed(12345)
indx <- sample(1:nrow(x_train), 0.3*nrow(x_train), replace = F)
x_test <- x_train[indx,]
y_test <- y_train[indx]

x_train <- x_train[-indx,]
y_train <- y_train[-indx]
gc()



# Numeric encoding for the factor variables
cols_fac_lev <- list()
cols_fac <- colnames(x_train)[sapply(x_train, class)=="character" | sapply(x_train, class)=="factor"]
for(c in cols_fac){
  
  print(c)
  
  levels_fac <- unique(x_train[, get(c)])
  levels_fac = sort(levels_fac)
  
  x_train[, (c) := as.integer(factor(get(c), levels = levels_fac))]
  x_test[, (c) := as.integer(factor(get(c), levels = levels_fac))]
  
  if("Others" %in% levels_fac){
    x_test[is.na(get(c)), (c) := "Others"]
  }
  
  print(length(which(is.na(x_test[, get(c)]))))
  
  cols_fac_lev[[c]] = levels_fac
  
}



## prepare data for xgboost - xgb.Dmatrix for train and validation -------------------------------------------------------------------
train.xg <- xgb.DMatrix(as.matrix(x_train), label=y_train, missing=NA)
test.xg <- xgb.DMatrix(as.matrix(x_test),label=y_test, missing=NA)
invisible(gc())



## Model parameters -------------------------------------------------------------------
log_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
if(testing){
  fname_msgs = paste0("../LOGS/msgs_xgb_exp_", log_time, ".txt")
}else{
  fname_msgs = paste0("../LOGS/msgs_xgb_", log_time, ".txt")
}
file_msgs <- file(fname_msgs, open="wt")
sink(file_msgs, type="output")
params <- list(
  "objective"           = "binary:logistic",
  "eval_metric"         = "auc",
  "eta"                 = 0.01,
  "max_depth"           = 10,
  "min_child_weight"    = 500,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)
print(params)
print("\n")



## Train Model with xgb.cv for optimal nrounds and then with xgb.train-------------------------------------------------------------------
Sys.time()
set.seed(123)
model_xgb <- xgb.train(data=train.xg, nrounds = 2501,
                       params = params, verbose = 1, missing = NA, 
                       early.stop.round = 200, 
                       maximize = T, print.every.n = 100,
                       watchlist = list(validation1 = test.xg, validation2 = train.xg)
)
Sys.time()
sink(file = NULL, type = "output")
closeAllConnections()



importance <- data.frame(xgb.importance(cols_select, model = model_xgb))



save(importance, file = "../MODELS/model_xgb_imp_2.Rdata")
save(model_xgb, file = "../MODELS/model_xgb_2.Rdata")
rm(x_train, x_test)
gc()



## Start scoring ----------------------------------
test <- fread("../DATA/test.csv")
load( "../DERIVED/songs.Rdata")
load( "../DERIVED/members.Rdata")
load("../DERIVED/songs_extra.Rdata")
test <- merge(test, members[, members_cols, with = F], by = "msno", all.x = T)
test <- merge(test, songs[, songs_cols, with = F], by = "song_id", all.x = T)
test <- merge(test, songs_extra[, songs_extra_cols, with = F], by = "song_id", all.x = T)

rm(members, songs, songs_extra)
gc()



load("../DERIVED/train_basic_features.Rdata")



## Song/Artist' Engagement---------------------------------
s1 = get.CV.stat.v2(df = rbind(copy(train[,c("song_id"), with = F]), copy(test[,c("song_id"), with = F])), nfold = 1, var1 = c("song_id"), var2 = NULL, thr = NULL, func = NULL)
s2 = get.CV.stat.v2(df = rbind(copy(train[,c("artist_name"), with = F]), copy(test[,c("artist_name"), with = F])), nfold = 1, var1 = c("artist_name"), var2 = NULL, thr = NULL, func = NULL)
s3 = get.CV.stat.v2(df = rbind(copy(train[,c("lyricist"), with = F]), copy(test[,c("lyricist"), with = F])), nfold = 1, var1 = c("lyricist"), var2 = NULL, thr = NULL, func = NULL)
s4 = get.CV.stat.v2(df = rbind(copy(train[,c("composer"), with = F]), copy(test[,c("composer"), with = F])), nfold = 1, var1 = c("composer"), var2 = NULL, thr = NULL, func = NULL)
s5 = get.CV.stat.v2(df = rbind(copy(train[,c("genre_ids"), with = F]), copy(test[,c("genre_ids"), with = F])), nfold = 1, var1 = c("genre_ids"), var2 = NULL, thr = NULL, func = NULL)
setnames(s1, "m", "song_plays")
setnames(s2, "m", "artist_plays")
setnames(s3, "m", "lyricist_plays")
setnames(s4, "m", "composer_plays")
setnames(s5, "m", "genre_ids_plays")
test <- merge(test, s1, by = "song_id", all.x = T)
test <- merge(test, s2, by = "artist_name", all.x = T)
test <- merge(test, s3, by = "lyricist", all.x = T)
test <- merge(test, s4, by = "composer", all.x = T)
test <- merge(test, s5, by = "genre_ids", all.x = T)
rm(s1, s2, s3, s4, s5)
gc()



## Members' Engagement---------------------------------
m1 = get.CV.stat.v2(df = rbind(copy(train[,c("msno"), with = F]), copy(test[,c("msno"), with = F])), nfold = 1, var1 = c("msno"), var2 = NULL, thr = NULL, func = NULL)
m2 = get.CV.stat.v2(df = rbind(copy(train[,c("msno","artist_name"), with = F]), copy(test[,c("msno","artist_name"), with = F])), nfold = 1, var1 = c("msno","artist_name"), var2 = NULL, thr = NULL, func = NULL)
m3 = get.CV.stat.v2(df = rbind(copy(train[,c("msno","source_type"), with = F]), copy(test[,c("msno","source_type"), with = F])), nfold = 1, var1 = c("msno","source_type"), var2 = NULL, thr = NULL, func = NULL)
m4 = get.CV.stat.v2(df = rbind(copy(train[,c("msno","language"), with = F]), copy(test[,c("msno","language"), with = F])), nfold = 1, var1 = c("msno","language"), var2 = NULL, thr = NULL, func = NULL)
m5 = get.CV.stat.v2(df = rbind(copy(train[,c("msno","song_length"), with = F]), copy(test[,c("msno","song_length"), with = F])), nfold = 1, var1 = c("msno"), var2 = "song_length", thr = NULL, func = function(x) return(mean(x)), return_count = F)
setnames(m1, "m", "member_song_plays")
setnames(m2, "m", "member_artist_plays")
setnames(m3, "m", "member_source_type_plays")
setnames(m4, "m", "member_language_plays")
setnames(m5, "m", "member_mean_song_length")
test <- merge(test, m1, by = "msno", all.x = T)
test <- merge(test, m2, by = c("msno","artist_name"), all.x = T)
test <- merge(test, m3, by = c("msno","source_type"), all.x = T)
test <- merge(test, m4, by = c("msno","language"), all.x = T)
test <- merge(test, m5, by = c("msno"), all.x = T)
test[, ratio_member_mean_song_length := song_length/member_mean_song_length]
rm(m1, m2, m3, m4, m5)
gc()



# ## Members' Replay Habits -------------------------------
m1 = get.CV.stat.v2(df = copy(train[,c("msno","source_type","target"), with = F]), nfold = 1, var1 = c("msno","source_type"), var2 = "target", thr = 50, func = function(x) return(mean(x)), return_count = F)
m2 = get.CV.stat.v2(df = copy(train[,c("msno","artist_name","target"), with = F]), nfold = 1, var1 = c("msno","artist_name"), var2 = "target", thr = 50, func = function(x) return(mean(x)), return_count = F)
m3 = get.CV.stat.v2(df = copy(train[,c("msno","genre_ids","target"), with = F]), nfold = 1, var1 = c("msno","genre_ids"), var2 = "target", thr = 50, func = function(x) return(mean(x)), return_count = F)
m4 = get.CV.stat.v2(df = copy(train[,c("msno","language","target"), with = F]), nfold = 1, var1 = c("msno","language"), var2 = "target", thr = 50, func = function(x) return(mean(x)), return_count = F)
m5 = get.CV.stat.v2(df = copy(train[,c("msno","language","source_type","target"), with = F]), nfold = 1, var1 = c("msno","language","source_type"), var2 = "target", thr = 50, func = function(x) return(mean(x)), return_count = F)
m6 = get.CV.stat.v2(df = copy(train[,c("msno","language","source_system_tab","target"), with = F]), nfold = 1, var1 = c("msno","language","source_system_tab"), var2 = "target", thr = 50, func = function(x) return(mean(x)), return_count = F)
setnames(m1, "m", "member_source_type_replay_prob")
setnames(m2, "m", "member_artist_replay_prob")
setnames(m3, "m", "member_genre_ids_prob")
setnames(m4, "m", "member_language_prob")
setnames(m5, "m", "member_language_source_type_prob")
setnames(m6, "m", "member_language_source_system_tab_prob")
test <- merge(test, m1, by = c("msno","source_type"), all.x = T)
test <- merge(test, m2, by = c("msno","artist_name"), all.x = T)
test <- merge(test, m3, by = c("msno","genre_ids"), all.x = T)
test <- merge(test, m4, by = c("msno","language"), all.x = T)
test <- merge(test, m5, by = c("msno","language","source_type"), all.x = T)
test <- merge(test, m6, by = c("msno","language","source_system_tab"), all.x = T)
rm(m1, m2, m3, m4, m5, m6)
gc()




## CV statistc for categorical variables with too many levels ------------------------
cols_fac <- c("song_id", "msno", "genre_ids", "artist_name", "composer", "lyricist")
thresholds = c(30, 50, 30, 30, 30, 30)

for(c in cols_fac){
  
  print(c)
  x <- get.CV.stat.v2(copy(train[,c(c,"target"), with = F]), nfold = 1, var1 = c, var2 = "target", thr = thresholds[cols_fac==c], func = function(x){return(mean(x))}, return_count = F)
  test <- merge(test, x, by = c, all.x = T)
  test[, (c) := NULL]
  setnames(test, "m", c)
  # if(c=="msno"){
  #   setnames(test, "n", paste0(c,"_replays"))
  # }else{
  #   test[, n := NULL]
  # }
  # 
  rm(x)
  gc()
  
}



rm(train)
gc()
write.csv(test, file = "../DERIVED/test.csv", row.names = F)



# select columns
x_score <- subset(test, select = cols_select)



# numeric encoding ----------------------------------------------------
cols_fac <- names(cols_fac_lev)
for(c in cols_fac){
  
  levels_fac <- cols_fac_lev[[c]]
  
  x_score[, (c) := as.integer(factor(get(c), levels = levels_fac))]
  
  if("Others" %in% levels_fac){
    x_score[is.na(get(c)), (c) := "Others"]
  }
  
}



## prepare data for xgboost - xgb.Dmatrix ------------------------------------------------------
score.xg <- xgb.DMatrix(as.matrix(x_score), missing = NA)
gc()



## Score data-------------------------------------------------------------------
Sys.time()
preds_score = predict(model_xgb, score.xg, ntreelimit = 2500, missing = NA)
test[, target := preds_score]
Sys.time()



## make submission -----------------------------------------------------
submission <- fread("../DATA/sample_submission.csv")
submission[, target := NULL]
submission <- merge(submission, test[,c("id", "target"), with = F], by = "id", all.x = T)

write.csv(submission, file = "../SUBMISSION/submission_14_xgb.csv", row.names = F)





