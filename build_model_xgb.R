
rm(list = ls())
gc()



library(data.table)
library(xgboost)
library(dplyr)



setwd("~/Documents/StudyWork/Kaggle/MusicRecommendation/CODES")
source("lib.R")



train <- fread("../DATA/train.csv")



## merge members information-----------------------------
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



## merge songs info---------------------------------
songs <- fread("../DATA/songs.csv", encoding = "Latin-1")
songs[, language := as.integer(language)]

# song features
songs[, num_genres := unlist(lapply(genre_ids, function(x) return(length(strsplit(x,"\\|")[[1]]))))]
songs[, num_artists :=  unlist(lapply(artist_name, function(x) return(length(strsplit(x,"\\|")[[1]]))))]
songs[, num_composers :=  unlist(lapply(composer, function(x) return(length(strsplit(x,"\\|")[[1]]))))]
songs[, num_lyricists :=  unlist(lapply(lyricist, function(x) return(length(strsplit(x,"\\|")[[1]]))))]
# artist features
artists_lang <- songs[, list(num_tracks_language_artist = .N), by = c("artist_name","language")]
songs <- merge(songs, artists_lang, by = c("artist_name","language"), all.x = T)
artists <- songs[, list(mean_song_length_artist = mean(song_length), 
                        num_tracks_artist = .N, 
                        num_languages_artist = length(unique(language))), by = "artist_name"]
songs <- merge(songs, artists, by = "artist_name", all.x = T)
songs[, ratio_mean_song_length_artist := song_length/mean_song_length_artist]

songs_cols = c("song_id", "song_length", "genre_ids", "artist_name", "composer", "lyricist", "language",
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



# Number of plays
cols <- c("song_id", "artist_name")
norm_cols = c("none", "num_tracks_artist")
for(l in 1:length(cols)){
  
  c = cols[l]
  n = norm_cols[l]

  if(n=="none"){
    x_agg <- train[, list(num_plays = .N), by = c]
  }else{
    x_agg <- train[, list(num_plays = .N/unique(get(n))), by = c]
  }

  setnames(x_agg, "num_plays", paste0("num_plays","_",c))
  train = merge(train, x_agg, by = c)
  rm(x_agg)
  gc()

}



## get final list of features - cols_select --------------------------
cols_select=c("song_id", "msno", "source_system_tab", "source_screen_name", "source_type", "city",
              "city", "bd", "gender", "registered_via", "registration_year", "registration_month",
              "expiration_year", "expiration_month", "member_since",
              "song_length", "genre_ids", "artist_name",       
              "composer", "lyricist", "language",
              "num_genres", "num_artists", "num_composers", "num_lyricists", "num_tracks_language_artist",
              "mean_song_length_artist", "num_tracks_artist", "num_languages_artist", "ratio_mean_song_length_artist",
              "country", "issuer", "year_issued",
              "num_plays_song_id", "num_plays_artist_name"
)



## CV statistc for categorical variables with too many levels ------------------------
cols.fac <- c("song_id", "msno", "genre_ids", "artist_name", "composer", "lyricist")
thresholds = c(30, 50, 30, 30, 30, 30)
train[, ID := 1:.N]

for(c in cols.fac){
  
  print(c)
  x <- get.CV.stat(copy(train[,c("ID",c,"target"), with = F]), 5, c, thresholds[cols.fac==c])
  train <- merge(train, x, by = "ID", all.x = T)
  train[, (c) := NULL]
  setnames(train, "m", c)
  rm(x)
  gc()
  
}

train[, ID := NULL]


save(train, file = "../DERIVED/train.Rdata")
write.csv(train, file = "../DERIVED/train.csv", row.names = F)



## separate independent and dependent variables -------------------------
x_train <- subset(train, select = cols_select)
y_train <- train$target
rm(train)
gc()



## create a 70-30 train-validation split -------------------------------------------------------------------
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
params <- list(
  "objective"           = "binary:logistic",
  "eval_metric"         = "auc",
  "eta"                 = 0.01,
  "max_depth"           = 10,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)



## Train Model with xgb.cv for optimal nrounds and then with xgb.train-------------------------------------------------------------------
log_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
fname_msgs = paste0("../LOGS/msgs_", log_time, ".txt")
file_msgs <- file(fname_msgs, open="wt")
sink(file_msgs, type="output")
Sys.time()
set.seed(123)
model_xgb <- xgb.train(data=train.xg, nrounds = 2501,
                       params = params, verbose = 1, missing = NA, 
                       early.stop.round = 200, 
                       maximize = T, print.every.n = 100,
                       watchlist = list(validation1 = test.xg)
)
Sys.time()
sink(file = NULL, type = "output")
closeAllConnections()




importance <- data.frame(xgb.importance(cols_select, model = model_xgb))



save(importance, file = "../MODELS/model_xgb_imp_3.Rdata")
save(model_xgb, file = "../MODELS/model_xgb_3.Rdata")
rm(x_train, x_test)
gc()



# Start scoring
test <- fread("../DATA/test.csv")
load( "../DERIVED/songs.Rdata")
load( "../DERIVED/members.Rdata")
load("../DERIVED/songs_extra.Rdata")
test <- merge(test, members[, members_cols, with = F], by = "msno", all.x = T)
test <- merge(test, songs[, songs_cols, with = F], by = "song_id", all.x = T)
test <- merge(test, songs_extra[, songs_extra_cols, with = F], by = "song_id", all.x = T)

rm(members, songs, songs_extra)
gc()



load("../DERIVED/train_raw.Rdata")



# Number of plays
cols <- c("song_id", "artist_name")
norm_cols = c("none", "num_tracks_artist")
for(l in 1:length(cols)){
  
  c = cols[l]
  n = norm_cols[l]
  
  if(n=="none"){
    df = rbind(train[,c,with=F], test[,c,with=F])
    x_agg <- df[, list(num_plays = .N), by = c]
  }else{
    df = rbind(train[,c(c,n),with=F], test[,c(c,n),with=F])
    x_agg <- df[, list(num_plays = .N/unique(get(n))), by = c]
  }
  
  setnames(x_agg, "num_plays", paste0("num_plays","_",c))
  test = merge(test, x_agg, by = c)
  rm(x_agg)
  gc()
  
}



## CV statistc for categorical variables with too many levels
cols.fac <- c("song_id", "msno", "genre_ids", "artist_name", "composer", "lyricist")
thresholds = c(30, 50, 30, 30, 30, 30)
for(c in cols.fac){
  agg <- get.CV.stat(copy(train), 1, c, thresholds[cols.fac==c])
  test <- merge(test, agg, by = c, all.x = T)
  test[, (c) := NULL]
  setnames(test, "m", c)
}



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
preds_score = predict(model_xgb, score.xg, ntreelimit = model_xgb$bestIteration, missing = NA)
test[, target := preds_score]



# make submission
submission <- fread("../DATA/sample_submission.csv")
submission[, target := NULL]
submission <- merge(submission, test[,c("id", "target"), with = F], by = "id", all.x = T)

write.csv(submission, file = "../SUBMISSION/submission_9.csv", row.names = F)





