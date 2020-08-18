library("mlr3") # mlr3 base package
library("mlr3misc") # contains some helper functions
library("mlr3pipelines") # create ML pipelines
library("mlr3tuning") # tuning ML algorithms
library("mlr3learners") # additional ML algorithms
library("mlr3viz") # autoplot for benchmarks
library("paradox") # hyperparameter space
library("smotefamily") # SM
library("lubridate")

data_path <- "/home/tarak/Gdrive_RA/events_data/opta"
if (!exists("training_data")) {
    training_data <- readRDS(file.path(data_path, "training_dt.RDS"))
}

# sample rows
dt_train <- training_data[sample(.N, floor(nrow(training_data) * 0.7))]
dt_train <- dt_train[, lapply(.SD, as.numeric)]
to_exclude_anyway <- c("type_id_a0", "type_id_a1", "type_id_a2", "goal_from_shot") 

## score task
to_exclude_score <- c(to_exclude_anyway, "concedes")
dt_score <- dt_train[, !..to_exclude_score]
dt_score[["scores"]] <- factor(ifelse(dt_score[["scores"]], "goal", "no_goal"), c("goal", "no_goal"))
task_score <- TaskClassif$new(id = "scores", backend = dt_score, target = "scores")
task_score$col_roles$name <- "event_id"
task_score$col_roles$feature <- setdiff(task_score$col_roles$feature, "event_id")

## concede task
to_exclude_concede <- c(to_exclude_anyway, "scores")
dt_concede = dt_train[, !..to_exclude_concede]
dt_concede[["concedes"]] <- factor(ifelse(dt_concede[["concedes"]],
                                          "goal", "no_goal"),
                                   c("goal", "no_goal")
                                   )
task_concede <- TaskClassif$new(id = "concedes", backend = dt_concede, target = "concedes")
task_concede$col_roles$name <- "event_id"
task_concede$col_roles$feature <- setdiff(task_concede$col_roles$feature, "event_id")

## undersample majority class (relative to majority class)
po_under <- po("classbalancing", id = "undersample",
               adjust = "major",
               reference = "major",
               shuffle = FALSE, ratio = 1 / 6)

## oversample majority class (relative to majority class)
po_over <- po("classbalancing", id = "oversample",
              adjust = "minor",
              reference = "minor",
              shuffle = FALSE,
              ratio = 6)

lrn_stats <- mlr3::lrn("classif.glmnet")
lrn_stats$predict_type <- "prob"

## combine learner with pipeline graph
lrn_under <- GraphLearner$new(po_under %>>% lrn_stats)
lrn_under$predict_type <- "prob"
lrn_over <- GraphLearner$new(po_over %>>% lrn_stats)
lrn_over$predict_type <- "prob"

## define parameter search space for each method
ps_under <- ParamSet$new(list(ParamDbl$new("undersample.ratio",
                                           lower = 1 / 6, upper = 1 / 4))
                         )
ps_under$add(lrn_under$param_set$params$undersample.shuffle)
ps_under$add(lrn_under$param_set$params$classif.glmnet.intercept)

ps_over = ParamSet$new(list(ParamDbl$new("oversample.ratio",
                                         lower = 1, upper = 4))
                       )
ps_over$add(lrn_over$param_set$params$oversample.shuffle)
ps_over$add(lrn_over$param_set$params$classif.glmnet.intercept)

## inner sampling strategy
inner_cv3 <- rsmp("cv", folds = 3)
measure <- msr("classif.logloss", id = "log_loss")
tuner <- tnr("random_search")

## evaluate 30 combination each time
trm <- term("clock_time", stop_time = Sys.time() + minutes(40))


## tuning instance
instance_under <- TuningInstance$new(task = task_score,
                                     learner = lrn_under,
                                     resampling = inner_cv3,
                                     measures = measure,
                                     param_set = ps_under,
                                     terminator = trm)


instance_over <- TuningInstance$new(task = task_score,
                                    learner = lrn_over,
                                    resampling = inner_cv3,
                                    measures = measure,
                                    param_set = ps_over,
                                    terminator = trm)

## execute benchmark
progressr::with_progress(under_results <- tuner$tune(instance_under))

## training the learning
lrn_under$param_set$values <- instance_under$result$params
train_ids <- sample(task_score$row_ids, floor(length(task_score$row_ids) * 0.8))
predict_ids <- task_score$row_ids[!task_score$row_ids %in% train_ids]
lrn_under$train(task_score, row_ids = train_ids)

probs <- lrn_under$predict(task_score, row_ids = predict_ids)
