library("mlr3") # mlr3 base package
library("mlr3misc") # contains some helper functions
library("mlr3pipelines") # create ML pipelines
library("mlr3tuning") # tuning ML algorithms
library("mlr3learners") # additional ML algorithms
library("mlr3viz") # autoplot for benchmarks
library("paradox") # hyperparameter space
library("lubridate")

data_path <- "/home/tarak/Downloads"
if (!exists("training_data")) {
    training_dt <- readRDS(file.path(data_path, "training_dt_atomic.RDS"))
}

## =============================================================================
## --------------------- format training data ----------------------------------
## =============================================================================
# sample rows
##training_dt <- training_data[, lapply(.SD, as.numeric)]
to_exclude_anyway <- c("type_id_a0", "type_id_a1", "type_id_a2")
                       #"goal_from_shot")

.prepare_dt <- function(dt, target = "scores", exclude = "concedes",
                        keep_prop = 0.4) {
    ## score task
    to_exclude_score <- c(to_exclude_anyway, exclude)
    dt_score <- dt[, !..to_exclude_score]
    ## dt_score[[target]] <- factor(ifelse(dt_score[[target]], "goal", "no_goal"),
    ##                              c("goal", "no_goal"))

    ## find all goals
    if (target == "scores") {
        dt_score_goals <- dt_score[scores == "goal"]
        dt_score_no_goals <- dt_score[scores == "no_goal"]
    } else {
        dt_score_goals <- dt_score[concedes == "goal"]
        dt_score_no_goals <- dt_score[concedes == "no_goal"]
    }
    dt_score_no_goals <-
        dt_score_no_goals[sample(.N, floor(nrow(dt_score_no_goals) * keep_prop))]
    dt_score <- rbind(dt_score_goals, dt_score_no_goals)
    dt_score[sample(.N, nrow(dt_score))]
}
## =============================================================================
## ------------------------------- task ----------------------------------------
## =============================================================================
## score task
dt_score <- .prepare_dt(training_dt, target = "scores", exclude = "concedes",
                        keep_prop = 0.1)
task_score <- TaskClassif$new(id = "scores", backend = dt_score,
                              target = "scores")
task_score$col_roles$name <- "event_id"
task_score$col_roles$feature <- setdiff(task_score$col_roles$feature,
                                        "event_id")

## concede task
dt_concede <- .prepare_dt(training_dt, target = "concedes", exclude = "scores",
                          keep_prop = 0.1)
task_concede <- TaskClassif$new(id = "concedes", backend = dt_concede,
                                target = "concedes")
task_concede$col_roles$name <- "event_id"
task_concede$col_roles$feature <- setdiff(task_concede$col_roles$feature,
                                          "event_id")

## =============================================================================
## --------------------------------------- learner -----------------------------
## =============================================================================
lrn_stats <- mlr3::lrn("classif.xgboost")
lrn_stats$predict_type <- "prob"

ps_lrn <- ParamSet$new(list(ParamDbl$new("gamma", lower = 0, upper = 10)))
ps_lrn$add(lrn_stats$param_set$params$eta)
ps_lrn$add(lrn_stats$param_set$params$subsample)

inner_cv3 <- rsmp("cv", folds = 3)
measure <- msr("classif.logloss", id = "log_loss")
tuner <- tnr("random_search")

## evaluate 30 combination each time
trm <- term("evals", n_evals = 60)

## =============================================================================
## -------------------------------- scores learner -----------------------------
## =============================================================================
## tuning instance
instance_score <- TuningInstance$new(task = task_score,
                                     learner = lrn_stats,
                                     resampling = inner_cv3,
                                     measures = measure,
                                     param_set = ps_lrn,
                                     terminator = trm)

## execute benchmark
results_score <- progressr::with_progress(tuner$tune(instance_score))

## training the learning
lrn_stats$param_set$values <- instance_score$result$params
## lrn_stats$param_set$values <- mlr3misc::insert_named(lrn_stats$param_set$values,
##                                                      list(gamma = 3.698,
##                                                           eta = 0.9883,
##                                                           subsample = 0.8411)
##                                                      )
train_ids <- sample(task_score$row_ids, floor(length(task_score$row_ids) * 0.8))
predict_ids <- task_score$row_ids[!task_score$row_ids %in% train_ids]
lrn_stats$train(task_score, row_ids = train_ids)
saveRDS(lrn_stats, "/home/tarak/Downloads/xgboost_scores_atomic.RDS")
probs_scores <- lrn_stats$predict(task_score, row_ids = predict_ids)

## =============================================================================
## -------------------------------- concede learner ----------------------------
## =============================================================================
instance_concede <- TuningInstance$new(task = task_concede,
                                       learner = lrn_stats,
                                       resampling = inner_cv3,
                                       measures = measure,
                                       param_set = ps_lrn,
                                       terminator = trm)

results_concede <- progressr::with_progress(tuner$tune(instance_concede))
lrn_stats$param_set$values <- instance_concede$result$params
train_ids <- sample(task_concede$row_ids, floor(length(task_concede$row_ids) * 0.8))
predict_ids <- task_concede$row_ids[!task_concede$row_ids %in% train_ids]
lrn_stats$train(task_concede, row_ids = train_ids)
saveRDS(lrn_stats, "/home/tarak/Downloads/xgboost_concedes_atomic.RDS")
probs_concedes <- lrn_stats$predict(task_concede, row_ids = predict_ids)
