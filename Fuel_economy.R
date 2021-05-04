library(h2o)
h2o.init()
data <- ggplot2::mpg 
data %>% view()



data%>% skim()
data %>% inspect_na()
 

 data.num <- data %>% select_if(is.numeric) %>% select(cty,everything())
 
 data.chr <- data %>% select_if(is.character)
 
 data.chr <- dummyVars(" ~ .", data = data.chr) %>% 
   predict(newdata = data.chr) %>% 
   as.data.frame()
 
 data <- cbind(data.chr,data.num) %>%
   select(cty,everything())
 
 names(data) <- names(data) %>% 
   str_replace_all(" ","_") %>%
   str_replace_all("-","_") %>%
   str_replace_all("\\(","") %>% 
   str_replace_all("\\)","") %>% 
   str_replace_all("\\'","")

 
 target <-'cty'
 features <- data %>% select(year,cyl,displ) %>% names()
 f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
 glm <- glm(f, data = data)
 
 glm %>% summary()

 
 coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
 features <- features[!features %in% coef_na]

 f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
 glm <- glm(f, data = data) 

 glm %>% summary()

 while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 1){
   AFVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
   f <- as.formula(paste(target, paste(AFVIF, collapse = " + "), sep = " ~ "))
   glm <- glm(f, data = data)
 }
 
 glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 

 data <- data %>% select(cty,features) 

 
 data %>% glimpse() 
 data[,-1] <- data[,-1] %>% scale() %>% as.data.frame()
 
 
 
 
 

 h2o_data <- data %>% as.h2o() 

 
 h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
 train <- h2o_data[[1]]
 test <- h2o_data[[2]] 

 target <- 'cty' 
 features <- data %>% select(-cty) %>% names() 

 
 model <- h2o.glm(
    x = features, y = target,
    training_frame = train,
    validation_frame = test,
    nfolds = 10, seed = 123,
    lambda = 0, compute_p_values = T) 
 
 model@model$coefficients_table %>%
    as.data.frame() %>%
    dplyr::select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) 

 while(model@model$coefficients_table %>%
       as.data.frame() %>%
       dplyr::select(names,p_value) %>%
       mutate(p_value = round(p_value,3)) %>%
       .[-1,] %>%
       arrange(desc(p_value)) %>%
       .[1,2] > 0.05) {
    model@model$coefficients_table %>%
       as.data.frame() %>%
       dplyr::select(names,p_value) %>%
       mutate(p_value = round(p_value,3)) %>%
       filter(!is.nan(p_value)) %>%
       .[-1,] %>%
       arrange(desc(p_value)) %>%
       .[1,1] -> v
    features <- features[features!=v]
    
    train_h2o <- train %>% as.data.frame() %>% select(target,features) %>% as.h2o()
    test_h2o <- test %>% as.data.frame() %>% select(target,features) %>% as.h2o()
    
    model <- h2o.glm(
       x = features, y = target,
       training_frame = train,
       validation_frame = test,
       nfolds = 10, seed = 123,
       lambda = 0, compute_p_values = T)
 } 

 model@model$coefficients_table %>%
    as.data.frame() %>%
    dplyr::select(names,p_value) %>%
    mutate(p_value = round(p_value,3))  
 
 
 y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
 y_pred$predict
 
 test_set <- test %>% as.data.frame()
 residuals = test_set$cty - y_pred$predict

 
 RMSE = sqrt(mean(residuals^2))
 
 y_test_mean = mean(test_set$cty)
 
 tss = sum((test_set$cty - y_test_mean)^2) 
 rss = sum(residuals^2)
 R2 = 1 - (rss/tss); R2 

 n <- test_set %>% nrow() 
 k <- features %>% length() 
 Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1)) 
 
 tibble(RMSE = round(RMSE,1),
        R2, Adjusted_R2)
 