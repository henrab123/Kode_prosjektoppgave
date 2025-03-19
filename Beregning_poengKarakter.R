######################################
## CALCULATING DIFFERENCE IN POINTS ##
######################################

V22_predicted_points           <- pred_to_dataframe(deterministic_prediction(estimated_parameters_V22, RTMB_V22, s=TRUE) , RTMB_V22)
V22_predicted_points_without_s <- pred_to_dataframe(deterministic_prediction(estimated_parameters_V22, RTMB_V22, s=FALSE), RTMB_V22)

V23_predicted_points           <- pred_to_dataframe(deterministic_prediction(estimated_parameters_V23, RTMB_V23, s=TRUE) , RTMB_V23)
V23_predicted_points_without_s <- pred_to_dataframe(deterministic_prediction(estimated_parameters_V23, RTMB_V23, s=FALSE), RTMB_V23)

V22_average <- result <- V22_predicted_points %>%
                         group_by(kommisjon) %>%
                         summarise(mean_sum = mean(Y.19 + Y.20)) %>%
                         pull(mean_sum)

V22_average_without <- result <- V22_predicted_points_without_s %>%
                                 group_by(kommisjon) %>%
                                 summarise(mean_sum = mean(Y.19 + Y.20)) %>%
                                 pull(mean_sum)

V23_average <- result <- V23_predicted_points %>%
                         group_by(kommisjon) %>%
                         summarise(mean_sum = mean(Y.16 + Y.17)) %>%
                         pull(mean_sum)

V23_average_without <- result <- V23_predicted_points_without_s %>%
                                 group_by(kommisjon) %>%
                                 summarise(mean_sum = mean(Y.16 + Y.17)) %>%
                                 pull(mean_sum)

V22_antall <- V22_predicted_points %>%
              count(kommisjon)
V23_antall <- V23_predicted_points %>%
  count(kommisjon)

V23_antall
V22_difference_in_points <- data.frame(
  kommisjon = RTMB_V22$kommisjon,
  kandidatnummer = RTMB_V22$kandidatnummer,
  karakter = V22$ext_inspera_finalGrade,
  poengsum = V22$score,
  Y.19 = V22_predicted_points$Y.19 - V22_predicted_points_without_s$Y.19,
  Y.20 = V22_predicted_points$Y.20 - V22_predicted_points_without_s$Y.20,
  poengsorskjell = V22_predicted_points$Y.19 - V22_predicted_points_without_s$Y.19 + V22_predicted_points$Y.20 - V22_predicted_points_without_s$Y.20
)

V23_difference_in_points <- data.frame(
  kommisjon = RTMB_V23$kommisjon,
  kandidatnummer = RTMB_V23$kandidatnummer,
  karakter = V23$ext_inspera_finalGrade,
  poengsum = V23$ext_inspera_autoScore + V23$ext_inspera_manual_Score4 + V23$ext_inspera_manual_Score5,
  Y.16 = V23_predicted_points$Y.16 - V23_predicted_points_without_s$Y.16,
  Y.17 = V23_predicted_points$Y.17 - V23_predicted_points_without_s$Y.17,
  poengsorskjell = V23_predicted_points$Y.16 - V23_predicted_points_without_s$Y.16 + V23_predicted_points$Y.17 - V23_predicted_points_without_s$Y.17
)


V22_difference_in_points$justertpoengsum <- V22_difference_in_points$poengsum - V22_difference_in_points$poengsorskjell
V23_difference_in_points$justertpoengsum <- V23_difference_in_points$poengsum - V23_difference_in_points$poengsorskjell

karaktergrense_V22 <- c(-Inf,   38, 52.5, 64.5, 76.5, 88.5, 150)
karaktergrense_V23 <- c(-Inf, 36.5, 52.5, 64.5, 76.5, 88.5, 150)
karakterer         <- c("F","E","D","C","B","A")

points_to_grade <- function(data, grades, grade_threshold){
  
  grading <- rep(0, length(data))
  
  for (i in 1:length(data)) {
    for (g in 1:length(grade_threshold)) {
      if (data[i] < grade_threshold[g]){
        grading[i] <- grades[g-1]
        break
      }
    }
  }
  
  return(grading)
}

# New Grade
V22_difference_in_points$justerkarakter <-  points_to_grade(V22_difference_in_points$justertpoengsum, karakterer, karaktergrense_V22)
V23_difference_in_points$justerkarakter <-  points_to_grade(V23_difference_in_points$justertpoengsum, karakterer, karaktergrense_V23)

#Antallet endrede karakterer
V23_changed_grades <- V23_difference_in_points %>%
                      filter(karakter != justerkarakter)
