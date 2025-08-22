


scoreSupplySection <- function(input, session_token) {
  knowledge_weights <- c(
    "supply_newconcepts_1" = 4,
    "supply_newconcepts_2" = 3,
    "supply_newconcepts_3" = 2,
    "supply_newconcepts_4" = 1,
    "supply_newconcepts_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$supply_newconcepts], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("supply_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("supply_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$supply_approach]] %||% 0
  at_score <- at_weights[[input$supply_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  
  data.frame(
    session_token = session_token,
    score_supply_knowledge = k_score,
    score_supply_action = ac_score,
    score_supply_attitude = at_score,
    score_supply_final = final_score
  )
}



scoreFoodSection <- function(input, session_token) {
  knowledge_weights <- c(
    "food_newconcepts_1" = 4,
    "food_newconcepts_2" = 3,
    "food_newconcepts_3" = 2,
    "food_newconcepts_4" = 1,
    "food_newconcepts_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$food_newconcepts], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("food_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("food_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$food_approach]] %||% 0
  at_score <- at_weights[[input$food_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  
  data.frame(
    session_token = session_token,
    score_food_knowledge = k_score,
    score_food_action = ac_score,
    score_food_attitude = at_score,
    score_food_final = final_score
  )
}



scoreEconSection <- function(input, session_token) {
  knowledge_weights <- c(
    "econ_newconcepts_1" = 4,
    "econ_newconcepts_2" = 3,
    "econ_newconcepts_3" = 2,
    "econ_newconcepts_4" = 1,
    "econ_newconcepts_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$econ_newconcepts], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("econ_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("econ_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$econ_approach]] %||% 0
  at_score <- at_weights[[input$econ_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_econ_knowledge = k_score,
    score_econ_action = ac_score,
    score_econ_attitude = at_score,
    score_econ_final = final_score
  )
}


scoreIntSecSection <- function(input, session_token) {
  knowledge_weights <- c(
    "intSec_newconcepts_1" = 4,
    "intSec_newconcepts_2" = 3,
    "intSec_newconcepts_3" = 2,
    "intSec_newconcepts_4" = 1,
    "intSec_newconcepts_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$intSec_newconcepts], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("intSec_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("intSec_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$intSec_approach]] %||% 0
  at_score <- at_weights[[input$intSec_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_intsec_knowledge = k_score,
    score_intsec_action = ac_score,
    score_intsec_attitude = at_score,
    score_intsec_final = final_score
  )
}


scoreDefenseSection <- function(input, session_token) {
  knowledge_weights <- c(
    "defense_newconcepts_1" = 4,
    "defense_newconcepts_2" = 3,
    "defense_newconcepts_3" = 2,
    "defense_newconcepts_4" = 1,
    "defense_newconcepts_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$defense_newconcepts], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("defense_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("defense_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$defense_approach]] %||% 0
  at_score <- at_weights[[input$defense_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_defense_knowledge = k_score,
    score_defense_action = ac_score,
    score_defense_attitude = at_score,
    score_defense_final = final_score
  )
}


scoreErosionSection <- function(input, session_token) {
  knowledge_weights <- c(
    "E_K_1" = 4,
    "E_K_2" = 3,
    "E_K_3" = 2,
    "E_K_4" = 1,
    "E_K_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$E_K], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("E_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("E_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$E_approach]] %||% 0
  at_score <- at_weights[[input$E_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_erosion_knowledge = k_score,
    score_erosion_action = ac_score,
    score_erosion_attitude = at_score,
    score_erosion_final = final_score
  )
}


scoreAcidSection <- function(input, session_token) {
  knowledge_weights <- c(
    "A_K_1" = 4,
    "A_K_2" = 3,
    "A_K_3" = 2,
    "A_K_4" = 1,
    "A_K_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$A_K], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("A_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("A_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$A_approach]] %||% 0
  at_score <- at_weights[[input$A_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_acid_knowledge = k_score,
    score_acid_action = ac_score,
    score_acid_attitude = at_score,
    score_acid_final = final_score
  )
}

scoreStructureSection <- function(input, session_token) {
  knowledge_weights <- c(
    "SD_K_A1" = 4,
    "SD_K_A2" = 3,
    "SD_K_A3" = 2,
    "SD_K_A4" = 1,
    "SD_K_A5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$SD_K], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("SD_approach_A", 1:4))
  at_weights <- setNames(1:4, paste0("SD_opinion_A", 1:4))
  
  ac_score <- ac_weights[[input$SD_approach]] %||% 0
  at_score <- at_weights[[input$SD_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_structure_knowledge = k_score,
    score_structure_action = ac_score,
    score_structure_attitude = at_score,
    score_structure_final = final_score
  )
}

scoreSalinitySection <- function(input, session_token) {
  knowledge_weights <- c(
    "S_K_1" = 4,
    "S_K_2" = 3,
    "S_K_3" = 2,
    "S_K_4" = 1,
    "S_K_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$S_K], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("S_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("S_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$S_approach]] %||% 0
  at_score <- at_weights[[input$S_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_salinisation_knowledge = k_score,
    score_salinisation_action = ac_score,
    score_salinisation_attitude = at_score,
    score_salinisation_final = final_score
  )
}

scoreBiodiversitySection <- function(input, session_token) {
  knowledge_weights <- c(
    "HL_K_1" = 4,
    "HL_K_2" = 3,
    "HL_K_3" = 2,
    "HL_K_4" = 1,
    "HL_K_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$HL_K], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("HL_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("HL_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$HL_approach]] %||% 0
  at_score <- at_weights[[input$HL_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_biodiversity_knowledge = k_score,
    score_biodiversity_action = ac_score,
    score_biodiversity_attitude = at_score,
    score_biodiversity_final = final_score
  )
}


scoreFertilisationSection <- function(input, session_token) {
  knowledge_weights <- c(
    "NM_K_1" = 4,
    "NM_K_2" = 3,
    "NM_K_3" = 2,
    "NM_K_4" = 1,
    "NM_K_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$NM_K], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("NM_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("NM_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$NM_approach]] %||% 0
  at_score <- at_weights[[input$NM_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_fertilisation_knowledge = k_score,
    score_fertilisation_action = ac_score,
    score_fertilisation_attitude = at_score,
    score_fertilisation_final = final_score
  )
}


scoreWaterSection <- function(input, session_token) {
  knowledge_weights <- c(
    "SW_K_1" = 4,
    "SW_K_2" = 3,
    "SW_K_3" = 2,
    "SW_K_4" = 1,
    "SW_K_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$SW_K], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("SW_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("SW_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$SW_approach]] %||% 0
  at_score <- at_weights[[input$SW_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_water_knowledge = k_score,
    score_water_action = ac_score,
    score_water_attitude = at_score,
    score_water_final = final_score
  )
}


scoreCarbonSection <- function(input, session_token) {
  knowledge_weights <- c(
    "DC_K_1" = 4,
    "DC_K_2" = 3,
    "DC_K_3" = 2,
    "DC_K_4" = 1,
    "DC_K_5" = 0
  )
  k_score <- 10 - sum(knowledge_weights[input$DC_K], na.rm = TRUE)
  
  ac_weights <- setNames(1:4, paste0("DC_approach_", 1:4))
  at_weights <- setNames(1:4, paste0("DC_opinion_", 1:4))
  
  ac_score <- ac_weights[[input$DC_approach]] %||% 0
  at_score <- at_weights[[input$DC_opinion]] %||% 0
  
  final_score <- round(mean(c(k_score / 10, ac_score / 4, at_score / 4)), 3)
  
  data.frame(
    session_token = session_token,
    score_carbon_knowledge = k_score,
    score_carbon_action = ac_score,
    score_carbon_attitude = at_score,
    score_carbon_final = final_score
  )
}

