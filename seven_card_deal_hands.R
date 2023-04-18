library(dplyr)
library(utils)

rm(list=ls())


# create deck of cards---
the.cards <- factor(rep(c(2:10,"J","Q","K","A"), each = 4), 
                    levels = c(2:10,"J","Q","K","A")) %>%
  as.ordered()

the.deck <- data.frame(fv = the.cards, 
                       suit = rep(c("D", "C", "S", "H"), length.out = 52), 
                       card_id = 1:52)

# functions that test for poker hand types----
is_01.royalFlush <- function(v_fv, v_suit){
  # OLD WAY FOR 5 CARDS: 
  # all(v_fv %in% c("A", "K", "Q", "J", "10")) &
  #   length(unique(v_suit)) == 1
  
  # NEW WAY FOR 7 CARDS: 
  if(any(unname(table(v_suit)) == 5)){
    out <- all(v_fv[which(v_suit == names(which(table(v_suit) == 5)))] %in% 
                 c("A", "K", "Q", "J", "10"))
  }else{
    out <- F
  }
  return(out)
}


is_02.straightFlush <- function(v_fv, v_suit){
  outA <- factor(v_fv, 
                 levels = c(2:10,"J","Q","K","A")) 
  outA1 <- as.numeric(outA) %>%
    .[order(.)] %>%
    diff() 
  
  outB <- factor(v_fv, 
                 levels = c("A",2:10,"J","Q","K")) 
  outB1 <- as.numeric(outB) %>%
    .[order(.)] %>%
    diff() 
  
  # OLD WAY FOR 5 CARDS: 
  # out <- ((length(outA1) == 4 & all(outA1 == 1)) |
  #           (length(outB1) == 4 & all(outB1 == 1))) & 
  #   length(unique(v_suit)) == 1
  
  # NEW WAY FOR 7 CARDS: 
  pattern <- "10{0,}10{0,}10{0,}1"
  checkA <- paste(outA1,sep="",collapse="")
  checkB <- paste(outB1,sep="",collapse="")
  out <- grepl(pattern, checkA) | grepl(pattern, checkB)
  
  return(out)
}




is_03.4ofKind <- function(v_fv, v_suit){
  4 %in% unname(table(unname(v_fv)))
}

is_04.fullHouse <- function(v_fv, v_suit){
  # OLD WAY FOR 5 CARDS: 
  #all(c(2,3) %in% unname(table(v_fv)))
  
  # NEW WAY FOR 7 CARDS: 
  any(c(2) %in% unname(table(v_fv))) & 
    any(c(3) %in% unname(table(v_fv)))
}

is_05.flush <- function(v_fv, v_suit){
  # OLD WAY FOR 5 CARDS: 
  #length(unique(v_suit)) == 1
  
  # NEW WAY FOR 7 CARDS: 
  any(unname(table(v_suit))>=5)
}

is_06.straight <- function(v_fv, v_suit){
  outA <- factor(v_fv, 
                 levels = c(2:10,"J","Q","K","A")) 
  outA1 <- as.numeric(outA) %>%
    .[order(.)] %>%
    diff() 
  
  outB <- factor(v_fv, 
                 levels = c("A",2:10,"J","Q","K")) 
  outB1 <- as.numeric(outB) %>%
    .[order(.)] %>%
    diff() 
  
  # OLD WAY FOR 5 CARDS: 
  # out <- ((length(outA1) == 4 & all(outA1 == 1)) |
  #           (length(outB1) == 4 & all(outB1 == 1))) 
  
  # NEW WAY FOR 7 CARDS: 
  pattern <- "10{0,}10{0,}10{0,}1"
  checkA <- paste(outA1,sep="",collapse="")
  checkB <- paste(outB1,sep="",collapse="")
  out <- grepl(pattern, checkA) | grepl(pattern, checkB)
  
  return(out)
}

is_07.3ofKind <- function(v_fv, v_suit){
  3 %in% unname(table(unname(v_fv)))
}

is_08.2pair <- function(v_fv, v_suit){
  sum(unname(table(v_fv)) == 2) == 2
}

is_09.1pair <- function(v_fv, v_suit){
  sum(unname(table(v_fv)) == 2) == 1
}


# RUN SIMULATION----
# function that simulates deal of 52 card deck into 7 groups of 7 cards and test
# to see if every group of 7 has at least 1 pair or better

fun_sim.g7 <- function(){
  the.deal <- the.deck[sample(1:nrow(the.deck), size = 49, replace = F),] %>%
    mutate(group7 = rep(letters[1:7], each = 7))
  
  the.summary <- data.frame(group7  = letters[1:7], 
                            with_01 = NA, 
                            with_02 = NA, 
                            with_03 = NA,
                            with_04 = NA,
                            with_05 = NA,
                            with_06 = NA,
                            with_07 = NA,
                            with_08 = NA,
                            with_09 = NA)
  
  for(i in 1:nrow(the.summary)){
    the.summary$with_01[i] <- is_01.royalFlush(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                               v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
    the.summary$with_02[i] <- is_02.straightFlush(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                                  v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
    the.summary$with_03[i] <- is_03.4ofKind(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                            v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
    the.summary$with_04[i] <- is_04.fullHouse(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                              v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
    the.summary$with_05[i] <- is_05.flush(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                          v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
    the.summary$with_06[i] <- is_06.straight(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                             v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
    the.summary$with_07[i] <- is_07.3ofKind(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                            v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
    the.summary$with_08[i] <- is_08.2pair(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                          v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
    the.summary$with_09[i] <- is_09.1pair(v_fv = the.deal$fv[the.deal$group7 == the.summary$group7[i]], 
                                          v_suit = the.deal$suit[the.deal$group7 == the.summary$group7[i]])
  }
  
  the.summary$with_any <- (the.summary$with_01 +
                             the.summary$with_02 +
                             the.summary$with_03 +
                             the.summary$with_04 +
                             the.summary$with_05 +
                             the.summary$with_06 +
                             the.summary$with_07 +
                             the.summary$with_08 +
                             the.summary$with_09) > 0
  return(all(the.summary$with_any))
  
}

# simulate multiple times----
x <- replicate(n = 10000, fun_sim.g7())

# summarise outcome----
table(x) %>% prop.table()

