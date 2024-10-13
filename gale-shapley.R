# The Gale-Shapley algorithm, also known as the deferred acceptance algorithm or
# the stable marriage algorithm, is a foundational concept in matching theory, 
# developed by economists David Gale and Lloyd Shapley in 1962. It solves the 
# problem of creating stable matches between two sets of agents with 
# preferences, such as applicants and programs, by ensuring that no pair 
# would rather be matched with each other than their current partners. 
# This theory forms the basis behind the National Resident Matching Program (NRMP) 
# algorithm, which matches medical students to residency programs. In the 
# NRMP, applicants and programs rank each other based on preferences, and 
# the algorithm works iteratively: 
#   applicants tentatively "propose" to their preferred programs, while 
#   programs hold onto their top choices and reject others. The process 
#   continues until all participants are matched in a way that guarantees 
#   stability, where no applicant-program pair would prefer to be matched to 
#   each other over their current match. This ensures a fair and efficient 
#   allocation of positions while respecting mutual preferences.



# Let A = {a_1, a_2, ..., a_n} represent the set of n applicants
# Let P = {p_1, p_2, ..., p_m} represent the set of m programs
# Each a_i has a preference list of length x L_a[i] = {p_j1, p_j2, ..., p_jx}
# Each p_j has a preference list of length y L_p[j] = {a_i1, a_i2, ..., a_iy}

# 1. Initialize:
# All applicants and positions are unmatched.
# matched_A[i] = NULL for all a_i in A
# matched_P[j] = NULL for all p_j in P

# 2. While there exists an unmatched applicant a_i:
#   a_i proposes to the highest-ranked position p_j on L_a[i] that hasn't rejected them.
#   p_j evaluates the proposal:

# 3. Position p_j accepts or rejects:
#   if (p_j is unmatched) {
#     matched_A[i] = p_j  # p_j accepts a_i
#     matched_P[j] = a_i
#   } else {
#     Let current_match = matched_P[j]
#     if (p_j prefers a_i over current_match) {
#       matched_A[i] = p_j  # p_j accepts a_i
#       matched_P[j] = a_i
#       matched_A[current_match] = NULL  # p_j rejects the current match
#     } else {
#       a_i is rejected, and they propose to the next position on L_a[i]
#     }
#   }

# 4. Repeat steps 2 and 3 until no unmatched applicants remain or applicants 
#    exhaust their preference lists.

# Stability Condition:
# A matching M is stable if there is no pair (a_i, p_j) such that both:
#   a_i prefers p_j over their current match
#   p_j prefers a_i over their current match

# Mathematically, stability can be expressed as:
# for all (a_i, p_j) not in the matching M:
#   a_i does not prefer p_j more than their current match AND
#   p_j does not prefer a_i more than their current match



# first, we need to define specific preference lists for 
# applicants and programs

# applicants' rank order lists
applicants <- list(
  "Applicant1" = c("Program1", "Program3"),
  "Applicant2" = c("Program1", "Program2", "Program4"),
  "Applicant3" = c("Program2", "Program3"),
  "Applicant4" = c("Program4", "Program3"),
  "Applicant5" = c("Program4", "Program1", "Program2"),
  "Applicant6" = c("Program2", "Program4"),
  "Applicant7" = c("Program3"),
  "Applicant8" = c("Program4", "Program1"),
  "Applicant9" = c("Program1", "Program4"),
  "Applicant10" = c("Program2", "Program3", "Program1")
)

# programs' rank order lists
programs <- list(
  "Program1" = c("Applicant4", "Applicant9", "Applicant1", "Applicant5"),
  "Program2" = c("Applicant4", "Applicant6", "Applicant10", "Applicant1"),
  "Program3" = c("Applicant7", "Applicant3", "Applicant6", "Applicant10", "Applicant4"),
  "Program4" = c("Applicant5", "Applicant8")
)

# set the number of spots per program
spots <- c("Program1" = 2, "Program2" = 3, "Program3" = 4, "Program4" = 2)

# initialize match results for each program as an empty character vector
match <- list(
  "Program1" = character(0),
  "Program2" = character(0),
  "Program3" = character(0),
  "Program4" = character(0)
)

# create a list of unmatched applicants to start with 
# (all applicants are unmatched at the beginning)
unmatched_applicants <- names(applicants)

print(unmatched_applicants)

# initialize a tracking system for each applicant's applications to programs
# this ensures a applicant doesn't apply to the same program more than once
# initially, no one has applied anywhere, so we create an all "FALSE" list
applicant_applied <- lapply(applicants, function(x) rep(FALSE, length(x)))

print(applicant_applied)

# start the matching process: continue until there are no more unmatched applicants
# we start a loop that keeps going as long as there are unmatched applicants
# while loop
while (length(unmatched_applicants) > 0) {
  
  # iterate/loop over unmatched applicants
  # for loop
  for (applicant in unmatched_applicants) {
    applicant_preferences <- applicants[[applicant]] # start by getting a 
                                            # list of applicants' preferred programs
    
    # check if the applicant has applied to all the programs on their 
    # preference list
    # if yes, and they still aren't matched, we mark them as 
    # "unmatched" and move on to the next applicant
    applied_to_all <- all(applicant_applied[[applicant]])  # check if the applicant 
                                            # has applied to all programs they rank
    if (applied_to_all) {
      # if the applicant has applied to all their choices and is still unmatched, 
      # they go unmatched
      cat(applicant, "has applied to all programs and remains unmatched.\n")
      # remove the applicant from the unmatched list, no further attempts for this applicant
      unmatched_applicants <- setdiff(unmatched_applicants, applicant)
      next
    }
    
    # if the applicant hasn't applied to all programs, we look for the first 
    # program they haven't applied to yet and mark it as "applied"
    # find the first unattempted program in the applicant's preference list
    for (i in seq_along(applicant_preferences)) {
      if (!applicant_applied[[applicant]][i]) {
        program <- applicant_preferences[i]  # the program that the applicant is applying to
        applicant_applied[[applicant]][i] <- TRUE  # mark that the applicant has applied to this program
        break
      }
    }
    
    cat(applicant, "is applying to", program, "\n")
    
    # the applicant applies to the program. If the program doesn't rank 
    # the applicant, their application is rejected, and they 
    # have to try another program
    # check if the program ranks the applicant
    if (!applicant %in% programs[[program]]) {
      cat(program, "does not rank", applicant, "- application rejected.\n")
      next  # if the program doesn't rank the applicant, the application is rejected
    }
    
    # check if the program has available spots
    if (length(match[[program]]) < spots[program]) {
      # if spots are available, match the applicant to the program
      match[[program]] <- c(match[[program]], applicant)
      
      # remove the applicant from the list of unmatched applicants
      unmatched_applicants <- setdiff(unmatched_applicants, applicant)
      
      cat(applicant, "was matched to", program, "\n")  # debugging output
      
    } else {
      # if the program is full, the applicant is only considered if they are 
      # more preferred than someone already matched
      current_applicants <- match[[program]]  # get the current applicants matched to this program
      program_pref <- programs[[program]]  # get the program's preferences list
      
      # rank the current applicants by how preferred they are by the program
      ranked_applicants <- sapply(current_applicants, function(res) which(program_pref == res))
      
      # find the least preferred applicant currently matched to the program
      least_preferred <- current_applicants[which.max(ranked_applicants)]
      
      cat("Least preferred in", program, "is", least_preferred, "\n")
      
      # check if the new applicant is preferred over the least preferred one
      if (which(program_pref == applicant) < which(program_pref == least_preferred)) {
        # if the new applicant is preferred, replace the least preferred applicant
        match[[program]] <- setdiff(match[[program]], least_preferred)
        match[[program]] <- c(match[[program]], applicant)
        
        # the replaced applicant becomes unmatched and needs to apply again
        unmatched_applicants <- c(unmatched_applicants, least_preferred)
        
        # remove the new applicant from the unmatched list
        unmatched_applicants <- setdiff(unmatched_applicants, applicant)
        
        cat(least_preferred, "was replaced by", applicant, "in", program, "\n")
        
      } else {
        cat(applicant, "is not preferred over", least_preferred, "- application rejected.\n")
      }
    }
  }
  
  # exit condition to prevent infinite loops if no changes are made to unmatched_applicants
  # this avoids infinite loops in cases where no matching is possible for certain applicants
  if (length(unmatched_applicants) == sum(sapply(applicant_applied, all))) {
    cat("No further matching possible. Exiting loop.\n")
    break
  }
}

# print the final matches
cat("Final Matches:\n")
for (program in names(match)) {
  cat(program, ":", paste(match[[program]], collapse = ", "), "\n")
}

# print unmatched applicants
unmatched_applicants <- setdiff(names(applicants), unlist(match))
cat("\nUnmatched Applicants:\n", paste(unmatched_applicants, collapse = ", "), "\n")
