
hcpcs <- S7::new_class(
  name = "hcpcs",
  package = "codex",
  properties = list(
    input = S7::class_vector,
    split_length = S7::new_property(
      class = S7::class_list,
      getter = \(self) split_lengths(self@input)),
    remove_redundant = S7::new_property(
      class = S7::class_list,
      getter = \(self) remove_redundant(self@split_length)),
    split_first = S7::new_property(
      class = S7::class_list,
      getter = \(self) split_first(self@remove_redundant)),
    process_groups = S7::new_property(
      class = S7::class_list,
      getter = \(self) process_groups(self@split_first))
  ))
# ,
#   validator = function(self) {
#     if (length(self@sex) != 1)                       return("@sex must be length 1")
#     if (!self@sex %in% c("Male", "Female", "Other")) return("@sex must be Male, Female, or Other")
#     if (length(self@dos) != 1)                       return("@dos must be length 1")
#     if (length(self@dob) != 1)                       return("@dob must be length 1")
#     if (length(self@age) != 1)                       return("@age must be length 1")
#     if (self@dob >= self@dos)                        return("@dob must be before @dos")
#     if (self@age <= 0)                               return("@age must be greater than 0 days")
#   }
# )

x <- hcpcs(input = random_hcpcs(500))

x
