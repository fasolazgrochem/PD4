# Encryption Function
caesar_encrypt <- function(text, shift) {
  alphabet <- strsplit("abcdefghijklmnopqrstuvwxyz", '')[[1]]
  result <- ""
  
  for (char in strsplit(tolower(text), '')[[1]]) {
    if (char %in% alphabet) {
      char_index <- match(char, alphabet)
      new_index <- (char_index + shift - 1) %% 26 + 1
      result <- paste0(result, alphabet[new_index])
    } else {
      result <- paste0(result, char)
    }
  }
  
  return(result)
}

# Decryption Function
caesar_decrypt <- function(text, shift) {
  alphabet <- strsplit("abcdefghijklmnopqrstuvwxyz", '')[[1]]
  result <- ""
  
  for (char in strsplit(tolower(text), '')[[1]]) {
    if (char %in% alphabet) {
      char_index <- match(char, alphabet)
      new_index <- (char_index - shift - 1) %% 26 + 1
      result <- paste0(result, alphabet[new_index])
    } else {
      result <- paste0(result, char)
    }
  }
  
  return(result)
}

# Example usage
encrypted_text <- caesar_encrypt("Get this message to the main server", 13)
print(encrypted_text)

decrypted_text <- caesar_decrypt("trg guvf zrffntr gb gur znva freire", 13)
print(decrypted_text)
