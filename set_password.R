library(sodium)

# --- Set the values below ---
# Preferred password
password <- "SET PASSWORD HERE"
# Path to the data file we would like to encrypt
raw_file_path <- "SET PATH HERE"
# Path to where we would like to store the encrypted data
encrypted_file_path <- "SET PATH HERE"

# --- No need to make any changes below this line ---

# Copy the public key printed in the console
# and paste it into app.R on line 36
private_key <- sha256(charToRaw(password))
public_key <- pubkey(private_key)
print(paste("Copy this key:", paste(public_key, collapse = " ")))

raw_data <- read.csv(raw_file_path)
# This line encrypts the data
encrypted_data <- simple_encrypt(serialize(raw_data, NULL), public_key)

# Save the encrypted data
saveRDS(encrypted_data, encrypted_file_path)