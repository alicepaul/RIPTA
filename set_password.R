library(sodium)

# Provide the preferred password
password <- "SET PASSWORD HERE"

# Copy the public key printed in the console
# and paste it into app.R on line 36
private_key <- sha256(charToRaw(password))
public_key <- paste(pubkey(private_key), collapse = " ")
print(paste("Copy this key:", public_key))