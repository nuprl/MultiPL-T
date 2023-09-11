from Crypto.PublicKey import RSA

# decode_message: Bytes Bytes -> Bytes
# Decodes the given message with the given key
def decode_message(key: bytes, encoded_message: bytes) -> bytes:
    # <solution>
    cipher = AES.new(key, AES.MODE_EAX)
    return cipher.decrypt(encoded_message)

# <tests>
keypair = RSA.generate(2048)
private_key, public_key = keypair, keypair.public_key()