from Crypto.PublicKey import RSA
from Crypto.Hash import SHA256
from Crypto.Cipher import PKCS1_OAEP

# start_aes: Bytes Bytes -> Bytes
# Receives a public key and the password and uses the
# password to generate an AES key via SHA256 and encrypts
# the key to send it back to the user
def start_aes(password: bytes, public_key: bytes) -> bytes:
    # <solution>
    key = SHA256.new(password).digest()
    cipher_rsa = PKCS1_OAEP.new(public_key)
    return cipher_rsa.encrypt(key)

# <tests>
def test(password: bytes):
    keypair = RSA.generate(2048)
    cipher_rsa = PKCS1_OAEP.new(keypair)
    encrypted_key = start_aes(password, keypair.public_key())
    assert encrypted_key, cipher_rsa.encrypt(SHA256.new(password).digest())

test(b"hello")
test(b"goodbye")
test(b"a wonderful evening")