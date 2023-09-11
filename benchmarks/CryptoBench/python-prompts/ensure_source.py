from Crypto.PublicKey import RSA
from Crypto.Signature import pkcs1_15
from Crypto.Hash import SHA256

# ensure_source: Bytes Bytes Bytes -> Boolean
# Verifies the identity of an author who sent the message.
# Returns True if the author did write the message
def ensure_source(message: bytes, signature: bytes, author_public_key: bytes) -> bool:
    # <solution>
    verifier = pkcs1_15.new(author_public_key)
    try: 
        verifier.verify(message, signature)
        return True
    except:
        return False

# <tests>
keypair_a, keypair_b = RSA.generate(2048), RSA.generate(2048)
private_key_a, public_key_a = keypair_a, keypair_a.public_key()
private_key_b, public_key_b = keypair_b, keypair_b.public_key()

message = SHA256.new(b'a message')

signer_a = pkcs1_15.new(private_key_a)
signer_b = pkcs1_15.new(private_key_b)

signature_a = signer_a.sign(message)
signature_b = signer_b.sign(message)

assert ensure_source(message, signature_a, public_key_a) == True
assert ensure_source(message, signature_b, public_key_a) == False
assert ensure_source(message, signature_b, public_key_b) == True
assert ensure_source(message, signature_a, public_key_b) == False