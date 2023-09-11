from Crypto.Cipher import AES
from Crypto.Random import get_random_bytes

MODE = AES.MODE_EAX

# decode_message: Bytes Bytes -> Bytes
# Decodes the given message with the given key
def decode_message(key: bytes, encoded_message: bytes) -> bytes:
    # <solution>
    cipher = AES.new(key, mode=MODE)
    return cipher.decrypt(encoded_message)

# <tests>
def test_message(data: bytes):
    key = get_random_bytes(32)
    cipher = AES.new(key, mode=MODE)
    message = cipher.encrypt(data)
    assert decode_message(key, message), data

test_message(b"A good message")
test_message(b"A great day to live")
test_message(b"No time to die")