local pkey = require("openssl.pkey")
local digest = require("openssl.digest")

-- start_aes: String String -> String
-- Receives a public key and the password and uses the
-- password to generate an AES key via SHA256 and encrypts
-- the key to send it back to the user
local function start_aes(password, public_key_pem)
    -- <solution>
    local sha256 = digest.new('SHA256')
    local key = sha256:final(password)
    
    local public_key = pkey.new(public_key_pem)
    return public_key:encrypt(key)
end

-- <tests>
local function test(password)
    local keypair = pkey.new({bits = 2048, alg = 'RSA'})
    local public_key_pem = keypair:toPEM('public')
    
    local encrypted_key = start_aes(password, public_key_pem)
    
    assert(encrypted_key)
    
    local decrypted_key = keypair:decrypt(encrypted_key)
    local sha256 = digest.new('SHA256')
    local expected_key = sha256:final(password)
    assert(decrypted_key == expected_key)
end

test("hello")
test("goodbye")
test("a wonderful evening")
