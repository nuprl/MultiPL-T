local pkey = require('openssl.pkey')
local digest = require('openssl.digest')

-- ensure_source: String String String -> Boolean
-- Verifies the identity of an author who sent the message.
-- Returns true if the author did write the message
local function ensure_source(message, signature, author_public_key_pem)
    -- <solution>
    local author_public_key = pkey.new(author_public_key_pem)
    return author_public_key:verify(signature, message)
end

-- <tests>
local keypair_a = pkey.new({bits = 2048, alg = 'RSA'})
local keypair_b = pkey.new({bits = 2048, alg = 'RSA'})

local private_key_a, public_key_a = keypair_a:toPEM('private'), keypair_a:toPEM('public')
local private_key_b, public_key_b = keypair_b:toPEM('private'), keypair_b:toPEM('public')

local message = digest.new('SHA256'):update('a message')

local private_key_a = pkey.new(private_key_a)
local private_key_b = pkey.new(private_key_b)
local signature_a = private_key_a:sign(message)
local signature_b = private_key_b:sign(message)

assert(ensure_source(message, signature_a, public_key_a) == true)
assert(ensure_source(message, signature_b, public_key_a) == false)
assert(ensure_source(message, signature_b, public_key_b) == true)
assert(ensure_source(message, signature_a, public_key_b) == false)
