local cipher = require("openssl.cipher")
local rand = require("openssl.rand")

local aes_ecb = cipher.new("AES-256-ECB")

-- decode_message: String String -> String
-- Decodes the given message with the given key
local function decode_message(key, encoded_message)
    -- <solution>
    aes_ecb:decrypt(key)
    return aes_ecb:update(encoded_message) .. aes_ecb:final()
end

-- <tests>
local function test_message(data)
    local key = rand.bytes(32)

    aes_ecb:encrypt(key)
    local message = aes_ecb:update(data) .. aes_ecb:final()
    local decoded = decode_message(key, message)

    if decoded ~= data then
        error("Decoded message does not match original!")
    end
end

test_message("A good message")
test_message("A great day to live")
test_message("No time to die")
