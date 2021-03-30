#! /usr/bin/env bash
set -euo pipefail

# Static header fields.
HEADER='{
	"type": "JWT",
	"alg": "RS256"
}'

Now=$(date +%s)
Expiry=$(($Now+3600))
echo $Now
echo $Expiry

payload='{
    "iss": "student-debt-projection-sa@student-debt-projection.iam.gserviceaccount.com",
    "scope": "https://www.googleapis.com/auth/spreadsheets",
    "aud": "https://oauth2.googleapis.com/token",
    "exp": '"$Expiry"',
    "iat": '"$Now"'
}'

echo $payload > thing.json

function b64enc() { openssl enc -base64 -A | tr '+/' '-_' | tr -d '='; }

function rs_sign() { openssl dgst -binary -sha256 -sign ./private.pem ; }

JWT_HDR_B64="$(echo -n "$HEADER" | b64enc)"
JWT_PAY_B64="$(echo -n "$payload" | b64enc)"
UNSIGNED_JWT="$JWT_HDR_B64.$JWT_PAY_B64"
SIGNATURE=$(echo -n "$UNSIGNED_JWT" | rs_sign | b64enc)

echo "$UNSIGNED_JWT.$SIGNATURE"
