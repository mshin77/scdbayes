## Input Validation

- Uploads: .csv, .xlsx, .xls only, size-capped
- Formulas and hypotheses: allowlist-checked

## API Key Security

- Masked input, session only, never saved or logged
- Shared Gemini key: hourly and daily request limits

## Network Security

- HTTPS with HSTS and security headers

## Data Protection

- AI audit log: in session, exported by the user
- Restricted-use data: keep context sharing off or run locally

## Infrastructure

- Docker + Caddy deployment
- Firewall, key-only SSH, security updates

## Privacy

- Ask AI sends chat and instructions. Optional context excludes data rows and case IDs.
- Provider terms apply. Sharing starts off; session data and keys clear on exit.
