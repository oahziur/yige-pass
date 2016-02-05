## Overview ##

This is a simple command line password management tool that stores credentials
into a JSON file. It encrypts password and note with AES-256 CTR mode with a
master password.

## Usage ##

`yige-pass --init pass.json` - Create `pass.json` and store the master password
into `pass.json`.

`yige-pass --new 20` - Generate a new password with an optional length. In this
case, it will generate a 20 character password. The generated password will be
copied to system clipboard.

`yige-pass --view pass.json` - Preview all accounts in `pass.json`, and then
select an account to decrypt. The decrypted password will be copied to system
clipboard.

`yige-pass --add pass.json` - Add a new account to `pass.json`.

`yige-pass -delete pass.json` - Delete an account from `pass.json`.

## Dependency ##

1. [The Haskell Tool Stack](http://docs.haskellstack.org/en/stable/README.html)

2. Requires `xclip` or `xsel` installed for Linux

## Installation ##

```bash
git clone https://github.com/oahziur/yige-pass
cd yige-pass && stack update && stack install
```
