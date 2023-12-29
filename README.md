<h2 align="center">SlackMacs</h1>
<p align="center">Emacs client for Slack communicator written in elisp and rust</p>

---

## Features

- Send messages
- Receive messages
- Delete messages
- Edit messages
- Reply to messages in threads
- List all channels and users

## Build and installation

1. Install [Rust and Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
2. Install [Emacs](https://www.gnu.org/software/emacs/download.html)
3. In the root directory run `make` command
4. After successful build `slackmacs-VERSION.tar` file will be created in the root directory
5. Open Emacs and install the package with `M-x package-install-file` command
6. In `~/.emacs` add the following line `(require 'slackmacs)`

## Configuration

1. Create file `~/.authinfo` with the following content:

```txt
machine slack-token login NAME_OF_THE_CHANNEL secret TOKEN
machine slack-cookie login NAME_OF_THE_CHANNEL secret COOKIE
```

2. Replace `NAME_OF_THE_CHANNEL`, `TOKEN` and `COOKIE` with your values
3. In `~/.emacs` add the following line `(setq slackmacs-team-domain "NAME_OF_THE_CHANNEL")`
4. Open Emacs and run `M-x slackmacs-start`

## How to obtain token and cookie

1. Open Slack channel customization page in your browser (https://<SLACK_CHANNEL_URL>/customize)
2. Open browser developer tools (F12 or Ctrl/Cmd+Shift+I or right click -> Inspect)
3. Open Console tab
4. Paste the following code and press Enter `console.log(TS.boot_data.api_token)`
5. Copy the token and save it
6. Open Application/Storage tab
7. Open Cookies and select https://<SLACK_CHANNEL_URL>
8. Find cookie with name `d`
9. Copy the value and save it (Show URL-decoded checkbox should be unchecked)

Unfortunately, there is no other way to authorize in Slack without going through those steps.
For more information please refer to [emojme project](https://github.com/jackellenberger/emojme?tab=readme-ov-file#finding-a-slack-token)

## How to secure token and cookie

Security of your token and cookie is a very important aspect.
If someone gets access to them he can impersonate you and send messages on your behalf.
To make sure that your token and cookie are secure you can encrypt them with GPG.

1. Install GnuPG
2. Generate GPG key `gpg --full-generate-key`
3. Make sure that you have `~/.authinfo` file with your credentials (see Configuration section)
4. Encrypt the file with `gpg -e -r USER_ID ~/.authinfo`. After that operation file `~/.authinfo.gpg` will be created
5. Remove the original file `rm ~/.authinfo`
