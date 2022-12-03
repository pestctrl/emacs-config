notmuch new
afew -v -m --all
notmuch new

mbsync fm
notmuch new
afew -v -t --new
notmuch tag +inbox -archive -- path:fastmail/INBOX/**
