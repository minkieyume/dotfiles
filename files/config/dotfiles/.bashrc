# Bash-specific initialization, including for non-login and remote
# shells (info "(bash) Bash Startup Files").

# Provide a default prompt.
PS1='\u@\h \w${GUIX_ENVIRONMENT:+ [env]}\$ '

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else, returning a successful return code.
    return 0
fi

for i in /etc/bashrc.d/*.sh; do
    [[ -r $i ]] && source "$i"
done

unset i
alias sudo='doas'
export TERM=xterm-256color
export GUIX_PROFILE=$HOME/.guix-profile
. $GUIX_PROFILE/etc/profile
