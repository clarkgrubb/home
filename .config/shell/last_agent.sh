echo > $HOME/.ssh/last_agent

for var in SSH_CLIENT SSH_TTY SSH_AUTH_SOCK SSH_CONNECTION DISPLAY
do
  echo "export $var=\""$(eval echo -n '$'$var)'"' >> $HOME/.ssh/last_agent
done
