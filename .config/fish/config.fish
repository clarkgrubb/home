if [ -e /usr/bin/uname ]
  set OS_TYPE (/usr/bin/uname -s)
else
  set OS_TYPE (/bin/uname -s)
end
set MANPATH ~/Local/man:(manpath)
set EDITOR 'emacs -q'

if [ $OS_TYPE = Darwin ]

  set PATH ~/Local/bin /usr/local/bin /usr/bin /bin /sbin /usr/sbin /usr/X11/bin

  function pman
    man -t $argv | open -f -a /Applications/Preview.app
  end

else if [ $OS_TYPE = Linux ]

  set PATH ~/Local/bin /usr/local/bin /usr/bin /bin /sbin /usr/sbin /usr/X11/bin

else

  echo "unrecognized OS: " $OS_TYPE

end

set fish_greeting ""

function fish_title
  echo $OS_TYPE
end

function fish_prompt
  if [ $status -eq 0 ]
    set face ' :) '
  else
    set face ' :( '
  end

  set_color red
  echo -n $OS_TYPE
  echo -n ':fish '

  set_color blue
  echo -n (prompt_pwd)

  set_color green
  if [ -d .git ]
    if [ ! -z (which git) ]
      echo -n ' git:'
      echo -n (git branch | grep '^*' | cut -c 3-)
    end
  end
  if [ -d .hg ]
    if [ ! -z (which hg) ]
      echo -n ' hg:'
      echo -n (hg branch)
    end
  end

  set_color black
  echo -n $face
end