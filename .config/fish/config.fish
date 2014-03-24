function fish_prompt
  if test $status -eq 0
    set face ' :) '
  else
    set face ' :( '
  end

  set_color red
  echo -n (uname -s)
  echo -n ':fish '

  set_color blue
  echo -n (prompt_pwd)

  set_color green
  if test -d .git
    if test ! -z (which git)
      echo -n ' git:'
      echo -n (git branch | grep '^*' | cut -c 3-)
    end
  end
  set_color black
  echo -n $face
end