function fish_prompt
  if [ $status -eq 0 ]
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