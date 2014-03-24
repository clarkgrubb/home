function fish_prompt
  if test $status -eq 0
    set prompt ' :) '
  else
    set prompt ' :( '
  end
  set_color red
  echo -n 'Darwin:fish '
  set_color blue
  echo -n (pwd)
  set_color black
  echo -n $prompt
end