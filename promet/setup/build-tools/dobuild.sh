$lazbuild $1 $2 $3
if [ "$?" -ne "0" ]; then
  $lazbuild $1 $2 $3
  if [ "$?" -ne "0" ]; then
    if [ "$?" -ne "0" ]; then
      exit $?
    fi
  fi
fi

