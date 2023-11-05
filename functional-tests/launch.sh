#!/bin/bash

usage=0
silent=0
unsup=0

while getopts 'shu' opt; do
  case "$opt" in
    h) echo -e "USAGE\n -h: get usage\n -s: disable error explanation\n -u: unsupported samples even tested"
    usage=1
      ;;
    s) silent=1
      ;;
    u) unsup=1
      ;;
    ?) usage=1
  esac
done

if [ ${usage} == 0 ]
then
  echo -e "  ________________________________"
  echo -e " /                                \\"
  echo -e "|                                  |"
  echo -e "|           \033[1;36mApollo Tests\033[0m           |"
  echo -e "|                                  |"
  echo -e " \\________________________________/"
  echo

  if [ ${silent} == 1 ]
  then
      echo "[ Silent mode ]"
      echo
  fi

  nbr=0
  nbr_passed=0

  Red='\033[0;31m'
  NoColor='\033[0m'
  Green='\033[0;32m'

  FILES_INPUT="functional-tests/samples/*"
  mkdir -p /tmp/Apollo_Tests
  if [ -z "$(ls -A functional-tests/samples)" ]; then
    echo -e "${Red}Warning: unsupported_samples folder empty ${NoColor}"
  else
    for f in $FILES_INPUT
    do
      FILE_BASE=$(basename -- "$f")
      FILE_ANSWER="${FILE_BASE%.*}"
      FILE_ARGS=$(FILE_BASE% ++ "args".*)
      if [ ! -f "functional-tests/answers/${FILE_ANSWER}" ]; then
        echo -e "${Red}Warning: Answer not found for ${NoColor}${FILE_BASE}${Red} file, file skipped ${NoColor}"
      else
        ((nbr=nbr+1))
        NAME=${f##*/}
        printf "Testing [%-15s] file:" $NAME
        ./apollo run $FILE_BASE < $f > -- < $FILE_ARGS
        DIFF=$(diff /tmp/Apollo_Tests/current_apollo functional-tests/answers/$FILE_ANSWER)
        if [ "$DIFF" == "" ]
        then
          ((nbr_passed=nbr_passed+1))
          echo -e "$2\t\t${Green}PASSED${NoColor}"
        else
          echo -e "$2\t\t${Red}NOT PASSED${NoColor}"
          if [ $silent == 0 ]
            then
            diff -y /tmp/Apollo_Tests/current_apollo functional-tests/answers/$FILE_ANSWER
          fi
        fi
      fi
    done
  fi

  if [ ${unsup} == 1 ]
  then
    echo
    echo "[ Usupported samples testing ]"
    echo
    FILES_INPUT="functional-tests/unsupported_samples/*"
    if [ -z "$(ls -A functional-tests/unsupported_samples)" ]; then
      echo -e "${Red}Warning: unsupported_samples folder empty ${NoColor}"
    else
      for f in $FILES_INPUT
      do
        FILE_BASE=$(basename -- "$f")
        FILE_ANSWER="${FILE_BASE%.*}"
        if [ ! -f "functional-tests/answers/${FILE_ANSWER}" ]; then
          echo -e "${Red}Warning: Answer not found for ${NoColor}${FILE_BASE}${Red} file, file skipped ${NoColor}"
        else
          ((nbr=nbr+1))
          NAME=${f##*/}
          printf "Testing [%-15s] file:" $NAME
          ./apollo run < $f > /tmp/Apollo_Tests/current_apollo
          DIFF=$(diff /tmp/Apollo_Tests/current_apollo functional-tests/answers/$FILE_ANSWER)
          if [ "$DIFF" == "" ]
          then
            ((nbr_passed=nbr_passed+1))
            echo -e "$2\t\t${Green}PASSED${NoColor}"
          else
            echo -e "$2\t\t${Red}NOT PASSED${NoColor}"
            if [ $silent == 0 ]
              then
              diff -y /tmp/Apollo_Tests/current_apollo functional-tests/answers/$FILE_ANSWER
            fi
          fi
        fi
      done
    fi
  fi

  echo
  echo  Tests passed [${nbr_passed} / ${nbr}]

  rm -rf /tmp/Apollo_Tests

  if [ $nbr_passed == $nbr ]; then
    exit 0
  else
    exit 1
  fi
fi
