#!/usr/bin/env sh
text=$(< 1-input)
open=$(echo $text | tr -d ')' | wc -c)
close=$(echo $text | tr -d '(' | wc -c)

echo "Round 1:" $(($open - $close))

current=0
pos=1
while read -n1 char
do
    case $char in
        "(" )
            current=$(($current + 1))
            ;;
        ")" )
            current=$(($current - 1))
            ;;
    esac
    if [ $current -lt 0 ]
    then
       break;
    fi
    pos=$(($pos + 1))
done < <(printf "%s" $text | tr -d '\n')

echo "Round 2:" $pos
