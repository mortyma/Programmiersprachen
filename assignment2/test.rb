require './parser'
require 'pp'

program = parse(<<-'END'
         maxnum a b c -  "$max$ $two$ $x$" {
           x = bla "$a$" "$b$" "1000";
           ab = exec "test $a$ -le $b$";
           bc = exec "test $b$ -le $c$";
           ab == "0" : bc == "0" : max = "$c$";
           ab == "0" : bc == "1" : max = "$b$";
           ab == "1" : bc == "1" : max = "$a$";
           ab == "1" : bc == "0" : ac = exec "test $a$ -le $c$";
           ac == "0" : max = "$c$";
           ac == "1" : max = "$a$";
           one two = split "b" "1b2";
           finally : max = "error";
         } 
         bla a b c -  "$max$\" \n bla" {
           ab = exec "test $a$ -le $b$";
           bc = exec "test $b$ -le $c$";
           ab == "0" : bc == "0" : max = "$c$";
           ab == "0" : bc == "1" : max = "$b$";
           ab == "1" : bc == "1" : max = "$a$";
           ab == "1" : bc == "0" : ac = exec "test $a$ -le $c$";
           ac == "0" : max = "$c$";
           ac == "1" : max = "$a$";
           finally : max = "error";
         }
  END
)

# ab  : max = maxnum;
#            ab == "1" : bc == "0" : ac  b = split "s" "test $a$ -le $c$";

 puts program.run('maxnum','1','400','2')