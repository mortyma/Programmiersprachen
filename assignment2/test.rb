require_relative 'parser'
require 'pp'

program = parse(<<-'END'
         maxnum a b c -  "$max$ $two$ $x$ $sleep$" {
           x = bla "$a$" "$b$" "1000";
           sleep = sleeps "2";
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
           x = maxnum "1" "3" "2";
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

         sleeps seconds - "$a$$b$$c$$d$"{
         	a = sleep "$seconds$";
         	b = sleep "$seconds$";
         	c = sleep "$seconds$";
         	d = sleep "$seconds$";
         }

         maptest - "$x$"{
          x = map sleep "," "1,1,1";
        }

         sleep seconds - "$ret$" {
         	ret = exec "sleep $seconds$";
         }
  END
)

# ab  : max = maxnum;
#            ab == "1" : bc == "0" : ac  b = split "s" "test $a$ -le $c$";

puts program.run('maptest')
# puts program.run('maxnum','1','400','2')

Thread.abort_on_exception=true

# q = program.queue_for_call('maxnum','1','400','2') { |ret| pp ret; q.kill }
q = program.queue_for_call('sleeps','5') { |ret| 
  pp ret
  q.kill
}

# workers = 1.upto(4).map { Thread.new { q.run } }
# workers.each{|t| t.join }

#puts  program.run('maxnum','1','400','2')

