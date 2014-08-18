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
          x = map sleep "," "2,2,2,2";
        }

         sleep seconds - "$ret$" {
         	ret = exec "sleep $seconds$";
         }

        mmap lst - "$y$" {
          lst == "" : y = "";
          lst != "" : head tail = split "," "$lst$";
          t = mmap "$tail$";
          h = sleep "$head$";
          tail == "" : y = "$h$";
          tail != "" : y = "$h$,$t$";
        }

        foldl init lst - "$y$" {
          lst == "" : y = "$init$";
          lst != "" : head tail = split "," "$lst$";
          z = "($init$-$head$)";
          y = foldl "$z$" "$tail$";
        }

        foldr init lst - "$y$" {
          lst == "" : y = "$init$";
          lst != "" : head tail = split "," "$lst$";
          t = foldr "$init$" "$tail$";
          y = "($head$-$t$)";
        }
  END
)

# ab  : max = maxnum;
#            ab == "1" : bc == "0" : ac  b = split "s" "test $a$ -le $c$";

puts program.run('foldl','x','2,3,4,5,6,')
puts program.run('maptest')
# puts program.run('maxnum','1','400','2')

Thread.abort_on_exception=true

# q = program.queue_for_call('maxnum','1','400','2') { |ret| pp ret; q.kill }
q = program.queue_for_call('mmap','2,2,2,2,2,2') { |ret| 
  pp ret
  q.kill
}

workers = 1.upto(6).map { Thread.new { q.run } }
workers.each{|t| t.join }

#puts  program.run('maxnum','1','400','2')

