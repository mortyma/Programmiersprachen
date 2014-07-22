require 'rubygems'
require 'parslet' 

class GCParser < Parslet::Parser
    rule(:space?)     { match('\s').repeat(1) }
    rule(:space?)     { match('\s').repeat }

	def spaced(character)
		str(character) >> space?
	end
  
	rule(:program) { procedure.repeat }
	rule(:procedure) { name >> name.repeat >> spaced('-') >> string.repeat(1) >> spaced('{') >> guarded_command.repeat >> spaced('}') }
	rule(:guarded_command) { (guard >> spaced(':')).repeat >> command >> spaced(';')}
	rule(:command) {
		name >> name.repeat >> spaced('=')  >>  spaced('exec') >> string >> string.maybe|
		name >> name  >> spaced('=') >> spaced('split') >> string >> string |
		name.repeat(1) >> spaced('=') >> name >> string.repeat |
		name >> spaced('=') >> string.repeat(1)
	}
	rule(:guard) {
		name >> spaced('==') >> string |
		name >> spaced('!=') >> string |
		spaced('finally') |
		name

	}
	rule(:string) {str('"') >> (match['^"'] | variable).repeat >> str('"') >> space?}
	rule(:variable) {str('$') >> identifier >> str('$')}
	rule(:name) { identifier >> space? }
	rule(:identifier) {match['a-zA-Z0-9'].repeat(1)}
	root :program
end


def parse(str)
	gc = GCParser.new
	gc.parse(str)
rescue Parslet::ParseFailed => failure
	puts failure.cause.ascii_tree
end

require 'pp'

pp parse(<<END
maxnum a b c -  "$max$" {
ab = exec "test $a$ -le $b$";
bc = exec "test $b$ -le $c$";
ab == "0" : bc == "0" : max = "$c$";
ab == "0" : bc == "1" : max = "$b$";
ab == "1" : bc == "1" : max = "$a$";
ab == "1" : bc == "0" : ac = exec "test $a$ -le $c$";
ac == "0" : max = "$c$";
ac == "1" : max = "$a$";
finally : max = "error";}
END
)