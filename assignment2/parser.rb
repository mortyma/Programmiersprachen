require 'rubygems'
require 'parslet' 

class GCParser < Parslet::Parser
    rule(:space?)     { match('\s').repeat(1) }
    rule(:space?)     { match('\s').repeat }
  	rule(:identifier) {match['a-zA-Z0-9'].repeat(1)}


	def spaced(character)
		str(character) >> space?
	end
  
	rule(:program) { procedure.as(:procedure).repeat }
	rule(:procedure) { name.as(:name) >> name.as(:param).repeat >> spaced('-') >> string.as(:result).repeat(1) >> spaced('{') >> guarded_command.as(:gcommand).repeat >> spaced('}') }
	rule(:guarded_command) { (guard.as(:guard) >> spaced(':')).repeat >> command.as(:command) >> spaced(';')}
	rule(:command) {
		(name >> name.repeat >> spaced('=')  >>  spaced('exec') >> string >> string.maybe).as(:exec)|
		(name >> name  >> spaced('=') >> spaced('split') >> string >> string).as(:split) |
		(name.repeat(1) >> spaced('=') >> name >> string.repeat).as(:call) |
		(name >> spaced('=') >> string.repeat(1)).as(:set)
	}
	rule(:guard) {
		(name >> spaced('==') >> string).as(:eq) |
		(name >> spaced('!=') >> string).as(:neq) |
		spaced('finally').as(:finally) |
		name.as(:isbound)

	}
	rule(:string) {str('"') >> (variable|match['^"$'].repeat(1).as(:content)).repeat.as(:string) >> str('"') >> space?}
	rule(:variable) {str('$') >> identifier.as(:var) >> str('$')}
	rule(:name) { identifier.as(:name) >> space? }
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