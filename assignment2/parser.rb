require 'rubygems'
require 'parslet'

class GCParser < Parslet::Parser
  rule(:space?)     { match('\s').repeat(1) }
  rule(:space?)     { match('\s').repeat }
  rule(:identifier) {match['a-zA-Z0-9'].repeat(1)}


  def spaced(character)
    str(character) >> space?
  end

  rule(:program) { space? >> procedure.repeat.as(:program) }
  rule(:procedure) { name.as(:procname) >> name.repeat.as(:params) >> spaced('-') >> string.repeat(1).as(:results) >> spaced('{') >> guarded_command.repeat.as(:gcommands) >> spaced('}') }
  rule(:guarded_command) { (guard >> spaced(':')).repeat.as(:guards) >> command.as(:command) >> spaced(';')}
  rule(:command) {
    name.as(:result) >> name.maybe.as(:stdout) >> spaced('=')  >>  spaced('exec') >> string.as(:cmd) >> string.maybe.as(:stdin)|
    name.as(:fst) >> name.as(:snd)  >> spaced('=') >> spaced('split') >> string.as(:delim) >> string.as(:str)|
    name.repeat(1).as(:results) >> spaced('=') >> name.as(:procname) >> string.repeat.as(:params) |
    name.as(:l) >> spaced('=') >> string.repeat(1).as(:r)
  }

  rule(:guard) {
    name.as(:l) >> spaced('==').as(:eq) >> string.as(:r) |
    name.as(:l) >> spaced('!=').as(:neq) >> string.as(:r) |
    spaced('finally').as(:finally) |
    name.as(:isbound)

  }
  rule(:string) {str('"') >> (variable|match['^"$'].repeat(1).as(:content)).repeat.as(:string) >> str('"') >> space?}
  rule(:variable) {str('$') >> identifier.as(:name) >> str('$')}
  rule(:name) { identifier.as(:name) >> space? }
  root :program
end

class Variable < Struct.new(:name)
  def expand(ctx)
    ctx.key? @name or raise "Cannot expand"
    ctx[@name]
  end
  def bound?(ctx)
    ctx.key? @name
  end
end

class Str < Struct.new(:contents)
  def expand(ctx)
    contents.map{|x|
      x.is_a?(Variable) ? x.expand(ctx) : x
    }.join("")
  end
  def bound?(ctx)
    contents.all?{|x| x.is_a?(String) or x.bound?(ctx) }
  end
end

class GCommand < Struct.new(:guards, :command)
  def ready?(ctx)
    guards.all{|g| g.ready?} and command.ready?
  end
  def run(ctx)
    command.run(ctx)
  end
end

class Guard < Struct.new(:data)
  def ready?(ctx)
    @lv.all?{|s| s.bound?}
  end
end

class Command < Struct.new(:data)
  def ready?(ctx)
    @lv.all?{|v| not v.bound?(ctx)} and @rv.all?{|s| s.bound?}
  end
end

class Procedure < Struct.new(:data)
  def finished?(ctx)
    @results.all?{|s|s.bound?(ctx)}
  end

  def run(*args)
    ctx={}
    # while(not finished?(ctx)) do
    #     @gcommands.each do |c|
    #       c.run(ctx) if c.ready?(ctx)
    #     end
    #   end      1
  end
end


class GCAst < Parslet::Transform
  rule(:name => simple(:x)) { Variable.new(x) }
  rule(:content => simple(:x)) { x.to_str }
  rule(:string => subtree(:x)) { s=Str.new(x);s.bound?({}) and pp s.expand({}); s }

  rule(:l => simple(:l), :eq => simple(:eq), :r =>simple(:r) ) { Guard.new([l,:eq,r]) }
  rule(:l => simple(:l), :neq => simple(:eq), :r =>simple(:r) ) { Guard.new([l,:neq,r]) }
  rule(:finally => simple(:x)) { Guard.new(:finally) }
  rule(:isbound => simple(:x)) { Guard.new([:isbound,x]) }

  rule(:result => simple(:result), :stdout => simple(:stdout), :cmd => simple(:cmd), :stdin=>simple(:stdin)) { Command.new([:exec, result,stdout,cmd,stdin]) }
  rule(:fst => simple(:fst), :snd => simple(:snd), :delim => simple(:delim), :str=>simple(:str)) { Command.new([:split, fst,snd,delim, str]) }
  rule(:results => sequence(:results), :procname =>simple(:procname), :params => sequence(:params)) { Command.new([:call, results, procname, params]) }
  rule(:l => simple(:l), :r=>sequence(:r)) { Command.new([:assignment, l,r]) }

  rule(:guards => sequence(:g), :command=> simple(:c)) { GCommand.new(g,c) }

  rule(:procname => simple(:n), :params => sequence(:p), :results=>sequence(:r), :gcommands => sequence(:c)) { Procedure.new([n, p,r,c]) }
  rule(:program => sequence(:p)) {p}

end

def parse(str)
  gcp = GCParser.new
  gct = GCAst.new
  gct.apply(gcp.parse(str))
rescue Parslet::ParseFailed => failure
  puts failure.cause.ascii_tree
end

require 'pp'

pp parse(<<END
         maxnum a b c -  "$max$" {
           ab = exec "test $a$ -le $b$";
           bc = exec "test $b$ -le $c$";
           ab  : max = maxnum;
           ab == "1" : bc == "0" : ac  b = split "s" "test $a$ -le $c$";

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
