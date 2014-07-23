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

class Variable 
 
  def initialize(name)
    @name=name.to_str
  end

 @@finally=Variable.new('finally')
  def self.finally
    @@finally
  end

  def name
    @name
  end

  def expand(ctx)
    ctx.key? @name or raise "Cannot expand"
    ctx[@name]
  end
  def bound?(ctx)
    # pp 'var' ,@name, ctx.key?( @name)
    ctx.key? @name
  end
end

class Str < Struct.new(:contents)
  def expand(ctx)
    contents.map{|x|
      x.is_a?(Variable) ? x.expand(ctx) : x
    }.join('')
  end
  def bound?(ctx)
    contents.all?{|x| x.is_a?(String) or x.bound?(ctx) }
  end
end

class GCommand < Struct.new(:guards, :command)
  def ready?(ctx)
    # pp command.ready?(ctx), guards.map{|g| [g,g.ready?(ctx)]}
    guards.all?{|g| g.ready?(ctx)} and command.ready?(ctx)
  end
  def run(ctx)
    command.run(ctx) if guards.all?{|g| g.true?(ctx)} 
  end
end

class Atom
  def initialize(lvs=[],rvs=[], &block)
    @lvs = lvs
    @rvs = rvs
    @block = block
  end

  def ready?(ctx)
    @rvs.all?{|x| x.nil? or x.bound?(ctx)} and @lvs.all?{|x| x.nil? or not x.bound?(ctx)}
  end

  def call(ctx)
    params = @rvs.map{|x| x.nil? ? nil : x.expand(ctx)}
    @block.call(*params)
  end

  def run(ctx)
    l=@lvs.map{|l| l.nil? ?nil: l.name}
    ret=Hash[l.zip(call(ctx))]
    ret.delete(nil)
    # pp "update", Hash[@lvs.zip(call(ctx))], ret
    ctx.merge!(ret)
  end
end

class Guard < Atom
  def initialize(rvs, &block)
    block ||= lambda {True}
    super([],rvs, &block)
  end

  def true?(ctx)
    ready?(ctx) and call(ctx)
  end
end

class Command < Atom

end

class Procedure
   def initialize(name,params,results, commands)
    @name=name
    @params=params
    @results=results
    @commands=commands

   end
  def finished?(ctx)
    @results.all?{|s|s.bound?(ctx)}
  end

  def run(*args)
    ctx={'a'=>1,'b'=>2,'c'=>3}
    while(not finished?(ctx)) do
    #for i in 1..5 do
         @commands.each do |c|
           c.run(ctx) if c.ready?(ctx)
         end
       end
    ctx   
  end
end

class Activation

end


class GCAst < Parslet::Transform
  rule(:name => simple(:x)) { Variable.new(x) }
  rule(:content => simple(:x)) { x.to_str }
  rule(:string => subtree(:x)) { s=Str.new(x) }

  rule(:l => simple(:l), :eq => simple(:eq), :r =>simple(:r) ) { Guard.new([l,r]){|l, r| l==r} }
  rule(:l => simple(:l), :neq => simple(:eq), :r =>simple(:r) ) { Guard.new([l,r]){|l, r| l!=r} }
  rule(:finally => simple(:x)) { Guard.new([Variable.finally]) }
  rule(:isbound => simple(:x)) { Guard.new([x]) }

  rule(:result => simple(:result), :stdout => simple(:stdout), :cmd => simple(:cmd), :stdin=>simple(:stdin)) {
    Command.new([result,stdout],[cmd,stdin]){|cmd,stdin|
      require 'Open3'
      Open3.popen3(cmd) {|i, o, e, t|  
        i.write(stdin)
        [t.value.to_i.to_s, o.read]
      }
    }
  }
  rule(:fst => simple(:fst), :snd => simple(:snd), :delim => simple(:delim), :str=>simple(:str)) {
    Command.new([fst,snd],[delim, str]){|delim,str|
      str.split(delim,2)
    }
  }
  rule(:results => sequence(:results), :procname =>simple(:procname), :params => sequence(:params)) {
    Command.new(results,[procname, params].flatten){|procname, *params|
    []
    }
  }
  rule(:l => simple(:l), :r=>sequence(:r)) {
    Command.new([l],r){|*r| [r.join('')] }
  }

  rule(:guards => sequence(:g), :command=> simple(:c)) { GCommand.new(g,c) }

  rule(:procname => simple(:n), :params => sequence(:p), :results=>sequence(:r), :gcommands => sequence(:c)) { Procedure.new(n, p,r,c) }
  rule(:program => sequence(:p)) {p}

end
  
def parse(str)
  gcp = GCParser.new
  gct = GCAst.new
  procs=gct.apply(gcp.parse(str))
rescue Parslet::ParseFailed => failure
  puts failure.cause.ascii_tree
end

require 'pp'

pp parse(<<END
         maxnum a b c -  "$max$$two$" {
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
END
)[0].run()

# ab  : max = maxnum;
#            ab == "1" : bc == "0" : ac  b = split "s" "test $a$ -le $c$";
