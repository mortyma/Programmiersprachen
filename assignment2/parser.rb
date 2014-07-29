require 'rubygems'
require 'parslet'
require './language'

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
  rule(:string) {str('"') >> (variable|(str('\"')|match['^"$']).repeat(1).as(:content)).repeat.as(:string) >> str('"') >> space?}
  rule(:variable) {str('$') >> identifier.as(:name) >> str('$')}
  rule(:name) { identifier.as(:name) >> space? }
  root :program
end


class GCAst < Parslet::Transform
  rule(:name => simple(:x)) { Variable.new(x) }
  rule(:content => simple(:x)) { x.to_str }
  rule(:string => sequence(:x)) { s=Str.new(x) }

  rule(:l => simple(:l), :eq => simple(:eq), :r =>simple(:r) ) { Guard.new([l,r]){|l, r| l==r} }
  rule(:l => simple(:l), :neq => simple(:eq), :r =>simple(:r) ) { Guard.new([l,r]){|l, r| l!=r} }
  rule(:finally => simple(:x)) { Guard.new([Variable.finally]) }
  rule(:isbound => simple(:x)) { Guard.new([x]) }

# exec
  rule(:result => simple(:result), :stdout => simple(:stdout), :cmd => simple(:cmd), :stdin=>simple(:stdin)) {
    BlockCommand.new([result,stdout],[cmd,stdin]){|cmd,stdin|
      require 'Open3'
      Open3.popen3(cmd) {|i, o, e, t|  
        i.write(stdin)
        [t.value.exitstatus.to_s, o.read]
      }
    }
  }

# split
  rule(:fst => simple(:fst), :snd => simple(:snd), :delim => simple(:delim), :str=>simple(:str)) {
    BlockCommand.new([fst,snd],[delim, str]){|delim,str|
      str.split(delim,2)
    }
  }

# procedure call
  rule(:results => sequence(:results), :procname =>simple(:procname), :params => sequence(:params)) {
    ProcCommand.new(results, procname, params)
  }

# assignment
  rule(:l => simple(:l), :r=>sequence(:r)) {
    BlockCommand.new([l],r){|*r| [r.join('')] }
  }

  rule(:guards => sequence(:g), :command=> simple(:c)) { GCommand.new(g,c) }
  rule(:procname => simple(:n), :params => sequence(:p), :results=>sequence(:r), :gcommands => sequence(:c)) { Procedure.new(n, p,r,c) }
  rule(:program => sequence(:p)) {Program.new(p)}
end


def parse(str)
  gcp = GCParser.new
  gct = GCAst.new
  procs=gct.apply(gcp.parse(str))
rescue Parslet::ParseFailed => failure
  raise "Parse error\n" + failure.cause.ascii_tree
end