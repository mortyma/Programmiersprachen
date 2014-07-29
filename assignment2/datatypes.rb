class Variable
  def self.finally
    Variable.new('finally')
  end

  def initialize(name)
    @name=name
  end

  def name
  	@name.to_str
  end
end

class Str < Struct.new(:contents)
end