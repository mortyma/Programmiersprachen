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

class Str
  attr_reader :contents
	def initialize(cts=[])
		@contents=cts.map{|x| Str.unescape(x)}
	end

  def self.unescape(x)
    return x unless x.is_a?(String)
    x.gsub('\"','"')
  end
end