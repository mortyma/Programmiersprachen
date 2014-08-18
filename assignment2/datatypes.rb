class Identifier
  def self.finally
    Identifier.new('finally')
  end

  def initialize(name)
    @name=name
  end

  def name
    @name.to_str # remove parser tag
  end

  def to_s
    "<id:#{@name}>"
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

  def to_s
    "str<#{@contents.map{|c| c.to_s}.join}>"
  end
end
