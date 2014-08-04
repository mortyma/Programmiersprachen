require_relative 'datatypes'

# this class corresponds to the activation record of procedures
# it it also the only place where synchronization would be necessary (if it weren't for the GIL)
class Context
  def initialize(keys=[], values=[], parent=nil)
    @vars={}
    @parent=parent
    @alive=true
    set_multiple keys, values
  end

  def set(key, value)
    unless @vars.key?(key.name) # Do not overwrite bound variables
      @vars[key.name] = value
    end
  end

  # set multiple values atomically
  # @param keys [Array<Identifier>]
  # @param values [Array<String>]
  def set_multiple(keys, values)
    raise "Invalid size" unless keys.size == values.size
    # pp keys,values
    keys.zip(values).each do |var, val|
      set(var,val) unless var.nil? or val.nil?
    end
  end

  # expand strings and Variables
  # @param x [Identifier, Str, String]
  def get(x)
    if x.is_a?(String)
      x
    elsif x.is_a?(Identifier)
      raise "Variable not bound" unless @vars.key?(x.name)
      @vars[x.name]
    elsif x.is_a?(Str)
      x.contents.map{|e| get(e) }.join('')
    else
      raise "Invalid Type"
    end
  end

  # this is only a convinience method, consistency is already ensured by single-assignment semantics
  # we ignore nils
  # @param list [Array<Identifier, Str, String, nil>]
  def get_multiple(list)
    list.map{ |x| x.nil? ? nil : get(x) }
  end

  # @param x [Identifier, Str, String]
  def bound?(x)
    if x.is_a?(String)
      true
    elsif x.is_a?(Identifier)
      @vars.key?(x.name)
    elsif x.is_a?(Str)
      all_bound?(x.contents)
    else
      raise "Invalid Type"
    end
  end

  #this is only a convinience method, consistency is ensured by single-assignment semantics
  # we ignore nils
  # @param list [Array<Identifier, Str, String, nil>]

  def all_bound?(list)
    list.all?{|x| x.nil? or bound?(x)}
  end

  def alive?
    @alive && (@parent.nil? || @parent.alive? )
  end

  def kill
    @alive=false
  end

end
