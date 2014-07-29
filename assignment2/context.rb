require_relative 'datatypes'

# this class corresponds to the activation record of procedures
# it it also the only place where synchronization would be necessary (if it weren't for the GIL)
class Context
  def initialize(keys=[], values=[])
    @vars={}
    set_multiple keys, values
  end

  def set(key, value)
    unless @vars.key?(key.name) # Do not overwrite bound variables
      @vars[key.name] = value
    end
  end

  # set multiple values atomically
  def set_multiple(keys, values)
    # pp keys,values
    merge Hash[keys.zip(values)]
    # pp @vars
  end

  # expand strings and variables, ignore nils
  def get(x)
    if x.is_a?(String) or x.nil?
      x
    elsif x.is_a?(Variable)
      raise "Variable not bound" unless @vars.key?(x.name)
      @vars[x.name]
    elsif x.is_a?(Str)
      x.contents.map{|e| get(e) }.join('')
    else
      raise "Invalid Type"
    end
  end

  #this is only a convinience method, consistency is already ensured by single-assignment semantics
  def get_multiple(params)
    params.map{ |x| x.nil? ? nil : get(x) }
  end

  # literals and nils are always bound
  def bound?(x)
    if x.is_a?(String) or x.nil?
      true
    elsif x.is_a?(Variable)
      @vars.key?(x.name)
    elsif x.is_a?(Str)
      all_bound?(x.contents)
    else
      raise "Invalid Type"
    end
  end

  #this is only a convinience method, consistency is ensured by single-assignment semantics
  def all_bound?(list)
    list.all?{|x| bound?(x)}
  end

  def merge(hash)
    hash.each do |var, val|
      set(var,val) unless var.nil? or val.nil?
    end
  end

  private :merge
end