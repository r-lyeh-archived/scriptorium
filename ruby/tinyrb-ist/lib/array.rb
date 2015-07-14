class Array
  include Enumerable
  
  def each
    i = 0
    while i < size
      yield self[i]
      i = i + 1
    end
  end
  
  def first
    self[0]
  end
  
  def join(sep="")
    s = ""
    each do |i|
      s << sep unless i == first
      s << i.to_s
    end
    s
  end
  
  def to_s
    join
  end
  
  def inspect
    str = map do |i|
      i.inspect
    end
    "[" + str.join(", ") + "]"
  end
end
