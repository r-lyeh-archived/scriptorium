module Enumerable
  def each_with_index
    i = 0
    each do |item|
      yield item, i
      i = i + 1
    end
  end
  
  def include?(item)
    each do |i|
      return true if i == item
    end
    false
  end
  
  def map
    a = []
    each do |i|
      a << yield i
    end
    a
  end
  
  def to_a
    a = []
    each do |item|
      a << item
    end
    a
  end
end