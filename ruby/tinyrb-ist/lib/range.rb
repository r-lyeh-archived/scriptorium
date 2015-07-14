class Range
  include Enumerable
  
  def to_s
    first.to_s + ".." + last.to_s
  end

  def each
    i = first
    last_el = last + 1
    while i < last_el
        yield i
        i = i + 1
    end
  end

end
