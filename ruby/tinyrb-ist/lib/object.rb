class Object
  def respond_to?(message)
    !!method(message)
  end
  
  def ==(other)
    object_id == other.object_id
  end
end