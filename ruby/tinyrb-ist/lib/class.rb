class Class
  def new(*args)
    obj = allocate
    obj.initialize(*args) if obj.respond_to?(:initialize)
    obj
  end
end
