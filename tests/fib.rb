def fib(n)
	if n < 2 
		n
	else
		fib(n-1) + fib(n-2)
	end
end

#start = Time.now
puts fib(34)
#puts "elapsed: " + (Time.now - start).to_s
