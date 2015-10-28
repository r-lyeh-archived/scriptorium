terra fib(a : int) : int
    if a < 2 then
        return a
    else
        return fib(a - 1) + fib(a - 2)
    end
end

print(fib(34))
