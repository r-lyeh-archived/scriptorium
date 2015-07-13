function fibR(n)

    if (n < 2) then return n end
    return (fibR(n-2) + fibR(n-1))
end

local start = os.clock()
N = 34 --Should return 433494437
print("fib: " .. fibR(N))
io.write(string.format("elapsed: %.8f\n", os.clock() - start))