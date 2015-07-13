proc fibR {x} {
    if {<= $x 1} {
        return $x
    } else {
        + [fibR [- $x 1]] [fibR [- $x 2]]
    }
}

puts [fibR 34]

