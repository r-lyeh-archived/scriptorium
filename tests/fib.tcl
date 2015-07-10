proc fibR {x} {
    if {<= $x 1} {
        return $x
    } else {
        + [fibR [- $x 1]] [fibR [- $x 2]]
    }
}

proc fibI {n} {
    if { == $n 0 } {
        return 0
    } else {
        set prev0 0
        set prev1 1
        set i 1
        while {< $i $n} {
            set tmp $prev1
            set prev1 [+ $prev0 $prev1]
            set prev0 $tmp
            set i [+ $i 1]
        }
        return $prev1
    }
}

puts [fibR 34]
puts [fibI 34]

