#
# This is development code for working with the handheld.
#
#
# $Id: devel.tcl,v 1.1.1.1 2001/04/29 20:36:22 karll Exp $
#

#
# wait ms - wait the specified number of milliseconds
#
# This is a workaround because the DOS "delay" command doesn't work properly
# on the Two Tech.
#
proc wait {ms} {
    set startClock [rawclock]
    while {[rawclock] - $startClock < $ms} continue
    return ""
}

#
# dir - sort of a dir command
#
proc dir {} {
    echo [glob *]
}

#
# cat - dump a file to stdout
#
proc cat {fileName} {
    set fp [open $fileName]
    while {[gets $fp line] >= 0} {
        puts $line
    }
    close $fp
}

#
# bench - run a little benchmark (useful for determining if you're at
#         8 or 16 MHz on the handheld)
#
proc bench {} {
    set clock [rawclock]
    for {set i 0} {$i < 1000} {incr i} continue
    set stopclock [rawclock]
    return "1K test took [expr {$stopclock - $clock}] ms"
}

#
# benchmark - run the benchmark and manage the display
#
proc benchmark {} {
    cls
    clear
    title "benchmark results"
    display [bench]
    anykey
}

#
# parray - print out the contents of a global array
#
proc parray {arrayName} {
    upvar #0 $arrayName array

    foreach element [lsort [array names array]] {
        puts stdout "$element = $array($element)"
    }
}

