#!/usr/bin/gml

fn triangle(n) =>
    for y in range(0, n) {
        for i in range(0, n - 1 - y) => print(" ");
        for x in range(0, n) =>
            if x & n - 1 - y => print("  "); else => print("* ");
        print("\n");
    }

triangle(32);
