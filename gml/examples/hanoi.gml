fn move(n, f, t, v) =>
    if n > 0 {
        move(n - 1, f, v, t);
        println("Move disk from pole", f, "to pole", t);
        move(n - 1, v, t, f);
    }

move(4, 1, 2, 3);
