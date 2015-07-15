fn beer(bottles) {
    if bottles != 0 {
        println(bottles, "bottles of beer on the wall");
        println(bottles, "bottles of beer");
        println("Take one down, pass it around");
        bottles = bottles - 1;
        println(bottles, "bottles of beer on the wall");
        beer(bottles);
    }
}

beer(99);
