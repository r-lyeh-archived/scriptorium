var float foo = 0;

void funcall() {}
void bar(string str) {}

void main(string str) {
    string pl;

    if (foo)
        return; // this is a block wher 'str' doesn't live
                // so the point-life will not overlap with str
    pl = "Got overwritten!\n"; // pl point-life

    print(str);

    pl = "Kill the lifrange here"; // pl life stops
    funcall(); // Now lock pl in case we have -Oglobal-temps
    bar(pl); // pl life starts here now
}
