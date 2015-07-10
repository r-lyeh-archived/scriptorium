float  time;
entity self;

.void() think;
.float  nextthink;
.float  frame;

void stprint(string fun) {
    print(fun,
          ", .frame=", ftos(self.frame),
          ", .nextthink=", ftos(self.nextthink),
          " (now: ", ftos(time), ")\n");
}

void st1() = [1, st2] { stprint("st1"); }
void st2() = [2, st3] { stprint("st2"); }
void st3() = [0, st1] { stprint("st3"); }

void main() {
    entity ea = spawn();
    entity eb = spawn();

    time = 10;
    self = ea;

    self.think     = st1;
    self.nextthink = time;
    self.frame     = 100;

    self.think();
    time = 11;
    self.think();
    time = 12;
    self.think();
    time = 13;
    self.think();
    time = 14;
    self.think();
};
