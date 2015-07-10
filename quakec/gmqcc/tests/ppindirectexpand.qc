#define STR1(x) #x
#define STR2(x) STR1(x)
#define THE_ANSWER 42
#define THE_ANSWER_STR STR2(THE_ANSWER)

THE_ANSWER_STR
