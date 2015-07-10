const float huge = 340282346638528859811704183484516925440.000000; // FLT_MAX

#ifdef DIVBYZERO
const float a = 1.0 / 0.0;
#endif

#ifdef OVERFLOW
const float a = huge * huge;
#endif

#ifdef UNDERFLOW
const float a = 1 / huge;
#endif
