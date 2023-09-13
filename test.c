void start() {
  int *p;
  int a;
  a = 42;
  p = &a;
  printi(*p);
  *p = 43;
  printi(a);
}
