void start() {
  int i = 0;
  for (i = 0; i < 10; i = i + 1) {
    if (i < 3) {
      printi i + 10;
    } else if (i < 8 && i >= 5) {
      printi i + 20;
    } else {
      printi i + 30;
    }
  }
  return;
}
