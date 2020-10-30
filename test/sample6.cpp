int main() {
  int n = 0;
  while (n < 10) {
    if (n / 2 / 2  == 1) { n = n + 2; } else { n = n + 1;}
    cout << n;
  }
  return n;
}