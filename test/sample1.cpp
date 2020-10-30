int func(int a) {
  return (a - 1) * (a + 1);
}

int main() {
  int a, b = 2;
  cin >> a;
  cout << "func(a) = " << func(a);
}