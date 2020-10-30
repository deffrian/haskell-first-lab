bool less(int a, int b) {
  return a < b;
}

int max(int a, int b) {
  int res = a;
  if (less(a, b)) {
    res = b;
  }
  return res;
}

int main() {
  int a, b;
  cin >> a >> b;
  cout << max(a, b);
  return -1;
}