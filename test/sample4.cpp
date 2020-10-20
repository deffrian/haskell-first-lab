int main() {
  double x, y;
  cin >> x >> y;
  if (x * x + y * y < 1) {
    cout << "Inside";
  } else {
    cout << "Outside";
  }
}