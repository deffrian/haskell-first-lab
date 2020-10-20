int main() {
  string curVal = "";
  string result = "";
  while (curVal != "q") {
    result = result + curVal;
    cin >> curVal;
  }
  cout << "Result: " << result;
}