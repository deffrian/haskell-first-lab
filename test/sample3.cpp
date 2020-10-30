int main() {
  string curVal = "";
  string result = "";
  while (curVal != "q") {
    if (curVal == "qqq") {
      return 1;
    }
    result = result + curVal;
    cin >> curVal;
  }
  cout << "Result: " << result;
  return 0;
}