void n() {
  int a = 123;           // decimal
  int b = 0123;          // octal
  int c = 0x123;         // hex
  int d = 0xABC;         // hex
  int g = 123l;          // decimal long
  int e = 124L;          // decimal long
  int h = 125ul;         // decimal unsigned long
  int i = 126UL;         // decimal unsigned long
  int j = 127ll;         // decimal long long
  int k = 128LL;         // decimal long long
  int l = 129ull;        // decimal unsigned long long
  int m = 130ULL;        // decimal unsigned long long
  int n = 131lu;         // decimal unsigned long
  int o = 132llu;        // decimal unsigned long long
  int p = 131LU;         // decimal unsigned long
  int q = 132LLu;        // decimal unsigned long long
  int r = 0123ull;       // octal unsigned long long
  int s = 0x123ull;      // hex unsigned long long
  int t = 0XAB12LLu;     // hex unsigned long long
  int err = 0xAB123LaLu; // error
  int err2 = 0ABC;       // error
  int err3 = 1AB;        // error
}
