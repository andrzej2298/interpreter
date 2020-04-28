<int, bool, string> x = new tuple(1, true, "string");
<int, <int, bool, string>> y = new tuple(1, x);
<int, <int, bool, string>> z = new tuple(1, new tuple(1, true, "string"));

// int i, j, k, l; bool a; string s;
// tie (i, (j, b, s)) = y; // variable assignment from tuple
// tie (k, _) = y; // variable assignment from tuple, one value ignored
// 
// l = y[0];
// 
// <int, int> double_int(int x) {
//     return new tuple(x, x);
// }
// 
// int sum_pair(<int, int> pair) {
//     int x, y;
//     tie (x, y) = pair;
//     return x + y;
// }
// 
// int get_first(<int, int> pair) {
//     return pair[0];
// }
