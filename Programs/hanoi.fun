// Towers of Hanoi in Fun

def hanoi(n: Int, a: Int, b: Int, c: Int) : Void =
  if n != 0 then {
    hanoi(n - 1, a, c, b);
    print_int(a);
    print_char('-'); print_char('>'); // prints out "->"
    print_int(b);
    print_char('\n');
    hanoi(n - 1, c, b, a)
  } else skip;

hanoi(4,1,2,3)