// Locals, assignment as an expression, shadowing in nested blocks.
// exit: 9
int main()
{
    int x = 1;
    int y;
    y = x = 4;          // assignment is right-associative, and yields a value
    {
        int x = 100;    // shadows the outer x, only inside this block
        y = y + x - 100;
    }
    return x + y + 1;   // 4 + 4 + 1
}
