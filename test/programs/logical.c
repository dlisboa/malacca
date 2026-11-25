// && || ! ?: and short-circuit evaluation (the right side must not run).
// exit: 15
int calls = 0;

int touch(int v)
{
    calls = calls + 1;
    return v;
}

int main()
{
    int r = 0;
    r = r + (1 && 2);              // 1
    r = r + (0 || 3) * 2;          // 2
    r = r + !0 * 4;                // 4
    r = r + (0 && touch(1));       // 0, touch is skipped
    r = r + (1 || touch(1));       // 1, touch is skipped
    r = r + (1 ? 5 : 100);         // 5
    r = r + (0 ? 100 : 2) + calls; // 2 + 0
    return r;
}
