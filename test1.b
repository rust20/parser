foo(a, b) {
    a[b] = b;
    return a[b] + a[b@1];
}
