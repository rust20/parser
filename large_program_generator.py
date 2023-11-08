m = 1000

n = 20000
for k in range(m):
    print('func'+str(k)+'(a){' + ''.join(f'{{register a{i}=a{(i-1)//2 if i>1 else ""}; '
                               for i in range(n)) + f'return a+a{n//2}+a{n-1};' +
          '}' * (n + 1))
    print()
