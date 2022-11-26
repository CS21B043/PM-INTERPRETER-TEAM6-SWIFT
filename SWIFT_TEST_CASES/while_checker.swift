let a = 13;
a = (a + 1)/2 + a;
while(a>a%3)
{
    print(a);
    print(a%3);
    a = a - 1;
}