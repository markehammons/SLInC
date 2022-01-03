struct a_t
{
   int a;
   int b;
};

struct b_t
{
   int c;
   struct a_t d;
};

struct c_t
{
   int a[3];
   float b[3];
};

struct b_t slinc_test_modify(struct b_t b)
{
   b.d.a += 6;
   return b;
}

struct c_t slinc_test_addone(struct c_t c)
{
   for (int i = 0; i < 3; i++)
   {
      c.a[i] += 1;
      c.b[i] += 1;
   }

   return c;
}

void slinc_test_passstaticarr(int res[3])
{
   return;
}

int *slinc_test_getstaticarr()
{

   int *ret = malloc(sizeof(int) * 3);
   ret[0] = 1;
   ret[1] = 2;
   ret[2] = 3;

   return ret;
}

int slinc_two_structs(struct a_t a, struct a_t b)
{
   return a.a * b.a;
}