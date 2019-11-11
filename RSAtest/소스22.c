#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>
#include <math.h>

typedef long long int  llint;
#define W	32
#define R	16
#define P	0
#define M1	13
#define M2	9
#define M3	5

#define MAT0POS(t,v)	(v^(v>>t))
#define MAT0NEG(t,v)	(v^(v<<(-(t))))

#define MAT3NEG(t,v)	(v<<(-(t)))
#define MAT4NEG(t,b,v)	(v^((v<<(-(t)))&b))

#define V0		STATE[state_i]
#define VM1		STATE[(state_i+M1) & 0x0000000fU]
#define VM2		STATE[(state_i+M2) & 0x0000000fU]
#define VM3		STATE[(state_i+M3) & 0x0000000fU]
#define VRm1	STATE[(state_i+15) & 0x0000000fU]
#define VRm2	STATE[(state_i+14) & 0x0000000fU]
#define newV0	STATE[(state_i+15) & 0x0000000fU]
#define newV1	STATE[state_i]
#define newVRm1	STATE[(state_i+14) & 0x0000000fU]

#define FACT	2.32830643653869628906e-10

#define RND_MAX	0xffffffff
#define RND_MIN	0x0fffffff
#define FALSE	0
#define TRUE	1

// mini RSA 관련 타입
typedef unsigned char bool;
typedef unsigned char byte;
typedef unsigned int  uint;
typedef long long int llint;

// WELL Random number generator 관련 전역 변수
static unsigned int state_i = 0;
static unsigned int STATE[R];
static unsigned int z0, z1, z2;


llint p, q, e, d, n;

// WELL Random number generator 관련 함수
void InitWELLRNG512a(uint * init);
double WELLRNG512a(void);

// 난수 생성을 위한 초기화 함수
void InitWELLRNG512a(uint * init) {
	int j;
	state_i = 0;
	for (j = 0; j < R; j++) STATE[j] = init[j];
}

// 난수 생성 함수
double WELLRNG512a(void) {
	z0 = VRm1;
	z1 = MAT0NEG(-16, V0) ^ MAT0NEG(-15, VM1);
	z2 = MAT0POS(11, VM2);
	newV1 = z1 ^ z2;
	newV0 = MAT0NEG(-2, z0) ^ MAT0NEG(-18, z1) ^ MAT3NEG(-28, z2) ^ MAT4NEG(-5, 0xda442d24U, newV1);
	state_i = (state_i + 15) & 0x0000000fU;
	return ((double)STATE[state_i]) * FACT;
}

llint even_odd(llint a) {
	llint result = TRUE;
	while (a > 0) {
		a -= 2;
		if (a == 1) {
			return FALSE;	//홀
		}
		else if (a == 0) {
			return TRUE;	//짝
		}
	}
}


llint GCD(llint a, llint b) {
	llint prev_a;

	while (b != 0) {
		printf("GCD(%lld, %lld)\n", a, b);
		prev_a = a;
		a = b;
		while (prev_a >= b) prev_a -= b;
		b = prev_a;
	}
	printf("GCD(%lld, %lld)\n\n", a, b);
	return a;
}

llint ModAdd(llint a, llint b, byte op, llint n) {
	llint result = 0;

	switch (op) {
	case '+':
		if (a == 0) {
			result = b;
		}

		while (a > 0) {
			a -= n;
			if (a < 0) {
				a += n;
				result += (a + b);
				break;
			}
			else if (a == 0) {
				result += b;
				break;
			}
		}

		break;

	case '-':
		if (a == 0)
			result -= b;

		while (a > 0) {
			a -= n;
			if (a < 0) {
				a += n;
				result += (a - b);
				break;
			}
			else if (a == 0) {
				result -= b;
				break;
			}
		}

		break;
	}


	if (result > 0) {
		while (result > 0) {
			result -= n;
			if (result < 0) {
				result += n;
				break;
			}
			else if (result == 0)
				break;
		}
	}
	else if (result < 0) {
		while (result < 0) {
			result += n;
			if (result >= n) {
				result -= n;
				break;
			}
			else if (result == 0)
				break;
		}
	}

	return result;
}

llint ModMul(llint x, llint y, llint n) {
	llint result = 0, temp = 0;
	//llint k = y >> 1, m = (y - 1) >> 1;


	//if (even_odd(y) == TRUE)
	//	result = ModAdd(ModMul(x, k, n), ModMul(x, k, n), '+', n);

	//else if (even_odd(y) == FALSE)
	//	result = ModAdd(ModAdd(x, 0, '+', n), ModAdd(ModMul(x, m, n), ModMul(x, m, n), '+', n), '+', n);
	//	

	temp = ModAdd(0, x, '+', n) * ModAdd(0, y, '+', n);

	result = ModAdd(0, temp, '+', n);

	return result;
}

llint ModPow(llint base, llint exp, llint n) {
	llint result = 1, count = 0, temp = 0, temp1 = 0;

	if (exp == 0)
		return result;

	while (exp != 0) {

		if (even_odd(exp) == FALSE) {	//홀
			result = (result * base);
			temp = result;
			while (temp > 0) {
				temp -= n;
				if (temp < 0) {
					temp += n;
					result = temp;
					break;
				}
				else if (temp == 0)
					result = temp;
				break;
			}
		}

		exp >>= 1;
		base *= base;
		temp1 = base;

		while (temp1 > 0) {
			temp1 -= n;
			if (temp1 < 0) {
				temp1 += n;
				base = temp1;
				break;
			}
			else if (temp1 == 0)
				base = temp1;
			break;
		}

	}

	return result;
}

bool IsPrime(llint testNum, llint repeat) {

	bool result = 'T';
	llint n = testNum, a, t = 0;
	llint s = n - 1;
	llint d = s;
	s = 0;

	if (n <= 1) {
		result = 'F';
		return result;
	}
	else if (n == 2 || n == 3) {
		return result;
	}
	else if (ModAdd(n, 0, '+', 2) == 0) {
		result = 'F';
		return result;
	}

	while (ModAdd(d, 0, '+', 2) == 0) {
		d >>= 1;
		s++;
	}

	//printf("d : %llu\n", d);
	//printf("s : %llu\n", s);

	for (int i = 0; i < repeat; i++) {
		state_i = i;
		a = (llint)(WELLRNG512a() * (n - 1)) + 1;
		for (int r = 0; r < s; r++) {
			/*printf("seed : %llu\n", a);*/
			t = ModPow(r, 2, n);
			t = ModMul(t, d, n);
			if (ModPow(a, d, n) != 1 && ModPow(a, (2, r) * d, n) != n - 1) {
				result = 'F';
			}
		}
	}
	return result;
}

llint ModInv(llint a, llint m) {
	llint s0 = 1, s1 = 0, r0 = a, r1 = m, q = 0, temp = 0;
	llint t0 = 0, t1 = 1;
	llint count;
	while (r1 > 0) {

		count = 0;
		temp = r0;
		while (temp >= r1) {
			temp -= r1;
			count++;
		}
		q = count;

		temp = r0;
		r0 = r1;
		r1 = temp - r1 * q;

		temp = s0;
		s0 = s1;
		s1 = temp - s1 * q;

		temp = t0;
		t0 = t1;
		t1 = temp - t1 * q;
	}
	if (s0 < 0) {
		s0 += m;
	}

	return s0;
}

void miniRSAKeygen(llint * p, llint * q, llint * e, llint * d, llint * n) {

	llint pi_n;

	*p = (llint)((WELLRNG512a() * (exp_squaring(2, 64))) + exp_squaring(2, 53));
	while (IsPrime(*p, 10) == 'F') {
		printf("random-number1 %llu selected.\n", *p);
		printf("%llu is not Prime.\n\n", *p);
		*p = (llint)((WELLRNG512a() * (exp_squaring(2, 64))) + exp_squaring(2, 53));
		if (IsPrime(*p, 10) == 'T')
			break;
	}

	printf("random-number1 %llu selected.\n", *p);
	printf("%llu may be Prime.\n\n", *p);

	*q = (llint)((WELLRNG512a() * (480)) + 1);
	while (IsPrime(*q, 10) == 'F') {
		printf("random-number2 %llu selected.\n", *q);
		printf("%llu is not Prime.\n\n", *q);
		*q = (llint)((WELLRNG512a() * (480)) + 1);
		if (IsPrime(*q, 10) == 'T')
			break;
	}

	printf("random-number2 %llu selected.\n", *q);
	printf("%llu may be Prime.\n\n", *q);

	*n = (*p) * (*q);

	printf("finally selected prime p, q  = %llu, %llu .\n", *p, *q);
	printf("this, n = %llu\n\n", *n);

	pi_n = (*p - 1) * (*q - 1);

	*e = (llint)((WELLRNG512a() * (pi_n - 1)) + 1);
	printf("e : %llu selected.\n", *e);
	while (GCD(*e, pi_n) != 1) {
		*e = (llint)((WELLRNG512a() * (pi_n - 1)) + 1);
		printf("e : %llu selected.\n", *e);
	}

	*d = ModInv(*e, pi_n);
	printf("d : %llu selected.\n\n", *d);

	printf("e, d, n, pi_n : %3llu %3llu %3llu %3llu\n", *e, *d, *n, pi_n);
	printf("e*d mod pi_n : %llu\n\n", ModMul(*e, *d, pi_n));
}

llint miniRSA(llint data, llint key, llint n) {
	llint result;
	result = ModPow(data, key, n);

	return result;
}

llint seed, qwe, a, b;
bool boo;

int main(void) {
	seed = time(NULL);
	InitWELLRNG512a(&seed);
	byte plain_text[4] = { 0x12, 0x34, 0x56, 0x78 };
	llint plain_data = 17, encrpyted_data, decrpyted_data;

	boo = IsPrime(2, 10);
	printf("IsPrime : %c\n", boo);

	miniRSAKeygen(&p, &q, &e, &d, &n);
	printf("0. Key generation is Success!\n ");
	printf("p : %llu\n q : %llu\n e : %llu\n d : %llu\n N : %llu\n\n", p, q, e, d, n);

	encrpyted_data = miniRSA(plain_data, e, n);
	printf("1. plain text : %llu\n", plain_data);
	printf("2. encrypted plain text : %llu\n\n", encrpyted_data);


	//decrpyted_data = miniRSA(encrpyted_data, d, n);
	//printf("3. cipher text : %llu\n", encrpyted_data);
	//printf("4. Decrypted plain text : %llu\n\n", decrpyted_data);



	return 0;
}