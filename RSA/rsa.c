/*
 * @file    rsa.c
 * @author  작성자 이름 / 학번
 * @date    작성 일자
 * @brief   mini RSA implementation code
 * @details 세부 설명
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "rsa.h"

llint p, q, e, d, n;

llint Pows(llint base, llint exp) {
	llint result = 1;

	if (exp == 0)
		return result;

	while (exp != 0) {

		if (exp & 1) 	//홀
			result = (result * base);

		exp >>= 1;
		base *= base;

	}

	return result;
}

/*
 * @brief     모듈러 덧셈 연산을 하는 함수.
 * @param     llint a     : 피연산자1.
 * @param     llint b     : 피연산자2.
 * @param     byte op    : +, - 연산자.
 * @param     llint n      : 모듈러 값.
 * @return    llint result : 피연산자의 덧셈에 대한 모듈러 연산 값. (a op b) mod n
 * @todo      모듈러 값과 오버플로우 상황을 고려하여 작성한다.
 */
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


/*
 * @brief      모듈러 곱셈 연산을 하는 함수.
 * @param      llint x       : 피연산자1.
 * @param      llint y       : 피연산자2.
 * @param      llint n       : 모듈러 값.
 * @return     llint result  : 피연산자의 곱셈에 대한 모듈러 연산 값. (a x b) mod n
 * @todo       모듈러 값과 오버플로우 상황을 고려하여 작성한다.
 */
llint ModMul(llint x, llint y, llint n) {

	llint result = 0;

	while (x > 0) {
		x -= n;
		if (x < 0) {
			x += n;
			break;
		}
		else if (x == 0) {
			break;
		}
	}

	while (y > 0) {
		y -= n;
		if (y < 0) {
			y += n;
			break;
		}
		else if (y == 0) {
			break;
		}
	}
	while (y) {
		if (y & 1) {
			result += x;
			while (result > 0) {
				result -= n;
				if (result < 0) {
					result += n;
					break;
				}
				else if (result == 0) {
					break;
				}
			}
		}
		y >>= 1;
		if (x < n - x) {
			x <<= 1;
		}
		else {
			x -= (n - x);
		}
	}
	return result;
}

/*
 * @brief      모듈러 거듭제곱 연산을 하는 함수.
 * @param      llint base   : 피연산자1.
 * @param      llint exp    : 피연산자2.
 * @param      llint n      : 모듈러 값.
 * @return     llint result : 피연산자의 연산에 대한 모듈러 연산 값. (base ^ exp) mod n
 * @todo       모듈러 값과 오버플로우 상황을 고려하여 작성한다.
               'square and multiply' 알고리즘을 사용하여 작성한다.
 */
llint ModPow(llint base, llint exp, llint n) {
	llint result = 1, count = 0, temp = 0, temp1 = 0;

	if (exp == 0)
		return result;

	while (exp > 0) {

		if (exp & 1) {	//홀
			result = ModMul(result, base, n);
			temp = result;
			while (temp > 0) {
				temp -= n;
				if (temp < 0) {
					temp += n;
					result = temp;
					break;
				}
				else if (temp == 0) {
					result = temp;
					break;
				}
			}
		}

		exp >>= 1;
		base = ModMul(base, base, n);
		temp1 = base;
		sleep(3);

		while (temp1 > 0) {
			temp1 -= n;
			if (temp1 < 0) {
				temp1 += n;
				base = temp1;
				break;
			}
			else if (temp1 == 0) {
				base = temp1;
				break;
			}

		}

	}

	return result;
}

/*
 * @brief      입력된 수가 소수인지 입력된 횟수만큼 반복하여 검증하는 함수.
 * @param      llint testNum   : 임의 생성된 홀수.
 * @param      llint repeat    : 판단함수의 반복횟수.
 * @return     llint result    : 판단 결과에 따른 TRUE, FALSE 값.
 * @todo       Miller-Rabin 소수 판별법과 같은 확률적인 방법을 사용하여,
               이론적으로 4N(99.99%) 이상 되는 값을 선택하도록 한다. 
 */
bool IsPrime(llint testNum, llint repeat) {
	llint n = testNum;
	llint d = n - 1;
	llint s = 0, a, t;

	uint seed = time(NULL);
	InitWELLRNG512a(&seed);

	if (n <= 1)
		return 'F';

	while (!(d & 1)) {
		d >>= 1;
		s++;
	}

	for (int i = 0; i < repeat; i++) {
		a = (llint)((WELLRNG512a() * (n - 1) + 1));
		t = ModPow(a, d, n);
		if ((t == 1) || (t == n - 1))
			continue;
		else {
			int con = 0;
			for (int r = 1; r < s; r++) {
				t = ModPow(t, 2, n);
				if (t == n - 1)
					con = 1;
				break;
				if (t == 1)
					return 'F';
			}
			if (con) continue;
		}
		return 'F';
	}
	return 'T';
}

/*
 * @brief       모듈러 역 값을 계산하는 함수.
 * @param       llint a      : 피연산자1.
 * @param       llint m      : 모듈러 값.
 * @return      llint result : 피연산자의 모듈러 역수 값.
 * @todo        확장 유클리드 알고리즘을 사용하여 작성하도록 한다.
 */
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

/*
 * @brief     RSA 키를 생성하는 함수.
 * @param     llint *p   : 소수 p.
 * @param     llint *q   : 소수 q.
 * @param     llint *e   : 공개키 값.
 * @param     llint *d   : 개인키 값.
 * @param     llint *n   : 모듈러 n 값.
 * @return    void
 * @todo      과제 안내 문서의 제한사항을 참고하여 작성한다.
 */
void miniRSAKeygen(llint* p, llint* q, llint* e, llint* d, llint* n) {

	llint pi_n = 1;
	llint max = Pows(2, 32);
	llint min = Pows(2, 27);
	llint pqmax = Pows(2, 64) - 1;
	llint pqmin = Pows(2, 53);

	*p = (llint)(WELLRNG512a() * (max - min - 1) + min);
	while (IsPrime(*p, 10) == 'F') {
		printf("random-number1 %llu selected.\n", *p);
		printf("%llu is not Prime.\n\n", *p);
		*p = (llint)(WELLRNG512a() * (max - min - 1) + min);
		if (IsPrime(*p, 10) == 'T')
			break;
	}

	printf("random-number1 %llu selected.\n", *p);
	printf("%llu may be Prime.\n\n", *p);

	*q = (llint)(WELLRNG512a() * (max - min - 1) + min);
	if ((*p) * (*q) >= pqmax && (*p) * (*q) <= pqmin) {
		while (IsPrime(*q, 10) == 'F') {
			printf("random-number2 %llu selected.\n", *q);
			printf("%llu is not Prime.\n\n", *q);
			*q = (llint)(WELLRNG512a() * (max - min - 1) + min);
			if ((*p) * (*q) < pqmax && (*p) * (*q) > pqmin)
				if (IsPrime(*q, 10) == 'T')
					break;
				else
					continue;
		}
	}
	else {
		while (IsPrime(*q, 10) == 'F') {
			printf("random-number2 %llu selected.\n", *q);
			printf("%llu is not Prime.\n\n", *q);
			*q = (llint)(WELLRNG512a() * (max - min - 1) + min);
			if ((*p) * (*q) < pqmax && (*p) * (*q) > pqmin)
				if (IsPrime(*q, 10) == 'T')
					break;
				else
					continue;
		}
	}

	printf("random-number2 %llu selected.\n", *q);
	printf("%llu may be Prime.\n\n", *q);

	*n = (*p) * (*q);

	printf("finally selected prime p, q  = %llu, %llu .\n", *p, *q);
	printf("this, n = %llu\n\n", *n);

	pi_n = ((*p) - 1) * ((*q) - 1);

	*e = (llint)((WELLRNG512a() * (pi_n - 2)) + 2);
	printf("e : %llu selected.\n", *e);
	if (GCD(*e, pi_n) != 1) {
		while (GCD(*e, pi_n) != 1) {
			*e = (llint)((WELLRNG512a() * (pi_n - 2)) + 2);
			printf("e : %llu selected.\n", *e);
		}
	}

	*d = ModInv(*e, pi_n);
	printf("d : %llu selected.\n\n", *d);

	printf("e, d, n, pi_n : %3llu %3llu %3llu %3llu\n", *e, *d, *n, pi_n);
	printf("e*d mod pi_n : %llu\n\n", ModMul(*e, *d, pi_n));
}

/*
 * @brief     RSA 암복호화를 진행하는 함수.
 * @param     llint data   : 키 값.
 * @param     llint key    : 키 값.
 * @param     llint n      : 모듈러 n 값.
 * @return    llint result : 암복호화에 결과값
 * @todo      과제 안내 문서의 제한사항을 참고하여 작성한다.
 */
llint miniRSA(llint data, llint key, llint n) {
	llint result;
	result = ModPow(data, key, n);

	printf("input data : %llu\n", data);
	printf("output data : %llu\n", result);

	return result;
}

llint GCD(llint a, llint b) {
	llint prev_a = 0, temp;

	if (a < b) {
		temp = a;
		a = b;
		b = temp;
	}

	while (b != 0) {
		printf("GCD(%llu, %llu)\n", a, b);
		//prev_a = a % b;
		while (a >= b) {
			a -= b;
		}
		temp = a;
		a = b;
		b = temp;
	}
	printf("GCD(%llu, %llu)\n\n", a, b);
	return a;
}

int main(int argc, char* argv[]) {
    byte plain_text[4] = {0x12, 0x34, 0x56, 0x78};
    llint plain_data, encrpyted_data, decrpyted_data;
    uint seed = time(NULL);

    memcpy(&plain_data, plain_text, 4);

    // 난수 생성기 시드값 설정
    seed = time(NULL);
    InitWELLRNG512a(&seed);

    // RSA 키 생성
    miniRSAKeygen(&p, &q, &e, &d, &n);
    printf("0. Key generation is Success!\n ");
    printf("p : %llu\n q : %llu\n e : %llu\n d : %llu\n N : %llu\n\n", p, q, e, d, n);

    // RSA 암호화 테스트
    encrpyted_data = miniRSA(plain_data, e, n);
    printf("1. plain text : %llu\n", plain_data);    
    printf("2. encrypted plain text : %llu\n\n", encrpyted_data);

    // RSA 복호화 테스트
    decrpyted_data = miniRSA(encrpyted_data, d, n);
    printf("3. cipher text : %llu\n", encrpyted_data);
    printf("4. Decrypted plain text : %llu\n\n", decrpyted_data);

    // 결과 출력
    printf("RSA Decryption: %s\n", (decrpyted_data == plain_data) ? "SUCCESS!" : "FAILURE!");

    return 0;
}