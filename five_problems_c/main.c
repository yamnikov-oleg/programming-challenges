#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

double sum_for(size_t len, double nums[]) {
    double sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum += nums[i];
    }
    return sum;
}

double sum_while(size_t len, double nums[]) {
    double sum = 0;
    double *elem = nums;
    while (elem - nums < len) {
        sum += *elem;
        elem++;
    }
    return sum;
}

double sum_rec(size_t len, double nums[]) {
    if (len == 0) return 0;

    return nums[0] + sum_rec(len - 1, nums + 1);
}

void combine_lists(size_t c1, int* l1, size_t c2, int* l2, size_t* retc, int** retl) {
    size_t halfc = c1 < c2 ? c1 : c2;

    int *combl = malloc(2 * halfc * sizeof(int));
    for (size_t i = 0; i < 2 * halfc; i++) {
        if (i % 2 == 0) {
            combl[i] = l1[i/2];
        } else {
            combl[i] = l2[(i-1)/2];
        }
    }

    *retc = 2*halfc;
    *retl = combl;
}

void fib(size_t count, uint64_t *dst) {
    uint64_t fib1 = 0, fib2 = 1, t;
    for (size_t i = 0; i < count; i++) {
        dst[i] = fib1;

        t = fib1 + fib2;
        fib1 = fib2;
        fib2 = t;
    }
}

void _split_digits(int num, size_t *dst_len, unsigned char **dst) {
    int tnum;

    tnum = num;
    *dst_len = 0;
    while (tnum > 0) {
        (*dst_len)++;
        tnum /= 10;
    }

    *dst = malloc(*dst_len * sizeof(unsigned char));
    tnum = num;
    for (ssize_t i = *dst_len - 1; i >= 0; i--) {
        (*dst)[i] = tnum % 10;
        tnum /= 10;
    }
}

int _cmp_num_strings(const void *v1, const void *v2) {
    int *num1 = (int *)v1;
    int *num2 = (int *)v2;

    unsigned char *digs1, *digs2;
    size_t digs1_cnt, digs2_cnt;

    _split_digits(*num1, &digs1_cnt, &digs1);
    _split_digits(*num2, &digs2_cnt, &digs2);

    size_t min_cnt = digs1_cnt < digs2_cnt ? digs1_cnt : digs2_cnt;

    for (size_t i = 0; i < min_cnt; i++) {
        if (digs1[i] < digs2[i]) {
            return -1;
        } else if (digs1[i] > digs2[i]) {
            return 1;
        }
    }

    return ((ssize_t)digs1_cnt) - ((ssize_t)digs2_cnt);
}

int _rev_cmp_num_strings(const void *num1, const void *num2) {
    return -_cmp_num_strings(num1, num2);
}

int _concat_nums(size_t count, int nums[]) {
    int ret = 0;
    int num;
    for (size_t i = 0; i < count; i++) {
        num = nums[i];
        while(num > 0) {
            ret *= 10;
            num /= 10;
        }
        ret += nums[i];
    }
    return ret;
}

void max_min_concat(size_t count, int nums[], int *max, int *min) {
    qsort(nums, count, sizeof(int), _cmp_num_strings);
    *min = _concat_nums(count, nums);

    qsort(nums, count, sizeof(int), _rev_cmp_num_strings);
    *max = _concat_nums(count, nums);
}

int _calc_exp(size_t count, int nums[], uint64_t op_bits) {
    int ret = nums[0];
    nums++;
    count--;

    while (count > 0) {
        if (op_bits % 2 == 0) {
            ret += nums[0];
        } else {
            ret -= nums[0];
        }
        nums++;
        count--;
        op_bits /= 2;
    }

    return ret;
}

void _print_exp(size_t count, int nums[], uint64_t op_bits) {
    printf("%d ", nums[0]);
    nums++;
    count--;

    while (count > 0) {
        if (op_bits % 2 == 0) {
            printf("+ %d ", nums[0]);
        } else {
            printf("- %d ", nums[0]);
        }
        nums++;
        count--;
        op_bits /= 2;
    }
    printf("\n");
}

void print_all_exps(size_t count, int nums[]) {
    uint64_t max_op_bits = 1;
    for (size_t i = 0; i < count - 2; i++) {
        max_op_bits = max_op_bits * 2 + 1;
    }

    int res;
    for (uint64_t op_bits = 0; op_bits <= max_op_bits; op_bits++) {
        res = _calc_exp(count, nums, op_bits);
        if (res == 100) _print_exp(count, nums, op_bits);
    }
}

void main(int argc, char** argv) {
    // PROBLEM 1
    // Sum of list

    #define NUMS_LEN 4
    double nums[NUMS_LEN] = {1, 5, -3, 8};

    printf("sum_for = %f\n", sum_for(NUMS_LEN, nums));
    printf("sum_while = %f\n", sum_while(NUMS_LEN, nums));
    printf("sum_rec = %f\n", sum_rec(NUMS_LEN, nums));

    // PROBLEM 2
    // Lists combination

    #define LIST1_LEN 4
    #define LIST2_LEN 5
    int list1[LIST1_LEN] = {1, 2, 3, 4};
    int list2[LIST2_LEN] = {-1, -2, -3, -4, -5};

    size_t clen;
    int *clist;
    combine_lists(LIST1_LEN, list1, LIST2_LEN, list2, &clen, &clist);

    printf("combine_lists = {");
    for (size_t i = 0; i < clen; i++) {
        printf("%d", clist[i]);
        if (i < clen - 1) printf(", ");
    }
    printf("}\n");

    // PROBLEM 3
    // Fibonacci numbers

    #define FIBS_COUNT 100

    // The numbers get overflown very fast.
    uint64_t *fibs = malloc(FIBS_COUNT * sizeof(uint64_t));
    fib(FIBS_COUNT, fibs);

    printf("fibs = {");
    for (size_t i = 0; i < FIBS_COUNT; i++) {
        printf("%lu", fibs[i]);
        if (i < FIBS_COUNT - 1) printf(", ");
    }
    printf("}\n");

    // PROBLEM 4
    // Minimal and maximal concatenation

    #define P4_COUNT 4
    int p4_nums[P4_COUNT] = {50, 2, 1, 9};
    int max, min;
    max_min_concat(P4_COUNT, p4_nums, &max, &min);
    printf("max_min_concat = %d, %d\n", max, min);

    // PROBLEM 5
    // Expression permutations

    #define P5_COUNT 9
    int nums_[P5_COUNT] = {1, 2, 34, 5, 67, 8, 9, 5, 5};
    printf("print_all_exps = \n");
    print_all_exps(P5_COUNT, nums_);
}
