from itertools import takewhile
from typing import *

from flask import Flask, request, Response

def genfib() -> Iterator[int]:
    """Makes a Fibonacci numbers generator starting with 1,1,2,3,5..."""
    prev_n = 0
    n = 1
    while True:
        yield n
        (prev_n, n) = (n, n + prev_n)

def to_dec(fib_str: str) -> int:
    """Converts fibonacci-base string into integer"""
    if any([d != '0' and d != '1' for d in fib_str]):
        raise ValueError('invalid fibonacci number')

    zipped = zip(fib_str[::-1], genfib())
    components = [fib for (dig, fib) in zipped if dig == '1']
    return sum(components)

def to_fib_min(num: int) -> str:
    """Converts integer into fibonacci-base string with minimum number of 1's"""
    fibs = list(takewhile(lambda f: f <= num, genfib()))
    result = ''
    for f in fibs[::-1]:
        if f > num:
            result += '0'
            continue
        num -= f
        result += '1'
    return result

def to_fib_max(num: int) -> str:
    """Converts integer into fibonacci-base string with maximum number of 1's"""
    fibs = list(takewhile(lambda f: f <= num, genfib()))
    def find_sum(fi: int, sum: int) -> Optional[str]:
        """
        Returns a digits string of fibonacci base starting with number `fibs[fi]`,
        equivalent to `num`; or None if no such string exists.
        """
        if fi >= len(fibs):
            return None

        incl_sum = sum + fibs[fi]

        if incl_sum > num:
            return None

        if incl_sum == num:
            return '1'

        incl = find_sum(fi+1, incl_sum)
        if incl:
            return incl + '1'

        excl = find_sum(fi+1, sum)
        if excl:
            return excl + '0'

        return None

    return find_sum(0, 0)

app = Flask(__name__)

class APIException(Exception):
    status_code = 500

class BadRequest(APIException):
    status_code = 400

@app.errorhandler(APIException)
def bad_request_handler(err: APIException):
    return Response(str(err), err.status_code)

@app.route('/to_fib')
def to_fib_view():
    try:
        num = int(request.args['num'])
    except (KeyError, ValueError):
        raise BadRequest('Please, specify integer parameter `num`')

    mode = request.args.get('mode', 'min')
    if mode not in ['min', 'max']:
        raise BadRequest('`mode` parameter must be either `min` or `max`')

    if mode == 'min':
        return to_fib_min(num)
    else:
        return to_fib_max(num)

@app.route('/to_dec')
def to_dec_view():
    try:
        return str(to_dec(request.args['num']))
    except (KeyError, ValueError):
        raise BadRequest('Please, specify binary parameter `num`')

@app.route('/')
def index():
    return """
    <html>
    <body>
    <h3>Usage examples:</h3>
    <p><a href="/to_dec?num=10101000">/to_dec?fib=10101000</a> → 32</p>
    <p><a href="/to_fib?num=28">/to_fib?num=28</a> → 10010100</p>
    <p><a href="/to_fib?num=28&mode=min">/to_fib?num=28&mode=min</a> → 10010100</p>
    <p><a href="/to_fib?num=28&mode=max">/to_fib?num=28&mode=max</a> → 1101111</p>
    </body>
    </html>
    """

if __name__ == '__main__':
    app.run(host='0.0.0.0')
