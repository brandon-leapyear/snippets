import json
import re
from pathlib import Path

def main():
    golden_files = Path('./server/test/tasty/goldens/').rglob('*.golden')
    for golden in golden_files:
        # load json goldens
        golden_content = golden.read_text()
        o = decode_aeson(golden_content)
        if o is None:
            continue
        print(f">>> {golden}")

        # process
        o = reorder_keys(o)

        # save golden
        golden_content = encode_aeson(o)
        golden.write_text(golden_content)

def reorder_keys(o):
    if isinstance(o, dict):
        keys = sorted(o.keys())
        move_to_front(keys, "response")
        move_to_front(keys, "query")
        move_to_front(keys, "user")
        return {
            k: reorder_keys(o[k])
            for k in keys
        }

    if isinstance(o, list):
        return [reorder_keys(x) for x in o]

    return o

def move_to_front(l, x):
    try:
        l.remove(x)
    except ValueError:
        return
    l.insert(0, x)

### aeson ###

def decode_aeson(s):
    try:
        return json.loads(s)
    except json.decoder.JSONDecodeError:
        return None

def encode_aeson(o):
    s = json.dumps(o, indent=4)

    def _to_scientific_notation(m):
        x = float(m[0])

        # https://www.stackage.org/haddock/lts-18.2/scientific-0.3.7.0/Data-ByteString-Builder-Scientific.html#v:scientificBuilder
        if abs(x) >= 0.1 and abs(x) < 9999999:
            return m[0]

        if x == 0:
            return m[0]

        base, exp = f'{x:e}'.split('e')

        base = re.sub(r'^(\d+\.\d*[1-9])0+$', r'\1', base)
        base = re.sub(r'^(\d+)\.0+$', r'\1.0', base)

        exp_sign, exp_num = re.match(r'^([-+])(\d+)$', exp).groups()
        exp_sign = '' if exp_sign == '+' else exp_sign
        exp_num = exp_num.lstrip('0')
        exp = exp_sign + exp_num

        return f'{base}e{exp}'

    s = re.sub(r'\b-?\d+(\.\d+)?(e[-+]?\d+)?', _to_scientific_notation, s)

    return s

main()
