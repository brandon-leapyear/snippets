#!/usr/bin/env python3
"""Count the number of times each anchor in the given YAML file is used."""

import argparse
import re
from pathlib import Path

def main(file: str):
    contents = Path(file).read_text()
    counter = {}

    for anchor in re.findall(r'(?<=&)[\w-]+', contents):
        if anchor in counter:
            raise Exception(f'Found repeated anchor: {anchor}')

        counter[anchor] = 0

    for ref in re.findall(r'(?<=\*)[\w-]+', contents):
        if ref not in counter:
            print(f'WARNING: Found unknown reference: {ref}')
            continue
            # raise Exception(f'Found unknown reference: {ref}')

        counter[ref] += 1

    for anchor, count in sorted(counter.items(), key=lambda x: x[1], reverse=True):
        print(f'{anchor}: {count}')

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('file', help='The YAML file to check')
    args = parser.parse_args()
    main(args.file)
