#!/usr/bin/env python3
import argparse
import math
import re
import string
import sys

DEFAULT_COLUMN_SIZE = 0x10
ANSI_COLORS = {
       'HEADER': '\033[95m',
       'OKBLUE': '\033[94m',
       'OKCYAN': '\033[96m',
      'OKGREEN': '\033[92m',
      'WARNING': '\033[93m',
         'FAIL': '\033[91m',
         'ENDC': '\033[0m',
         'BOLD': '\033[1m',
    'UNDERLINE': '\033[4m'
}


highlight = lambda s: '{}{}{}'.format(ANSI_COLORS['WARNING'], s, ANSI_COLORS['OKGREEN'])

def highlight_newlines(text):
    pattern = r'(?<!( \d))(0a)(?!(\d ))'  # highlight any "0a" that is not intra-byte (e.g. avoid "30af")
    return re.sub(pattern, highlight('0a'), text)


def dump_hex(column_size, raw_text, *args, **kwargs):
    global ANSI_COLORS

    hex_width = 2 * column_size + math.ceil((column_size / 2) - 1)
    address = 0
    while (raw := raw_text.read(column_size)):

        readable = ''.join(
            highlight('.') if ch in string.whitespace[1:] else ch
            for ch in raw)

        hex_raw = [hex(ord(ch))[2:].zfill(2) for ch in raw]
        #hex_raw = [highlight(ch) for ch in hex_raw if ch == '0a']
        hex_pairs = zip(hex_raw[::2], hex_raw[1::2] + [''])  # +[''] hack to preserve dangling bytes in odd-numbered columns (e.g. -c 3)
        hex_readable = ' '.join(''.join(pair) for pair in hex_pairs)
        hex_readable_annotated = highlight_newlines(hex_readable)

        annotation_width = len(hex_readable_annotated) - len(hex_readable)
        print('{:0>8x}: {}{:{width}}  {}{}'.format(
            address,
            ANSI_COLORS['BOLD'] + ANSI_COLORS['OKGREEN'],
            hex_readable_annotated,
            readable,
            ANSI_COLORS['ENDC'],
            width=(hex_width + annotation_width)))

        address += column_size


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Make a hex dump or do the reverse.')
    parser.add_argument('infile', nargs='?', help='If no infile is given, standard input is read.  If infile is specified as a `-` character, then input is taken from standard input.')
    parser.add_argument('-c', '--col', type=int, dest='column_size', default=DEFAULT_COLUMN_SIZE, help='Format <cols> octets per line. Default 16 (-i: 12, -ps: 30, -b: 6). Max 256.  No maximum for -ps. With -ps, 0 results in one long line of output.')
    args = parser.parse_args()

    with (open(args.infile) if args.infile else sys.stdin) as args.raw_text:
        dump_hex(**vars(args))
