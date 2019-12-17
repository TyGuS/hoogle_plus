#!/usr/bin/python3

import os
import argparse

BASE_CMD = 'gtimeout 60 stack exec -- hplus \"{query}\" --stop-refine\
            --stop-threshold=10 --cnt=5'


def main():
    parser = argparse.ArgumentParser(description='Run a single hoogle+ query')
    parser.add_argument('query', help='the signature to be searched')
    args = parser.parse_args()
    os.system(BASE_CMD.format(query=args.query))


if __name__ == "__main__":
    main()
