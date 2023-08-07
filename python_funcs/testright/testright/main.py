import argparse

from testright.implant import implant

parser = argparse.ArgumentParser()
parser.add_argument("file", help="The file to parse")
parser.add_argument("function", help="The function name")
args = parser.parse_args()

with open(args.file, "rb") as f:
    code = f.read()


print(implant(code, args.function))
