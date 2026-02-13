import sys

with open(sys.argv[1], 'r', encoding='utf-8') as f:
    content = f.read()

popen = content.count('(')
pclose = content.count(')')
sopen = content.count('[')
sclose = content.count(']')
copen = content.count('{')
cclose = content.count('}')

print(f"( open: {popen}, close: {pclose}, diff: {popen - pclose}")
print(f"[ open: {sopen}, close: {sclose}, diff: {sopen - sclose}")
print(f"{{ open: {copen}, close: {cclose}, diff: {copen - cclose}")
print(f"Total diff: {abs(popen-pclose) + abs(sopen-sclose) + abs(copen-cclose)}")
