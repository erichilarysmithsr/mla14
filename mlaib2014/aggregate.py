"""
Usage:
python aggregate_ris.py data1.ris data2.ris ... > all_data.csv

Convert multiple RIS files to a single CSV on standard output. The
columns of the CSV are labeled by the RIS 2-character field codes. When
an RIS record has multiple occurrences of the same field, these are
concatenated together with ;; as the separator; check that ';;' does not
occur in your data. The script traverses all the input files twice, once
to determine the set of field codes and once to output the CSV.
"""

import re
import csv

MULTI_DELIM = ";;"
RIS_PAT = re.compile('(\w\w)  -(.*)$')

class RISException(Exception):
    pass

def ris_line(line):
    text = line.strip()
    if text == "":
        return None,None
    m = RIS_PAT.match(text)
    if not m:
        raise RISException("Problem parsing line: " + text)
    # strip leading whitespace on the field value
    return m.group(1),m.group(2).strip()


def ris_fields(f):
    fields = set()
    for line in f:
        key,val = ris_line(line)
        if key is not None and key != 'ER':
            fields.add(key)
    return fields

def ris2csv(f):
    entries = []
    data = dict()
    
    for line in f:
        key,val = ris_line(line)
        if key is None:
            continue
        if key == 'ER':
            # done reading previous entry; start new one
            entries.append(data)
            data = dict()
            continue
        if key == 'TY' and len(data) > 0:
            raise RISException("New entry does not start with TY: " + line)
            continue
        # RIS allows repeating the same data field multiple times
        if key in data:
            data[key] += MULTI_DELIM + val
        else:
            data[key] = val

    return entries


def main(files,output):
    fields = set()
    for filename in files:
        with open(filename) as f:
            fields = fields.union(ris_fields(f))

    headers = sorted(fields)
    w = csv.DictWriter(output,headers,quoting=csv.QUOTE_ALL)
    w.writeheader()
            
    for filename in files:
        with open(filename) as f:
            w.writerows(ris2csv(f))

if __name__=='__main__':
    import sys
    main(sys.argv[1:],sys.stdout)
