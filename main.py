# Copyright 2020 RoadrunnerWMC
#
# This file is part of NSMBW Bahp Editor.
#
# NSMBW Bahp Editor is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# NSMBW Bahp Editor is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with NSMBW Bahp Editor.  If not, see
# <https://www.gnu.org/licenses/>.

import argparse
import json
import pathlib

import brseq


def brseq2Json(args):
    """
    Convert BRSEQ to JSON
    """
    with open(args.input_file, 'rb') as f:
        bahpInfo = brseq.BRSEQ(f.read()).getBahpInfo()

    outf = args.output_file
    if outf is None: outf = args.input_file.with_suffix('.json')

    with open(outf, 'w', encoding='utf-8') as f:
        json.dump(bahpInfo, f, indent=4)


def json2Brseq(args):
    """
    Convert JSON to BRSEQ
    """
    with open(args.input_file, 'r', encoding='utf-8') as f:
        bahpInfo = json.load(f)

    outData = brseq.BRSEQ.fromBahpInfo(bahpInfo).save()

    outf = args.output_file
    if outf is None: outf = args.input_file.with_suffix('.brseq')

    with open(outf, 'wb') as f:
        f.write(outData)


def main():
    parser = argparse.ArgumentParser(description='Convert NSMBW bahp BRSEQs to/from JSON.')
    parser.add_argument('input_file', type=pathlib.Path,
                        help='input file (JSON or BRSEQ)')
    parser.add_argument('output_file', nargs='?', type=pathlib.Path,
                        help='output file (BRSEQ or JSON)')
    args = parser.parse_args()

    # First, detect file type
    with open(args.input_file, 'rb') as f:
        inputIsBRSEQ = (f.read(4) == b'RSEQ')

    # Then convert appropriately
    if inputIsBRSEQ:
        brseq2Json(args)
    else:
        json2Brseq(args)


if __name__ == '__main__':
    main()
