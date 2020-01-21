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

try:
    import mido  # pip install mido
except ImportError:
    mido = None

import brseq


QUARTER_NOTE = 96  # Length of a quarter note, in BRSEQ time units (ticks)
MIDDLE_C = 60      # As defined for MIDI (though it's also true for BRSEQ)


def assertMido(to_or_from):
    """
    Throw an exception if Mido is not installed.
    """
    if mido is None:
        raise RuntimeError(f'You requested conversion {to_or_from} MIDI, but the '
            'Mido module is not installed. Please install it with '
            '`[however you run Python] -m pip install mido`.')


def createMidi(tempo, bahp_points):
    """
    Create a mido.MidiFile object from an initial tempo (int) and a list
    of bahp points (ints)
    """
    assertMido('to')

    bahp_points.sort()

    # MIDI setup
    midi = mido.MidiFile(type=0, ticks_per_beat=QUARTER_NOTE)
    track = mido.MidiTrack()
    midi.tracks.append(track)

    # Tempo
    track.append(mido.MetaMessage('set_tempo', tempo=mido.bpm2tempo(tempo), time=0))

    # Bahps (notes)
    current_time = 0
    NOTE_LEN = QUARTER_NOTE // 4
    for t in bahp_points:
        track.append(mido.Message('note_on', note=MIDDLE_C, velocity=127, time=(t - current_time)))
        track.append(mido.Message('note_off', note=MIDDLE_C, velocity=0, time=NOTE_LEN))
        current_time = t + NOTE_LEN

    return midi


def readMidi(filename, only_middle_c):
    """
    Read a MIDI from the provided filename, and return its tempo (int)
    and a list of bahp points (ints).
    If only_middle_c is True, notes other than Middle C will be ignored.
    """
    assertMido('from')

    # Open file
    midi = mido.MidiFile(str(filename))

    # Get tempo, which could be defined on any track
    tempo = None
    for track in midi.tracks:
        for msg in track:
            if msg.type == 'set_tempo':
                tempo = int(round(mido.tempo2bpm(msg.tempo)))

    if tempo is None:
        raise ValueError('MIDI has no tempo')

    # Get bahp points
    # (We read from all tracks because FL Studio puts the notes on the
    # third track, for some reason)
    bahp_points = []
    for track in midi.tracks:
        current_time = 0
        for msg in track:
            current_time += msg.time

            # Ignore note_on events with velocity=0, because that is
            # equivalent to note_off
            # https://www.midi.org/forum/228-writing-midi-software-send-note-off,-or-zero-velocity-note-on
            if msg.type == 'note_on' and msg.velocity > 0:
                if only_middle_c and msg.note != MIDDLE_C:
                    continue

                bahp_points.append(int(round(current_time * QUARTER_NOTE / midi.ticks_per_beat)))

    bahp_points.sort()
    return tempo, bahp_points


def brseq2JsonMidi(args):
    """
    Convert BRSEQ to JSON[+MIDI]
    """
    # Load BRSEQ
    with open(args.input_file, 'rb') as f:
        bahpInfo = brseq.BRSEQ(f.read()).getBahpInfo()

    # Get, or generate, output filename
    outf = args.output_file
    if outf is None: outf = args.input_file.with_suffix('.json')

    # Output a midi instead of the 'tempo' and 'bahp_points' JSON elements
    # (which we pop from the dictionary), if requested
    if args.midi is not None:
        midi = createMidi(bahpInfo.pop('tempo'), bahpInfo.pop('bahp_points'))
        midi.save(str(args.midi))  # .save() does not officially accept pathlib.Path,
                                   # so, better safe than sorry

    # Save JSON
    with open(outf, 'w', encoding='utf-8') as f:
        json.dump(bahpInfo, f, indent=4)


def jsonMidi2Brseq(args):
    """
    Convert JSON[+MIDI] to BRSEQ
    """
    # Load JSON
    with open(args.input_file, 'r', encoding='utf-8') as f:
        bahpInfo = json.load(f)

    # Create the 'tempo' and 'bahp_points' JSON elements from a MIDI
    # file, if provided
    if args.midi is not None:
        bahpInfo['tempo'], bahpInfo['bahp_points'] = readMidi(args.midi, args.only_middle_c)

    # This could be a common mistake
    if 'tempo' not in bahpInfo or 'bahp_points' not in bahpInfo:
        raise ValueError('JSON is missing "tempo" and/or "bahp_points" elements. '
            'Either add them, or specify a MIDI file using the "--midi" argument.')

    # Get, or generate, output filename
    outf = args.output_file
    if outf is None: outf = args.input_file.with_suffix('.brseq')

    # Save BRSEQ
    outData = brseq.BRSEQ.fromBahpInfo(bahpInfo).save()
    with open(outf, 'wb') as f:
        f.write(outData)


def main():
    parser = argparse.ArgumentParser(description='Convert NSMBW bahp BRSEQs to/from JSON, or a combination of JSON+MIDI.')
    parser.add_argument('input_file', type=pathlib.Path,
                        help='input file (JSON or BRSEQ)')
    parser.add_argument('output_file', nargs='?', type=pathlib.Path,
                        help='output file (BRSEQ or JSON)')
    parser.add_argument('--midi', type=pathlib.Path, metavar='midi_file',
                        help='midi file (input if converting JSON+MIDI -> BRSEQ, or output if converting BRSEQ -> JSON+MIDI)')
    parser.add_argument('--only_middle_c', action='store_true',
                        help='ignore notes other than Middle C when importing a MIDI')
    args = parser.parse_args()

    # First, detect file type
    with open(args.input_file, 'rb') as f:
        inputIsBRSEQ = (f.read(4) == b'RSEQ')

    # Then convert appropriately
    if inputIsBRSEQ:
        brseq2JsonMidi(args)
    else:
        jsonMidi2Brseq(args)


if __name__ == '__main__':
    main()
