NSMBW Bahp Editor
=================

A little tool that can convert NSMBW BRSEQ files (ones that define the "bahp"
points in the music tracks) to/from a JSON representation, or JSON & MIDI
together.

Requires Python 3. Also requires Mido (`pip install mido`) if using the
optional MIDI feature.


Usage
-----

    $ python3 main.py -h
    usage: main.py [-h] [--midi midi_file] [--only_middle_c]
                   input_file [output_file]

    Convert NSMBW bahp BRSEQs to/from JSON, or a combination of JSON+MIDI.

    positional arguments:
      input_file        input file (JSON or BRSEQ)
      output_file       output file (BRSEQ or JSON)

    optional arguments:
      -h, --help        show this help message and exit
      --midi midi_file  midi file (input if converting JSON+MIDI -> BRSEQ, or
                        output if converting BRSEQ -> JSON+MIDI)
      --only_middle_c   ignore notes other than Middle C when importing a MIDI
