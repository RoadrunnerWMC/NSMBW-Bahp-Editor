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

# Note: this is mostly based on ndspy's soundSequence module

import enum
import struct


def noteName(value):
    note = value - 60
    letter = 'ccddeffggaab'[note % 12]
    sharp  = ' ♯ ♯  ♯ ♯ ♯ '[note % 12].strip()
    if note < -12:
        letter = letter.upper()
    accents = ''
    while note >= 0:
        accents += "'"
        note -= 12
    while note < -24:
        accents += '͵'
        note += 12
    return letter + sharp + accents


def _readVariableLengthInt(data, startOffset, limit=4):
    """
    Read a variable-length integer (as SSEQ encodes them) from `data`
    beginning at `startOffset`, limiting the number of read bytes to
    `limit`.
    
    While the code below looks complicated, the method to read such an
    integer is simple:
    - Read a byte. AND it with 0x7F for the part relevant to the
      integer value.
    - If its MSB (that you just trimmed off) is set, left-shift the int
      value by 7, move on to the next byte and repeat.

    So you read 7 bits as a time for as long as the MSB continues to be
    set.
    """
    offset = startOffset
    value = data[offset] & 0x7F; offset += 1
    length = 0
    while data[offset - 1] & 0x80:
        value <<= 7
        value |= data[offset] & 0x7F; offset += 1; length += 1
        if length > limit:
            raise ValueError('Read variable-length int past its end')
    return value


def _lengthOfVariableLengthInt(x):
    """
    Returns the length of a variable-length integer `x`, as encoded in
    SSEQ. See _readVariableLengthInt() for a description of the format.
    This can be implemented more concisely, but I opted for readability.
    """
    if x < 0:
        raise ValueError(f'Cannot write a negative variable-length int: {x}')
    bits = x.bit_length()
    length = 0
    while bits > 0:
        length += 1
        bits -= 7
    return max(1, length)


def _writeVariableLengthInt(x):
    """
    Find the bytes representing the arbitrarily-large (positive) integer
    `x` in the format used by SSEQ variable-length integer fields.
    See _readVariableLengthInt() for a description of this format.
    """
    if x < 0:
        raise ValueError(f'Cannot write a negative variable-length int: {x}')

    ret = []

    while x:
        value = x & 0x7F
        x >>= 7
        ret.insert(0, value)

    for i, v in enumerate(ret[:-1]):
        ret[i] = v | 0x80

    return bytes(ret)



class RSEQEvent:
    """
    An abstract base class representing any sequence event in a BRSEQ file
    """
    dataLength = 1


    def __init__(self, type):
        self.type = type


    def save(self, eventsToOffsets=None):
        """
        Generate data representing this sequence event. This abstract
        base class implementation simply returns a single byte
        containing .type. Subclasses should reimplement this function to
        append their own data to this byte.
        """
        return bytes([self.type])


    @classmethod
    def fromData(cls, type, data, startOffset=0):
        """
        Create an instance of the RSEQEvent subclass this function
        is called on, using a particular type value and reading data
        beginning at some offset. This abstract base class
        implementation simply raises NotImplementedError.
        """
        raise NotImplementedError('RSEQEvent subclasses that can '
            'load themselves from data without context can implement this.')


    @classmethod
    def fromJSON(cls, type, data, startOffset=0):
        raise NotImplementedError('RSEQEvent subclasses that can '
            'load themselves from JSON without context can implement this.')


    def asJSON(self):
        raise NotImplementedError('RSEQEvent subclasses should implement this.')


    def __str__(self):
        return f'<sequence event {hex(self.type)}>'

    def __repr__(self):
        return f'{type(self).__name__}({hex(self.type)})'


def _make_simple_sequence_event_class(typeNum, shortName, name, description):
    """
    Helper function to make a simple SequenceEvent subclass with one
    parameter.
    """
    __doc__ = f'A sequence event {description}. This is sequence event type 0x{typeNum:02X}.'

    def __init__(self, value):
        RSEQEvent.__init__(self, typeNum)
        self.value = value

    def save(self, eventsToOffsets=None):
        return RSEQEvent.save(self) + bytes([self.value])

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        return cls(data[startOffset + 1])

    @classmethod
    def fromJSON(cls, type, value):
        return cls(value)

    def asJSON(self):
        return (self.type, self.value)

    def __str__(self):
        return f'<{shortName.lower()} {self.value}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.value!r})'

    return type(name,
                (RSEQEvent,),
                {'__doc__': __doc__,
                 '__init__': __init__,
                 'save': save,
                 'fromData': fromData,
                 'fromJSON': fromJSON,
                 'asJSON': asJSON,
                 '__str__': __str__,
                 '__repr__': __repr__,
                 'dataLength': 2})


class NoteSequenceEvent(RSEQEvent):
    """
    0x00-0x7F
    """
    def __init__(self, type, velocityAndFlag, duration):
        super().__init__(type)
        self.velocity = velocityAndFlag & 0x7F
        self.unknownFlag = bool(velocityAndFlag & 0x80)
        self.duration = duration

    @property
    def name(self):
        return noteName(self.type)

    @property
    def dataLength(self):
        return 2 + _lengthOfVariableLengthInt(self.duration)

    @property
    def pitch(self):
        return self.type
    @pitch.setter
    def pitch(self, value):
        self.type = value

    def save(self, eventsToOffsets=None):
        if self.type < 0:
            raise ValueError(f'Note pitch must be >= 0 (found:'
                             f' {self.type})')
        if self.type > 127:
            raise ValueError(f'Note pitch must be < 128 (found:'
                             f' {self.type})')
        if self.velocity > 127:
            raise ValueError(f'Note velocity must be < 128 (found:'
                             f' {self.velocity})')
        velocityValue = self.velocity | (0x80 if self.unknownFlag else 0)
        return (super().save()
                + bytes([velocityValue])
                + _writeVariableLengthInt(self.duration))

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        velocity = data[startOffset + 1]
        duration = _readVariableLengthInt(data, startOffset + 2)
        return cls(type, velocity, duration)

    @classmethod
    def fromJSON(cls, type, velocity, unknownFlag, duration):
        return cls(type, velocity, (unknownFlag << 7) | duration)

    def asJSON(self):
        return (self.type, self.velocity, self.unknownFlag, self.duration)

    def __str__(self):
        flag = ' unknown-flag' if self.unknownFlag else ''
        return f'<{self.name} velocity={self.velocity} duration={self.duration}{flag}>'

    def __repr__(self):
        velocityValue = self.velocity | (0x80 if self.unknownFlag else 0)
        return f'{type(self).__name__}({self.type}, {velocityValue!r}, {self.duration!r})'


class RestSequenceEvent(RSEQEvent):
    """
    0x80
    """

    def __init__(self, duration):
        super().__init__(0x80)
        self.duration = duration

    @property
    def dataLength(self):
        return 1 + _lengthOfVariableLengthInt(self.duration)

    def save(self, eventsToOffsets=None):
        return super().save() + _writeVariableLengthInt(self.duration)

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        duration = _readVariableLengthInt(data, startOffset + 1)
        return cls(duration)

    @classmethod
    def fromJSON(cls, type, duration):
        return cls(duration)

    def asJSON(self):
        return (self.type, self.duration)

    def __str__(self):
        return f'<rest {self.duration}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.duration!r})'


class InstrumentSwitchSequenceEvent(RSEQEvent):
    """
    0x81
    """
    def __init__(self, bankID, instrumentID):
        super().__init__(0x81)
        self.bankID = bankID
        self.instrumentID = instrumentID

    @property
    def dataLength(self):
        return 1 + _lengthOfVariableLengthInt(
            self.bankID << 7 | self.instrumentID)

    def save(self, eventsToOffsets=None):
        value = self.instrumentID & 0x7F | self.bankID << 7
        return super().save() + _writeVariableLengthInt(value)

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        value = _readVariableLengthInt(data, startOffset + 1)
        instrumentID = value & 0x7F
        bankID = value >> 7
        return cls(bankID, instrumentID)

    @classmethod
    def fromJSON(cls, type, bankID, instrumentID):
        return cls(bankID, instrumentID)

    def asJSON(self):
        return (self.type, self.bankID, self.instrumentID)

    def __str__(self):
        return f'<instrument {self.bankID}/{self.instrumentID}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.bankID!r}, {self.instrumentID!r})'


class BeginTrackSequenceEvent(RSEQEvent):
    """
    0x93
    """
    dataLength = 5

    def __init__(self, trackNumber, firstEvent):
        super().__init__(0x93)
        self.trackNumber = trackNumber
        self.firstEvent = firstEvent

    def save(self, eventsToOffsets=None):
        return (super().save()
                + self.trackNumber.to_bytes(1, 'big')
                + eventsToOffsets[self.firstEvent].to_bytes(3, 'big'))

    def __str__(self):
        return f'<begin track {self.trackNumber} id={id(self.firstEvent)}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.trackNumber!r}, {self.firstEvent!r} at {id(self.firstEvent)})'


class JumpSequenceEvent(RSEQEvent):
    """
    0x89
    """
    dataLength = 4

    def __init__(self, destination):
        super().__init__(0x89)
        self.destination = destination

    def save(self, eventsToOffsets=None):
        return (super().save()
                + struct.pack('>I', eventsToOffsets[self.destination])[1:])

    def __str__(self):
        return f'<jump id={id(self.destination)}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.destination!r} at {id(self.destination)})'


UnknownB0SequenceEvent = _make_simple_sequence_event_class(0xB0,
    'Unknown', 'UnknownB0SequenceEvent',
    'with an unknown purpose')


PanSequenceEvent = _make_simple_sequence_event_class(0xC0,
    'Pan', 'PanSequenceEvent',
    'that sets the stereo panning value for the current track')


TrackVolumeSequenceEvent = _make_simple_sequence_event_class(0xC1,
    'Track volume', 'TrackVolumeSequenceEvent',
    'that sets the volume of the current track')


PortamentoSequenceEvent = _make_simple_sequence_event_class(0xC4,
    'Portamento', 'PortamentoSequenceEvent', 'related to portamentos')


ModulationDepthSequenceEvent = _make_simple_sequence_event_class(0xCA,
    'Modulation depth', 'ModulationDepthSequenceEvent', 'sets the modulation depth (?)')


class MonoPolySequenceEvent(RSEQEvent):
    """
    0xC7
    """
    dataLength = 2

    class Value(enum.IntEnum):
        POLY = 0
        MONO = 1

    def __init__(self, value):
        super().__init__(0xC7)
        self.value = self.Value(value)

    def save(self, eventsToOffsets=None):
        return super().save() + bytes([self.value])

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        return cls(data[startOffset + 1])

    @classmethod
    def fromJSON(cls, type, value):
        return cls(value)

    def asJSON(self):
        return (self.type, self.value.value)

    def __str__(self):
        return '<mono>' if self.value else '<poly>'

    def __repr__(self):
        return f'{type(self).__name__}({self.value!r})'


class TempoSequenceEvent(RSEQEvent):
    """
    0xE1
    """
    dataLength = 3

    def __init__(self, value):
        super().__init__(0xE1)
        self.value = value

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        return cls(struct.unpack_from('>H', data, startOffset + 1)[0])

    @classmethod
    def fromJSON(cls, type, value):
        return cls(value)

    def asJSON(self):
        return (self.type, self.value)

    def save(self, eventsToOffsets=None):
        return super().save() + struct.pack('>H', self.value)

    def __str__(self):
        return f'<tempo {self.value}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.value!r})'


UnknownDESequenceEvent = _make_simple_sequence_event_class(0xDE,
    'unknown-de', 'UnknownDESequenceEvent', '')


class UnknownF0SequenceEvent(RSEQEvent):
    """
    0xF0
    (this is for bahps)
    """
    dataLength = 5

    def __init__(self, value):
        super().__init__(0xF0)
        self.value = value  # (bytes of length 4)

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        return cls(data[startOffset + 1 : startOffset + 5])

    def save(self, eventsToOffsets=None):
        return super().save() + self.value

    @classmethod
    def fromJSON(cls, type, a, b, c, d):
        return cls(bytes([a, b, c, d]))

    def asJSON(self):
        return (self.type, *self.value)

    def __str__(self):
        return f'<unknown-f0 {self.value!r}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.value!r})'


class DefineTracksSequenceEvent(RSEQEvent):
    """
    0xFE
    """
    dataLength = 3

    def __init__(self, trackNumbers):
        super().__init__(0xFE)
        self.trackNumbers = trackNumbers

    def save(self, eventsToOffsets=None):
        tracksBitfield = 0
        for i in range(16):
            if i in self.trackNumbers:
                tracksBitfield |= 1 << i
        return super().save() + struct.pack('>H', tracksBitfield)

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        tracksBitfield, = struct.unpack_from('>H', data, startOffset + 1)
        trackNumbers = set()
        for i in range(16):
            if tracksBitfield & (1 << i):
                trackNumbers.add(i)
        return cls(trackNumbers)

    @classmethod
    def fromJSON(cls, type, trackNumbers):
        return cls(trackNumbers)

    def asJSON(self):
        return (self.type, self.trackNumbers)

    def __str__(self):
        return f'<define tracks {" ".join(str(x) for x in sorted(self.trackNumbers))}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.trackNumbers!r})'


class EndTrackSequenceEvent(RSEQEvent):
    """
    0xFF
    """
    def __init__(self):
        super().__init__(0xFF)

    @classmethod
    def fromData(cls, type, data, startOffset=0):
        return cls()

    @classmethod
    def fromJSON(cls, type):
        return cls()

    def asJSON(self):
        return (self.type)

    def __str__(self):
        return '<end track>'

    def __repr__(self):
        return f'{type(self).__name__}()'


class RawDataSequenceEvent(RSEQEvent):
    """
    A dummy sequence event that represents raw binary data that seems to
    be unreachable as far as ndspy can tell.
    """
    @property
    def dataLength(self):
        return len(self.data)

    def __init__(self, data):
        super().__init__(None)
        self.data = data

    def save(self, eventsToOffsets=None):
        return self.data

    def __str__(self):
        return f'<raw data {bytes(self.data)}>'

    def __repr__(self):
        return f'{type(self).__name__}({self.data!r})'


_EVENT_TYPES = {
    0x80: RestSequenceEvent,
    0x81: InstrumentSwitchSequenceEvent,
    0x88: BeginTrackSequenceEvent,
    0xB0: UnknownB0SequenceEvent,
    0xC0: PanSequenceEvent,
    0xC1: TrackVolumeSequenceEvent,
    0xC4: PortamentoSequenceEvent,
    0xC7: MonoPolySequenceEvent,
    0xCA: ModulationDepthSequenceEvent,
    0xDE: UnknownDESequenceEvent,
    0xE1: TempoSequenceEvent,
    0xF0: UnknownF0SequenceEvent,
    0xFE: DefineTracksSequenceEvent,
    0xFF: EndTrackSequenceEvent,
}


def readSequenceEvents(data, notableOffsets=None):
    """
    Convert raw sequence event data (as seen in SSEQ and SSAR files) to
    a list of SequenceEvent objects. This is the inverse of
    saveSequenceEvents().

    A second list will also be returned that contains the elements from
    the first list that appeared in the input data at the offsets given
    in notableOffsets.
    """
    if notableOffsets is None: notableOffsets = []

    events = {}

    FATE_INPROGRESS = 0
    FATE_RETURN = 1
    FATE_LOOP = 2
    FATE_EOT = 3
    fates = {}

    def parse_at(off):
        offsetsOfMySequentialEvents = []

        while off < len(data):
            if off in fates:
                fate = fates[off]
                if fate == FATE_INPROGRESS:
                    fate = FATE_LOOP
                for off_ in offsetsOfMySequentialEvents:
                    fates[off_] = fate
                return fate

            try:
                type = data[off]

                if type == 0x88: # BeginTrack
                    trackNumber = data[off + 1]
                    firstEventOff = int.from_bytes(data[off + 2 : off + 5], 'big') # 3-byte int

                    event = BeginTrackSequenceEvent(trackNumber, None)
                    events[off] = event
                    fates[off] = FATE_INPROGRESS
                    parse_at(firstEventOff)
                    event.firstEvent = events[firstEventOff]

                elif type == 0x89: # Jump
                    destination = int.from_bytes(data[off + 1 : off + 4], 'big') # 3-byte int

                    event = JumpSequenceEvent(None)
                    events[off] = event
                    fates[off] = FATE_INPROGRESS
                    fate = parse_at(destination)
                    event.destination = events[destination]

                    for off_ in offsetsOfMySequentialEvents:
                        fates[off_] = fate

                    # Should we keep parsing past here? Only if this
                    # is part of an if statement (and thus might be
                    # skipped).
                    x = off - 1
                    while x not in events and x >= 0:
                        x -= 1
                    if x == -1:
                        partOfIfStatement = False
                    else:
                        partOfIfStatement = (events[x].type == 0xA2)

                    if not partOfIfStatement:
                        return fate

                # elif type == 0x95: # Call
                #     destination, = struct.unpack_from('>I', data, off)
                #     destination >>= 8

                #     event = CallSequenceEvent(None)
                #     events[off] = event
                #     fates[off] = FATE_INPROGRESS
                #     fate = parse_at(destination)
                #     event.destination = events[destination]

                #     if fate == FATE_EOT:
                #         fates[off] = fate
                #         for off_ in offsetsOfMySequentialEvents:
                #             fates[off_] = fate
                #         return fate
                #     elif fate == FATE_RETURN:
                #         pass
                #     elif fate == FATE_LOOP:
                #         fates[off] = fate
                #         for off_ in offsetsOfMySequentialEvents:
                #             fates[off_] = fate
                #         return fate

                elif type == 0xFD: # Return
                    events[off] = ReturnSequenceEvent()
                    fates[off] = FATE_RETURN
                    for off_ in offsetsOfMySequentialEvents:
                        fates[off_] = FATE_RETURN
                    return FATE_RETURN

                elif type == 0xFF: # EoT
                    events[off] = EndTrackSequenceEvent()
                    fates[off] = FATE_EOT
                    for off_ in offsetsOfMySequentialEvents:
                        fates[off_] = FATE_EOT
                    return FATE_EOT

                else:

                    if type <= 0x7F:
                        eventCls = NoteSequenceEvent
                    elif type not in _EVENT_TYPES:
                        raise ValueError(f'Event {hex(type)} unrecognized, at {hex(off)}.')
                    else:
                        eventCls = _EVENT_TYPES[type]

                    event = eventCls.fromData(type, data, off)
                    events[off] = event
                    fates[off] = FATE_INPROGRESS

                offsetsOfMySequentialEvents.append(off)
                off += event.dataLength

            except (struct.error, IndexError):
                raise EOFError('Reached EoF of sequence.')
        raise EOFError('Reached EoF of sequence.')

    starts = notableOffsets
    if not starts: starts = [0]
    for start in starts:
        ultimateFate = parse_at(start)
        assert ultimateFate in (FATE_EOT, FATE_LOOP)

    eventsList = []
    i = 0
    while i < len(data):
        if i in events:
            eventsList.append(events[i])
            i += events[i].dataLength
        else:
            j = i
            while j not in events and j < len(data):
                j += 1
            eventsList.append(RawDataSequenceEvent(data[i:j]))
            i = j

    # pop padding data "event"
    if eventsList and isinstance(eventsList[-1], RawDataSequenceEvent) and not any(eventsList[-1].data):
        eventsList.pop()

    notableEvents = [events[off] for off in notableOffsets]

    return eventsList, notableEvents


def saveSequenceEvents(events, notableEvents=None):
    """
    Convert a list of SequenceEvent objects to raw sequence event data.
    This is the inverse of readSequenceEvents().

    A second list will also be returned that contains the offsets in the
    output data of the elements from notableEvents.
    """
    if notableEvents is None: notableEvents = []
    
    events2Offsets = {}

    off = 0
    for e in events:
        events2Offsets[e] = off
        off += e.dataLength
    data = bytearray(off)

    for event, offset in events2Offsets.items():
        eData = event.save(events2Offsets)
        data[offset : offset + len(eData)] = eData

    notableOffsets = [events2Offsets[e] for e in notableEvents]

    return data, notableOffsets


class UnsupportedBRSEQError(Exception):
    pass


class BRSEQ:
    def __init__(self, data=None):
        self.events = [] # event, ...
        self.labels = [] # (name, eventRef), ...

        if data:
            assert data.startswith(b'RSEQ\xFE\xFF\1\0')
            assert data[0x0C:0x10] == b'\0\x20\0\2'
            dataOffs, dataLen, lablOffs, lablLen = struct.unpack_from('>4I', data, 0x10)
            dataSec = data[dataOffs : dataOffs + dataLen]
            lablSec = data[lablOffs : lablOffs + lablLen]

            # Read LABL
            assert lablSec.startswith(b'LABL')
            numLabels, = struct.unpack_from('>I', lablSec, 8)

            labelNamesAndOffsets = []
            labeledOffsets = set()
            for offs in struct.unpack_from(f'>{numLabels}I', lablSec, 12):
                labelDataOffs, labelStrLen = struct.unpack_from(f'>2I', lablSec, 8 + offs)
                labelStr = lablSec[8 + offs + 8 : 8 + offs + 8 + labelStrLen].decode('latin-1')
                labelNamesAndOffsets.append((labelStr, labelDataOffs))
                labeledOffsets.add(labelDataOffs)

            for name, _ in labelNamesAndOffsets:
                if name == 'SMF_staffcredit_v_Begin':
                    raise UnsupportedBRSEQError('The credits theme is unsupported.')
                elif name.startswith('SMF_') and name.endswith('_v_Begin'):
                    break
            else:
                raise UnsupportedBRSEQError('This is not a valid bahp BRSEQ.')

            # Read DATA and populate self.events and offs2Event
            assert dataSec.startswith(b'DATA')
            labeledOffsetsList = list(labeledOffsets)
            self.events, notableEvents = readSequenceEvents(dataSec[0xC:], labeledOffsetsList)
            offs2Event = {labeledOffsetsList[i]: notableEvents[i] for i in range(len(labeledOffsets))}

            # Populate self.labels
            self.labels = [[name, offs2Event[offs]] for name, offs in labelNamesAndOffsets]


    def save(self):
        """
        """

        # Save events
        startEvents = [b for a, b in self.labels]
        lbl2StartEvent = {a: b for a, b in self.labels}
        eventsData, startOffs = saveSequenceEvents(self.events, startEvents)

        # Create DATA section
        dataSec = bytearray(struct.pack('>4s2I', b'DATA', 0, 0xC))

        dataSec.extend(eventsData)

        while len(dataSec) % 0x20:
            dataSec.append(0)
        struct.pack_into('>I', dataSec, 4, len(dataSec))

        # Create LABL section
        lablSec = bytearray(struct.pack('>4s2I', b'LABL', 0, len(self.labels)))
        lablSec.extend(b'\0' * (4 * len(self.labels)))
        for i, (lblName, firstEvent) in enumerate(self.labels):
            if lblName in lbl2StartEvent:
                off = startOffs[startEvents.index(lbl2StartEvent[lblName])]
            else:
                off = -1

            struct.pack_into('>I', lablSec, 0xC + 4 * i, len(lablSec) - 8)

            lablSec.extend(struct.pack('>II', off, len(lblName)))
            lablSec.extend(lblName.encode('latin-1'))
            lablSec.append(0)  # this is dumb, but Nintendo does it, so
            while len(lablSec) % 4:
                lablSec.append(0)

        while len(lablSec) % 0x20:
            lablSec.append(0)
        struct.pack_into('>I', lablSec, 4, len(lablSec))

        # Put everything together
        data = bytearray()
        data.extend(struct.pack('>4sHHIHH', b'RSEQ', 0xFEFF, 0x100, 0x20 + len(dataSec) + len(lablSec), 32, 2))
        data.extend(struct.pack('>IIII', 0x20, len(dataSec), 0x20 + len(dataSec), len(lablSec)))
        data.extend(dataSec)
        data.extend(lablSec)

        return bytes(data)


    def getBahpInfo(self):

        beginLabel = self.labels[0][0]
        assert beginLabel.startswith('SMF_')
        assert beginLabel.endswith('_v_Begin')
        internalName = beginLabel[4:-8]

        if isinstance(self.events[0], DefineTracksSequenceEvent):
            # TEMP
            raise UnsupportedBRSEQError

        for e in self.events:
            if isinstance(e, JumpSequenceEvent):
                loopStart = e.destination
                break
        else:
            loopStart = None

        header = []
        for e in self.events:
            if isinstance(e, (NoteSequenceEvent, RestSequenceEvent)):
                break
            else:
                header.append(e.asJSON())

        time = 0
        tempo = 0
        loopStartIdx = -1
        loopStartTime = -1
        loopEndTime = -1
        trackVolumeSetAfterDelay = 0
        bahpPointTimes = []
        for i, e in enumerate(self.events):
            if e is loopStart:
                if i < len(header):
                    loopStartIdx = i
                else:
                    loopStartTime = time

            if isinstance(e, TempoSequenceEvent):
                tempo = e.value
            elif isinstance(e, RestSequenceEvent):
                time += e.duration
            elif isinstance(e, NoteSequenceEvent):
                bahpPointTimes.append((time, e.pitch, e.velocity, e.duration))
            elif isinstance(e, UnknownF0SequenceEvent):
                bahpPointTimes.append((time, int.from_bytes(e.value[1:], 'big'), 127, 1))
            elif isinstance(e, TrackVolumeSequenceEvent) and time > 0:
                trackVolumeSetAfterDelay = time
            elif isinstance(e, JumpSequenceEvent):
                loopEndTime = time
                break

        d = {
            'version': 1,
            'header': header,
            'internal_name': internalName,
            'tempo': tempo,
            'loop_end': loopEndTime,
            'bahp_points': bahpPointTimes,
        }
        if loopStartIdx != -1:
            d['loop_start_index'] = loopStartIdx
        else:
            d['loop_start'] = loopStartTime
        if trackVolumeSetAfterDelay:
            d['set_track_volume_after_delay'] = trackVolumeSetAfterDelay
        return d


    @classmethod
    def fromBahpInfo(cls, info):
        if info['version'] != 1:
            raise NotImplementedError

        self = cls()

        self.events = [RestSequenceEvent(info['loop_end'])]

        timeToEvent = {0: self.events[-1]}
        def insertAtTime(t, event=None):

            if t in timeToEvent:
                followingRest = timeToEvent[t]
                insertAt = self.events.index(followingRest)
                while not isinstance(self.events[insertAt - 1], RestSequenceEvent):
                    insertAt -= 1
                if event is not None:
                    self.events.insert(insertAt, event)
                return self.events[insertAt]

            precedingRestT = max(x for x in timeToEvent if x <= t)
            precedingRest = timeToEvent[precedingRestT]
            precedingRestI = self.events.index(precedingRest)

            # Slice into two rests
            origDuration = precedingRest.duration
            precedingRest.duration = t - precedingRestT
            newRest = RestSequenceEvent(origDuration - precedingRest.duration)
            self.events.insert(precedingRestI + 1, newRest)
            timeToEvent[t] = newRest

            # Insert new event
            if event is not None:
                self.events.insert(precedingRestI + 1, event)

            return newRest


        for t, p, v, d in info['bahp_points']:
            if p < 20:
                insertAtTime(t, UnknownF0SequenceEvent(b'\x80' + p.to_bytes(3, 'big')))
            else:
                insertAtTime(t, NoteSequenceEvent(p, v, d))

        if 'set_track_volume_after_delay' in info:
            insertAtTime(info['set_track_volume_after_delay'], TrackVolumeSequenceEvent(0))

        def addHeader():
            for ed in reversed(info['header']):
                if ed[0] < 0x80:
                    noteCls = NoteSequenceEvent
                else:
                    noteCls = _EVENT_TYPES[ed[0]]
                self.events.insert(0, noteCls.fromJSON(*ed))

        if 'loop_start_index' in info:
            addHeader()
            loopStartEvent = self.events[info['loop_start_index']]
        else:
            loopStartEvent = insertAtTime(info['loop_start'])
            addHeader()

        self.events.append(JumpSequenceEvent(loopStartEvent))
        self.events.append(EndTrackSequenceEvent())
        self.events.append(EndTrackSequenceEvent())

        prefix = 'SMF_' + info['internal_name'] + '_v_'
        self.labels.append((prefix + 'Begin', self.events[0]))    # unknown B0
        self.labels.append((prefix + 'End', self.events[-1]))     # track end
        self.labels.append((prefix + 'Start', self.events[0]))    # unknown B0
        self.labels.append((prefix + 'Track_0', self.events[1]))  # mono-poly
        self.labels.append((prefix + 'Track_0_LoopStart', loopStartEvent))

        return self
