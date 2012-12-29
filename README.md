libsoundGS
==========

Apple IIGS sound library

Streams a long-playing song from disk, also supports playing of other one-shot sounds while this is happening.

Written in ORCA/M assembler, but presents a C stack-based calling interface (was written with the intent of linking to ORCA/C modules).

Noteworthy as a demonstration of most aspects of controlling the Ensoniq 5503 DOC sound chip, including the Apple IIGS technique of moving Direct Page to $C000 and using the 65816 stack instructions to move data into the sound chip's 1-byte data register as quickly as possible.
