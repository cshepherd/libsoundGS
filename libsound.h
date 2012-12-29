/*
 * libsound.h
 *
 * function prototypes and definitions
 */

/*
 * int soundInit( int MMID );
 * pass Memory Manager ID to soundInit (it will request $4000 bytes with this
 * ID)
 */
extern int soundInit( int MMID );

/*
 * int soundPlayStart( char *path, int rate, int oversample, int echodelay );
 * *path is pathname of sound file to begin playing
 * NOTE: *path isn't a c-string, it's a GSOS string (2 byte length+data)
 * oversample = 0 for no oversample, 1 for 2X, 2 for 4X
 * echodelay is the delay (in VBL intervals) between L and R channels for
 * a faux stereo effect
 */
int soundPlayStart( char *path, int rate, int oversample, int echodelay );

void BGSoundStart( void );

/*
 * int soundMaint( void );
 * call this function often to keep the music playing
 */
int soundMaint( void );

/*
 * int soundStop( void );
 * call this to stop the sound from playing
 */
int soundStop( void );

/*
 * int soundPlayShort( int soundid, int rate );
 * swap a short (<32KB) sound into available DOC RAM and
 * play it on an available generator
 * rate is DOC playback rate
 * soundid is sound ID returned from loadOneSound(), see next function
 */
int soundPlayShort( int soundid, int rate );

/*
 * int loadOneSound( char *path );
 * load a sound from disk via GS/OS, allocate RAM for it from MM,
 * and return a sound ID to use for future soundPlayShort() calls
 * *path is a GS/OS path (16-bit length + data)
 */
int loadOneSound( char *path );
