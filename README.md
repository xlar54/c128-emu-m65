**C128 EMU**

This is a simple Commodore 128 emulator for the Mega65 computer

The TAB key on the keyboard acts as the 40/80 key on the monitor.  The machine will auto-start in 80 column mode.  GRAPHIC 0 will switch to the 40 column screen, and
GRAPHIC 5 will switch to the 80 column screen.  (Again you can always toggle views via the TAB key)

Theres A LOT to do.  Still in super early alpha phase, but trying to get more speed out of it is my primary goal before bothering adding functionality.  The 128 has a lot of things going on.  So Im aiming to bypass many functions by hooking into the routine and then running it natively, then returning to the ROMs.  Slow, tedious, and potential for errors.  But natively running some aspects of the ROMs provides the best opportunities for enhancment.  A great deal of RAM is needed to make this work... 2 blocks of 64KB of ram, 2 roms, a kernal, a charrom, a VDC of 16K, color ram, etc... So the emulator uses a lot of memory in mulitple banks.  

ClaudeAI helped build this - no way I could have gotten it this far on my own.  Thanks Anthropic!
