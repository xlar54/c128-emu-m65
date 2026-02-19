; ============================================================
; c128_sound.asm - C128 SID Sound
; 
; The C128 uses a SID chip (6581/8580) at $D400-$D41F.
; The MEGA65 has a real SID implementation, so we can mostly
; just pass writes through directly in the memory handler.
;
; This module handles initialization and any special cases.
; ============================================================

        .cpu "45gs02"

; SID base address on MEGA65
SID_BASE = $D400

; ============================================================
; C128_SndInit - Initialize SID
; ============================================================
C128_SndInit:
        ; Silence all SID voices
        ldx #$18
        lda #$00
_snd_clr:
        sta SID_BASE,x
        dex
        bpl _snd_clr
        
        ; Set default volume to 0
        lda #$00
        sta SID_BASE+$18        ; Filter/volume register
        rts