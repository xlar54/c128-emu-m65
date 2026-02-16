; ============================================================
; main.asm - C128 Emulator for MEGA65
; Assembler: 64tass
;
; Memory Layout:
;   Bank 1 ($10000-$1FFFF): C128 ROMs (56KB)
;     $10000-$11FFF: Character ROM (C128 chargen, 8KB)
;     $14000-$17FFF: BASIC LO ROM  (C128 $4000-$7FFF)
;     $18000-$1BFFF: BASIC HI ROM  (C128 $8000-$BFFF)
;     $1C000-$1FFFF: KERNAL ROM    (C128 $C000-$FFFF)
;   Bank 4 ($40000-$4FFFF): C128 RAM Bank 0 (64KB)
;   Bank 5 ($50000-$5FFFF): C128 RAM Bank 1 (64KB)
;
; ROM files on SD card:
;   kernal.bin   = 318020-05  KERNAL     (16KB -> $1C000)
;   basiclo.bin  = 318018-04  BASIC LO   (16KB -> $14000)
;   basichi.bin  = 318019-04  BASIC HI   (16KB -> $18000)
;   chargen.bin  = 390059-01  CHAR ROM   ( 8KB -> $10000)
; ============================================================

        .cpu "45gs02"
        
        * = $2001

; BASIC stub to auto-start
        .word (+), 2025
        .byte $fe, $02, $30     ; BANK 0
        .byte ':'
        .byte $9e               ; SYS
        .text "8210"            ; Start address
        .byte 0
+       .word 0

        * = $2012

; MEGA65 / C65 KERNAL calls
SETNAM  = $FFBD
SETLFS  = $FFBA
SETBNK  = $FF6B
LOAD    = $FFD5
CHROUT  = $FFD2
GETIN   = $FFE4
OPEN    = $FFC0
CLOSE   = $FFC3
CHKIN   = $FFC6
CHKOUT  = $FFC9
CHRIN   = $FFCF
CLRCHN  = $FFCC
READST  = $FFB7

; MEGA65 DMA controller
DMA_REG = $D707

; ============================================================
; Entry point
; ============================================================
start:
        ; Enable MEGA65 mode and fast CPU
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F              ; Unlock MEGA65 registers

        ; Print banner
        ldx #0
_banner:
        lda banner_msg,x
        beq _banner_done
        jsr CHROUT
        inx
        bne _banner
_banner_done:

        ; ============================================================
        ; Clear C128 RAM Bank 0 (MEGA65 bank 4) via DMA fill
        ; ============================================================
        lda #$00
        sta $D707
        .byte $80, $00          ; enhanced dma - src bits 20-27 = 0
        .byte $81, $00          ; enhanced dma - dest bits 20-27 = 0
        .byte $00               ; end of job options
        .byte $03               ; fill                                 
        .word $FFFF             ; count (64K-1, wraps to fill all)
        .word $0000             ; fill with $00
        .byte $00               ; unused with fill
        .word $0000             ; destination start
        .byte $04               ; dest bank 4
        .byte $00               ; command high byte
        .word $0000             ; modulo (ignored)

        ; ============================================================
        ; Clear C128 RAM Bank 1 (MEGA65 bank 5) via DMA fill
        ; ============================================================
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03               ; fill
        .word $FFFF
        .word $0000
        .byte $00
        .word $0000
        .byte $05               ; dest bank 5
        .byte $00
        .word $0000

        ; Set KERNAL load bank
        lda #$00
        ldx #$00
        jsr SETBNK

        ; ============================================================
        ; Load KERNAL ROM (16KB -> staging $8000, DMA to $1C000)
        ; ============================================================
        ldx #<kernal_name
        ldy #>kernal_name
        lda #kernal_name_end - kernal_name
        jsr SETNAM
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS
        lda #$40               ; Load to specified address
        ldx #<$8000
        ldy #>$8000
        jsr LOAD
        bcc _kernal_ok
        jmp load_fail

_kernal_ok:
        ldx #0
_k_msg: lda kernal_ok_msg,x
        beq _k_done
        jsr CHROUT
        inx
        bne _k_msg
_k_done:

        ; DMA: $08000 -> $1C000 (KERNAL in bank 1, $C000 offset)
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00               ; copy
        .byte $00, $40          ; length = $4000 (16KB)
        .byte $00, $80, $00     ; src: $08000
        .byte $00, $C0, $01     ; dest: $1C000
        .byte $00
        .word $0000

        ; ============================================================
        ; Load BASIC LO ROM (16KB -> staging $8000, DMA to $14000)
        ; ============================================================
        ldx #<basiclo_name
        ldy #>basiclo_name
        lda #basiclo_name_end - basiclo_name
        jsr SETNAM
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS
        lda #$40
        ldx #<$8000
        ldy #>$8000
        jsr LOAD
        bcc _basiclo_ok
        jmp load_fail

_basiclo_ok:
        ldx #0
_blo_msg: lda basiclo_ok_msg,x
        beq _blo_done
        jsr CHROUT
        inx
        bne _blo_msg
_blo_done:

        ; DMA: $08000 -> $14000 (BASIC LO in bank 1, $4000 offset)
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00               ; copy
        .byte $00, $40          ; length = $4000
        .byte $00, $80, $00     ; src: $08000
        .byte $00, $40, $01     ; dest: $14000
        .byte $00
        .word $0000

        ; ============================================================
        ; Load BASIC HI ROM (16KB -> staging $8000, DMA to $18000)
        ; ============================================================
        ldx #<basichi_name
        ldy #>basichi_name
        lda #basichi_name_end - basichi_name
        jsr SETNAM
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS
        lda #$40
        ldx #<$8000
        ldy #>$8000
        jsr LOAD
        bcc _basichi_ok
        jmp load_fail

_basichi_ok:
        ldx #0
_bhi_msg: lda basichi_ok_msg,x
        beq _bhi_done
        jsr CHROUT
        inx
        bne _bhi_msg
_bhi_done:

        ; DMA: $08000 -> $18000 (BASIC HI in bank 1, $8000 offset)
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00               ; copy
        .byte $00, $40          ; length = $4000
        .byte $00, $80, $00     ; src: $08000
        .byte $00, $80, $01     ; dest: $18000
        .byte $00
        .word $0000

        ; ============================================================
        ; Load Character ROM (8KB -> staging $8000, DMA to $10000)
        ; ============================================================
        ldx #<chargen_name
        ldy #>chargen_name
        lda #chargen_name_end - chargen_name
        jsr SETNAM
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS
        lda #$40
        ldx #<$8000
        ldy #>$8000
        jsr LOAD
        bcc _chargen_ok
        jmp load_fail

_chargen_ok:
        ldx #0
_cg_msg: lda chargen_ok_msg,x
        beq _cg_done
        jsr CHROUT
        inx
        bne _cg_msg
_cg_done:

        ; DMA: $08000 -> $10000 (chargen in bank 1, $0000 offset)
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00               ; copy
        .byte $00, $20          ; length = $2000 (8KB)
        .byte $00, $80, $00     ; src: $08000
        .byte $00, $00, $01     ; dest: $10000
        .byte $00
        .word $0000

        ; ============================================================
        ; Initialize memory system
        ; ============================================================
        ldx #0
_init_msg:
        lda init_msg,x
        beq _init_done
        jsr CHROUT
        inx
        bne _init_msg
_init_done:

        jsr C128_MemInit

        ldx #0
_ready_msg:
        lda ready_msg,x
        beq _ready_done
        jsr CHROUT
        inx
        bne _ready_msg
_ready_done:

        ; Wait for keypress
        ldx #0
_press: lda press_msg,x
        beq _wait
        jsr CHROUT
        inx
        bne _press
_wait:
        jsr GETIN
        beq _wait

        ; ============================================================
        ; Initialize video and start emulation
        ; ============================================================

        jsr C128_VideoInit
        
        ; Set border to black = boot starting
        lda #$00
        sta $D020
        
        jsr C128_CPUReset

main_loop:
        ; --- Check if print requested ---
        lda p4h_print_pending
        beq _no_print_start
        jsr P4Host_StartPrint

_no_print_start:
        jsr C128_CPUStepMultiple  ; Execute batch of 8502 instructions

        ; --- If printing, check if done ---
        lda p4h_print_active
        beq _no_print_check
        jsr P4Host_CheckPrintDone

_no_print_check:
        jmp main_loop

; ============================================================
; Load failure handler
; ============================================================
load_fail:
        jsr print_hex8

        lda #$00
        sta $D702

        ldx #0
_lf:    lda fail_msg,x
        beq _lf_hang
        jsr CHROUT
        inx
        bne _lf
_lf_hang:
        jmp _lf_hang

; ============================================================
; Messages
; ============================================================
banner_msg:
        .byte $93, 27, 52
        .text "c128 emulator for mega65"
        .byte $0D, 0

kernal_name:
        .text "kernal.bin"
kernal_name_end:

basiclo_name:
        .text "basiclo.bin"
basiclo_name_end:

basichi_name:
        .text "basichi.bin"
basichi_name_end:

kernal_ok_msg:
        .text "kernal rom loaded"
        .byte $0D, 0

basiclo_ok_msg:
        .text "basic lo rom loaded"
        .byte $0D, 0

basichi_ok_msg:
        .text "basic hi rom loaded"
        .byte $0D, 0

chargen_name:
        .text "chargen.bin"
chargen_name_end:

chargen_ok_msg:
        .text "chargen rom loaded"
        .byte $0D, 0

init_msg:
        .text "initializing c128 memory..."
        .byte $0D, 0

ready_msg:
        .text "ready."
        .byte $0D, 0

press_msg:
        .text "press any key to start"
        .byte $0D, 0

fail_msg:
        .text "?load error"
        .byte $0D, 0

; ============================================================
; Include emulator components
; ============================================================
        .include "c128_cpu_8502.asm"
        .include "c128_cpu_z80.asm"
        .include "c128_hooks.asm"
        .include "c128_mem.asm"
        .include "c128_sound.asm"
        .include "c128_host.asm"
        .include "c128_monitor.asm"