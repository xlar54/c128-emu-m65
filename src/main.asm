; ============================================================
; main.asm - C128 Emulator for MEGA65
; Assembler: 64tass
;
; Memory Layout:
;   Bank 0:
;     $08000-$09FFF: Character ROM (C128 chargen, 8KB, stays at staging area)
;   Bank 1 ($10000-$1FFFF): C128 ROMs
;     $10000-$11FFF: Character ROM (chargen, 8KB)
;     $14000-$17FFF: BASIC LO ROM  (C128 $4000-$7FFF)
;     $18000-$1BFFF: BASIC HI ROM  (C128 $8000-$BFFF)
;     $1C000-$1FFFF: KERNAL ROM    (C128 $C000-$FFFF)
;   Bank 3 ($32000-$35FFF): VDC RAM (16KB, overwrites C65 BASIC, ROM write-protect off)
;   Bank 4 ($40000-$4FFFF): C128 RAM Bank 0 (64KB)
;   Bank 5 ($50000-$5FFFF): C128 RAM Bank 1 (64KB)
;
;   COLPTR at default $0FF80000 (color RAM accessed via 32-bit pointers)
;
; ROM files on SD card:
;   kernal.bin   = 318020-05  KERNAL     (16KB -> $1C000)
;   basiclo.bin  = 318018-04  BASIC LO   (16KB -> $14000)
;   basichi.bin  = 318019-04  BASIC HI   (16KB -> $18000)
;   chargen.bin  = 390059-01  CHAR ROM   ( 8KB -> $08000, stays there)
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

        ; ============================================================
        ; Load roms.bin (64KB) byte-by-byte into bank 1 ($10000-$1FFFF)
        ; File layout: chargen@$0000, basiclo@$4000, basichi@$8000, kernal@$C000
        ; Also copies chargen ($10000-$11FFF) to $08000 for VIC-IV
        ; ============================================================

        ; kernal
        ; Load to bank 0 first (bank 1 has color RAM window at $1F800-$1FFFF)
        ; Then DMA copy to bank 1, and relocate $F800-$FFFF to $12000

        lda #$00
        ldx #$00
        jsr SETBNK
        lda #$00
        ldx #$08
        ldy #$00
        jsr SETLFS

        lda #kernal_name_end - kernal_name
        ldx #<kernal_name
        ldy #>kernal_name
        jsr SETNAM
        lda #$40
        ldx #$00
        ldy #$C0
        jsr LOAD

        ; DMA copy $C000-$FFFF from bank 0 to bank 1 (16KB)
        ; ($1F800-$1FFFF will be overridden by color RAM window on reads,
        ;  but the first 14KB at $1C000-$1F7FF will be accessible)
        lda #$00
        sta $D707
        .byte $80, $00          ; src MB = 0
        .byte $81, $00          ; dst MB = 0
        .byte $00               ; end options
        .byte $00               ; copy command
        .word $4000             ; count = 16384
        .word $C000             ; src = $C000
        .byte $00               ; src bank 0
        .word $C000             ; dst = $C000
        .byte $01               ; dst bank 1
        .byte $00               ; command high byte
        .word $0000             ; modulo

        ; DMA copy $F800-$FFFF from bank 0 to $2000 in bank 1 (2KB)
        ; This is the relocated copy that avoids the color RAM window
        lda #$00
        sta $D707
        .byte $80, $00          ; src MB = 0
        .byte $81, $00          ; dst MB = 0
        .byte $00               ; end options
        .byte $00               ; copy command
        .word $0800             ; count = 2048
        .word $F800             ; src = $F800
        .byte $00               ; src bank 0
        .word $2000             ; dst = $2000
        .byte $01               ; dst bank 1 ($12000)
        .byte $00               ; command high byte
        .word $0000             ; modulo


        ; basic hi

        lda #$01
        ldx #$00
        jsr SETBNK
        lda #$00
        ldx #$08
        ldy #$00
        jsr SETLFS

        lda #basichi_name_end - basichi_name
        ldx #<basichi_name
        ldy #>basichi_name
        jsr SETNAM
        lda #$40
        ldx #$00
        ldy #$80
        jsr LOAD


        ; basic lo

        lda #$01
        ldx #$00
        jsr SETBNK
        lda #$00
        ldx #$08
        ldy #$00
        jsr SETLFS

        lda #basiclo_name_end - basiclo_name
        ldx #<basiclo_name
        ldy #>basiclo_name
        jsr SETNAM
        lda #$40
        ldx #$00
        ldy #$40
        jsr LOAD


        ; chargen

        lda #$01
        ldx #$00
        jsr SETBNK
        lda #$00
        ldx #$08
        ldy #$00
        jsr SETLFS

        lda #chargen_name_end - chargen_name
        ldx #<chargen_name
        ldy #>chargen_name
        jsr SETNAM
        lda #$40
        ldx #$00
        ldy #$00
        jsr LOAD

        ; ============================================================
        ; Relocate KERNAL $F800-$FFFF to $12000 in bank 1
        ; The MEGA65 maps color RAM over $1F800-$1FFFF in bank 1,
        ; so we keep a second copy at $12000 for the emulator to read
        ; ============================================================
        lda #$00
        sta $D707
        .byte $80, $00          ; src MB = 0
        .byte $81, $00          ; dst MB = 0
        .byte $00               ; end options
        .byte $00               ; copy command
        .word $0800             ; count = 2048 bytes
        .word $F800             ; src addr = $F800
        .byte $00               ; src bank 0 (where KERNAL was loaded)
        .word $2000             ; dst addr = $2000
        .byte $01               ; dst bank 1 ($12000)
        .byte $00               ; command high byte
        .word $0000             ; modulo

        ; ============================================================
        ; Toggle ROM write-protect off (banks 2-3)
        ; This allows us to use $32000-$35FFF for VDC RAM
        ; Must be AFTER all C65 KERNAL LOAD calls (needs INTERFACE at $2C800)
        ; ============================================================
        lda #$70
        sta $D640
        clv

        ; ============================================================
        ; Clear VDC RAM ($32000-$35FFF, 16KB) via DMA fill
        ; Overwrites C65 BASIC in bank 3 (not needed by C128 emulator)
        ; ============================================================
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03               ; fill
        .word $4000             ; count = 16KB (16384 bytes)
        .word $0000             ; fill with $00
        .byte $00
        .word $2000             ; dest start = $2000 in bank 3 = $32000
        .byte $03               ; dest bank 3
        .byte $00
        .word $0000

        ; ============================================================
        ; Initialize and start emulation
        ; (C128_CPUReset calls C128_MemInit and C128_VideoInit internally)
        ; ============================================================

        ; Initialize LOW_RAM_BUFFER pointer for ZP/stack access
        jsr lrb_ptr_init

        jsr C128_CPUReset

main_loop:
        ; --- Check if print requested ---
        lda c128h_print_pending
        beq _no_print_start
        jsr C128Host_StartPrint

_no_print_start:
        jsr C128_CPUStepMultiple  ; Execute batch of 8502 instructions

        ; --- If printing, check if done ---
        lda c128h_print_active
        beq _no_print_check
        jsr C128Host_CheckPrintDone

_no_print_check:
        jmp main_loop

; ============================================================
; ============================================================
; Messages
; ============================================================
banner_msg:
        .byte $93, 27, 52
        .text "c128 emulator for mega65"
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

loading_msg:
        .text "loading roms..."
        .byte $0D, 0

chargen_name:
        .text "chargen.bin"
chargen_name_end:

basiclo_name:
        .text "basiclo.bin"
basiclo_name_end:

basichi_name:
        .text "basichi.bin"
basichi_name_end:

kernal_name:
        .text "kernal.bin"
kernal_name_end:

roms_ok_msg:
        .text "roms loaded."
        .byte $0D, 0

rom_fail_msg:
        .text "?error loading roms.bin"
        .byte $0D, 0

; ============================================================
; Include emulator components
; ============================================================
        .include "c128_cpu_8502.asm"
        .include "c128_hooks.asm"
        .include "c128_mem.asm"
        .include "c128_sound.asm"
        .include "c128_host.asm"
