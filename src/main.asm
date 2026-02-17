; ============================================================
; main.asm - C128 Emulator for MEGA65
; Assembler: 64tass
;
; Memory Layout:
;   Bank 0:
;     $08000-$09FFF: Character ROM (C128 chargen, 8KB, stays at staging area)
;   Bank 1 ($10000-$1FFFF): C128 ROMs + VDC RAM
;     $10000-$13FFF: VDC RAM (16KB)
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

        ; ============================================================
        ; Load roms.bin (64KB) byte-by-byte into bank 1 ($10000-$1FFFF)
        ; File layout: chargen@$0000, basiclo@$4000, basichi@$8000, kernal@$C000
        ; Also copies chargen ($10000-$11FFF) to $08000 for VIC-IV
        ; ============================================================

        ; Print loading message
        ldx #0
_ld_msg: lda loading_msg,x
        beq _ld_msg_done
        jsr CHROUT
        inx
        bne _ld_msg

_ld_msg_done:

        ; kernal

        lda #$01
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




        ; Print done message
        ldx #0
_ld_ok: lda roms_ok_msg,x
        beq _ld_ok_done
        jsr CHROUT
        inx
        bne _ld_ok
_ld_ok_done:
        jmp _rom_load_continue

_rom_load_fail:
        ldx #0
_lf:    lda rom_fail_msg,x
        beq _lf_hang
        jsr CHROUT
        inx
        bne _lf
_lf_hang:
        jmp _lf_hang

_rom_load_continue:

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
        
        ; ROM check after VideoInit
        lda #$ED
        sta C128_MEM_PTR+0
        lda #$FB
        sta C128_MEM_PTR+1
        lda #$01
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        sta $7010              ; should be $D7 if ROM intact after VideoInit

        ; Initialize LOW_RAM_BUFFER pointer for ZP/stack access
        jsr lrb_ptr_init

        lda #$ED
        sta C128_MEM_PTR+0
        lda #$FB
        sta C128_MEM_PTR+1
        lda #$01
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        sta $7011              ; after lrb_ptr_init

        ; Set border to black = boot starting
        lda #$00
        sta $D020
        
        jsr C128_CPUReset

        lda #$ED
        sta C128_MEM_PTR+0
        lda #$FB
        sta C128_MEM_PTR+1
        lda #$01
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        sta $7012              ; after CPUReset

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
        .include "c128_cpu_z80.asm"
        .include "c128_hooks.asm"
        .include "c128_mem.asm"
        .include "c128_sound.asm"
        .include "c128_host.asm"
        ;.include "c128_monitor.asm"  ; DISABLED - writes to bank 1, corrupts ROMs

; Stub out monitor entry points
P4MON_Check:
        clc             ; carry clear = no monitor
        rts
P4MON_Enter:
        rts