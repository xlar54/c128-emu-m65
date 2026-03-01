; ============================================================
; main.asm - C128 Emulator for MEGA65
; Assembler: 64tass
;
; MEGA65 Host Memory Layout:
;
;   Bank 0 ($00000-$0FFFF): Host code + staging areas
;     $02001-$02011: BASIC stub (SYS 8210)
;     $02012+:       Emulator code (main + included modules)
;     $06000-$06FFF: Directory staging buffer (4KB)
;     $08000-$09FFF: Character ROM (chargen, 8KB, VIC-IV reads from here)
;     $0B000-$0BFFF: LOW_RAM_BUFFER (mirror of C128 RAM $0000-$0FFF)
;
;   Bank 1 ($10000-$1FFFF): C128 ROMs (48KB used)
;     $10000-$11FFF: Character ROM (chargen, 8KB, for $D000 mapping)
;     $12000-$127FF: KERNAL $F800-$FFFF relocated (2KB, avoids color RAM window)
;     $14000-$17FFF: BASIC LO ROM  (C128 $4000-$7FFF, 16KB)
;     $18000-$1BFFF: BASIC HI ROM  (C128 $8000-$BFFF, 16KB)
;     $1C000-$1F7FF: KERNAL ROM    (C128 $C000-$F7FF, 14KB accessible)
;     $1F800-$1FFFF: (blocked by MEGA65 color RAM window - see $12000)
;
;   Bank 2 ($20000-$2FFFF): Screen + color save buffers
;     $20400-$207E7: VIC screen RAM staging (1KB)
;     $2A000-$2A3E7: 40-col color save buffer (1000 bytes)
;     $2A800-$2AFEF: 80-col color save buffer (2000 bytes)
;
;   Bank 3 ($30000-$3FFFF): VDC display RAM
;     $32000-$35FFF: VDC RAM (16KB, overwrites C65 BASIC, ROM write-protect off)
;
;   Bank 4 ($40000-$4FFFF): C128 RAM Bank 0 (64KB)
;   Bank 5 ($50000-$5FFFF): C128 RAM Bank 1 (64KB)
;
;   Color RAM at $0FF80000 (accessed via 32-bit pointers)
;
; Host Zero Page Usage ($00-$FF):
;   $02-$04:  c128_a, c128_x, c128_y (emulated CPU registers)
;   $05:      c128_sp (emulated stack pointer)
;   $06:      c128_p (emulated processor flags)
;   $07-$08:  c128_pc (emulated program counter)
;   $09-$0A:  c128_addr (address scratch)
;   $0B:      c128_data (data byte scratch)
;   $0C-$0D:  c128_vec (vector scratch)
;   $0E-$0F:  c128_tmp, c128_tmp2
;   $11:      c128_xtra (page-crossing cycle count)
;   $12:      c128_dec_a (decimal mode saved A)
;   $14-$17:  c128_zp_ptr (4-byte ZP pointer: lo, $00, bank, $00)
;   $19:      c128_irq_pending
;   $1A-$1B:  c128_inst_pc (instruction PC for hooks)
;   $1C:      c128_nmi_pending
;   $E0-$E3:  c128_code_ptr (4-byte code-fetch cache pointer)
;   $E4:      c128_code_page_hi (cached guest PC page)
;   $E5:      c128_code_valid (code cache valid flag)
;   $E6:      c128_code_romvis (cached MMU CR for ROM visibility)
;   $E7:      c128_hook_pc_changed (set by hooks when PC modified)
;   $E8-$EB:  c128_stack_ptr (4-byte stack pointer: SP, $01, bank, $00)
;   $F0-$F3:  C128_MEM_PTR (4-byte general memory pointer)
;   $F4:      c128_saved_data (temp storage for writes)
;   $F8-$FB:  C128_RAM_PTR (4-byte fast RAM pointer, dedicated)
;   $FB:      C128H_STR_PTR / DIR_PTR (2 bytes, shared/context-dependent)
;
; ROM files on SD card:
;   kernal.bin   = 318020-05  KERNAL     (16KB -> $0C000, DMA to $1C000 + $12000)
;   basiclo.bin  = 318018-04  BASIC LO   (16KB -> $14000)
;   basichi.bin  = 318019-04  BASIC HI   (16KB -> $18000)
;   chargen.bin  = 390059-01  CHAR ROM   ( 8KB -> $10000, also at $08000)
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


        ; Patch BASIC ROM: Place trap opcodes for native hooks
        ; We use illegal opcode $02 (JAM) at hook points.
        ; The emulator's opcode decoder catches $02 and dispatches
        ; to our native handler based on PC address.
        ; This avoids any need for $FF page trampolines.
        
        ; Bank 1 pointer already set up from basic_lo LOAD above
        lda #$01
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        
        ; Hook 1: GONE dispatch at $4B3F (Execute/Trace Statement)
        ; Original byte at $4B3F: BEQ $4B3E (opcode $F0)
        ; Replace with $02 (trap)
        lda #$3F
        sta C128_MEM_PTR+0
        lda #$4B
        sta C128_MEM_PTR+1
        lda #$02                ; trap opcode
        ldz #0
        sta [C128_MEM_PTR],z
        
        ; Hook 2: Crunch Tokens at $430D - DISABLED (tokenizer has bugs)
        ; TODO: Fix in-place tokenization to match ROM contract
;        lda #$0D
;        sta C128_MEM_PTR+0
;        lda #$43
;        sta C128_MEM_PTR+1
;        lda #$12                ; trap opcode
;        ldz #0
;        sta [C128_MEM_PTR],z

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
; ROMS
; ============================================================

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

; ============================================================
; Include emulator components
; ============================================================
        .include "c128_cpu_8502.asm"
        .include "c128_hooks.asm"
        .include "c128_mem.asm"
        .include "c128_sound.asm"
        .include "c128_host.asm"