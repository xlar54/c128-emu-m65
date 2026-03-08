; ============================================================
; main.asm - C128 Emulator for MEGA65
; Assembler: 64tass
;
; MEGA65 Host Memory Layout:
;
;   Bank 0 ($00000-$0FFFF): Host code + staging areas
;     $02001-$02011: BASIC stub (SYS 8210)
;     $02012-$07FFF: Emulator code (must end before $8000 for KERNAL MAP)
;     $09400-$09FFF: Directory staging buffer (~3KB)
;     $0A000-$0AFFF: LOW_RAM_BUFFER (cache of C128 RAM $0000-$0FFF, DMA-synced before hooks)
;
;   Bank 1 ($10000-$1FFFF): C128 ROMs (48KB used)
;     $10000-$11FFF: RESERVED - C65 KERNAL DOS variables (do not use!)
;     $12000-$127FF: KERNAL $F800-$FFFF relocated (2KB, avoids color RAM window)
;     $14000-$17FFF: BASIC LO ROM  (C128 $4000-$7FFF, 16KB)
;     $18000-$1BFFF: BASIC HI ROM  (C128 $8000-$BFFF, 16KB)
;     $1C000-$1F7FF: KERNAL ROM    (C128 $C000-$F7FF, 14KB accessible)
;     $1F800-$1FFFF: (blocked by MEGA65 color RAM window - see $12000)
;
;   Bank 2 ($20000-$2FFFF): C65 ROM (write-protect off after boot)
;     $2A000-$2BFFF: Character ROM (chargen, 8KB, loaded last)
;
;   Bank 3 ($30000-$3FFFF): Unused (C65 BASIC ROM area)
;
;   Bank 4 ($40000-$4FFFF): C128 RAM Bank 0 (64KB)
;   Bank 5 ($50000-$5FFFF): VDC display RAM + 80-col screen + color save buffers
;
;   Attic ($8000000-$800FFFF): C128 RAM Bank 1 (64KB, DMA-only access)
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
;   $EC-$EF:  vdc_color_ptr / _crunch_kw_ptr (shared)
;   $F0-$F3:  C128_MEM_PTR (4-byte general memory pointer)
;   $F4:      c128_saved_data (temp storage for writes)
;   $F5-$F6:  DIR_PTR / C128H_STR_PTR (2 bytes, shared, never simultaneous)
;   $F8-$FB:  C128_RAM_PTR (4-byte fast RAM pointer, dedicated - DO NOT alias $FB)
;
; ROM files on SD card:
;   kernal.bin   = 318020-05  KERNAL     (16KB -> $0C000, DMA to $1C000 + $12000)
;   basiclo.bin  = 318018-04  BASIC LO   (16KB -> $14000)
;   basichi.bin  = 318019-04  BASIC HI   (16KB -> $18000)
;   chargen.bin  = 390059-01  CHAR ROM   ( 8KB -> $10000 in bank 1 only)
;                                         DO NOT copy to bank 0 - $08000 is emulator code
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

; Macros (must be included before use)
        .include "macros.asm"

; ============================================================
; Entry point
; ============================================================
start:
        lda #65         ; 40mhz mode
        sta $00

        ; ============================================================
        ; Clear C128 RAM Bank 0 (MEGA65 bank 4) via DMA fill
        ; ============================================================
        #dma_fill_chip $04, $0000, $FFFF, $00

        ; ============================================================
        ; Clear C128 RAM Bank 1 (attic $8000000) via DMA fill
        ; ============================================================
        #dma_fill_attic $00, $0000, $FFFF, $00

        ; ============================================================
        ; Clear LOW_RAM_BUFFER ($A000-$AFFF) to match cleared C128 RAM
        ; DMA-synced from bank 4 before hooks that read it
        ; ============================================================
        #dma_fill_chip $00, $A000, $1000, $00

        ; ============================================================
        ; Load ROM files into bank 1 ($10000-$1FFFF)
        ; File layout: chargen@$0000, basiclo@$4000, basichi@$8000, kernal@$C000
        ; Chargen stays in bank 1 only (except chargen which is in bank 2).
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
        ; Even though $C000-$FFFF is mapped, DMA still reads the underlying RAM
        ; ($1F800-$1FFFF will be overridden by color RAM window on reads,
        ;  but the first 14KB at $1C000-$1F7FF will be accessible)
        #dma_copy_chip $00, $C000, $01, $C000, $4000

        ; DMA copy $F800-$FFFF from bank 0 to $2000 in bank 1 (2KB)
        ; This is the relocated copy that avoids the color RAM window
        #dma_copy_chip $00, $F800, $01, $2000, $0800


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

        ; Loaded to bank 2 at $A000 ($02A000).
        ; Bank 2 requires ROM write-protect off (trap $70).

        ; Disable ROM write-protect for bank 2
        lda #$70
        sta $D640
        clv

        lda #$02
        ldx #$00
        jsr SETBNK              ; Load to bank 2
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
        ldy #$A0
        jsr LOAD                ; Load chargen to $A000 in bank 2 = $02A000

        ; Chargen is in bank 2 at $2A000 only.
        ; DO NOT DMA copy to bank 0 - $08000-$09FFF is emulator code.
        ; VIC-IV CHARPTR points directly to bank 2 chargen at $02A000.
        ; Emulated CPU reads use read_from_chargen which reads bank 2 directly.

        ; ============================================================
        ; Clear display area in bank 5 (VDC RAM + screen + color saves)
        ; ============================================================
        #dma_fill_chip $05, $0000, $0000, $00

        ; ============================================================
        ; Initialize and start emulation
        ; (C128_CPUReset calls C128_MemInit and C128_VideoInit internally)
        ; ============================================================

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
        .include "c128_vdc.asm"
        .include "c128_sound.asm"
        .include "c128_host.asm"