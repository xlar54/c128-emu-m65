; ============================================================
; macros.asm - Shared macros for C128 emulator
; Host: MEGA65
; Assembler: 64tass (45GS02)
;
; DMA macros that expand to inline enhanced DMA lists.
; Each macro emits a complete DMA operation at the call site.
; No shared routine or JSR — fully reentrant, zero overhead.
;
; Usage examples:
;   #dma_fill $00, $05, $4000, 2000, $20
;   #dma_copy $00, $04, $0000, $00, $00, LOW_RAM_BUFFER, $1000
;   #dma_copy_col2chip $08, $0000, $05, $A000, 1000
;   #dma_fill_chip $05, $0000, 2000, $20
;
; For dynamic (runtime-computed) parameters, use the _dyn
; variants which emit labeled fields that can be patched
; before triggering the DMA.
; ============================================================

        .cpu "45gs02"


; ============================================================
; dma_fill - Fill a memory region (fully static parameters)
;
; Parameters:
;   dst_mb   - Destination megabyte ($00=chip, $80=attic)
;   dst_bank - Destination bank (e.g. $05)
;   dst_addr - Destination address within bank
;   count    - Number of bytes to fill (0 = 64KB)
;   value    - Fill value (byte)
; ============================================================
dma_fill .macro dst_mb, dst_bank, dst_addr, count, value
        lda #$00
        sta $D707
        .byte $80, $00                  ; src MB (don't care for fill)
        .byte $81, \dst_mb              ; dst MB
        .byte $00                       ; end options
        .byte $03                       ; fill command
        .word \count                    ; count
        .word \value                    ; fill value (in src addr field)
        .byte $00                       ; src bank (ignored for fill)
        .word \dst_addr                 ; dst addr
        .byte \dst_bank                 ; dst bank
        .byte $00                       ; command high byte
        .word $0000                     ; modulo
.endmacro


; ============================================================
; dma_copy - Copy memory (fully static parameters)
;
; Parameters:
;   src_mb   - Source megabyte
;   src_bank - Source bank
;   src_addr - Source address within bank
;   dst_mb   - Destination megabyte
;   dst_bank - Destination bank
;   dst_addr - Destination address within bank
;   count    - Number of bytes to copy
; ============================================================
dma_copy .macro src_mb, src_bank, src_addr, dst_mb, dst_bank, dst_addr, count
        lda #$00
        sta $D707
        .byte $80, \src_mb              ; src MB
        .byte $81, \dst_mb              ; dst MB
        .byte $00                       ; end options
        .byte $00                       ; copy command
        .word \count                    ; count
        .word \src_addr                 ; src addr
        .byte \src_bank                 ; src bank
        .word \dst_addr                 ; dst addr
        .byte \dst_bank                 ; dst bank
        .byte $00                       ; command high byte
        .word $0000                     ; modulo
.endmacro


; ============================================================
; Convenience: chip RAM shortcuts (MB $00/$00)
; These cover ~70% of all DMA operations in the emulator.
; ============================================================

; dma_fill_chip - Fill within chip RAM (MB $00)
dma_fill_chip .macro dst_bank, dst_addr, count, value
        #dma_fill $00, \dst_bank, \dst_addr, \count, \value
.endmacro

; dma_copy_chip - Copy within chip RAM (MB $00 both sides)
dma_copy_chip .macro src_bank, src_addr, dst_bank, dst_addr, count
        #dma_copy $00, \src_bank, \src_addr, $00, \dst_bank, \dst_addr, \count
.endmacro


; ============================================================
; Convenience: color RAM shortcuts
; Color RAM is at $0FF80000 = MB $FF, bank $08, addr $0000+
; ============================================================

; dma_copy_chip2col - Copy from chip RAM to color RAM
;   src_bank, src_addr = source in chip RAM
;   col_offset         = offset within color RAM
;   count              = bytes to copy
dma_copy_chip2col .macro src_bank, src_addr, col_offset, count
        #dma_copy $00, \src_bank, \src_addr, $FF, $08, \col_offset, \count
.endmacro

; dma_copy_col2chip - Copy from color RAM to chip RAM
;   col_offset         = offset within color RAM
;   dst_bank, dst_addr = destination in chip RAM
;   count              = bytes to copy
dma_copy_col2chip .macro col_offset, dst_bank, dst_addr, count
        #dma_copy $FF, $08, \col_offset, $00, \dst_bank, \dst_addr, \count
.endmacro

; dma_fill_col - Fill color RAM region
;   col_offset = offset within color RAM
;   count      = bytes to fill
;   value      = fill value
dma_fill_col .macro col_offset, count, value
        #dma_fill $FF, $08, \col_offset, \count, \value
.endmacro


; ============================================================
; Convenience: attic RAM shortcuts
; Attic is at MB $80. Used for C128 RAM bank 1.
; ============================================================

; dma_fill_attic - Fill attic RAM region
;   dst_bank, dst_addr = destination in attic
;   count              = bytes to fill
;   value              = fill value
dma_fill_attic .macro dst_bank, dst_addr, count, value
        #dma_fill $80, \dst_bank, \dst_addr, \count, \value
.endmacro

; dma_copy_chip2attic - Copy from chip RAM to attic
dma_copy_chip2attic .macro src_bank, src_addr, dst_bank, dst_addr, count
        #dma_copy $00, \src_bank, \src_addr, $80, \dst_bank, \dst_addr, \count
.endmacro

; dma_copy_attic2chip - Copy from attic to chip RAM
dma_copy_attic2chip .macro src_bank, src_addr, dst_bank, dst_addr, count
        #dma_copy $80, \src_bank, \src_addr, $00, \dst_bank, \dst_addr, \count
.endmacro


; ============================================================
; Dynamic DMA operations
;
; These emit labeled fields that can be patched at runtime
; before the DMA is triggered. The label prefix is passed
; as the first parameter to avoid symbol collisions.
;
; Usage:
;   ; Set up dynamic fields
;   lda computed_addr_lo
;   sta myop_dst_addr
;   lda computed_addr_hi
;   sta myop_dst_addr+1
;   ; Trigger the DMA (inline, self-contained)
;   #dma_fill_dyn myop, $00, $05
;
; The macro emits labels like: {prefix}_count, {prefix}_value,
; {prefix}_dst_addr, etc. that can be patched with sta.
; ============================================================

; dma_fill_dyn - Fill with runtime-patchable fields
;
; Parameters:
;   prefix   - Label prefix for patchable fields
;   dst_mb   - Destination megabyte (static)
;   dst_bank - Destination bank (static)
;
; Patchable fields (patch before this code runs):
;   {prefix}_count    - 2 bytes: fill count
;   {prefix}_value    - 2 bytes: fill value (lo byte used, hi=0)
;   {prefix}_dst_addr - 2 bytes: destination address
dma_fill_dyn .macro prefix, dst_mb, dst_bank
        lda #$00
        sta $D707
        .byte $80, $00                  ; src MB (don't care)
        .byte $81, \dst_mb              ; dst MB
        .byte $00                       ; end options
        .byte $03                       ; fill command
\prefix\._count:
        .word $0000                     ; count (patch me)
\prefix\._value:
        .word $0000                     ; fill value (patch me)
        .byte $00                       ; src bank (ignored)
\prefix\._dst_addr:
        .word $0000                     ; dst addr (patch me)
        .byte \dst_bank                 ; dst bank
        .byte $00                       ; command high byte
        .word $0000                     ; modulo
.endmacro


; dma_copy_dyn - Copy with runtime-patchable fields
;
; Parameters:
;   prefix   - Label prefix for patchable fields
;   src_mb   - Source megabyte (static)
;   src_bank - Source bank (static)
;   dst_mb   - Destination megabyte (static)
;   dst_bank - Destination bank (static)
;
; Patchable fields:
;   {prefix}_count    - 2 bytes: copy count
;   {prefix}_src_addr - 2 bytes: source address
;   {prefix}_dst_addr - 2 bytes: destination address
dma_copy_dyn .macro prefix, src_mb, src_bank, dst_mb, dst_bank
        lda #$00
        sta $D707
        .byte $80, \src_mb              ; src MB
        .byte $81, \dst_mb              ; dst MB
        .byte $00                       ; end options
        .byte $00                       ; copy command
\prefix\._count:
        .word $0000                     ; count (patch me)
\prefix\._src_addr:
        .word $0000                     ; src addr (patch me)
        .byte \src_bank                 ; src bank
\prefix\._dst_addr:
        .word $0000                     ; dst addr (patch me)
        .byte \dst_bank                 ; dst bank
        .byte $00                       ; command high byte
        .word $0000                     ; modulo
.endmacro


; dma_copy_dyn_full - Copy with ALL fields patchable
;
; For cases where even the banks vary at runtime
; (e.g. get_physical_bank determines src bank).
;
; Patchable fields:
;   {prefix}_count, {prefix}_src_addr, {prefix}_src_bank,
;   {prefix}_dst_addr, {prefix}_dst_bank
dma_copy_dyn_full .macro prefix, src_mb, dst_mb
        lda #$00
        sta $D707
        .byte $80, \src_mb              ; src MB
        .byte $81, \dst_mb              ; dst MB
        .byte $00                       ; end options
        .byte $00                       ; copy command
\prefix\._count:
        .word $0000                     ; count (patch me)
\prefix\._src_addr:
        .word $0000                     ; src addr (patch me)
\prefix\._src_bank:
        .byte $00                       ; src bank (patch me)
\prefix\._dst_addr:
        .word $0000                     ; dst addr (patch me)
\prefix\._dst_bank:
        .byte $00                       ; dst bank (patch me)
        .byte $00                       ; command high byte
        .word $0000                     ; modulo
.endmacro


; ============================================================
; Single-byte DMA operations (attic access)
;
; The 45GS02 can't reach attic via 32-bit ZP pointers (28-bit
; limit), so single-byte DMA is the only access method.
; These use the F018B list format option ($0B).
; ============================================================

; dma_read_byte - Read 1 byte via DMA
;
; Parameters:
;   prefix   - Label prefix for patchable src addr
;   src_mb   - Source megabyte
;   src_bank - Source bank
;   dst_addr - Destination address in chip RAM (staging byte)
;
; Patchable fields:
;   {prefix}_src_addr - 2 bytes: source address to read from
;
; After DMA completes, read result from dst_addr.
dma_read_byte .macro prefix, src_mb, src_bank, dst_addr
        lda #$00
        sta $D707
        .byte $80, \src_mb              ; src MB
        .byte $81, $00                  ; dst MB = $00 (chip RAM)
        .byte $0b                       ; option: F018B list format
        .byte $00                       ; end options
        .byte $00                       ; copy command
        .word $0001                     ; count = 1 byte
\prefix\._src_addr:
        .word $0000                     ; src addr (patch me)
        .byte \src_bank                 ; src bank
        .word \dst_addr                 ; dst addr (staging)
        .byte $00                       ; dst bank 0
        .byte $00                       ; command high
        .word $0000                     ; modulo
.endmacro


; dma_write_byte - Write 1 byte via DMA
;
; Parameters:
;   prefix   - Label prefix for patchable dst addr
;   src_addr - Source address in chip RAM (staging byte)
;   dst_mb   - Destination megabyte
;   dst_bank - Destination bank
;
; Patchable fields:
;   {prefix}_dst_addr - 2 bytes: destination address to write to
;
; Store the byte to write at src_addr before triggering.
dma_write_byte .macro prefix, src_addr, dst_mb, dst_bank
        lda #$00
        sta $D707
        .byte $80, $00                  ; src MB = $00 (chip RAM)
        .byte $81, \dst_mb              ; dst MB
        .byte $0b                       ; option: F018B list format
        .byte $00                       ; end options
        .byte $00                       ; copy command
        .word $0001                     ; count = 1 byte
        .word \src_addr                 ; src addr (staging)
        .byte $00                       ; src bank 0
\prefix\._dst_addr:
        .word $0000                     ; dst addr (patch me)
        .byte \dst_bank                 ; dst bank
        .byte $00                       ; command high
        .word $0000                     ; modulo
.endmacro