; ============================================================
; c128_cpu_8502.asm - C128 8502 CPU core - ALL 151 LEGAL OPCODES
; Assembler: 64tass
;
; The 8502 is functionally identical to the 6510/6502 for
; instruction execution. The differences are in the I/O port
; at $00/$01 and the memory banking (handled by the MMU).
;
; Timing uses VIC-II PAL: 63 cycles/line, 312 lines/frame.
; CIA timers provide the main interrupt source (CIA1 Timer A
; drives the system IRQ at ~60Hz).
; ============================================================

        .cpu "45gs02"

; Zero-page CPU state
c128_a        = $02
c128_x        = $03
c128_y        = $04
c128_sp       = $05
c128_p        = $06
c128_pc_lo    = $07
c128_pc_hi    = $08
c128_addr_lo  = $09
c128_addr_hi  = $0A
c128_data     = $0b
c128_vec_lo   = $0c
c128_vec_hi   = $0d
c128_tmp      = $0e
c128_tmp2     = $0f

c128_xtra     = $11
c128_dec_a    = $12           ; Saved A for decimal mode ADC/SBC
c128_inst_pc_lo = $1A
c128_inst_pc_hi = $1B
c128_irq_pending = $19
c128_nmi_pending = $1C


; Status flags
P_C = %00000001
P_Z = %00000010
P_I = %00000100
P_D = %00001000
P_B = %00010000
P_U = %00100000
P_V = %01000000
P_N = %10000000


; C128 keyboard state (CIA1-based, no separate selector register)
c128_kbd_col:        .byte $FF  ; Last keyboard column written to CIA1 $DC00

; Debug trace variables
c128_trace_enabled:    .byte $00  ; Set to 1 after LOAD to enable tracing
c128_trace_pos_lo:     .byte $00  ; Screen position low byte
c128_trace_pos_hi:     .byte $08  ; Screen position high byte ($0800)

; VIC-II timing constants (PAL)
VIC_CYCLES_PER_LINE = 63    ; 63 cycles per scanline (VIC-II PAL)
VIC_LINES_PER_FRAME = 312   ; PAL has 312 lines


set_zna .macro
        tax                     ; X = result byte (original A)
        lda c128_p
        and #(~(P_Z|P_N)) & $ff  ; clear old Z/N
        ora zn_table,x           ; OR in new Z/N
        sta c128_p
        txa
.endmacro

; Same as set_zna but doesn't restore A (saves 2 cycles)
; Use when A is immediately overwritten after (e.g., lda #cycles)
set_zn_fast .macro
        tax
        lda c128_p
        and #(~(P_Z|P_N)) & $ff
        ora zn_table,x
        sta c128_p
.endmacro

; -----------------------------------------------------------------
; read_data_fast: Inline read from c128_addr_hi:c128_addr_lo
; For addr < $40 page, sets up C128_MEM_PTR for bank 4 and reads directly.
; For addr >= $4000, falls back to jsr C128_ReadFast.
; Result in A. Clobbers Z.
; -----------------------------------------------------------------
read_data_fast .macro
        lda c128_addr_hi
        cmp #$40
        bcs _rdf_slow\@
        ; Fast inline: set up C128_MEM_PTR for bank 4 read
        sta C128_MEM_PTR+1
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda #BANK_RAM0
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        bra _rdf_done\@
_rdf_slow\@:
        jsr C128_ReadFast
_rdf_done\@:
.endmacro

; -----------------------------------------------------------------
; write_data_fast: Inline write c128_data to c128_addr_hi:lo
; For addr < $D0 page, writes directly to RAM0 via C128_MEM_PTR.
; For addr >= $D000, falls back to jsr C128_Write (I/O dispatch).
; c128_data must be set before using this macro.
; -----------------------------------------------------------------
write_data_fast .macro
        lda c128_addr_hi
        cmp #$D0
        bcs _wdf_slow\@
        ; Invalidate code cache if writing to current code page
        cmp c128_code_page_hi
        bne _wdf_no_inv\@
        lda #0
        sta c128_code_valid
        lda c128_addr_hi
_wdf_no_inv\@:
        sta C128_MEM_PTR+1
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda #BANK_RAM0
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda c128_data
        sta [C128_MEM_PTR],z
        bra _wdf_done\@
_wdf_slow\@:
        jsr C128_Write
_wdf_done\@:
.endmacro

; ------------------------------------------------------------
; finish_cycles_inline - Inlined cycle accounting macro
; A = base cycle count on entry
; Most instructions won't cross a scanline, so we inline the fast path
; ------------------------------------------------------------
finish_cycles_inline .macro
        clc
        adc c128_xtra             ; Add extra cycles (page crossing, etc.)
        adc vic_cycle_accum     ; Add to accumulated cycles
        sta vic_cycle_accum
        cmp #VIC_CYCLES_PER_LINE
        bcc _fc_skip\@
        jsr finish_do_scanline  ; Only call if we crossed scanline boundary
_fc_skip\@:
        jmp finish_and_loop     ; Decrement batch counter, loop or return
.endmacro

; ------------------------------------------------------------
; finish_cycles_no_xtra - For opcodes that never page-cross
; Skips the c128_xtra add. A = cycle count on entry.
; ------------------------------------------------------------
finish_cycles_no_xtra .macro
        clc
        adc vic_cycle_accum
        sta vic_cycle_accum
        cmp #VIC_CYCLES_PER_LINE
        bcc _fc_skip\@
        jsr finish_do_scanline
_fc_skip\@:
        jmp finish_and_loop     ; Decrement batch counter, loop or return
.endmacro


; ------------------------------------------------------------
; Z/N lookup table: entry = (val==0 ? P_Z : 0) | (val&$80 ? P_N : 0)
; ------------------------------------------------------------
zn_table:
        .byte $02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
        .byte $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80


; -----------------------------------------------------------------
; ZP: fast code-fetch cache (host-side)
; -----------------------------------------------------------------
c128_code_ptr       = $E0   ; 4 bytes: lo, hi, bank, megabyte
c128_code_page_hi   = $E4   ; cached c128_pc_hi (guest)
c128_code_valid     = $E5   ; 0=invalid, !=0 valid
c128_code_romvis    = $E6   ; cached mmu_cr snapshot (for ROM visibility change detection)
c128_hook_pc_changed = $E7  ; Set by hooks when they modify PC
c128_stack_ptr      = $E8   ; 4 bytes: dedicated stack pointer (lo=SP, hi=$01, bank, $00)
c128_zp_ptr         = $14   ; 4 bytes: dedicated ZP pointer (lo=addr, hi=$00, bank4, $00)

; Call this when you want to force cache rebuild (optional helper)
invalidate_code_cache:
        lda #$00
        sta c128_code_valid
        rts

; -----------------------------------------------------------------
; fetch8_operand: Inline operand fetch when cache is likely valid.
; Must check code_valid since I/O pages ($D0-$DF, $FF) are never cached.
; Also checks for page boundary (pc_lo == 0).
; Result in A. Clobbers Z.
; -----------------------------------------------------------------
fetch8_operand .macro
        lda c128_code_valid
        beq _f8op_slow\@        ; Cache invalid (I/O page, etc.)
        lda c128_pc_lo
        beq _f8op_slow\@        ; Page wrapped - need full fetch8
        taz
        lda [c128_code_ptr],z
        inw c128_pc_lo
        bra _f8op_done\@
_f8op_slow\@:
        jsr fetch8
_f8op_done\@:
.endmacro

; -----------------------------------------------------------------
; fetch8_fast: A = mem[PC], PC++
; Fast path for:
;   - $0000-$0FFF via LOW_RAM_BUFFER mirror
;   - $1000-$3FFF via BANK_RAM0/1 (always RAM)
;   - $4000-$FFFF via BANK_ROM when ROM mapped, else BANK_RAM
; Slow fallback for:
;   - $D0xx-$DFxx (I/O), $FF00-$FF04 (MMU)
; -----------------------------------------------------------------
fetch8:
        ; If cache invalid, rebuild
        lda c128_code_valid
        beq _f8_rebuild

        ; If page changed, rebuild
        lda c128_pc_hi
        cmp c128_code_page_hi
        bne _f8_rebuild

        ; Cache valid + page matches = safe to read
        ; (I/O pages $D0-$DF and $FF are marked invalid during rebuild,
        ;  so they always go through _f8_rebuild -> _f8_build_slow)

_f8_do_read:
        ldz c128_pc_lo
        lda [c128_code_ptr],z
        inw c128_pc_lo
        rts

; -----------------------------
; Cache rebuild
; -----------------------------
_f8_rebuild:
        lda c128_pc_hi
        sta c128_code_page_hi

        ; Remember current MMU config for change detection
        lda mmu_cr
        sta c128_code_romvis

        lda c128_pc_hi

        ; $0000-$0FFF => physical bank (same as other RAM)
        cmp #$10
        bcc _f8_build_ram

        ; Be conservative: $D0-$DF (I/O) / $FF (MMU) => slow path
        cmp #$FF
        beq _f8_build_slow
        cmp #$D0
        bcs _f8_check_io_slow

        ; $1000-$3FFF => always RAM
        cmp #$40
        bcc _f8_build_ram

        ; $4000-$7FFF: check BASIC LO ROM
        cmp #$80
        bcs _f8_check_mid
        lda mmu_basic_lo_rom
        bne _f8_build_rom
        jmp _f8_build_ram

_f8_check_mid:
        ; $8000-$BFFF: check BASIC HI ROM
        cmp #$C0
        bcs _f8_check_hi
        lda mmu_basic_hi_rom
        bne _f8_build_rom
        jmp _f8_build_ram

_f8_check_hi:
        ; $C000-$CFFF or $E000-$FEFF: check KERNAL ROM
        lda mmu_kernal_rom
        bne _f8_build_rom
        jmp _f8_build_ram

_f8_check_io_slow:
        ; $D0-$DF or $FF: slow path
        cmp #$E0
        bcc _f8_build_slow      ; $D0-$DF = I/O
        ; $E0-$FE: check KERNAL ROM
        lda mmu_kernal_rom
        bne _f8_build_rom
        jmp _f8_build_ram

_f8_build_rom:
        ; ROM visible => BANK_ROM (bank 1), same hi byte
        ; Exception: pages $F8-$FF are relocated to $2000-$27FF
        ; because $1F800-$1FFFF is the MEGA65 color RAM window
        lda #$00
        sta c128_code_ptr+0
        lda c128_pc_hi
        cmp #$F8
        bcc _f8_rom_normal
        ; Relocated: $F8->$20, $F9->$21, ..., $FF->$27
        sec
        sbc #$D8
        sta c128_code_ptr+1
        jmp _f8_rom_set_bank
_f8_rom_normal:
        sta c128_code_ptr+1
_f8_rom_set_bank:
        lda #BANK_ROM
        sta c128_code_ptr+2
        lda #$00
        sta c128_code_ptr+3
        lda #$01
        sta c128_code_valid
        jmp _f8_do_read

_f8_build_ram:
        ; RAM => use physical bank from MMU (with shared RAM logic)
        lda #$00
        sta c128_code_ptr+0
        lda c128_pc_hi
        sta c128_code_ptr+1
        ; get_physical_bank uses c128_addr_hi, so set it to PC page
        sta c128_addr_hi
        jsr get_physical_bank   ; Returns MEGA65 bank in A
        sta c128_code_ptr+2
        lda #$00
        sta c128_code_ptr+3
        lda #$01
        sta c128_code_valid
        jmp _f8_do_read

_f8_build_low:
        lda #$00
        sta c128_code_ptr+0
        lda c128_pc_hi
        clc
        adc #>LOW_RAM_BUFFER
        sta c128_code_ptr+1
        lda #$00
        sta c128_code_ptr+2
        sta c128_code_ptr+3
        lda #$01
        sta c128_code_valid
        jmp _f8_do_read

_f8_build_slow:
        lda #$00
        sta c128_code_valid
        ; fall through

; -----------------------------
; Slow path: use full memory read
; -----------------------------
_f8_slow:
        lda c128_pc_lo
        sta c128_addr_lo
        lda c128_pc_hi
        sta c128_addr_hi
        #read_data_fast
        inw c128_pc_lo
        rts

;===========end new




; --- fetch16_to_addr ---
fetch16_to_addr:
        ; Fast path: if code cache is valid and both bytes on same page,
        ; read both directly without re-validating for second byte.
        lda c128_code_valid
        beq _f16_slow
        lda c128_pc_hi
        cmp c128_code_page_hi
        bne _f16_slow

        ; I/O pages never have cache valid (set in _f8_build_slow),
        ; so no I/O exclusion check needed here.

_f16_fast:
        ; Both bytes on same page? (PC lo must be < $FF)
        lda c128_pc_lo
        cmp #$FF
        beq _f16_slow           ; Page split - use safe path

        ; Read both bytes directly from code pointer
        taz
        lda [c128_code_ptr],z   ; First byte (lo)
        sta c128_addr_lo
        inz
        lda [c128_code_ptr],z   ; Second byte (hi)
        sta c128_addr_hi
        ; Advance PC by 2
        lda c128_pc_lo
        clc
        adc #2
        sta c128_pc_lo
        bcs _f16_pc_carry
        rts
_f16_pc_carry:
        inc c128_pc_hi
        rts

_f16_slow:
        ; Safe path: two fetch8 calls (handles page crossings, cache rebuilds)
        ; Must use c128_tmp because fetch8 slow path clobbers c128_addr_lo
        jsr fetch8
        sta c128_tmp
        jsr fetch8
        sta c128_addr_hi
        lda c128_tmp
        sta c128_addr_lo
        rts

; Cached bank for stack page ($0100-$01FF)
; Stack page is always in shared bottom region -> always BANK_RAM0.
cached_stack_bank: .byte BANK_RAM0

; --- init_stack_ptr ---
; Call once at startup to initialize the dedicated c128_stack_ptr
; After this, only c128_stack_ptr+0 (low byte = SP) needs updating
init_stack_ptr:
        lda #$00
        sta c128_stack_ptr+0    ; Will be set to SP before each use
        lda #$01
        sta c128_stack_ptr+1    ; Page $01 (stack page)
        lda #BANK_RAM0
        sta c128_stack_ptr+2    ; Always bank 4
        lda #$00
        sta c128_stack_ptr+3    ; Megabyte 0
        rts

; --- push_data ---
push_data:
        lda c128_sp
        sta c128_stack_ptr+0
        ldz #0
        lda c128_data
        sta [c128_stack_ptr],z
        dec c128_sp
        rts

; --- pull_to_a ---
pull_to_a:
        inc c128_sp
        lda c128_sp
        sta c128_stack_ptr+0
        ldz #0
        lda [c128_stack_ptr],z
        rts

; ============================================================
; Fast ZP read/write - using 32-bit pointer rebuilt each call
; Cannot keep a persistent pointer in ZP because C128 code writes to all ZP.
; Instead, rebuild C128_MEM_PTR ($F0-$F3) in each helper from constants.
; Special handling for $00/$01 (8502 CPU port registers)
; ============================================================

; No persistent pointer needed - removed LRB_PTR

; 8502 port register state
cpu_port_ddr:   .byte $2F       ; Data Direction Register (default: bits 0-3,5 output)
cpu_port_data:  .byte $07       ; Port data register (default: LORAM+HIRAM+CHAREN on)
cpu_port_ext:   .byte $FF       ; External input lines (active low, no cart = all high)

; Initialize pointers - set up dedicated stack and ZP pointers
lrb_ptr_init:
        jsr init_stack_ptr
        ; Initialize dedicated ZP pointer: $04:$00:$00:XX
        lda #$00
        sta c128_zp_ptr+0       ; Will be set before each use
        sta c128_zp_ptr+1       ; Page $00
        sta c128_zp_ptr+3       ; Megabyte 0
        lda #BANK_RAM0
        sta c128_zp_ptr+2       ; Bank 4
        rts

; Helper: read ZP[X] -> A  (preserves X)
lrb_read_x:
        stx c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        rts

; Helper: write A -> ZP[X]  (preserves A, X)
lrb_write_x:
        stx c128_zp_ptr+0
        ldz #0
        sta [c128_zp_ptr],z
        rts

; Helper: read ZP[Y] -> A  (preserves Y)
lrb_read_y:
        sty c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        rts

; Helper: ASL ZP[X]  (preserves X, sets flags from result)
lrb_asl_x:
        stx c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        asl a
        sta [c128_zp_ptr],z
        rts

; Helper: DEC ZP[X]  (preserves X, sets flags from result)
lrb_dec_x:
        stx c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        sec
        sbc #1
        sta [c128_zp_ptr],z
        rts

; Helper: INC ZP[X]  (preserves X, sets flags from result)
lrb_inc_x:
        stx c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        clc
        adc #1
        sta [c128_zp_ptr],z
        rts

; Helper: set C128_MEM_PTR to LOW_RAM_BUFFER base (call before [C128_MEM_PTR],z access)
setup_lrb:
        lda #>LOW_RAM_BUFFER
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+0
        sta C128_MEM_PTR+2
        sta C128_MEM_PTR+3
        rts

; Macro: inline setup of C128_MEM_PTR to LOW_RAM_BUFFER
; Clobbers A only
setup_lrb_inline .macro
        lda #>LOW_RAM_BUFFER
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+0
        sta C128_MEM_PTR+2
        sta C128_MEM_PTR+3
        .endm

; Macro: inline setup of C128_MEM_PTR to bank 4 page 0 (for ZP access)
setup_bank4_zp .macro
        lda #$00
        sta C128_MEM_PTR+0
        sta C128_MEM_PTR+1
        sta C128_MEM_PTR+3
        lda #BANK_RAM0
        sta C128_MEM_PTR+2
        .endm

; Read from ZP address in X, result in A
read_zp_x:
        cpx #$02
        bcc _rzpx_port
        stx c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        rts
_rzpx_port:
        cpx #$00
        beq _rzpx_ddr
        ; Read port $01: output bits from data reg, input bits from external
        lda cpu_port_ddr
        and cpu_port_data       ; output bits
        sta _rzpx_tmp
        lda cpu_port_ddr
        eor #$FF                ; invert DDR = input mask
        and cpu_port_ext        ; input bits from external
        ora _rzpx_tmp
        rts
_rzpx_ddr:
        lda cpu_port_ddr
        rts
_rzpx_tmp: .byte 0

; Read from ZP address in c128_addr_lo, result in A
read_zp:
        ldx c128_addr_lo
        cpx #$02
        bcc _rzp_port
        stx c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        rts
_rzp_port:
        cpx #$00
        beq _rzp_ddr
        ; Read port $01
        lda cpu_port_ddr
        and cpu_port_data
        sta _rzp_tmp
        lda cpu_port_ddr
        eor #$FF
        and cpu_port_ext
        ora _rzp_tmp
        rts
_rzp_ddr:
        lda cpu_port_ddr
        rts
_rzp_tmp: .byte 0

; Write A to ZP address in X
write_zp_x:
        cpx #$02
        bcc _wzpx_port
        stx c128_zp_ptr+0
        ldz #0
        sta [c128_zp_ptr],z
        rts
_wzpx_port:
        cpx #$00
        beq _wzpx_ddr
        sta cpu_port_data
        ; Also write to bank 4
        pha
        lda #$01
        sta c128_zp_ptr+0
        ldz #0
        lda cpu_port_data
        sta [c128_zp_ptr],z
        pla
        rts
_wzpx_ddr:
        sta cpu_port_ddr
        ; Also write to bank 4
        pha
        lda #$00
        sta c128_zp_ptr+0
        ldz #0
        lda cpu_port_ddr
        sta [c128_zp_ptr],z
        pla
        rts

; Write c128_data to ZP address in c128_addr_lo
write_zp:
        ldx c128_addr_lo
        cpx #$02
        bcc _wzp_port
        stx c128_zp_ptr+0
        ldz #0
        lda c128_data
        sta [c128_zp_ptr],z
        rts
_wzp_port:
        lda c128_data
        cpx #$00
        beq _wzp_ddr
        sta cpu_port_data
        ; Also write to bank 4
        pha
        lda #$01
        sta c128_zp_ptr+0
        ldz #0
        lda cpu_port_data
        sta [c128_zp_ptr],z
        pla
        rts
_wzp_ddr:
        sta cpu_port_ddr
        ; Also write to bank 4
        pha
        lda #$00
        sta c128_zp_ptr+0
        ldz #0
        lda cpu_port_ddr
        sta [c128_zp_ptr],z
        pla
        rts

; Read 16-bit pointer from ZP address Y, result in c128_addr_lo/hi
; (Used for indirect addressing modes)
read_zp_ptr_y:
        sty c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        sta c128_addr_lo
        inc c128_zp_ptr+0
        lda [c128_zp_ptr],z
        sta c128_addr_hi
        rts

; --- Addressing modes ---
addr_zp:
        jsr fetch8
        sta c128_addr_lo
        lda #$00
        sta c128_addr_hi
        rts

addr_zpx:
        jsr fetch8
        clc
        adc c128_x
        sta c128_addr_lo
        lda #$00
        sta c128_addr_hi
        rts

addr_zpy:
        jsr fetch8
        clc
        adc c128_y
        sta c128_addr_lo
        lda #$00
        sta c128_addr_hi
        rts

addr_abs:
        jmp fetch16_to_addr

addr_absx:
        jsr fetch16_to_addr
        lda #$00
        sta c128_xtra
        lda c128_addr_lo
        clc
        adc c128_x
        sta c128_addr_lo
        bcc _absx_nc
        inc c128_addr_hi
        lda #$01
        sta c128_xtra
_absx_nc:
        rts

addr_absy:
        jsr fetch16_to_addr
        lda #$00
        sta c128_xtra
        lda c128_addr_lo
        clc
        adc c128_y
        sta c128_addr_lo
        bcc _absy_nc
        inc c128_addr_hi
        lda #$01
        sta c128_xtra
_absy_nc:
        rts

addr_indx:
        jsr fetch8
        clc
        adc c128_x
        ; Read 16-bit pointer directly from ZP (inlined lrb_read)
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z             ; ptr lo
        sta c128_addr_lo
        inc c128_zp_ptr+0               ; wraps within ZP page
        lda [c128_zp_ptr],z             ; ptr hi
        sta c128_addr_hi
        rts

addr_indy:
        jsr fetch8
        tay                             ; Y = ZP address
        lda #$00
        sta c128_xtra
        ; Read 16-bit pointer directly from ZP (inlined lrb_read)
        sty c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z             ; ptr lo
        sta c128_tmp
        iny
        sty c128_zp_ptr+0
        lda [c128_zp_ptr],z             ; ptr hi
        ; Add Y register to form final address
        sta c128_addr_hi                ; tentative high byte
        lda c128_tmp
        clc
        adc c128_y
        sta c128_addr_lo
        bcc _indy_no_carry
        inc c128_addr_hi                ; page crossed: increment high
        lda #$01
        sta c128_xtra
_indy_no_carry:
        rts

addr_ind_jmp:
        jsr fetch16_to_addr

;debug
        ;  save the pointer address (e.g. $0318)
        lda c128_addr_lo
        sta c128_vec_lo
        lda c128_addr_hi
        sta c128_vec_hi
;

        #read_data_fast
        sta c128_tmp
        inc c128_addr_lo
        #read_data_fast
        sta c128_tmp2

        lda c128_tmp
        sta c128_addr_lo
        lda c128_tmp2
        sta c128_addr_hi
        rts

fetch_rel:
        jsr fetch8
        sta c128_tmp
        rts

; ============================================================
; VIC-II / CIA Scanline Processing
; Called when cycle accumulator >= 63 (one scanline worth)
;
; C128 interrupt sources:
;   - CIA1 Timer A: system IRQ (~60Hz, drives KERNAL IRQ handler)
;   - VIC-II raster: raster compare interrupt
;   - CIA2: NMI source (optional)
; ============================================================
finish_do_scanline:

_vic_line_loop:
        ; Subtract one scanline
        lda vic_cycle_accum
        sec
        sbc #VIC_CYCLES_PER_LINE
        sta vic_cycle_accum

        ; === CIA1 Timer A countdown (approximate: per-line) ===
        ; Only count if timer is running (bit 0 of control)
        lda cia1_timer_a_ctrl
        and #$01
        beq _timer_a_no_borrow

        ; C128 KERNAL sets Timer A to ~$4025 for ~60Hz IRQ
        ; We subtract 63 per scanline as approximation
        lda cia1_timer_a_lo
        sec
        sbc #VIC_CYCLES_PER_LINE
        sta cia1_timer_a_lo
        bcs _timer_a_no_borrow

        ; Borrow from high byte
        dec cia1_timer_a_hi
        lda cia1_timer_a_hi
        cmp #$FF                ; Underflow?
        bne _timer_a_no_borrow

        ; Timer A underflowed - reload from latch
        lda cia1_timer_a_latch_lo
        sta cia1_timer_a_lo
        lda cia1_timer_a_latch_hi
        sta cia1_timer_a_hi

        ; Set Timer A IRQ flag in CIA1 ICR (bit 0)
        lda cia1_icr_data
        ora #$01
        sta cia1_icr_data

        ; Check if Timer A IRQ enabled (CIA1 ICR mask bit 0)
        lda cia1_icr_mask
        and #$01
        beq _timer_a_no_borrow

        ; Trigger IRQ
        lda cia1_icr_data
        ora #$80                ; Set IR bit
        sta cia1_icr_data
        lda #1
        sta c128_irq_pending

_timer_a_no_borrow:
        ; Advance raster line
        inc vic_raster_lo
        bne _vic_check_wrap
        inc vic_raster_hi

_vic_check_wrap:
        ; Wrap at 312 lines (PAL)
        lda vic_raster_hi
        cmp #$01
        bcc _vic_check_raster_irq
        lda vic_raster_lo
        cmp #$38                ; 312 = $138
        bcc _vic_check_raster_irq

        ; Frame complete - wrap to line 0
        lda #0
        sta vic_raster_lo
        sta vic_raster_hi

        ; Per-frame tasks
        jsr VIC_FrameTasks

_vic_check_raster_irq:
        ; VIC-II raster compare: $D012 (low byte) + bit 7 of $D011 (high bit)
        lda vic_raster_compare_lo
        cmp vic_raster_lo
        bne _vic_next_line
        lda vic_raster_compare_hi
        cmp vic_raster_hi
        bne _vic_next_line

        ; Raster match - set raster IRQ flag (bit 0 of $D019)
        lda vic_regs+$19
        and #$01
        bne _vic_next_line      ; Already flagged

        lda vic_regs+$19
        ora #$01                ; Set raster flag
        sta vic_regs+$19

        ; Check if raster IRQ enabled (bit 0 of $D01A)
        lda vic_regs+$1A
        and #$01
        beq _vic_next_line

        ; Trigger IRQ
        lda vic_regs+$19
        ora #$80                ; Set IRQ flag
        sta vic_regs+$19
        lda #1
        sta c128_irq_pending

_vic_next_line:
        ; More lines to process?
        lda vic_cycle_accum
        cmp #VIC_CYCLES_PER_LINE
        bcs _vic_line_loop
        rts


; ============================================================
; VIC_FrameTasks - Called once per frame (at raster line 0)
; ============================================================
VIC_FrameTasks:
        ; Keyboard injection - read MEGA65 hardware keyboard and inject
        ; into C128 keyboard buffer (replaces emulated CIA matrix scan)
        jsr C128_KeyboardInject

        ; VDC 80-column rendering (only when in 80-col mode)
        lda vdc_mode_active
        beq _skip_vdc_render
        jsr VDC_RenderFrame
_skip_vdc_render:

        ; Cursor blink (text mode only)
        lda c128_video_mode
        bne _frame_done

        inc c128_cur_div
        lda c128_cur_div
        cmp #8
        bcc _frame_done

        lda #0
        sta c128_cur_div
        lda c128_cur_phase
        eor #1
        sta c128_cur_phase
        ; TODO: jsr C128_VID_UpdateCursor

_frame_done:
        rts

; ============================================================
; C128_KeyboardInject - Read MEGA65 keyboard and inject into
; C128 keyboard buffer. Called once per frame from VIC_FrameTasks.
;
; MEGA65 $D619: PETSCII keycode (read dequeues, 0 = empty)
;
; C128 keyboard buffer:
;   $034A-$0353 = KEYD (10 bytes)
;   $D0 (ZP)    = NDX (number of chars in buffer)
; ============================================================
C128_KEYD_OFFSET = $034A        ; Keyboard buffer start in C128 RAM
C128_KEYD_MAX    = 10           ; Max buffer size
C128_NDX_ZP      = $D0          ; ZP address of buffer count

C128_KeyboardInject:
        ; Read MEGA65 hardware keyboard buffer (PETSCII)
        lda $D619
        beq _ki_done            ; $00 = no key pending
        cmp #$FF
        beq _ki_done            ; $FF = no key pending

        ; Dequeue the key by writing 1 to $D610
        pha
        lda #$01
        sta $D610
        pla

        ; A = PETSCII code, ready to inject
        ; Check if buffer has room
        pha                     ; Save PETSCII code
        lda #C128_NDX_ZP
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z     ; A = current buffer count
        cmp #C128_KEYD_MAX
        bcs _ki_full            ; Buffer full

        ; Store key at KEYD[NDX]
        ; Write to bank4:$034A+NDX
        tax                     ; X = NDX (buffer index)
        pla                     ; A = PETSCII code

        ; Set up pointer to bank4:$034A+X
        pha                     ; Save code again
        txa
        clc
        adc #<C128_KEYD_OFFSET
        sta C128_MEM_PTR+0
        lda #>C128_KEYD_OFFSET
        adc #0
        sta C128_MEM_PTR+1
        lda #BANK_RAM0
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        pla                     ; Get code back
        sta [C128_MEM_PTR],z    ; Write to keyboard buffer

        ; Increment NDX
        lda #C128_NDX_ZP
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        clc
        adc #1
        sta [c128_zp_ptr],z

_ki_done:
        rts

_ki_full:
        pla                     ; Discard key code
        rts


; ============================================================
; Hook routines for init code that hangs in emulation
; ============================================================

; hook_vdc_screen_clear - Skip $CE0C VDC RAM clear loop
; In 80-col mode, we must actually clear VDC screen and attribute RAM.
; The KERNAL clears screen ($0000-$07CF) with spaces ($20) and
; attributes ($0800-$09CF) with the default attribute byte.
; We do this via DMA fill to bank 3 VDC RAM at $32000, then set VDC regs as the
; KERNAL would have after the loop, and skip to the RTS at $CE4B.
hook_vdc_screen_clear:
        ; Clear VDC screen RAM ($0000-$07CF) with $20 (space)
        ; VDC RAM is at $32000: screen at $32000, attrs at $32800
        lda #$00
        sta $D707               ; Enhanced DMA job
        .byte $80, $00          ; src MB = $00 (don't care for fill)
        .byte $81, $00          ; dst MB = $00 (chip RAM)
        .byte $00               ; end options
        .byte $03               ; fill command
        .word 2000              ; count = 2000 bytes
        .word $0020             ; fill value = $20 (space char)
        .byte $00               ; src bank (ignored for fill)
        .word $2000             ; dst addr = $2000 ($32000 = VDC screen)
        .byte $03               ; dst bank = 3
        .byte $00               ; command high byte
        .word $0000             ; modulo

        ; Clear VDC attribute RAM ($0800-$09CF) with default attr
        ; Default: $07 = light cyan foreground, no flags
        lda #$00
        sta $D707               ; Enhanced DMA job
        .byte $80, $00          ; src MB = $00 (don't care for fill)
        .byte $81, $00          ; dst MB = $00 (chip RAM)
        .byte $00               ; end options
        .byte $03               ; fill command
        .word 2000              ; count = 2000 bytes
        .word $0007             ; fill value = $07 (cyan, no flags)
        .byte $00               ; src bank (ignored for fill)
        .word $2800             ; dst addr = $2800 ($32800 = VDC attrs)
        .byte $03               ; dst bank = 3
        .byte $00               ; command high byte
        .word $0000             ; modulo

        ; Update VDC shadow registers as KERNAL would after clear
        ; R18:R19 = address after last write
        lda #$20
        ldx #18
        sta vdc_regs,x
        lda #$00
        ldx #19
        sta vdc_regs,x

        ; Update guest ZP $DA/$DB (used by KERNAL screen editor)
        lda #$DA
        sta c128_zp_ptr+0
        ldz #0
        lda #$00
        sta [c128_zp_ptr],z
        inc c128_zp_ptr+0       ; $DB
        lda #$E0
        sta [c128_zp_ptr],z

        ; Skip to RTS at $CE4B
        lda #1
        sta vdc_screen_dirty    ; DMA filled screen RAM
        sta vdc_attr_dirty      ; DMA filled attribute RAM
        lda #$4B
        sta c128_pc_lo
        lda #$CE
        sta c128_pc_hi
        lda #0
        sta c128_code_valid
        rts

; hook_raster_wait_1 - Skip "LDA $D011 / BPL $E142" at $E142
; This waits for raster >= 256. Skip to $E147.
hook_raster_wait_1:
        lda #$47
        sta c128_pc_lo
        lda #$E1
        sta c128_pc_hi
        lda #0
        sta c128_code_valid
        lda #4
        #finish_cycles_inline
        rts

; hook_raster_wait_2 - Skip "LDA $D011 / BMI $E147" at $E14E
; This waits for raster < 256. Skip to $E153.
hook_raster_wait_2:
        lda #$53
        sta c128_pc_lo
        lda #$E1
        sta c128_pc_hi
        lda #0
        sta c128_code_valid
        lda #4
        #finish_cycles_inline
        rts

; _hook_check_keybuf - Read C128 ZP $D0 (keybuf count) and $D1 (prog key count)
; Returns: Z flag set if both are zero (empty), Z clear if key waiting
hook_check_keybuf:
        lda #$D0
        sta C128_MEM_PTR+0
        lda #$00
        sta C128_MEM_PTR+1
        lda #BANK_RAM0          ; Bank 4 = C128 RAM bank 0
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z    ; Read $D0 (keyboard buffer count)
        bne +                   ; Key waiting - return NZ
        inz
        lda [C128_MEM_PTR],z    ; Read $D1 (prog key string count)
+       rts

; hook_keyboard_idle - Skip one iteration of the $C25E idle loop
; The tight loop (LDA $D0 / ORA $D1 / BEQ $C25E) is 9 cycles.
; We skip the instruction decode overhead by just adding 9 cycles.
; This saves the ~25 MEGA65 cycles of instruction decode per iteration
; while keeping the C128 timing identical.
hook_keyboard_idle:
        lda #63
        #finish_cycles_no_xtra
        rts

; write_milestone - Write progress byte A to $0FE0F
; Uses 32-bit pointer to write to host RAM
write_milestone:
        pha
        lda #$0F
        sta C128_MEM_PTR+0
        lda #$FE
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+2
        sta C128_MEM_PTR+3
        ldz #0
        pla
        sta [C128_MEM_PTR],z
        rts

; write_loop_counter - Increment byte at $0FE0E
; Shows how many times the VDC init loop at E1DC iterates
write_loop_counter:
        lda #$0E
        sta C128_MEM_PTR+0
        lda #$FE
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+2
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        inc a
        sta [C128_MEM_PTR],z
        rts

; ============================================================
; CIA1 Timer State
; ============================================================
cia1_timer_a_lo:       .byte $25     ; Current Timer A value
cia1_timer_a_hi:       .byte $40     ; (default $4025 for ~60Hz)
cia1_timer_a_latch_lo: .byte $25     ; Timer A latch (reload value)
cia1_timer_a_latch_hi: .byte $40
cia1_icr_data:         .byte $00     ; CIA1 ICR data (read clears)
cia1_icr_mask:         .byte $00     ; CIA1 ICR mask (written)
cia1_timer_a_ctrl:     .byte $00     ; CIA1 Timer A control

; VIC-II raster state
vic_cycle_accum:       .byte 0
vic_raster_lo:         .byte 0
vic_raster_hi:         .byte 0
vic_raster_compare_lo: .byte $FF     ; Raster compare low byte
vic_raster_compare_hi: .byte $01     ; Raster compare high bit

; Cursor state
c128_cur_div:          .byte 0
c128_cur_phase:        .byte 0
pc_trace_idx:          .byte 0
pc_trace_hi:           .fill 128, 0
pc_trace_lo:           .fill 128, 0
pc_trace_sp:           .fill 128, 0

; ============================================================
; cpu_take_irq - Execute IRQ sequence
; Push PC, push P (with B=0), set I flag, load vector from $FFFE
; ============================================================
cpu_take_irq:

        ; Push PC high
        lda c128_pc_hi
        sta c128_data
        jsr push_data
        ; Push PC low
        lda c128_pc_lo
        sta c128_data
        jsr push_data
        ; Push P with B=0, U=1
        lda c128_p
        and #(~P_B) & $ff       ; Clear B flag
        ora #P_U
        sta c128_data
        jsr push_data
        ; Set I flag
        lda c128_p
        ora #P_I
        sta c128_p
        ; Load IRQ vector from $FFFE/$FFFF - ALWAYS from ROM
        ; On real C128, vectors bypass MMU and always read from KERNAL ROM
        ; $FFFE is in $F800+ range, so read from relocation area at $12000
        ; $FFFE -> $12000 + ($FFFE - $F800) = $127FE
        lda #$FE
        sta C128_MEM_PTR+0
        lda #$27                ; $F8->$20, $FF->$27
        sta C128_MEM_PTR+1
        lda #BANK_ROM           ; bank 1
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z    ; read $127FE = lo byte
        sta c128_pc_lo
        inz
        lda [C128_MEM_PTR],z    ; read $127FF = hi byte
        sta c128_pc_hi
        rts

; ============================================================
; cpu_take_nmi - Execute NMI sequence
; Push PC, push P (with B=0), set I flag, load vector from $FFFA
; ============================================================
cpu_take_nmi:
        ; Push PC high
        lda c128_pc_hi
        sta c128_data
        jsr push_data
        ; Push PC low
        lda c128_pc_lo
        sta c128_data
        jsr push_data
        ; Push P with B=0, U=1
        lda c128_p
        and #(~P_B) & $ff
        ora #P_U
        sta c128_data
        jsr push_data
        ; Set I flag
        lda c128_p
        ora #P_I
        sta c128_p
        ; Load NMI vector from $FFFA/$FFFB - ALWAYS from ROM
        ; $FFFA -> $12000 + ($FFFA - $F800) = $127FA
        lda #$FA
        sta C128_MEM_PTR+0
        lda #$27                ; $FF->$27
        sta C128_MEM_PTR+1
        lda #BANK_ROM           ; bank 1
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z    ; read $127FA = lo byte
        sta c128_pc_lo
        inz
        lda [C128_MEM_PTR],z    ; read $127FB = hi byte
        sta c128_pc_hi
        rts

; --- Reset/Step ---
C128CPU_Reset:
C128_CPUReset:
        ; Reset video mode first
        lda #0
        sta c128_video_mode
        sta c128_file_op_active
        
        ; Re-initialize memory system (MMU, VIC, CIA shadows)
        jsr C128_MemInit
        
        ; Re-initialize video 
        jsr C128_VideoInit
        
        ; Reset hook state
        jsr C128Hook_Reset

        ; ============================================================
        ; Simulate Z80 boot initialization
        ; On a real C128, the Z80 runs first after power-on and writes
        ; critical data to RAM before switching to the 8502.
        ; We must pre-populate RAM with what the Z80 would have written.
        ; ============================================================
        jsr z80_pre_init
        
        ; Reset CPU registers
        lda #$00
        sta c128_a
        sta c128_x
        sta c128_y
        sta c128_irq_pending
        sta c128_nmi_pending
        sta vic_cycle_accum
        sta vic_raster_lo
        sta vic_raster_hi

        ; Initialize CIA1 Timer A for ~60Hz IRQ ($4025 latch)
        lda #$25
        sta cia1_timer_a_lo
        sta cia1_timer_a_latch_lo
        lda #$40
        sta cia1_timer_a_hi
        sta cia1_timer_a_latch_hi
        lda #$81               ; Enable Timer A IRQ in CIA1
        sta cia1_icr_mask
        lda #$11               ; Start Timer A, continuous
        sta cia1_timer_a_ctrl
        lda #$00
        sta cia1_icr_data

        lda #$FF
        sta c128_sp
        lda #(P_I|P_U)
        sta c128_p

        ; Get reset vector from ROM ($FFFC/$FFFD)
        lda #$fc
        sta c128_addr_lo
        lda #$ff
        sta c128_addr_hi
        #read_data_fast
        sta c128_pc_lo
        lda #$fd
        sta c128_addr_lo
        #read_data_fast
        sta c128_pc_hi
        lda #0
        sta c128_code_valid
        rts

; VDC test program data removed (test passed - VDC pipeline works)
; To re-enable, uncomment and change C128_CPUReset to inject at $3000

; ============================================================
; fix_80col_zp_vars - Write correct 80-col screen editor ZP values
; Called once when BASIC starts ($B000) in 80-col mode.
; CINT's screen clear corrupts $E0-$FA with $20 (spaces).
; We restore them to the correct values from VICE.
; Also fix $D5 (LNMX) and $DA/$DB (screen line ptr).
; ============================================================
fix_80col_zp_vars:
        ; Write $E0-$FA from table
        ldx #0
_fix_zp_loop:
        lda vice_80col_zp,x
        sta c128_data
        txa
        clc
        adc #$E0
        sta c128_addr_lo
        lda #$00
        sta c128_addr_hi
        phx
        #write_data_fast
        plx
        inx
        cpx #27                 ; 27 bytes: $E0-$FA
        bcc _fix_zp_loop

        ; Fix $D5 (LNMX) = $4F (79 = 80 columns - 1)
        lda #$4F
        sta c128_data
        lda #$D5
        sta c128_addr_lo
        lda #$00
        sta c128_addr_hi
        #write_data_fast

        ; Fix $D7 (MODE) = $80 (80-col flag)
        lda #$80
        sta c128_data
        lda #$D7
        sta c128_addr_lo
        #write_data_fast

        ; Fix $DA (screen line ptr lo) = $00
        lda #$00
        sta c128_data
        lda #$DA
        sta c128_addr_lo
        #write_data_fast

        ; Fix $DB (screen line ptr hi) = $04
        lda #$04
        sta c128_data
        lda #$DB
        sta c128_addr_lo
        #write_data_fast

        ; Fix $0A03 = $50 (Z80 boot signature)
        lda #$50
        sta c128_data
        lda #$03
        sta c128_addr_lo
        lda #$0A
        sta c128_addr_hi
        #write_data_fast

        rts

; 80-col screen editor ZP values ($E0-$FA) from VICE
vice_80col_zp:
        .byte $30, $02, $30, $0A, $18, $00, $00, $4F
        .byte $07, $00, $05, $07, $00, $18, $4F, $0D
        .byte $0D, $07, $07, $00, $00, $00, $00, $00
        .byte $00, $00, $07
        lda #$fc
        sta c128_addr_lo
        lda #$ff
        sta c128_addr_hi
        #read_data_fast
        sta c128_pc_lo
        lda #$fd
        sta c128_addr_lo
        #read_data_fast
        sta c128_pc_hi
        rts

; ============================================================
; z80_pre_init - Simulate what the Z80 boot ROM writes to RAM
;
; The Z80 runs first on a real C128 and does:
;   1. Writes 8502->Z80 switchover routine at $FFD0-$FFDF
;   2. Writes Z80->8502 switchover routine at $FFE0-$FFEF
;   3. Sets INIT_STATUS ($0A04) to 0
;   4. Sets $0A03 to initial value
;   5. Writes 8502 boot trampoline at $1100
;
; All writes go to C128 RAM bank 0 (MEGA65 bank 4)
; ============================================================
z80_pre_init:
        ; Use DMA to copy the Z80-prepared RAM blocks into bank 4

        ; --- Write 8502->Z80 switch routine at $FFD0 ---
        ; FFD0: SEI / LDA #$3E / STA $FF00 / LDA #$B0 / STA $D505 / NOP / JMP $1100 / NOP
        ldx #0
z80_wr_ffd0:
        lda z80_ffd0_data,x
        sta c128_data
        txa
        clc
        adc #$D0
        sta c128_addr_lo
        lda #$FF
        sta c128_addr_hi
        phx
        #write_data_fast
        plx
        inx
        cpx #16
        bcc z80_wr_ffd0

        ; --- Write Z80->8502 switch routine at $FFE0 ---
        ; FFE0: DI / LD A,$3E / LD ($FF00),A / LD BC,$D505 / LD A,$B1 / OUT (C),A / NOP / RST 08
        ldx #0
z80_wr_ffe0:
        lda z80_ffe0_data,x
        sta c128_data
        txa
        clc
        adc #$E0
        sta c128_addr_lo
        lda #$FF
        sta c128_addr_hi
        phx
        #write_data_fast
        plx
        inx
        cpx #16
        bcc z80_wr_ffe0

        ; --- Write 8502 boot trampoline at $1100 ---
        ; 1100: LDA #$00 / STA $FF00 / JMP ($FFFC)
        ldx #0
z80_wr_1100:
        lda z80_1100_data,x
        sta c128_data
        txa
        clc
        adc #$00
        sta c128_addr_lo
        lda #$11
        sta c128_addr_hi
        phx
        #write_data_fast
        plx
        inx
        cpx #8
        bcc z80_wr_1100

        ; --- Set INIT_STATUS ($0A04) = $14 ---
        ; (Matches real C128 Z80 boot value from VICE)
        lda #$14
        sta c128_data
        lda #$04
        sta c128_addr_lo
        lda #$0A
        sta c128_addr_hi
        #write_data_fast

        ; --- Set $0A03 = $50 (Z80 boot signature) ---
        ; On real C128, the Z80 boot writes $50 here regardless of 40/80 mode.
        ; The 40/80 column mode is determined by mmu_mcr bit 7 ($D505), not $0A03.
        lda #$50
        sta c128_data
        lda #$03
        sta c128_addr_lo
        lda #$0A
        sta c128_addr_hi
        #write_data_fast

        ; --- Set warm start vector $0A00-$0A01 = $4000 (BASIC LO cold start) ---
        ; The Z80 boot sets this so MONITOR's X command (JMP ($0A00)) enters
        ; BASIC LO cold start, which initializes $0300-$0313 vectors and
        ; then changes $0A00 to $03 (warm start entry $4003) for subsequent use.
        lda #$00
        sta c128_data
        lda #$00
        sta c128_addr_lo
        lda #$0A
        sta c128_addr_hi
        #write_data_fast
        lda #$40
        sta c128_data
        lda #$01
        sta c128_addr_lo
        #write_data_fast

        rts

; 8502->Z80 switchover routine (written to RAM $FFD0-$FFDF)
z80_ffd0_data:
        .byte $78               ; FFD0: SEI
        .byte $A9, $3E          ; FFD1: LDA #$3E
        .byte $8D, $00, $FF     ; FFD3: STA $FF00
        .byte $A9, $B0          ; FFD6: LDA #$B0
        .byte $8D, $05, $D5     ; FFD8: STA $D505
        .byte $EA               ; FFDB: NOP
        .byte $4C, $00, $11     ; FFDC: JMP $1100
        .byte $EA               ; FFDF: NOP

; Z80->8502 switchover routine (written to RAM $FFE0-$FFEF)
; (These are Z80 opcodes, not 6502)
z80_ffe0_data:
        .byte $F3               ; FFE0: DI
        .byte $3E, $3E          ; FFE1: LD A,$3E
        .byte $32, $00, $FF     ; FFE3: LD ($FF00),A
        .byte $01, $05, $D5     ; FFE6: LD BC,$D505
        .byte $3E, $B1          ; FFE9: LD A,$B1
        .byte $ED, $79          ; FFEB: OUT (C),A
        .byte $EA               ; FFED: NOP (filler, not executed)
        .byte $CF               ; FFEE: RST 08

; 8502 boot trampoline (written to RAM $1100)
z80_1100_data:
        .byte $A9, $00          ; 1100: LDA #$00
        .byte $8D, $00, $FF     ; 1102: STA $FF00
        .byte $6C, $FC, $FF     ; 1105: JMP ($FFFC)

; ============================================================
; Trace/breakpoint for debugging
; ============================================================
TRACE_ENABLED = 0       ; Disabled - no breakpoint checking

; Breakpoint address - halt when PC hits this
BREAK_ADDR_LO = $00
BREAK_ADDR_HI = $FD     ; Break at $FD00

; ============================================================
; C128CPU_StepMultiple - Execute multiple instructions
; Interrupt checks inlined into batch loop to reduce overhead
; ============================================================
BATCH_SIZE = 64         ; Number of instructions per batch

C128CPU_StepMultiple:
C128_CPUStepMultiple:
        ; Check monitor ONCE per batch, not per instruction
        jsr C128Mon_Check
        bcs sm_monitor_active  ; Monitor took over, skip batch
        
        lda #BATCH_SIZE+1       ; +1 because finish_and_loop decrements first
        sta sm_counter

sm_batch_loop:
        jmp finish_and_loop     ; Enter the main execute loop

sm_monitor_active:
        rts

sm_counter: .byte 0

; ============================================================
; finish_and_loop - End of instruction trampoline
; Called from finish macros via jmp. Handles batch counter,
; interrupt check, hook check, fetch, and dispatch - all without
; any JSR/RTS overhead back to the batch loop.
; Saves ~15 cycles per instruction vs old jsr StepDecode path.
; ============================================================
finish_and_loop:
        dec sm_counter
        beq _fal_done           ; Batch complete

        ; --- Interrupt check (inlined) ---
        lda c128_nmi_pending
        bne _fal_do_nmi
        lda c128_irq_pending
        beq _fal_dispatch
        lda c128_p
        and #P_I
        beq _fal_do_irq

_fal_dispatch:
        ; --- Hook check + fetch ---
        lda c128_pc_hi
        bpl step_fetch          ; PC < $8000: skip hooks, go to fetch
        jmp C128CPU_StepDecode_hooks

_fal_do_nmi:
        lda #0
        sta c128_nmi_pending
        jsr cpu_take_nmi
        lda #7
        #finish_cycles_inline
        ; loops back via jmp finish_and_loop

_fal_do_irq:
        lda #0
        sta c128_irq_pending
        jsr cpu_take_irq
        lda #7
        #finish_cycles_inline
        ; loops back via jmp finish_and_loop

_fal_done:
        rts                     ; Return to main loop

; ============================================================
; C128CPU_Step - Entry point for single instruction (for external callers)
; ============================================================
C128CPU_Step:
        ; Single instruction mode: set batch counter to 1
        ; so the finish macros will rts after one instruction
        lda #1
        sta sm_counter
        lda c128_nmi_pending
        bne _css_nmi
        lda c128_irq_pending
        beq C128CPU_StepDecode
        lda c128_p
        and #P_I
        bne C128CPU_StepDecode
        lda #0
        sta c128_irq_pending
        jsr cpu_take_irq
        lda #7
        #finish_cycles_inline
        ; finish_cycles_inline handles rts when counter reaches 0
_css_nmi:
        lda #0
        sta c128_nmi_pending
        jsr cpu_take_nmi
        lda #7
        #finish_cycles_inline
        ; finish_cycles_inline handles rts when counter reaches 0

; ============================================================
; C128CPU_StepDecode - Fetch, decode, and execute one instruction
; (No interrupt check - caller handles that)
; ============================================================
C128CPU_StepDecode:
C128CPU_StepDecode_hooks:
        ; (c128_xtra is initialized by addressing mode helpers that need it)

        ; --------------------------------------------------------
        ; Hooks - only check for ROM area ($8000+)
        ; First check inline hooks (fast), then call hooks file
        ; --------------------------------------------------------
        lda c128_pc_hi
        bpl step_fetch         ; Skip all hooks if PC < $8000

        ; --- Inline hooks for busy-wait loops ---
        ; A = c128_pc_hi throughout this chain (reloaded only when clobbered)

        ; Most common KERNAL pages are $C0-$CF, $E0-$FF, $F8-$FF
        ; Order checks by frequency to minimize comparisons

        ; Hook VDC polling loops at $CDCF, $CDDD
        cmp #$CD
        bne _hook_not_cdxx
        lda c128_pc_lo
        cmp #$CF
        beq _hook_vdc_poll_skip
        cmp #$DD
        beq _hook_vdc_poll_skip
        jmp _hook_chain_kernal
_hook_not_cdxx:

        ; Hook $CE0C: VDC screen clear (skip the polling loop)
        cmp #$CE
        bne _hook_not_ce
        lda c128_pc_lo
        cmp #$0C
        bne _hook_chain_kernal_reload
        jsr hook_vdc_screen_clear
        lda #4
        #finish_cycles_inline
        rts
_hook_not_ce:

        ; Hook $C543: VDC poll
        cmp #$C5
        bne _hook_not_c5xx
        lda c128_pc_lo
        cmp #$43
        beq _hook_c543_vdc_poll
        ; NOTE: $C55D SCNKEY hook disabled - skipping it broke the
        ; KERNAL IRQ handler flow (screen editor, cursor blink, scroll).
        ; Keyboard injection via $D619 in VIC_FrameTasks works without
        ; this hook since injected keys go directly into the buffer.
        jmp _hook_chain_kernal_reload
_hook_c543_vdc_poll:
        lda #$48
        sta c128_pc_lo
        lda #0
        sta c128_code_valid
        lda #4
        #finish_cycles_inline
        rts
_hook_not_c5xx:

        ; Hook $E1xx: raster waits and delay loop
        cmp #$E1
        bne _hook_not_e1
        lda c128_pc_lo
        cmp #$42
        beq hook_raster_wait_1
        cmp #$4E
        beq hook_raster_wait_2
        ; E1A8 = delay loop (DEY/BNE/DEX/BNE, 65K iterations) - skip it
        cmp #$A8
        bne _hook_chain_kernal_reload
        lda #$AE
        sta c128_pc_lo          ; Skip to $E1AE
        lda #$00
        sta c128_x              ; X=0 (loop finished)
        sta c128_y              ; Y=0 (loop finished)
        sta c128_code_valid
        lda #4
        #finish_cycles_inline
        rts
_hook_not_e1:

        ; Hook $E03A: PLA reads cold/warm start flag (boot only)
        cmp #$E0
        bne _hook_chain_kernal
        lda c128_pc_lo
        cmp #$3A
        bne _hook_chain_kernal_reload
        lda #$80
        sta c128_a
        lda c128_p
        ora #P_N
        and #<~(P_Z)
        sta c128_p
        inc c128_pc_lo
        bne +
        inc c128_pc_hi
+       inc c128_sp
        lda #0
        sta c128_code_valid
        lda #4
        #finish_cycles_inline
        rts

_hook_vdc_poll_skip:
        lda c128_pc_lo
        clc
        adc #$05
        sta c128_pc_lo
        lda #0
        sta c128_code_valid
        lda #4
        #finish_cycles_inline
        rts

_hook_chain_kernal_reload:
        lda c128_pc_hi
_hook_chain_kernal:
        ; --- KERNAL call hooks (LOAD, SAVE, OPEN, etc.) ---
        ; Only call if PC is in hook-relevant pages ($A8, $C8, $F8, $FF)
        cmp #$FF
        beq _hook_do_check
        cmp #$F8
        beq _hook_do_check
        cmp #$C8
        beq _hook_do_check
        cmp #$A8
        beq _hook_do_check
        jmp step_fetch         ; No hooks for this page

_hook_do_check:
        lda #0
        sta c128_hook_pc_changed
        jsr C128Hook_CheckAndRun
        lda c128_hook_pc_changed
        beq step_fetch
        lda #0
        sta c128_code_valid
        
step_fetch:
        ; --------------------------------------------------------
        ; Fetch opcode and dispatch (inlined fast path)
        ; --------------------------------------------------------
        lda c128_code_valid
        beq _step_fetch_slow
        lda c128_pc_hi
        cmp c128_code_page_hi
        bne _step_fetch_slow
        ; Cache hit - read directly
        ldz c128_pc_lo
        lda [c128_code_ptr],z
        inw c128_pc_lo
_step_dispatch:
        asl                     ; opcode * 2, carry set if opcode >= $80
        tax
        bcc _step_dispatch_lo
        jmp (op_table_hi,x)     ; opcodes $80-$FF
_step_dispatch_lo:
        jmp (op_table_lo,x)     ; opcodes $00-$7F

_step_fetch_slow:
        jsr fetch8
        jmp _step_dispatch

; ============================================================
; finish_cycles - Handles cycle accounting, returns with RTS
; A = base cycle count on entry
; ============================================================
finish_cycles:
        ; Add extra cycles and accumulate
        clc
        adc c128_xtra
        adc vic_cycle_accum
        sta vic_cycle_accum
        
        ; Check if we've completed a scanline (63+ cycles)
        cmp #VIC_CYCLES_PER_LINE
        bcc _fc_done
        
        ; Need scanline processing
        jsr finish_do_scanline

_fc_done:
        rts

op_illegal:
        ; ILLEGAL OPCODE - just halt with red border
        ; The live trace above already printed where we are
        lda #$02
        sta $D020
_ill_halt:
        jmp _ill_halt

; ---- Convert nibble in A to screen code ----
; 0-9 -> $30-$39, A-F -> $01-$06
trace_to_scrcode:
        cmp #$0A
        bcc trace_sc_digit
        sec
        sbc #$09                ; $0A->$01, $0F->$06
        rts
trace_sc_digit:
        ora #$30                ; $00->$30, $09->$39
        rts

live_trace_on: .byte 0
trace_last_page: .byte 0
trace_line_num: .byte 5
trace_count_lo: .byte 0
trace_count_hi: .byte 0

; --- branch_do ---
; Called when branch is taken. Sets c128_xtra to:
;   1 = branch taken, no page cross
;   2 = branch taken, page crossed
branch_do:
        lda #$01                ; Start with 1 (branch taken)
        sta c128_xtra
        lda c128_pc_lo
        clc
        adc c128_tmp
        sta c128_addr_lo
        lda c128_pc_hi
        adc #$00
        sta c128_addr_hi
        lda c128_tmp
        bpl _br_hi_ok
        dec c128_addr_hi
_br_hi_ok:
        lda c128_pc_hi
        cmp c128_addr_hi
        beq _br_same
        inc c128_xtra             ; Page crossed, increment to 2
_br_same:
        lda c128_addr_lo
        sta c128_pc_lo
        lda c128_addr_hi
        sta c128_pc_hi
        rts

; --- do_adc ---  (supports decimal mode when P_D set)
do_adc:
        sta c128_tmp

        ; If D flag clear -> original binary path
        lda c128_p
        and #P_D
        beq _do_adc_bin

        ; ---------- decimal ADC ----------
        ; Save original A
        lda c128_a
        sta c128_dec_a

        ; carry_in -> c128_vec_lo (0/1)
        lda c128_p
        and #P_C
        beq _adc_dec_c0
        lda #1
        bne _adc_dec_cstore
_adc_dec_c0:
        lda #0
_adc_dec_cstore:
        sta c128_vec_lo

        ; Binary add first (for V computation)
        lda c128_vec_lo
        beq _adc_dec_clc
        sec
        bne _adc_dec_go
_adc_dec_clc:
        clc
_adc_dec_go:
        lda c128_dec_a
        adc c128_tmp
        sta c128_a
        php                     ; save binary flags (esp V)

        ; Low nibble adjust test: (A_lo + M_lo + carry_in) > 9 ?
        lda c128_tmp
        and #$0F
        sta c128_tmp2             ; m_lo
        lda c128_dec_a
        and #$0F
        clc
        adc c128_tmp2
        clc
        adc c128_vec_lo           ; + carry_in (0/1)
        cmp #$0A
        bcc _adc_dec_no6
        lda c128_a
        clc
        adc #$06
        sta c128_a
_adc_dec_no6:

        ; High adjust if result >= $9A (i.e., > 99 in BCD)
        lda #0
        sta c128_vec_hi           ; decimal carry out (0/1)
        lda c128_a
        cmp #$9A
        bcc _adc_dec_no60
        clc
        adc #$60
        sta c128_a
        lda #1
        sta c128_vec_hi
_adc_dec_no60:

        ; Restore binary flags for V via PLP, then rebuild c128_p C/V
        plp
        lda c128_p
        and #(~(P_C|P_V)) & $ff
        sta c128_p

        ; C from decimal carry (c128_vec_hi)
        lda c128_vec_hi
        beq _adc_dec_noc
        lda c128_p
        ora #P_C
        sta c128_p
_adc_dec_noc:

        ; V from binary add (host V flag after PLP)
        bvc _adc_dec_nov
        lda c128_p
        ora #P_V
        sta c128_p
_adc_dec_nov:
        lda c128_a
        ;jsr set_zn_a
        #set_zna
        rts

_do_adc_bin:
        ; ---------- your original binary ADC ----------
        lda c128_p
        and #P_C
        beq _adc_nc
        sec
        jmp _adc_go2
_adc_nc:
        clc
_adc_go2:
        lda c128_a
        adc c128_tmp
        sta c128_a
        php
        lda c128_p
        and #(~(P_C|P_V)) & $ff
        sta c128_p
        plp
        bcc _adc_noc2
        lda c128_p
        ora #P_C
        sta c128_p
_adc_noc2:
        bvc _adc_nov2
        lda c128_p
        ora #P_V
        sta c128_p
_adc_nov2:
        lda c128_a
        ;jsr set_zn_a
        #set_zna
        rts


; --- do_sbc --- (supports decimal mode when P_D set)
do_sbc:
        sta c128_tmp

        ; If D flag clear -> original binary path
        lda c128_p
        and #P_D
        bne _do_sbc_decimal      ; D set, do decimal
        jmp _do_sbc_bin          ; D clear, do binary

_do_sbc_decimal:
        ; ---------- decimal SBC ----------
        ; Save original A for nibble comparisons
        lda c128_a
        sta c128_dec_a

        ; Do binary subtraction first (for V flag)
        lda c128_p
        and #P_C
        beq _sbc_dec_clc
        sec
        bne _sbc_dec_go
_sbc_dec_clc:
        clc
_sbc_dec_go:
        lda c128_a
        sbc c128_tmp
        sta c128_a
        php                     ; save flags for V

        ; Save carry (1 = no borrow, 0 = borrow)
        lda #0
        rol                     ; A = carry (0 or 1)
        sta c128_vec_hi           ; save for later

        ; Check if low nibble needs adjustment
        ; If (A_lo & $0F) > (orig_A_lo & $0F), we had a borrow from high nibble
        lda c128_a
        and #$0F
        sta c128_tmp2             ; result low nibble
        lda c128_dec_a
        and #$0F                ; original low nibble
        cmp c128_tmp2
        bcs _sbc_no_lo_adj      ; orig >= result, no low borrow
        ; Low nibble borrowed, subtract 6
        lda c128_a
        sec
        sbc #$06
        sta c128_a
_sbc_no_lo_adj:

        ; Check if high nibble needs adjustment
        ; If we had an overall borrow (carry was 0), subtract $60
        lda c128_vec_hi
        bne _sbc_no_hi_adj      ; carry was 1, no borrow
        lda c128_a
        sec
        sbc #$60
        sta c128_a
_sbc_no_hi_adj:

        ; Restore flags and set C/V in c128_p
        plp
        lda c128_p
        and #(~(P_C|P_V)) & $ff
        sta c128_p

        ; Set C from saved carry
        lda c128_vec_hi
        beq _sbc_dec_noc
        lda c128_p
        ora #P_C
        sta c128_p
_sbc_dec_noc:

        ; Set V from binary subtract
        bvc _sbc_dec_nov
        lda c128_p
        ora #P_V
        sta c128_p
_sbc_dec_nov:
        lda c128_a
        ;jsr set_zn_a
        #set_zna
        rts

_do_sbc_bin:
        ; ---------- your original binary SBC ----------
        lda c128_p
        and #P_C
        bne _sbc_c
        clc
        jmp _sbc_go2
_sbc_c:
        sec
_sbc_go2:
        lda c128_a
        sbc c128_tmp
        sta c128_a
        php
        lda c128_p
        and #(~(P_C|P_V)) & $ff
        sta c128_p
        plp
        bcc _sbc_noc2
        lda c128_p
        ora #P_C
        sta c128_p
_sbc_noc2:
        bvc _sbc_nov2
        lda c128_p
        ora #P_V
        sta c128_p
_sbc_nov2:
        lda c128_a
        ;jsr set_zn_a
        #set_zna
        rts

; --- do_cmp ---
do_cmp:
        sta c128_tmp
        lda c128_a
        sec
        sbc c128_tmp            ; A-M, sets native C/Z/N
        ; Now extract flags from the native 65C02 result:
        ; C = set if A >= M (no borrow)
        ; Z = set if A == M
        ; N = set if (A-M) bit 7 set
        php                     ; Save native flags
        pla                     ; A = native P register
        and #(P_C|P_Z|P_N)     ; Keep only C, Z, N
        sta c128_tmp
        lda c128_p
        and #(~(P_C|P_Z|P_N)) & $ff
        ora c128_tmp
        sta c128_p
        rts

; --- do_cpx ---
do_cpx:
        sta c128_tmp
        lda c128_x
        sec
        sbc c128_tmp
        php
        pla
        and #(P_C|P_Z|P_N)
        sta c128_tmp
        lda c128_p
        and #(~(P_C|P_Z|P_N)) & $ff
        ora c128_tmp
        sta c128_p
        rts

; --- do_cpy ---
do_cpy:
        sta c128_tmp
        lda c128_y
        sec
        sbc c128_tmp
        php
        pla
        and #(P_C|P_Z|P_N)
        sta c128_tmp
        lda c128_p
        and #(~(P_C|P_Z|P_N)) & $ff
        ora c128_tmp
        sta c128_p
        rts

; ============================================================
; OPCODE HANDLERS
; ============================================================

; $00 BRK
op_00:
        ; ============================================================
        ; BRK - Dump state for debugging, then execute normally
        ; Border = YELLOW, info at $7050+
        ; ============================================================
        lda #$07                ; yellow border
        sta $D020

        ; Dump BRK diagnostic info at $7050
        lda c128_pc_hi
        sta $7050               ; PC hi (points past BRK opcode)
        lda c128_pc_lo
        sta $7051               ; PC lo
        lda c128_sp
        sta $7052
        lda c128_a
        sta $7053
        lda c128_x
        sta $7054
        lda c128_y
        sta $7055
        lda c128_p
        sta $7056
        lda mmu_cr
        sta $7057

        ; Standard BRK sequence
        ; BRK: increment PC past signature byte
        inw c128_pc_lo
        ; Push PC high
        lda c128_pc_hi
        sta c128_data
        jsr push_data
        ; Push PC low
        lda c128_pc_lo
        sta c128_data
        jsr push_data
        ; Push P with B=1, U=1
        lda c128_p
        ora #P_B                ; Set B flag (distinguishes BRK from IRQ)
        ora #P_U
        sta c128_data
        jsr push_data
        ; Set I flag
        lda c128_p
        ora #P_I
        sta c128_p
        ; Load IRQ vector from $FFFE/$FFFF - ALWAYS from ROM
        ; $FFFE -> $127FE (relocated KERNAL area)
        lda #$FE
        sta C128_MEM_PTR+0
        lda #$27
        sta C128_MEM_PTR+1
        lda #BANK_ROM
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        sta c128_pc_lo
        inz
        lda [C128_MEM_PTR],z
        sta c128_pc_hi
        lda #0
        sta c128_code_valid
        lda #7
        jmp finish_cycles

; $01 ORA (zp,X)
op_01:
        jsr addr_indx
        #read_data_fast
        ora c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $05 ORA zp
op_05:
        ; Optimized ORA zp
        jsr fetch8
        tax
        jsr lrb_read_x
        ora c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #3
        #finish_cycles_no_xtra
        rts

; $06 ASL zp
op_06:
        jsr fetch8
        tax
        ; ASL directly in LOW_RAM_BUFFER
        jsr lrb_asl_x
        ; Update carry flag
        lda c128_p
        and #(~P_C) & $ff
        bcc _op06_nc
        ora #P_C
_op06_nc:
        sta c128_p
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zn_fast
        lda #5
        #finish_cycles_inline
        rts

; $08 PHP
op_08:
        lda c128_p
        ora #(P_B|P_U)
        sta c128_data
        jsr push_data
        lda #3
        #finish_cycles_no_xtra
        rts

; $09 ORA #imm
op_09:
        #fetch8_operand
        ora c128_a
        sta c128_a
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $0A ASL A
op_0a:
        lda c128_a
        asl
        sta c128_a
        php
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        plp
        bcc _op0a_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op0a_nc:
        lda c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #2
        #finish_cycles_inline
        rts

; $0D ORA abs
op_0d:
        jsr addr_abs
        #read_data_fast
        ora c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $0E ASL abs
op_0e:
        jsr addr_abs
        #read_data_fast
        asl
        php
        sta c128_data
        #write_data_fast
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        plp
        bcc _op0e_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op0e_nc:
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_inline
        rts

; $10 BPL
op_10:
        #fetch8_operand
        sta c128_tmp
        bbs 7, c128_p,_op10_nt
        ;lda c128_p
        ;and #P_N
        ;bne _op10_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op10_nt:
        lda #2
        #finish_cycles_no_xtra
        rts

; $11 ORA (zp),Y
op_11:
        jsr addr_indy
        #read_data_fast
        ora c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #5
        #finish_cycles_inline
        rts

; $15 ORA zp,X
op_15:
        ; Optimized ORA zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_read_x
        ora c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $16 ASL zp,X
op_16:
        ; Optimized ASL zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        ; ASL directly in LOW_RAM_BUFFER
        jsr lrb_asl_x
        ; Update carry flag
        lda c128_p
        and #(~P_C) & $ff
        bcc _op16_nc
        ora #P_C
_op16_nc:
        sta c128_p
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zn_fast
        lda #6
        #finish_cycles_inline
        rts

; $18 CLC
op_18:
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda #2
        #finish_cycles_no_xtra
        rts

; $19 ORA abs,Y
op_19:
        jsr addr_absy
        #read_data_fast
        ora c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $1D ORA abs,X
op_1d:
        jsr addr_absx
        #read_data_fast
        ora c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $1E ASL abs,X
op_1e:
        jsr addr_absx
        #read_data_fast
        asl
        php
        sta c128_data
        #write_data_fast
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        plp
        bcc _op1e_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op1e_nc:
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #7
        #finish_cycles_inline
        rts

; $20 JSR
op_20:
        jsr fetch16_to_addr
        lda c128_addr_lo
        sta c128_vec_lo
        lda c128_addr_hi
        sta c128_vec_hi
        lda c128_pc_lo
        sec
        sbc #1
        sta c128_tmp
        lda c128_pc_hi
        sbc #0
        sta c128_data
        jsr push_data
        lda c128_tmp
        sta c128_data
        jsr push_data
        lda c128_vec_lo
        sta c128_pc_lo
        lda c128_vec_hi
        sta c128_pc_hi
        lda #6
        #finish_cycles_no_xtra
        rts

; $21 AND (zp,X)
op_21:
        jsr addr_indx
        #read_data_fast
        and c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $24 BIT zp
op_24:
        ; Optimized BIT zp
        jsr fetch8
        tax
        jsr lrb_read_x
        sta c128_tmp
        lda c128_p
        and #(~(P_N|P_V|P_Z)) & $ff
        sta c128_p
        lda c128_tmp
        and #P_N
        ora c128_p
        sta c128_p
        lda c128_tmp
        and #P_V
        ora c128_p
        sta c128_p
        lda c128_tmp
        and c128_a
        bne _op24_nz
        lda c128_p
        ora #P_Z
        sta c128_p
_op24_nz:
        lda #3
        #finish_cycles_inline
        rts

; $25 AND zp
op_25:
        ; Optimized AND zp
        jsr fetch8
        tax
        jsr lrb_read_x
        and c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #3
        #finish_cycles_no_xtra
        rts

; $26 ROL zp
op_26:
        jsr fetch8
        tax
        ; Get old carry into bit 0 position
        lda c128_p
        and #P_C
        sta c128_tmp2
        ; Get memory, save bit 7 for new carry
        jsr lrb_read_x
        sta c128_tmp
        ; Shift left and OR in old carry
        asl
        ora c128_tmp2
        jsr lrb_write_x
        ; Update carry from old bit 7
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        bpl _op26_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op26_nc:
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zn_fast
        lda #5
        #finish_cycles_inline
        rts

; $28 PLP
op_28:
        jsr pull_to_a
        and #(~P_B) & $ff
        ora #P_U
        sta c128_p
        lda #4
        #finish_cycles_no_xtra
        rts

; $29 AND #imm
op_29:
        #fetch8_operand
        and c128_a
        sta c128_a
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $2A ROL A
op_2a:
        lda c128_p
        and #P_C
        sta c128_tmp2
        lda c128_a
        sta c128_tmp
        asl
        ora c128_tmp2
        sta c128_a
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$80
        beq _op2a_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op2a_nc:
        lda c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #2
        #finish_cycles_inline
        rts

; $2C BIT abs
op_2c:
        jsr addr_abs
        #read_data_fast
        sta c128_tmp
        lda c128_p
        and #(~(P_N|P_V|P_Z)) & $ff
        sta c128_p
        lda c128_tmp
        and #P_N
        ora c128_p
        sta c128_p
        lda c128_tmp
        and #P_V
        ora c128_p
        sta c128_p
        lda c128_tmp
        and c128_a
        bne _op2c_nz
        lda c128_p
        ora #P_Z
        sta c128_p
_op2c_nz:
        lda #4
        #finish_cycles_inline
        rts

; $2D AND abs
op_2d:
        jsr addr_abs
        #read_data_fast
        and c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $2E ROL abs
op_2e:
        jsr addr_abs
        #read_data_fast
        sta c128_tmp
        lda c128_p
        and #P_C
        sta c128_tmp2
        lda c128_tmp
        asl
        ora c128_tmp2
        sta c128_data
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$80
        beq _op2e_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op2e_nc:
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_inline
        rts

; $30 BMI
op_30:
        #fetch8_operand
        sta c128_tmp
        bbr 7, c128_p, _op30_nt
        ;lda c128_p
        ;and #P_N
        ;beq _op30_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op30_nt:
        lda #2
        #finish_cycles_no_xtra
        rts

; $31 AND (zp),Y
op_31:
        jsr addr_indy
        #read_data_fast
        and c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #5
        #finish_cycles_inline
        rts

; $35 AND zp,X
op_35:
        ; Optimized AND zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_read_x
        and c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $36 ROL zp,X
op_36:
        ; Optimized ROL zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        ; Get old carry into bit 0 position
        lda c128_p
        and #P_C
        sta c128_tmp2
        ; Get memory, save bit 7 for new carry
        jsr lrb_read_x
        sta c128_tmp
        ; Shift left and OR in old carry
        asl
        ora c128_tmp2
        jsr lrb_write_x
        ; Update carry from old bit 7
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        bpl _op36_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op36_nc:
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zn_fast
        lda #6
        #finish_cycles_inline
        rts

; $38 SEC
op_38:
        lda c128_p
        ora #P_C
        sta c128_p
        lda #2
        #finish_cycles_no_xtra
        rts

; $39 AND abs,Y
op_39:
        jsr addr_absy
        #read_data_fast
        and c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $3D AND abs,X
op_3d:
        jsr addr_absx
        #read_data_fast
        and c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $3E ROL abs,X
op_3e:
        jsr addr_absx
        #read_data_fast
        sta c128_tmp
        lda c128_p
        and #P_C
        sta c128_tmp2
        lda c128_tmp
        asl
        ora c128_tmp2
        sta c128_data
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$80
        beq _op3e_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op3e_nc:
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #7
        #finish_cycles_inline
        rts

; $40 RTI
op_40:
        jsr pull_to_a
        and #(~P_B) & $ff
        ora #P_U
        sta c128_p
        jsr pull_to_a
        sta c128_pc_lo
        jsr pull_to_a
        sta c128_pc_hi
        lda #6
        #finish_cycles_no_xtra
        rts

; $41 EOR (zp,X)
op_41:
        jsr addr_indx
        #read_data_fast
        eor c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $45 EOR zp
op_45:
        ; Optimized EOR zp
        jsr fetch8
        tax
        jsr lrb_read_x
        eor c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #3
        #finish_cycles_no_xtra
        rts

; $46 LSR zp
op_46:
        jsr fetch8
        tax
        ; Check bit 0 for carry before shift
        jsr lrb_read_x
        lsr
        jsr lrb_write_x
        ; Update carry - carry flag is already set correctly by LSR
        lda c128_p
        and #(~P_C) & $ff
        bcc _op46_nc
        ora #P_C
_op46_nc:
        sta c128_p
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zn_fast
        lda #5
        #finish_cycles_inline
        rts

; $48 PHA
op_48:
        lda c128_a
        sta c128_data
        jsr push_data
        lda #3
        #finish_cycles_no_xtra
        rts

; $49 EOR #imm
op_49:
        #fetch8_operand
        eor c128_a
        sta c128_a
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $4A LSR A
op_4a:
        lda c128_a
        sta c128_tmp
        lsr
        sta c128_a
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$01
        beq _op4a_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op4a_nc:
        lda c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #2
        #finish_cycles_inline
        rts

; $4C JMP abs
op_4c:
        jsr fetch16_to_addr
        lda c128_addr_lo
        sta c128_pc_lo
        lda c128_addr_hi
        sta c128_pc_hi
        lda #3
        #finish_cycles_no_xtra
        rts

; $4D EOR abs
op_4d:
        jsr addr_abs
        #read_data_fast
        eor c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $4E LSR abs
op_4e:
        jsr addr_abs
        #read_data_fast
        sta c128_tmp
        lsr
        sta c128_data
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$01
        beq _op4e_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op4e_nc:
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_inline
        rts

; $50 BVC
op_50:
        #fetch8_operand
        sta c128_tmp
        bbs 6, c128_p, _op50_nt
        ;lda c128_p
        ;and #P_V
        ;bne _op50_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op50_nt:
        lda #2
        #finish_cycles_no_xtra
        rts

; $51 EOR (zp),Y
op_51:
        jsr addr_indy
        #read_data_fast
        eor c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #5
        #finish_cycles_inline
        rts

; $55 EOR zp,X
op_55:
        ; Optimized EOR zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_read_x
        eor c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $56 LSR zp,X
op_56:
        ; Optimized LSR zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        ; Shift right directly
        jsr lrb_read_x
        lsr
        jsr lrb_write_x
        ; Update carry - carry flag already set by LSR
        lda c128_p
        and #(~P_C) & $ff
        bcc _op56_nc
        ora #P_C
_op56_nc:
        sta c128_p
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zn_fast
        lda #6
        #finish_cycles_inline
        rts

; $58 CLI
op_58:
        lda c128_p
        and #(~P_I) & $ff
        sta c128_p
        lda #2
        #finish_cycles_no_xtra
        rts

; $59 EOR abs,Y
op_59:
        jsr addr_absy
        #read_data_fast
        eor c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $5D EOR abs,X
op_5d:
        jsr addr_absx
        #read_data_fast
        eor c128_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $5E LSR abs,X
op_5e:
        jsr addr_absx
        #read_data_fast
        sta c128_tmp
        lsr
        sta c128_data
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$01
        beq _op5e_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op5e_nc:
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #7
        #finish_cycles_inline
        rts

; $60 RTS
op_60:
        jsr pull_to_a
        sta c128_pc_lo
        jsr pull_to_a
        sta c128_pc_hi
        inw c128_pc_lo            ; 16-bit increment
        lda #6
        #finish_cycles_no_xtra
        rts

; $61 ADC (zp,X)
op_61:
        jsr addr_indx
        #read_data_fast
        jsr do_adc
        lda #6
        #finish_cycles_no_xtra
        rts

; $65 ADC zp
op_65:
        ; Optimized ADC zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_adc
        lda #3
        #finish_cycles_no_xtra
        rts

; $66 ROR zp
op_66:
        jsr fetch8
        tax
        ; Get old carry into bit 7 position
        lda c128_p
        and #P_C
        beq _op66_nci
        lda #$80
        sta c128_tmp2
        bra _op66_do
_op66_nci:
        lda #$00
        sta c128_tmp2
_op66_do:
        ; Get memory, save bit 0 for new carry
        jsr lrb_read_x
        sta c128_tmp
        ; Shift right and OR in old carry
        lsr
        ora c128_tmp2
        jsr lrb_write_x
        ; Update carry from old bit 0
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$01
        beq _op66_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op66_nc:
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zn_fast
        lda #5
        #finish_cycles_inline
        rts

; $68 PLA
op_68:
        jsr pull_to_a
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $69 ADC #imm
op_69:
        jsr fetch8
        jsr do_adc
        lda #2
        #finish_cycles_no_xtra
        rts

; $6A ROR A
op_6a:
        lda c128_p
        and #P_C
        beq _op6a_nci
        lda #$80
        sta c128_tmp2
        jmp _op6a_do
_op6a_nci:
        lda #$00
        sta c128_tmp2
_op6a_do:
        lda c128_a
        sta c128_tmp
        lsr
        ora c128_tmp2
        sta c128_a
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$01
        beq _op6a_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op6a_nc:
        lda c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #2
        #finish_cycles_inline
        rts

; $6C JMP (ind)
op_6c:
        jsr addr_ind_jmp

        lda c128_addr_lo
        sta c128_pc_lo
        lda c128_addr_hi
        sta c128_pc_hi
        lda #5
        #finish_cycles_no_xtra
        rts

; $6D ADC abs
op_6d:
        jsr addr_abs
        #read_data_fast
        jsr do_adc
        lda #4
        #finish_cycles_no_xtra
        rts

; $6E ROR abs
op_6e:
        jsr addr_abs
        #read_data_fast
        sta c128_tmp
        lda c128_p
        and #P_C
        beq _op6e_nci
        lda #$80
        sta c128_tmp2
        jmp _op6e_do
_op6e_nci:
        lda #$00
        sta c128_tmp2
_op6e_do:
        lda c128_tmp
        lsr
        ora c128_tmp2
        sta c128_data
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$01
        beq _op6e_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op6e_nc:
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_inline
        rts

; $70 BVS
op_70:
        #fetch8_operand
        sta c128_tmp
        bbr 6, c128_p, _op70_nt
        ;lda c128_p
        ;and #P_V
        ;beq _op70_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op70_nt:
        lda #2
        #finish_cycles_no_xtra
        rts

; $71 ADC (zp),Y
op_71:
        jsr addr_indy
        #read_data_fast
        jsr do_adc
        lda #5
        #finish_cycles_inline
        rts

; $75 ADC zp,X
op_75:
        ; Optimized ADC zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_read_x
        jsr do_adc
        lda #4
        #finish_cycles_no_xtra
        rts

; $76 ROR zp,X
op_76:
        ; Optimized ROR zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        ; Get old carry into bit 7 position
        lda c128_p
        and #P_C
        beq _op76_nci
        lda #$80
        sta c128_tmp2
        bra _op76_do
_op76_nci:
        lda #$00
        sta c128_tmp2
_op76_do:
        ; Get memory, save bit 0 for new carry
        jsr lrb_read_x
        sta c128_tmp
        ; Shift right and OR in old carry
        lsr
        ora c128_tmp2
        jsr lrb_write_x
        ; Update carry from old bit 0
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$01
        beq _op76_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op76_nc:
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zn_fast
        lda #6
        #finish_cycles_inline
        rts

; $78 SEI
op_78:
        lda c128_p
        ora #P_I
        sta c128_p
        lda #2
        #finish_cycles_no_xtra
        rts

; $79 ADC abs,Y
op_79:
        jsr addr_absy
        #read_data_fast
        jsr do_adc
        lda #4
        #finish_cycles_inline
        rts

; $7D ADC abs,X
op_7d:
        jsr addr_absx
        #read_data_fast
        jsr do_adc
        lda #4
        #finish_cycles_inline
        rts

; $7E ROR abs,X
op_7e:
        jsr addr_absx
        #read_data_fast
        sta c128_tmp
        lda c128_p
        and #P_C
        beq _op7e_nci
        lda #$80
        sta c128_tmp2
        jmp _op7e_do
_op7e_nci:
        lda #$00
        sta c128_tmp2
_op7e_do:
        lda c128_tmp
        lsr
        ora c128_tmp2
        sta c128_data
        lda c128_p
        and #(~P_C) & $ff
        sta c128_p
        lda c128_tmp
        and #$01
        beq _op7e_nc
        lda c128_p
        ora #P_C
        sta c128_p
_op7e_nc:
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #7
        #finish_cycles_inline
        rts

; $81 STA (zp,X)
op_81:
        jsr addr_indx
        lda c128_a
        sta c128_data
        #write_data_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $84 STY zp
op_84:
        #fetch8_operand
        sta c128_zp_ptr+0
        ldz #0
        lda c128_y
        sta [c128_zp_ptr],z
        lda #3
        #finish_cycles_no_xtra
        rts


; $85 STA zp
op_85:
        #fetch8_operand
        sta c128_zp_ptr+0
        ldz #0
        lda c128_a
        sta [c128_zp_ptr],z
        lda #3
        #finish_cycles_no_xtra
        rts

; $86 STX zp  
op_86:
        #fetch8_operand
        sta c128_zp_ptr+0
        ldz #0
        lda c128_x
        sta [c128_zp_ptr],z
        lda #3
        #finish_cycles_no_xtra
        rts

; $88 DEY
op_88:
        dec c128_y
        lda c128_y
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $8A TXA
op_8a:
        lda c128_x
        sta c128_a
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $8C STY abs
op_8c:
        jsr addr_abs
        lda c128_y
        sta c128_data
        #write_data_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $8D STA abs
op_8d:
        jsr addr_abs
        lda c128_a
        sta c128_data
        #write_data_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $8E STX abs
op_8e:
        jsr addr_abs
        lda c128_x
        sta c128_data
        #write_data_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $90 BCC
op_90:
        #fetch8_operand
        sta c128_tmp

        ; P_C is Bit 0. If Bit 0 is Set, we do NOT take the branch (BCC).
        bbs 0, c128_p, _op90_nt 

        ;lda c128_p
        ;and #P_C
        ;bne _op90_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op90_nt:
        lda #2
        #finish_cycles_no_xtra
        rts

; $91 STA (zp),Y
op_91:
        jsr addr_indy
        lda c128_a
        sta c128_data
        #write_data_fast
        lda #6
        #finish_cycles_inline
        rts

; $94 STY zp,X
op_94:
        ; Optimized STY zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        lda c128_y
        jsr lrb_write_x
        lda #4
        #finish_cycles_no_xtra
        rts

; $95 STA zp,X
op_95:
        ; Optimized STA zp,X - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc c128_x
        tax
        lda c128_a
        jsr lrb_write_x
        lda #4
        #finish_cycles_no_xtra
        rts

; $96 STX zp,Y
op_96:
        ; Optimized STX zp,Y - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc c128_y
        tax
        lda c128_x
        jsr lrb_write_x
        lda #4
        #finish_cycles_no_xtra
        rts

; $98 TYA
op_98:
        lda c128_y
        sta c128_a
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $99 STA abs,Y
op_99:
        jsr addr_absy
        lda c128_a
        sta c128_data
        #write_data_fast
        lda #5
        #finish_cycles_inline
        rts

; $9A TXS
op_9a:
        lda c128_x
        sta c128_sp
        lda #2
        #finish_cycles_no_xtra
        rts

; $9D STA abs,X
op_9d:
        jsr addr_absx
        lda c128_a
        sta c128_data
        #write_data_fast
        lda #5
        #finish_cycles_inline
        rts

; $A0 LDY #imm
op_a0:
        #fetch8_operand
        sta c128_y
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $A1 LDA (zp,X)
op_a1:
        jsr addr_indx
        #read_data_fast
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $A2 LDX #imm
op_a2:
        #fetch8_operand
        sta c128_x
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $A4 LDY zp
op_a4:
        #fetch8_operand
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        sta c128_y
        #set_zn_fast
        lda #3
        #finish_cycles_no_xtra
        rts

; $A5 LDA zp
op_a5:
        ; Optimized LDA zp - inline operand + ZP read
        #fetch8_operand         ; Get ZP address in A (inline fast path)
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z     ; Read from bank 4 ZP
        sta c128_a
        #set_zn_fast
        lda #3
        #finish_cycles_no_xtra
        rts

; $A6 LDX zp
op_a6:
        #fetch8_operand
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        sta c128_x
        #set_zn_fast
        lda #3
        #finish_cycles_no_xtra
        rts

; $A8 TAY
op_a8:
        lda c128_a
        sta c128_y
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $A9 LDA #imm
op_a9:
        #fetch8_operand
        sta c128_a
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $AA TAX
op_aa:
        lda c128_a
        sta c128_x
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $AC LDY abs
op_ac:
        jsr addr_abs
        #read_data_fast
        sta c128_y
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $AD LDA abs
op_ad:
        jsr addr_abs
        #read_data_fast
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $AE LDX abs
op_ae:
        jsr addr_abs
        #read_data_fast
        sta c128_x
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $B0 BCS
op_b0:
        #fetch8_operand
        sta c128_tmp
        bbr 0, c128_p, _opb0_nt
        ;lda c128_p
        ;and #P_C
        ;beq _opb0_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_opb0_nt:
        lda #2
        #finish_cycles_no_xtra
        rts

; $B1 LDA (zp),Y
op_b1:
        jsr addr_indy
        #read_data_fast
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #5
        #finish_cycles_inline
        rts

; $B4 LDY zp,X
op_b4:
        ; Optimized LDY zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_read_x
        sta c128_y
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $B5 LDA zp,X
op_b5:
        ; Optimized LDA zp,X - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_read_x
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $B6 LDX zp,Y
op_b6:
        ; Optimized LDX zp,Y - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc c128_y
        tax
        jsr lrb_read_x
        sta c128_x
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_no_xtra
        rts

; $B8 CLV
op_b8:
        lda c128_p
        and #(~P_V) & $ff
        sta c128_p
        lda #2
        #finish_cycles_no_xtra
        rts

; $B9 LDA abs,Y
op_b9:
        jsr addr_absy
        #read_data_fast
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $BA TSX
op_ba:
        lda c128_sp
        sta c128_x
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $BC LDY abs,X
op_bc:
        jsr addr_absx
        #read_data_fast
        sta c128_y
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $BD LDA abs,X
op_bd:
        jsr addr_absx
        #read_data_fast
        sta c128_a
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $BE LDX abs,Y
op_be:
        jsr addr_absy
        #read_data_fast
        sta c128_x
        ;jsr set_zn_a
        #set_zn_fast
        lda #4
        #finish_cycles_inline
        rts

; $C0 CPY #imm
op_c0:
        jsr fetch8
        jsr do_cpy
        lda #2
        #finish_cycles_no_xtra
        rts

; $C1 CMP (zp,X)
op_c1:
        jsr addr_indx
        #read_data_fast
        jsr do_cmp
        lda #6
        #finish_cycles_no_xtra
        rts

; $C4 CPY zp
op_c4:
        ; Optimized CPY zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_cpy
        lda #3
        #finish_cycles_no_xtra
        rts

; $C5 CMP zp
op_c5:
        #fetch8_operand
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        jsr do_cmp
        lda #3
        #finish_cycles_no_xtra
        rts

; $C6 DEC zp
op_c6:
        #fetch8_operand
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z     ; Read
        sec
        sbc #1                  ; Decrement
        sta [c128_zp_ptr],z     ; Write back
        #set_zn_fast
        lda #5
        #finish_cycles_no_xtra
        rts

; $C8 INY
op_c8:
        inc c128_y
        lda c128_y
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $C9 CMP #imm
op_c9:
        #fetch8_operand
        jsr do_cmp
        lda #2
        #finish_cycles_no_xtra
        rts

; $CA DEX
op_ca:
        dec c128_x
        lda c128_x
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $CC CPY abs
op_cc:
        jsr addr_abs
        #read_data_fast
        jsr do_cpy
        lda #4
        #finish_cycles_no_xtra
        rts

; $CD CMP abs
op_cd:
        jsr addr_abs
        #read_data_fast
        jsr do_cmp
        lda #4
        #finish_cycles_no_xtra
        rts

; $CE DEC abs
op_ce:
        jsr addr_abs
        #read_data_fast
        sec
        sbc #1
        sta c128_data
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $D0 BNE
op_d0:
        #fetch8_operand
        sta c128_tmp
        bbs 1, c128_p, _opd0_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_opd0_nt:
        lda #2
        #finish_cycles_no_xtra
        rts

; $D1 CMP (zp),Y
op_d1:
        jsr addr_indy
        #read_data_fast
        jsr do_cmp
        lda #5
        #finish_cycles_inline
        rts

; $D5 CMP zp,X
op_d5:
        ; Optimized CMP zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_read_x
        jsr do_cmp
        lda #4
        #finish_cycles_no_xtra
        rts

; $D6 DEC zp,X
op_d6:
        ; Optimized DEC zp,X - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_dec_x
        jsr lrb_read_x
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $D8 CLD
op_d8:
        lda c128_p
        and #(~P_D) & $ff
        sta c128_p
        lda #2
        #finish_cycles_no_xtra
        rts

; $D9 CMP abs,Y
op_d9:
        jsr addr_absy
        #read_data_fast
        jsr do_cmp
        lda #4
        #finish_cycles_inline
        rts

; $DD CMP abs,X
op_dd:
        jsr addr_absx
        #read_data_fast
        jsr do_cmp
        lda #4
        #finish_cycles_inline
        rts

; $DE DEC abs,X
op_de:
        jsr addr_absx
        #read_data_fast
        sec
        sbc #1
        sta c128_data
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #7
        #finish_cycles_inline
        rts

; $E0 CPX #imm
op_e0:
        jsr fetch8
        jsr do_cpx
        lda #2
        #finish_cycles_no_xtra
        rts

; $E1 SBC (zp,X)
op_e1:
        jsr addr_indx
        #read_data_fast
        jsr do_sbc
        lda #6
        #finish_cycles_no_xtra
        rts

; $E4 CPX zp
op_e4:
        ; Optimized CPX zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_cpx
        lda #3
        #finish_cycles_no_xtra
        rts

; $E5 SBC zp
op_e5:
        ; Optimized SBC zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_sbc
        lda #3
        #finish_cycles_no_xtra
        rts

; $E6 INC zp
op_e6:
        #fetch8_operand
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z     ; Read
        clc
        adc #1                  ; Increment
        sta [c128_zp_ptr],z     ; Write back
        #set_zn_fast
        lda #5
        #finish_cycles_no_xtra
        rts

; $E8 INX
op_e8:
        inc c128_x
        lda c128_x
        #set_zn_fast
        lda #2
        #finish_cycles_no_xtra
        rts

; $E9 SBC #imm
op_e9:
        jsr fetch8
        jsr do_sbc
        lda #2
        #finish_cycles_no_xtra
        rts

; $EA NOP
op_ea:
        lda #2
        #finish_cycles_no_xtra
        rts

; $EC CPX abs
op_ec:
        jsr addr_abs
        #read_data_fast
        jsr do_cpx
        lda #4
        #finish_cycles_no_xtra
        rts

; $ED SBC abs
op_ed:
        jsr addr_abs
        #read_data_fast
        jsr do_sbc
        lda #4
        #finish_cycles_no_xtra
        rts

; $EE INC abs
op_ee:
        jsr addr_abs
        #read_data_fast
        clc
        adc #1
        sta c128_data
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $F0 BEQ
op_f0:
        #fetch8_operand
        sta c128_tmp
        bbr 1, c128_p, _opf0_nt
        ;lda c128_p
        ;and #P_Z
        ;beq _opf0_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_opf0_nt:
        lda #2
        #finish_cycles_no_xtra
        rts

; $F1 SBC (zp),Y
op_f1:
        jsr addr_indy
        #read_data_fast
        jsr do_sbc
        lda #5
        #finish_cycles_inline
        rts

; $F5 SBC zp,X
op_f5:
        ; Optimized SBC zp,X
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_read_x
        jsr do_sbc
        lda #4
        #finish_cycles_no_xtra
        rts

; $F6 INC zp,X
op_f6:
        ; Optimized INC zp,X - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc c128_x
        tax
        jsr lrb_inc_x
        jsr lrb_read_x
        ;jsr set_zn_a
        #set_zn_fast
        lda #6
        #finish_cycles_no_xtra
        rts

; $F8 SED
op_f8:
        lda c128_p
        ora #P_D
        sta c128_p
        lda #2
        #finish_cycles_no_xtra
        rts

; $F9 SBC abs,Y
op_f9:
        jsr addr_absy
        #read_data_fast
        jsr do_sbc
        lda #4
        #finish_cycles_inline
        rts

; $FD SBC abs,X
op_fd:
        jsr addr_absx
        #read_data_fast
        jsr do_sbc
        lda #4
        #finish_cycles_inline
        rts

; $FE INC abs,X
op_fe:
        jsr addr_absx
        #read_data_fast
        clc
        adc #1
        sta c128_data
        #write_data_fast
        lda c128_data
        ;jsr set_zn_a
        #set_zn_fast
        lda #7
        #finish_cycles_inline
        rts

; ============================================================
; Illegal opcodes
; Many programs rely on these - implement common ones as NOPs
; ============================================================

; NOP zp (2 bytes) - $04, $44, $64
op_04:
op_44:
op_64:
        jsr fetch8              ; Skip the ZP byte
        lda #3
        #finish_cycles_no_xtra
        rts

; NOP abs (3 bytes) - $0C
op_0c:
        jsr fetch8              ; Skip low byte
        jsr fetch8              ; Skip high byte
        lda #4
        #finish_cycles_no_xtra
        rts

; NOP abs,X (3 bytes) - $1C, $3C, $5C, $7C, $DC, $FC
op_1c:
op_3c:
op_5c:
op_7c:
op_dc:
op_fc:
        jsr fetch8              ; Skip low byte
        jsr fetch8              ; Skip high byte
        lda #4
        #finish_cycles_no_xtra
        rts

; NOP zp,X (2 bytes) - $14, $34, $54, $74, $D4, $F4
op_14:
op_34:
op_54:
op_74:
op_d4:
op_f4:
        jsr fetch8              ; Skip the ZP byte
        lda #4
        #finish_cycles_no_xtra
        rts

; NOP implied (1 byte) - $1A, $3A, $5A, $7A, $DA, $FA
op_1a:
op_3a:
op_5a:
op_7a:
op_da:
op_fa:
        lda #2
        #finish_cycles_no_xtra
        rts

; NOP immediate (2 bytes) - $80, $82, $89, $C2, $E2
op_80:
op_82:
op_89:
op_c2:
op_e2:
        jsr fetch8              ; Skip immediate byte
        lda #2
        #finish_cycles_no_xtra
        rts

; JAM/KIL - halt CPU (these will crash if hit)
op_02:
op_12:
op_22:
op_32:
op_42:
op_52:
op_62:
op_72:
op_92:
op_b2:
op_d2:
op_f2:
        jmp op_illegal

; Remaining illegal opcodes - jump to illegal handler
op_03: jmp op_illegal
op_07: jmp op_illegal
op_0b: jmp op_illegal
op_0f: jmp op_illegal
op_13: jmp op_illegal
op_17: jmp op_illegal
op_1b: jmp op_illegal
op_1f: jmp op_illegal
op_23: jmp op_illegal
op_27: jmp op_illegal
op_2b: jmp op_illegal
op_2f: jmp op_illegal
op_33: jmp op_illegal
op_37: jmp op_illegal
op_3b: jmp op_illegal
op_3f: jmp op_illegal
op_43: jmp op_illegal
op_47: jmp op_illegal
op_4b: jmp op_illegal
op_4f: jmp op_illegal
op_53: jmp op_illegal
op_57: jmp op_illegal
op_5b: jmp op_illegal
op_5f: jmp op_illegal
op_63: jmp op_illegal
op_67: jmp op_illegal
op_6b: jmp op_illegal
op_6f: jmp op_illegal
op_73: jmp op_illegal
op_77: jmp op_illegal
op_7b: jmp op_illegal
op_7f: jmp op_illegal
op_83: jmp op_illegal
; $87 SAX zp (illegal but used by some software)
; Stores (A AND X) to zero page address
op_87:
        jsr fetch8
        tax
        lda c128_a
        and c128_x
        jsr lrb_write_x
        lda #3
        #finish_cycles_no_xtra
        rts
op_8b: jmp op_illegal
op_8f: jmp op_illegal
op_93: jmp op_illegal
op_97: jmp op_illegal
op_9b: jmp op_illegal
op_9c: jmp op_illegal
op_9e: jmp op_illegal
op_9f: jmp op_illegal
op_a3: jmp op_illegal
op_a7: jmp op_illegal
op_ab: jmp op_illegal
op_af: jmp op_illegal
op_b3: jmp op_illegal
op_b7: jmp op_illegal
op_bb: jmp op_illegal
op_bf: jmp op_illegal
op_c3: jmp op_illegal
op_c7: jmp op_illegal
op_cb: jmp op_illegal
op_cf: jmp op_illegal
op_d3: jmp op_illegal
op_d7: jmp op_illegal
op_db: jmp op_illegal
op_df: jmp op_illegal
op_e3: jmp op_illegal
op_e7: jmp op_illegal
op_eb: jmp op_illegal
op_ef: jmp op_illegal
op_f3: jmp op_illegal
op_f7: jmp op_illegal
op_fb: jmp op_illegal
op_ff: jmp op_illegal


; ============================================================
; Opcode vector tables (split for fast indexing)
; ============================================================

; Two 128-entry word tables for jmp (table,x) dispatch
; op_table_lo handles opcodes $00-$7F
; op_table_hi handles opcodes $80-$FF
; X register = (opcode * 2) & $FF

op_table_lo:
        .word op_00, op_01, op_02, op_03, op_04, op_05, op_06, op_07
        .word op_08, op_09, op_0a, op_0b, op_0c, op_0d, op_0e, op_0f
        .word op_10, op_11, op_12, op_13, op_14, op_15, op_16, op_17
        .word op_18, op_19, op_1a, op_1b, op_1c, op_1d, op_1e, op_1f
        .word op_20, op_21, op_22, op_23, op_24, op_25, op_26, op_27
        .word op_28, op_29, op_2a, op_2b, op_2c, op_2d, op_2e, op_2f
        .word op_30, op_31, op_32, op_33, op_34, op_35, op_36, op_37
        .word op_38, op_39, op_3a, op_3b, op_3c, op_3d, op_3e, op_3f
        .word op_40, op_41, op_42, op_43, op_44, op_45, op_46, op_47
        .word op_48, op_49, op_4a, op_4b, op_4c, op_4d, op_4e, op_4f
        .word op_50, op_51, op_52, op_53, op_54, op_55, op_56, op_57
        .word op_58, op_59, op_5a, op_5b, op_5c, op_5d, op_5e, op_5f
        .word op_60, op_61, op_62, op_63, op_64, op_65, op_66, op_67
        .word op_68, op_69, op_6a, op_6b, op_6c, op_6d, op_6e, op_6f
        .word op_70, op_71, op_72, op_73, op_74, op_75, op_76, op_77
        .word op_78, op_79, op_7a, op_7b, op_7c, op_7d, op_7e, op_7f

op_table_hi:
        .word op_80, op_81, op_82, op_83, op_84, op_85, op_86, op_87
        .word op_88, op_89, op_8a, op_8b, op_8c, op_8d, op_8e, op_8f
        .word op_90, op_91, op_92, op_93, op_94, op_95, op_96, op_97
        .word op_98, op_99, op_9a, op_9b, op_9c, op_9d, op_9e, op_9f
        .word op_a0, op_a1, op_a2, op_a3, op_a4, op_a5, op_a6, op_a7
        .word op_a8, op_a9, op_aa, op_ab, op_ac, op_ad, op_ae, op_af
        .word op_b0, op_b1, op_b2, op_b3, op_b4, op_b5, op_b6, op_b7
        .word op_b8, op_b9, op_ba, op_bb, op_bc, op_bd, op_be, op_bf
        .word op_c0, op_c1, op_c2, op_c3, op_c4, op_c5, op_c6, op_c7
        .word op_c8, op_c9, op_ca, op_cb, op_cc, op_cd, op_ce, op_cf
        .word op_d0, op_d1, op_d2, op_d3, op_d4, op_d5, op_d6, op_d7
        .word op_d8, op_d9, op_da, op_db, op_dc, op_dd, op_de, op_df
        .word op_e0, op_e1, op_e2, op_e3, op_e4, op_e5, op_e6, op_e7
        .word op_e8, op_e9, op_ea, op_eb, op_ec, op_ed, op_ee, op_ef
        .word op_f0, op_f1, op_f2, op_f3, op_f4, op_f5, op_f6, op_f7
        .word op_f8, op_f9, op_fa, op_fb, op_fc, op_fd, op_fe, op_ff