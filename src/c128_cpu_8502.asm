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
p4_a        = $02
p4_x        = $03
p4_y        = $04
p4_sp       = $05
p4_p        = $06
p4_pc_lo    = $07
p4_pc_hi    = $08
p4_addr_lo  = $09
p4_addr_hi  = $0A
p4_data     = $0b
p4_vec_lo   = $0c
p4_vec_hi   = $0d
p4_tmp      = $0e
p4_tmp2     = $0f

p4_xtra     = $11
p4_dec_a    = $12           ; Saved A for decimal mode ADC/SBC
p4_inst_pc_lo = $1A
p4_inst_pc_hi = $1B
p4_irq_pending = $19
p4_nmi_pending = $1C


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
p4_trace_enabled:    .byte $00  ; Set to 1 after LOAD to enable tracing
p4_trace_pos_lo:     .byte $00  ; Screen position low byte
p4_trace_pos_hi:     .byte $08  ; Screen position high byte ($0800)

; VIC-II timing constants (PAL)
VIC_CYCLES_PER_LINE = 63    ; 63 cycles per scanline (VIC-II PAL)
VIC_LINES_PER_FRAME = 312   ; PAL has 312 lines


set_zna .macro
        tax                     ; X = result byte (original A)
        lda p4_p
        and #(~(P_Z|P_N)) & $ff  ; clear old Z/N
        ora zn_table,x           ; OR in new Z/N
        sta p4_p
        txa
.endmacro

; ------------------------------------------------------------
; finish_cycles_inline - Inlined cycle accounting macro
; A = base cycle count on entry
; Most instructions won't cross a scanline, so we inline the fast path
; ------------------------------------------------------------
finish_cycles_inline .macro
        clc
        adc p4_xtra             ; Add extra cycles (page crossing, etc.)
        adc vic_cycle_accum     ; Add to accumulated cycles
        sta vic_cycle_accum
        cmp #VIC_CYCLES_PER_LINE
        bcc _fc_skip\@
        jsr finish_do_scanline  ; Only call if we crossed scanline boundary
_fc_skip\@:
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
p4_code_ptr       = $E0   ; 4 bytes: lo, hi, bank, megabyte
p4_code_page_hi   = $E4   ; cached p4_pc_hi (guest)
p4_code_valid     = $E5   ; 0=invalid, !=0 valid
p4_code_romvis    = $E6   ; cached mmu_cr snapshot (for ROM visibility change detection)
p4_hook_pc_changed = $E7  ; Set by hooks when they modify PC

; Call this when you want to force cache rebuild (optional helper)
invalidate_code_cache:
        lda #$00
        sta p4_code_valid
        rts

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
        lda p4_code_valid
        beq _f8_rebuild

        ; If page changed, rebuild
        lda p4_pc_hi
        cmp p4_code_page_hi
        bne _f8_rebuild

        ; If we are in >=$40, ROM visibility affects whether we read ROM or RAM
        lda p4_pc_hi
        cmp #$40
        bcc _f8_do_read

        ; Conservative: never fast-fetch from $D0-$DF (I/O) or $FF (MMU regs)
        cmp #$FF
        beq _f8_slow
        cmp #$D0
        bcc _f8_check_romvis
        cmp #$E0
        bcc _f8_slow            ; $D000-$DFFF = I/O area

_f8_check_romvis:
        ; MMU config changed since cache built?
        lda mmu_cr
        cmp p4_code_romvis
        bne _f8_rebuild

_f8_do_read:
        ldz p4_pc_lo
        lda [p4_code_ptr],z
        inw p4_pc_lo
        rts

; -----------------------------
; Cache rebuild
; -----------------------------
_f8_rebuild:
        lda p4_pc_hi
        sta p4_code_page_hi

        ; Remember current MMU config for change detection
        lda mmu_cr
        sta p4_code_romvis

        lda p4_pc_hi

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
        sta p4_code_ptr+0
        lda p4_pc_hi
        cmp #$F8
        bcc _f8_rom_normal
        ; Relocated: $F8->$20, $F9->$21, ..., $FF->$27
        sec
        sbc #$D8
        sta p4_code_ptr+1
        jmp _f8_rom_set_bank
_f8_rom_normal:
        sta p4_code_ptr+1
_f8_rom_set_bank:
        lda #BANK_ROM
        sta p4_code_ptr+2
        lda #$00
        sta p4_code_ptr+3
        lda #$01
        sta p4_code_valid
        jmp _f8_do_read

_f8_build_ram:
        ; RAM => use physical bank from MMU (with shared RAM logic)
        lda #$00
        sta p4_code_ptr+0
        lda p4_pc_hi
        sta p4_code_ptr+1
        ; get_physical_bank uses p4_addr_hi, so set it to PC page
        sta p4_addr_hi
        jsr get_physical_bank   ; Returns MEGA65 bank in A
        sta p4_code_ptr+2
        lda #$00
        sta p4_code_ptr+3
        lda #$01
        sta p4_code_valid
        jmp _f8_do_read

_f8_build_low:
        lda #$00
        sta p4_code_ptr+0
        lda p4_pc_hi
        clc
        adc #>LOW_RAM_BUFFER
        sta p4_code_ptr+1
        lda #$00
        sta p4_code_ptr+2
        sta p4_code_ptr+3
        lda #$01
        sta p4_code_valid
        jmp _f8_do_read

_f8_build_slow:
        lda #$00
        sta p4_code_valid
        ; fall through

; -----------------------------
; Slow path: use full memory read
; -----------------------------
_f8_slow:
        lda p4_pc_lo
        sta p4_addr_lo
        lda p4_pc_hi
        sta p4_addr_hi
        jsr C128_ReadFast
        inw p4_pc_lo
        rts

;===========end new




; --- fetch16_to_addr ---
fetch16_to_addr:
        jsr fetch8
        sta p4_tmp
        jsr fetch8
        sta p4_addr_hi
        lda p4_tmp
        sta p4_addr_lo
        rts

; Cached bank for stack page ($0100-$01FF)
; Updated by mmu_update_derived whenever MMU config changes.
; This avoids calling get_physical_bank on every push/pull.
cached_stack_bank: .byte BANK_RAM0

; --- push_data ---
push_data:
        lda cached_stack_bank
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        lda p4_sp
        sta C128_MEM_PTR+0
        lda #$01
        sta C128_MEM_PTR+1
        ldz #0
        lda p4_data
        sta [C128_MEM_PTR],z
        dec p4_sp
        rts

; --- pull_to_a ---
pull_to_a:
        inc p4_sp
        lda cached_stack_bank
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        lda p4_sp
        sta C128_MEM_PTR+0
        lda #$01
        sta C128_MEM_PTR+1
        ldz #0
        lda [C128_MEM_PTR],z
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

; Initialize - nothing needed now
lrb_ptr_init:
        rts

; Helper: read LOW_RAM_BUFFER[X] -> A  (preserves X)
; Now reads from bank 4 directly
lrb_read_x:
        #setup_bank4_zp
        stx C128_MEM_PTR+0
        ldz #0
        lda [C128_MEM_PTR],z
        rts

; Helper: write A -> ZP[X]  (preserves A, X)
; Writes to both LOW_RAM_BUFFER and bank 4
lrb_write_x:
        pha
        ; Write to bank 4 ($04:$00:$00:XX)
        lda #$00
        sta C128_MEM_PTR+0
        sta C128_MEM_PTR+1
        sta C128_MEM_PTR+3
        lda #BANK_RAM0
        sta C128_MEM_PTR+2
        stx C128_MEM_PTR+0
        ldz #0
        pla
        sta [C128_MEM_PTR],z
        rts

; Helper: read LOW_RAM_BUFFER[Y] -> A  (preserves Y)
; Now reads from bank 4 directly
lrb_read_y:
        #setup_bank4_zp
        sty C128_MEM_PTR+0
        ldz #0
        lda [C128_MEM_PTR],z
        rts

; Helper: ASL ZP[X]  (preserves X, sets flags from result)
lrb_asl_x:
        #setup_bank4_zp
        stx C128_MEM_PTR+0
        ldz #0
        lda [C128_MEM_PTR],z
        asl a
        sta [C128_MEM_PTR],z
        rts

; Helper: DEC ZP[X]  (preserves X, sets flags from result)
lrb_dec_x:
        #setup_bank4_zp
        stx C128_MEM_PTR+0
        ldz #0
        lda [C128_MEM_PTR],z
        sec
        sbc #1
        sta [C128_MEM_PTR],z
        rts

; Helper: INC ZP[X]  (preserves X, sets flags from result)
lrb_inc_x:
        #setup_bank4_zp
        stx C128_MEM_PTR+0
        ldz #0
        lda [C128_MEM_PTR],z
        clc
        adc #1
        sta [C128_MEM_PTR],z
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
        #setup_bank4_zp
        stx C128_MEM_PTR+0
        ldz #0
        lda [C128_MEM_PTR],z
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

; Read from ZP address in p4_addr_lo, result in A
read_zp:
        ldx p4_addr_lo
        cpx #$02
        bcc _rzp_port
        #setup_bank4_zp
        stx C128_MEM_PTR+0
        ldz #0
        lda [C128_MEM_PTR],z
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
        pha
        #setup_bank4_zp
        stx C128_MEM_PTR+0
        ldz #0
        pla
        sta [C128_MEM_PTR],z
        rts
_wzpx_port:
        cpx #$00
        beq _wzpx_ddr
        sta cpu_port_data
        ; Also write to bank 4
        pha
        #setup_bank4_zp
        lda #$01
        sta C128_MEM_PTR+0
        ldz #0
        lda cpu_port_data
        sta [C128_MEM_PTR],z
        pla
        rts
_wzpx_ddr:
        sta cpu_port_ddr
        ; Also write to bank 4
        pha
        #setup_bank4_zp
        ldz #0
        lda cpu_port_ddr
        sta [C128_MEM_PTR],z
        pla
        rts

; Write p4_data to ZP address in p4_addr_lo
write_zp:
        ldx p4_addr_lo
        cpx #$02
        bcc _wzp_port
        #setup_bank4_zp
        stx C128_MEM_PTR+0
        ldz #0
        lda p4_data
        sta [C128_MEM_PTR],z
        rts
_wzp_port:
        lda p4_data
        cpx #$00
        beq _wzp_ddr
        sta cpu_port_data
        ; Also write to bank 4
        pha
        #setup_bank4_zp
        lda #$01
        sta C128_MEM_PTR+0
        ldz #0
        lda cpu_port_data
        sta [C128_MEM_PTR],z
        pla
        rts
_wzp_ddr:
        sta cpu_port_ddr
        ; Also write to bank 4
        pha
        #setup_bank4_zp
        ldz #0
        lda cpu_port_ddr
        sta [C128_MEM_PTR],z
        pla
        rts

; Read 16-bit pointer from ZP address Y, result in p4_addr_lo/hi
; (Used for indirect addressing modes)
read_zp_ptr_y:
        #setup_bank4_zp
        sty C128_MEM_PTR+0
        ldz #0
        lda [C128_MEM_PTR],z
        sta p4_addr_lo
        inc C128_MEM_PTR+0
        lda [C128_MEM_PTR],z
        sta p4_addr_hi
        rts

; --- Addressing modes ---
addr_zp:
        jsr fetch8
        sta p4_addr_lo
        lda #$00
        sta p4_addr_hi
        rts

addr_zpx:
        jsr fetch8
        clc
        adc p4_x
        sta p4_addr_lo
        lda #$00
        sta p4_addr_hi
        rts

addr_zpy:
        jsr fetch8
        clc
        adc p4_y
        sta p4_addr_lo
        lda #$00
        sta p4_addr_hi
        rts

addr_abs:
        jmp fetch16_to_addr

addr_absx:
        jsr fetch16_to_addr
        lda #$00
        sta p4_xtra
        lda p4_addr_lo
        clc
        adc p4_x
        sta p4_addr_lo
        bcc _absx_nc
        inc p4_addr_hi
        lda #$01
        sta p4_xtra
_absx_nc:
        rts

addr_absy:
        jsr fetch16_to_addr
        lda #$00
        sta p4_xtra
        lda p4_addr_lo
        clc
        adc p4_y
        sta p4_addr_lo
        bcc _absy_nc
        inc p4_addr_hi
        lda #$01
        sta p4_xtra
_absy_nc:
        rts

addr_indx:
        jsr fetch8
        clc
        adc p4_x
        tay                             ; Y = ZP address
        ; Read 16-bit pointer directly from ZP
        jsr lrb_read_y
        sta p4_addr_lo
        iny
        jsr lrb_read_y
        sta p4_addr_hi
        rts

addr_indy:
        jsr fetch8
        tay                             ; Y = ZP address
        lda #$00
        sta p4_xtra
        ; Read 16-bit pointer directly from ZP
        jsr lrb_read_y
        sta p4_tmp
        iny
        jsr lrb_read_y
        sta p4_tmp2
        ; Add Y register to form final address
        lda p4_tmp
        clc
        adc p4_y
        sta p4_addr_lo
        lda p4_tmp2
        adc #$00
        sta p4_addr_hi
        ; Check for page crossing
        lda p4_tmp
        clc
        adc p4_y
        bcc _indy_nc
        lda #$01
        sta p4_xtra
_indy_nc:
        rts

addr_ind_jmp:
        jsr fetch16_to_addr

;debug
        ;  save the pointer address (e.g. $0318)
        lda p4_addr_lo
        sta p4_vec_lo
        lda p4_addr_hi
        sta p4_vec_hi
;

        jsr C128_ReadFast
        sta p4_tmp
        inc p4_addr_lo
        jsr C128_ReadFast
        sta p4_tmp2

        lda p4_tmp
        sta p4_addr_lo
        lda p4_tmp2
        sta p4_addr_hi
        rts

fetch_rel:
        jsr fetch8
        sta p4_tmp
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
        sta p4_irq_pending

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
        sta p4_irq_pending

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
        ; TODO: Video frame update (charset sync, bitmap render, etc.)
        ; jsr C128_VID_Frame

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
; Hook routines for init code that hangs in emulation
; ============================================================

; hook_vdc_screen_clear - Skip $CE0C VDC RAM clear loop
; Sets VDC shadow regs and advances PC to the RTS at $CE4B
hook_vdc_screen_clear:
        lda #$20
        ldx #18
        sta vdc_regs,x
        lda #$00
        ldx #19
        sta vdc_regs,x
        #setup_bank4_zp
        ldz #$DA
        lda #$00
        sta [C128_MEM_PTR],z
        ldz #$DB
        lda #$E0
        sta [C128_MEM_PTR],z
        lda #$4B
        sta p4_pc_lo
        lda #$CE
        sta p4_pc_hi
        lda #0
        sta p4_code_valid
        rts

; hook_raster_wait_1 - Skip "LDA $D011 / BPL $E142" at $E142
; This waits for raster >= 256. Skip to $E147.
hook_raster_wait_1:
        lda #$47
        sta p4_pc_lo
        lda #$E1
        sta p4_pc_hi
        lda #0
        sta p4_code_valid
        lda #4
        #finish_cycles_inline
        rts

; hook_raster_wait_2 - Skip "LDA $D011 / BMI $E147" at $E14E
; This waits for raster < 256. Skip to $E153.
hook_raster_wait_2:
        lda #$53
        sta p4_pc_lo
        lda #$E1
        sta p4_pc_hi
        lda #0
        sta p4_code_valid
        lda #4
        #finish_cycles_inline
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
        lda p4_pc_hi
        sta p4_data
        jsr push_data
        ; Push PC low
        lda p4_pc_lo
        sta p4_data
        jsr push_data
        ; Push P with B=0, U=1
        lda p4_p
        and #(~P_B) & $ff       ; Clear B flag
        ora #P_U
        sta p4_data
        jsr push_data
        ; Set I flag
        lda p4_p
        ora #P_I
        sta p4_p
        ; Load IRQ vector from $FFFE/$FFFF
        lda #$FE
        sta p4_addr_lo
        lda #$FF
        sta p4_addr_hi
        jsr C128_ReadFast
        sta p4_pc_lo
        lda #$FF
        sta p4_addr_lo
        jsr C128_ReadFast
        sta p4_pc_hi
        rts

; ============================================================
; cpu_take_nmi - Execute NMI sequence
; Push PC, push P (with B=0), set I flag, load vector from $FFFA
; ============================================================
cpu_take_nmi:
        ; Push PC high
        lda p4_pc_hi
        sta p4_data
        jsr push_data
        ; Push PC low
        lda p4_pc_lo
        sta p4_data
        jsr push_data
        ; Push P with B=0, U=1
        lda p4_p
        and #(~P_B) & $ff
        ora #P_U
        sta p4_data
        jsr push_data
        ; Set I flag
        lda p4_p
        ora #P_I
        sta p4_p
        ; Load NMI vector from $FFFA/$FFFB
        lda #$FA
        sta p4_addr_lo
        lda #$FF
        sta p4_addr_hi
        jsr C128_ReadFast
        sta p4_pc_lo
        lda #$FB
        sta p4_addr_lo
        jsr C128_ReadFast
        sta p4_pc_hi
        rts

; --- Reset/Step ---
P4CPU_Reset:
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
        jsr P4HOOK_Reset

        ; ============================================================
        ; Simulate Z80 boot initialization
        ; On a real C128, the Z80 runs first after power-on and writes
        ; critical data to RAM before switching to the 8502.
        ; We must pre-populate RAM with what the Z80 would have written.
        ; ============================================================
        jsr z80_pre_init
        
        ; Reset CPU registers
        lda #$00
        sta p4_a
        sta p4_x
        sta p4_y
        sta p4_irq_pending
        sta p4_nmi_pending
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
        sta p4_sp
        lda #(P_I|P_U)
        sta p4_p
        
        ; Get reset vector from ROM ($FFFC/$FFFD)
        lda #$fc
        sta p4_addr_lo
        lda #$ff
        sta p4_addr_hi
        jsr C128_ReadFast
        sta p4_pc_lo
        lda #$fd
        sta p4_addr_lo
        jsr C128_ReadFast
        sta p4_pc_hi
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
        sta p4_data
        txa
        clc
        adc #$D0
        sta p4_addr_lo
        lda #$FF
        sta p4_addr_hi
        phx
        jsr C128_Write
        plx
        inx
        cpx #16
        bcc z80_wr_ffd0

        ; --- Write Z80->8502 switch routine at $FFE0 ---
        ; FFE0: DI / LD A,$3E / LD ($FF00),A / LD BC,$D505 / LD A,$B1 / OUT (C),A / NOP / RST 08
        ldx #0
z80_wr_ffe0:
        lda z80_ffe0_data,x
        sta p4_data
        txa
        clc
        adc #$E0
        sta p4_addr_lo
        lda #$FF
        sta p4_addr_hi
        phx
        jsr C128_Write
        plx
        inx
        cpx #16
        bcc z80_wr_ffe0

        ; --- Write 8502 boot trampoline at $1100 ---
        ; 1100: LDA #$00 / STA $FF00 / JMP ($FFFC)
        ldx #0
z80_wr_1100:
        lda z80_1100_data,x
        sta p4_data
        txa
        clc
        adc #$00
        sta p4_addr_lo
        lda #$11
        sta p4_addr_hi
        phx
        jsr C128_Write
        plx
        inx
        cpx #8
        bcc z80_wr_1100

        ; --- Set INIT_STATUS ($0A04) = 0 ---
        lda #$00
        sta p4_data
        lda #$04
        sta p4_addr_lo
        lda #$0A
        sta p4_addr_hi
        jsr C128_Write

        ; --- Set $0A03 = 0 (40/80 column flag area) ---
        lda #$00
        sta p4_data
        lda #$03
        sta p4_addr_lo
        lda #$0A
        sta p4_addr_hi
        jsr C128_Write

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
TRACE_ENABLED = 1       ; Set to 0 to disable breakpoint

; Breakpoint address - halt when PC hits this
BREAK_ADDR_LO = $00
BREAK_ADDR_HI = $FD     ; Break at $FD00

; ============================================================
; P4CPU_StepMultiple - Execute multiple instructions
; Simple batch version - calls P4CPU_Step repeatedly
; ============================================================
BATCH_SIZE = 64         ; Number of instructions per batch

P4CPU_StepMultiple:
C128_CPUStepMultiple:
        ; Check monitor ONCE per batch, not per instruction
        jsr P4MON_Check
        bcs _sm_monitor_active  ; Monitor took over, skip batch
        
        ldx #BATCH_SIZE
_sm_batch_loop:
        phx                     ; Save counter
        jsr P4CPU_Step
        plx                     ; Restore counter
        dex
        bne _sm_batch_loop
_sm_monitor_active:
        rts

; ============================================================
; P4CPU_Step - Single instruction execution
; ============================================================
P4CPU_Step:
        ; --- Boot milestone & hook checks ---
        ; Milestones write progress byte to $0FE0F via 32-bit store.
        ; Check $0FE0F in the monitor to see how far boot got.
        ;
        ; Values:  $01=E000  $02=E0CD  $03=E1F0  $04=E242
        ;          $05=E109  $06=E147(past raster1)  $07=E024
        ;          $08=E093  $09=E056  $0A=C07B  $0B=B000
        ;          $0C=B021  $0D=BASIC running
        ;
        lda p4_pc_hi

        ; Hook $CE0C: VDC screen clear (skip the polling loop)
        cmp #$CE
        bne _hook_not_ce
        lda p4_pc_lo
        cmp #$0C
        bne _hook_not_ce
        jsr hook_vdc_screen_clear
        lda #4
        #finish_cycles_inline
        rts
_hook_not_ce:

        ; Hook $E142/$E14E: raster wait loops (skip busy waits)
        cmp #$E1
        bne _hook_not_e1
        lda p4_pc_lo
        cmp #$42
        beq hook_raster_wait_1
        cmp #$4E
        beq hook_raster_wait_2
        ; Fine-grained milestones inside E109-E154
        cmp #$09
        bne +
        lda #$05
        jsr write_milestone     ; $05 = E109 entered
+       lda p4_pc_lo
        cmp #$14
        bne +
        lda #$15
        jsr write_milestone     ; $15 = E114 (STA $DC0E)
+       lda p4_pc_lo
        cmp #$2E
        bne +
        lda #$16
        jsr write_milestone     ; $16 = E12E (LDA #$07 STA $DD00)
+       lda p4_pc_lo
        cmp #$38
        bne +
        lda #$17
        jsr write_milestone     ; $17 = E138 (STA $01 cpu port)
+       lda p4_pc_lo
        cmp #$40
        bne +
        lda #$18
        jsr write_milestone     ; $18 = E140 (LDX #$FF before raster)
+       lda p4_pc_lo
        cmp #$47
        bne +
        lda #$19
        jsr write_milestone     ; $19 = E147 (past raster wait 1)
+       lda p4_pc_lo
        cmp #$53
        bne +
        lda #$1A
        jsr write_milestone     ; $1A = E153 (INX past raster wait 2)
+       lda p4_pc_lo
        cmp #$54
        bne +
        lda #$10
        jsr write_milestone     ; $10 = E154
+       lda p4_pc_lo
        cmp #$5C
        bne +
        lda #$11
        jsr write_milestone     ; $11 = E15C
+       lda p4_pc_lo
        cmp #$6E
        bne +
        lda #$12
        jsr write_milestone     ; $12 = E16E
+       lda p4_pc_lo
        cmp #$79
        bne +
        lda #$13
        jsr write_milestone     ; $13 = E179 (JSR E1DC)
+       lda p4_pc_lo
        cmp #$7E
        bne +
        lda #$14
        jsr write_milestone     ; $14 = E17E (after JSR E1DC returns)
+       ; E1DC = VDC init loop entry
        lda p4_pc_lo
        cmp #$DC
        bne +
        lda #$20
        jsr write_milestone     ; $20 = E1DC (VDC loop entry/iteration)
        jsr write_loop_counter  ; increment counter at $0FE0E
+       ; E1E6 = STY $D600 inside VDC loop
        lda p4_pc_lo
        cmp #$E6
        bne +
        lda #$21
        jsr write_milestone     ; $21 = E1E6 (STY $D600 in loop)
+       ; E1EC = BPL at end of VDC loop
        lda p4_pc_lo
        cmp #$EC
        bne +
        lda #$22
        jsr write_milestone     ; $22 = E1EC (BPL loop-back)
+       ; E1EE = loop exit (end marker hit)
        lda p4_pc_lo
        cmp #$EE
        bne +
        lda #$23
        jsr write_milestone     ; $23 = E1EE (loop done, about to RTS)
+       ; E1EF = RTS from E1DC
        lda p4_pc_lo
        cmp #$EF
        bne +
        lda #$24
        jsr write_milestone     ; $24 = E1EF (RTS)
+       jmp _no_hooks
_hook_not_e1:

        ; Milestones in $E0xx page
        cmp #$E0
        bne _hook_not_e0
        lda p4_pc_lo
        cmp #$00
        bne +
        lda #$01
        jsr write_milestone     ; $01 = E000
+       lda p4_pc_lo
        cmp #$24
        bne +
        lda #$07
        jsr write_milestone     ; $07 = E024 (post-E109)
+       lda p4_pc_lo
        cmp #$93
        bne +
        lda #$08
        jsr write_milestone     ; $08 = E093 (IOINIT)
+       lda p4_pc_lo
        cmp #$56
        bne +
        lda #$09
        jsr write_milestone     ; $09 = E056 (RAMTAS)
+       lda p4_pc_lo
        cmp #$CD
        bne _hook_not_e0
        lda #$02
        jsr write_milestone     ; $02 = E0CD (bank copy)
_hook_not_e0:

        ; Milestones in other pages
        lda p4_pc_hi
        cmp #$E2
        bne +
        lda p4_pc_lo
        cmp #$42
        bne +
        lda #$04
        jsr write_milestone     ; $04 = E242 (C128 mode check)
+
        lda p4_pc_hi
        cmp #$E1
        bne +
        lda p4_pc_lo
        cmp #$F0
        bne +
        lda #$03
        jsr write_milestone     ; $03 = E1F0 (auto-boot check)
+
        ; CINT screen editor init
        lda p4_pc_hi
        cmp #$C0
        bne _hook_not_c0
        lda p4_pc_lo
        cmp #$7B
        bne +
        lda #$0A
        jsr write_milestone     ; $0A = C07B (CINT entered)
+       lda p4_pc_lo
        cmp #$8B
        bne +
        lda #$40
        jsr write_milestone     ; $40 = C08B (JSR $FFCC / CLRCHN)
+       lda p4_pc_lo
        cmp #$8E
        bne +
        lda #$41
        jsr write_milestone     ; $41 = C08E (past CLRCHN)
+       lda p4_pc_lo
        cmp #$D0
        bne +
        lda #$42
        jsr write_milestone     ; $42 = C0D0 (JSR $C983)
+       lda p4_pc_lo
        cmp #$D3
        bne +
        lda #$43
        jsr write_milestone     ; $43 = C0D3 (past C983)
+       lda p4_pc_lo
        cmp #$F6
        bne +
        lda #$44
        jsr write_milestone     ; $44 = C0F6 (copy loop done)
+
_hook_not_c0:

        ; Deeper CINT milestones (page $C1)
        lda p4_pc_hi
        cmp #$C1
        bne _hook_not_c1
        lda p4_pc_lo
        cmp #$01
        bne +
        lda #$45
        jsr write_milestone     ; $45 = C101 (BIT $0A04)
+       lda p4_pc_lo
        cmp #$11
        bne +
        lda #$46
        jsr write_milestone     ; $46 = C111 (copy CEA8 to $1000)
+       lda p4_pc_lo
        cmp #$24
        bne +
        lda #$47
        jsr write_milestone     ; $47 = C124 (JSR $CD2E)
+       lda p4_pc_lo
        cmp #$2A
        bne +
        lda #$48
        jsr write_milestone     ; $48 = C12A (JSR $CA24)
+       lda p4_pc_lo
        cmp #$2D
        bne +
        lda #$49
        jsr write_milestone     ; $49 = C12D (JSR $C142)
+       lda p4_pc_lo
        cmp #$30
        bne +
        lda #$4A
        jsr write_milestone     ; $4A = C130 (JSR $CD2E again)
+       lda p4_pc_lo
        cmp #$39
        bne +
        lda #$4B
        jsr write_milestone     ; $4B = C139 (BIT $D505)
+       lda p4_pc_lo
        cmp #$41
        bne +
        lda #$4C
        jsr write_milestone     ; $4C = C141 (RTS from CINT)
+       ; Inside C142
        lda p4_pc_lo
        cmp #$42
        bne +
        lda #$50
        jsr write_milestone     ; $50 = C142 (entered)
+       lda p4_pc_lo
        cmp #$45
        bne +
        lda #$51
        jsr write_milestone     ; $51 = C145 (JSR $C15E)
+       lda p4_pc_lo
        cmp #$48
        bne +
        lda #$52
        jsr write_milestone     ; $52 = C148 (JSR $C4A5)
+       lda p4_pc_lo
        cmp #$4B
        bne +
        lda #$53
        jsr write_milestone     ; $53 = C14B (CPX loop check)
+       lda p4_pc_lo
        cmp #$5C
        bne +
        lda #$55
        jsr write_milestone     ; $55 = C15C (after CPX loop)
+       lda p4_pc_lo
        cmp #$7A
        bne +
        lda #$56
        jsr write_milestone     ; $56 = C17A
+       lda p4_pc_lo
        cmp #$91
        bne +
        lda #$57
        jsr write_milestone     ; $57 = C191
+       lda p4_pc_lo
        cmp #$93
        bne +
        lda #$58
        jsr write_milestone     ; $58 = C193 (CINT RTS!)
+
_hook_not_c1:

        ; C4A5 screen clear milestones
        lda p4_pc_hi
        cmp #$C4
        bne _hook_not_c4
        lda p4_pc_lo
        cmp #$A5
        bne +
        lda #$54
        jsr write_milestone     ; $54 = C4A5 (entered)
+
_hook_not_c4:
        jmp _past_c4_data
_c4c0_fired: .byte 0
_past_c4_data:
        ; Post-CINT milestones
        lda p4_pc_hi
        cmp #$E0
        bne _hook_not_e0_post
        lda p4_pc_lo
        cmp #$3A
        bne +
        lda #$30
        jsr write_milestone     ; $30 = E03A (PLA after CINT returned)
+       lda p4_pc_lo
        cmp #$3B
        bne +
        lda #$31
        jsr write_milestone     ; $31 = E03B (CLI - enables interrupts!)
+       lda p4_pc_lo
        cmp #$3E
        bne +
        lda #$32
        jsr write_milestone     ; $32 = E03E (JMP $B000)
+       lda p4_pc_lo
        cmp #$3C
        bne +
        lda #$33
        jsr write_milestone     ; $33 = E03C (instruction after CLI)
+
_hook_not_e0_post:

        ; BASIC cold start paths
        lda p4_pc_hi
        cmp #$E0
        bne _not_e0_alt
        lda p4_pc_lo
        cmp #$41
        bne +
        lda #$35
        jsr write_milestone     ; $35 = E041 (BMI target - alt boot)
+       lda p4_pc_lo
        cmp #$45
        bne +
        lda #$36
        jsr write_milestone     ; $36 = E045 JMP ($0A00)
+
_not_e0_alt:

        lda p4_pc_hi
        cmp #$40
        bne _not_40
        lda p4_pc_lo
        cmp #$00
        bne +
        lda #$37
        jsr write_milestone     ; $37 = 4000 (BASIC LO entry!)
+       lda p4_pc_lo
        cmp #$23
        bne +
        lda #$38
        jsr write_milestone     ; $38 = 4023 (BASIC LO cold start)
+       lda p4_pc_lo
        cmp #$45
        bne +
        lda #$39
        jsr write_milestone     ; $39 = 4045
+       lda p4_pc_lo
        cmp #$1C
        bne +
        lda #$3A
        jsr write_milestone     ; $3A = 401C (CLI before main loop)
+
        ; Check for $4Dxx range
        lda p4_pc_hi
        cmp #$4D
        bne _not_4d
        lda p4_pc_lo
        cmp #$37
        bne +
        lda #$3B
        jsr write_milestone     ; $3B = 4D37 (BASIC main loop entry)
+       lda p4_pc_lo
        cmp #$B7
        bne +
        lda #$3C
        jsr write_milestone     ; $3C = 4DB7 (after JMP ($0300))
+       lda p4_pc_lo
        cmp #$C6
        bne +
        lda #$3D
        jsr write_milestone     ; $3D = 4DC6 (input loop)
+
_not_4d:
        ; Check for $4Fxx (input routine)
        lda p4_pc_hi
        cmp #$4F
        bne _not_4f
        lda p4_pc_lo
        cmp #$93
        bne +
        lda #$3E
        jsr write_milestone     ; $3E = 4F93 (line input)
+
_not_4f:

_not_40:

        lda p4_pc_hi
        cmp #$B0
        bne +
        lda p4_pc_lo
        cmp #$00
        bne +
        lda #$0B
        jsr write_milestone     ; $0B = B000 (BASIC HI entry)
+       lda p4_pc_hi
        cmp #$B0
        bne +
        lda p4_pc_lo
        cmp #$03
        bne +
        lda #$34
        jsr write_milestone     ; $34 = B003 (BASIC IRQ handler)
+       lda p4_pc_hi
        cmp #$B0
        bne +
        lda p4_pc_lo
        cmp #$21
        bne +
        lda #$0C
        jsr write_milestone     ; $0C = B021 (BASIC HI init)
+
        ; Hook: VDC polling loops at $C543, $CDCF, $CDDD
        ; These poll $D600 bit 7 (VDC ready). Since we return $A0,
        ; they should exit immediately. But add hooks as safety.
        lda p4_pc_hi
        cmp #$CD
        bne _hook_not_cdxx
        lda p4_pc_lo
        cmp #$CF
        beq _hook_vdc_poll_skip
        cmp #$DD
        beq _hook_vdc_poll_skip
        jmp _hook_not_cdxx
_hook_vdc_poll_skip:
        ; Skip the BIT/BPL loop: advance PC past BPL
        ; BIT $D600 = 3 bytes, BPL = 2 bytes, total 5
        lda p4_pc_lo
        clc
        adc #$05
        sta p4_pc_lo
        lda #0
        sta p4_code_valid
        lda #4
        #finish_cycles_inline
        rts
_hook_not_cdxx:

        lda p4_pc_hi
        cmp #$C5
        bne _no_hooks
        lda p4_pc_lo
        cmp #$43
        bne _no_hooks
        ; $C543 VDC poll - skip it
        lda #$48                ; $C543 + 5 = $C548
        sta p4_pc_lo
        lda #0
        sta p4_code_valid
        lda #4
        #finish_cycles_inline
        rts

_no_hooks:
        ; DEBUG TRACE DISABLED
        ; lda p4_trace_enabled
        ; beq _skip_trace
        ; ... trace code ...
        
_skip_trace_old:
        ; Diagnostic: check if mmu_ram_bank is correct
        lda mmu_ram_bank
        sta $7700
        lda mmu_io_visible
        sta $7701
        lda mmu_kernal_rom
        sta $7702
        lda p4_pc_lo
        sta $7710
        lda p4_pc_hi
        sta $7711
        ; --------------------------------------------------------
        ; Check for pending interrupts (NMI first, then IRQ)
        ; --------------------------------------------------------
        lda p4_nmi_pending
        beq _step_chk_irq
        lda #0
        sta p4_nmi_pending
        jsr cpu_take_nmi
        lda #7
        #finish_cycles_inline
        rts
        
_step_chk_irq:
        lda p4_irq_pending
        beq _step_execute
        lda p4_p
        and #P_I
        bne _step_execute       ; I=1, IRQ masked
        
        lda #0
        sta p4_irq_pending
        jsr cpu_take_irq
        lda #7
        #finish_cycles_inline
        rts

_step_execute:
        lda #$00
        sta p4_xtra
        
        ; Save instruction PC for debugging
        lda p4_pc_lo
        sta p4_inst_pc_lo
        lda p4_pc_hi
        sta p4_inst_pc_hi

        ; Record PC and SP in trace ring buffer (only on page change or branch)
        ldx pc_trace_idx
        dex
        txa
        and #$7F
        tax
        lda p4_pc_hi
        cmp pc_trace_hi,x       ; Same page as last entry?
        beq _skip_trace2
        ; Different page - record it
        ldx pc_trace_idx
        sta pc_trace_hi,x
        lda p4_pc_lo
        sta pc_trace_lo,x
        lda p4_sp
        sta pc_trace_sp,x
        inx
        txa
        and #$7F
        sta pc_trace_idx
_skip_trace2:

        ; LIVE TRACE - DISABLED
live_trace_done:


        ; Check breakpoint BEFORE we fetch/execute
.if TRACE_ENABLED
        lda p4_inst_pc_hi
        cmp #BREAK_ADDR_HI
        bne _no_break
        lda p4_inst_pc_lo
        cmp #BREAK_ADDR_LO
        bne _no_break
        ; Hit breakpoint! Halt with red border
        lda #$02
        sta $d020
_break_halt:
        jmp _break_halt
_no_break:
.endif

        ; --------------------------------------------------------
        ; BASIC/KERNAL hooks - only for ROM area ($8000+)
        ; --------------------------------------------------------
        lda p4_pc_hi
        bpl _step_fetch         ; Skip hooks if PC < $8000
        
        ; Clear hook PC changed flag before calling hooks
        lda #0
        sta p4_hook_pc_changed
        
        jsr P4HOOK_CheckAndRun
        
        ; Check if hook modified PC - if so, invalidate code cache
        lda p4_hook_pc_changed
        beq _step_fetch
        lda #0
        sta p4_code_valid       ; Force code cache rebuild
        jmp _step_fetch

_trap_fb:
        ; Breakpoint hit at $FBEA - dump 128-entry trace to $7000
        ldx pc_trace_idx
        ldy #0
        sty _trap_sp_idx
_trap_trace:
        lda pc_trace_hi,x
        sta $7000,y
        iny
        lda pc_trace_lo,x
        sta $7000,y
        iny
        ; SP trace stored sequentially at $7100
        phy
        ldy _trap_sp_idx
        lda pc_trace_sp,x
        sta $7100,y
        iny
        sty _trap_sp_idx
        ply
        inx
        txa
        and #$7F
        tax
        cpy #0                  ; 256 bytes = 128 entries * 2
        bne _trap_trace
        lda p4_pc_hi
        sta $7200
        lda p4_pc_lo
        sta $7201
        lda p4_sp
        sta $7202
        lda mmu_cr
        sta $7203
        lda #$CC               ; trap marker
        sta $7204
        ; Check ROM byte at $1FF7F (should be $FA)
        lda #$7F
        sta C128_MEM_PTR+0
        lda #$FF
        sta C128_MEM_PTR+1
        lda #$01
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        sta $7205              ; actual byte at $1FF7F
        ; Dump shared RAM state
        lda shared_bottom_on
        sta $7206
        lda shared_bottom_mask
        sta $7207
        lda mmu_ram_bank
        sta $7208
        lda mmu_rcr
        sta $7209
        lda mmu_cr
        sta $720A
        lda p4_sp
        sta $720B
        ; Halt with white border
        lda #$01
        sta $d020
_trap_halt:
        jmp _trap_halt

_trap_sp_idx: .byte 0
_trap_prev_hi: .byte 0
        
_step_fetch:
        ; --------------------------------------------------------
        ; Fetch opcode and dispatch
        ; --------------------------------------------------------
        jsr fetch8
        asl                     ; opcode * 2, carry set if opcode >= $80
        tax
        bcc _step_dispatch_lo
        jmp (op_table_hi,x)     ; opcodes $80-$FF
_step_dispatch_lo:
        jmp (op_table_lo,x)     ; opcodes $00-$7F

; ============================================================
; finish_cycles - Handles cycle accounting, returns with RTS
; A = base cycle count on entry
; ============================================================
finish_cycles:
        ; Add extra cycles and accumulate
        clc
        adc p4_xtra
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
; Called when branch is taken. Sets p4_xtra to:
;   1 = branch taken, no page cross
;   2 = branch taken, page crossed
branch_do:
        lda #$01                ; Start with 1 (branch taken)
        sta p4_xtra
        lda p4_pc_lo
        clc
        adc p4_tmp
        sta p4_addr_lo
        lda p4_pc_hi
        adc #$00
        sta p4_addr_hi
        lda p4_tmp
        bpl _br_hi_ok
        dec p4_addr_hi
_br_hi_ok:
        lda p4_pc_hi
        cmp p4_addr_hi
        beq _br_same
        inc p4_xtra             ; Page crossed, increment to 2
_br_same:
        lda p4_addr_lo
        sta p4_pc_lo
        lda p4_addr_hi
        sta p4_pc_hi
        rts

; --- do_adc ---  (supports decimal mode when P_D set)
do_adc:
        sta p4_tmp

        ; If D flag clear -> original binary path
        lda p4_p
        and #P_D
        beq _do_adc_bin

        ; ---------- decimal ADC ----------
        ; Save original A
        lda p4_a
        sta p4_dec_a

        ; carry_in -> p4_vec_lo (0/1)
        lda p4_p
        and #P_C
        beq _adc_dec_c0
        lda #1
        bne _adc_dec_cstore
_adc_dec_c0:
        lda #0
_adc_dec_cstore:
        sta p4_vec_lo

        ; Binary add first (for V computation)
        lda p4_vec_lo
        beq _adc_dec_clc
        sec
        bne _adc_dec_go
_adc_dec_clc:
        clc
_adc_dec_go:
        lda p4_dec_a
        adc p4_tmp
        sta p4_a
        php                     ; save binary flags (esp V)

        ; Low nibble adjust test: (A_lo + M_lo + carry_in) > 9 ?
        lda p4_tmp
        and #$0F
        sta p4_tmp2             ; m_lo
        lda p4_dec_a
        and #$0F
        clc
        adc p4_tmp2
        clc
        adc p4_vec_lo           ; + carry_in (0/1)
        cmp #$0A
        bcc _adc_dec_no6
        lda p4_a
        clc
        adc #$06
        sta p4_a
_adc_dec_no6:

        ; High adjust if result >= $9A (i.e., > 99 in BCD)
        lda #0
        sta p4_vec_hi           ; decimal carry out (0/1)
        lda p4_a
        cmp #$9A
        bcc _adc_dec_no60
        clc
        adc #$60
        sta p4_a
        lda #1
        sta p4_vec_hi
_adc_dec_no60:

        ; Restore binary flags for V via PLP, then rebuild p4_p C/V
        plp
        lda p4_p
        and #(~(P_C|P_V)) & $ff
        sta p4_p

        ; C from decimal carry (p4_vec_hi)
        lda p4_vec_hi
        beq _adc_dec_noc
        lda p4_p
        ora #P_C
        sta p4_p
_adc_dec_noc:

        ; V from binary add (host V flag after PLP)
        bvc _adc_dec_nov
        lda p4_p
        ora #P_V
        sta p4_p
_adc_dec_nov:
        lda p4_a
        ;jsr set_zn_a
        #set_zna
        rts

_do_adc_bin:
        ; ---------- your original binary ADC ----------
        lda p4_p
        and #P_C
        beq _adc_nc
        sec
        jmp _adc_go2
_adc_nc:
        clc
_adc_go2:
        lda p4_a
        adc p4_tmp
        sta p4_a
        php
        lda p4_p
        and #(~(P_C|P_V)) & $ff
        sta p4_p
        plp
        bcc _adc_noc2
        lda p4_p
        ora #P_C
        sta p4_p
_adc_noc2:
        bvc _adc_nov2
        lda p4_p
        ora #P_V
        sta p4_p
_adc_nov2:
        lda p4_a
        ;jsr set_zn_a
        #set_zna
        rts


; --- do_sbc --- (supports decimal mode when P_D set)
do_sbc:
        sta p4_tmp

        ; If D flag clear -> original binary path
        lda p4_p
        and #P_D
        bne _do_sbc_decimal      ; D set, do decimal
        jmp _do_sbc_bin          ; D clear, do binary

_do_sbc_decimal:
        ; ---------- decimal SBC ----------
        ; Save original A for nibble comparisons
        lda p4_a
        sta p4_dec_a

        ; Do binary subtraction first (for V flag)
        lda p4_p
        and #P_C
        beq _sbc_dec_clc
        sec
        bne _sbc_dec_go
_sbc_dec_clc:
        clc
_sbc_dec_go:
        lda p4_a
        sbc p4_tmp
        sta p4_a
        php                     ; save flags for V

        ; Save carry (1 = no borrow, 0 = borrow)
        lda #0
        rol                     ; A = carry (0 or 1)
        sta p4_vec_hi           ; save for later

        ; Check if low nibble needs adjustment
        ; If (A_lo & $0F) > (orig_A_lo & $0F), we had a borrow from high nibble
        lda p4_a
        and #$0F
        sta p4_tmp2             ; result low nibble
        lda p4_dec_a
        and #$0F                ; original low nibble
        cmp p4_tmp2
        bcs _sbc_no_lo_adj      ; orig >= result, no low borrow
        ; Low nibble borrowed, subtract 6
        lda p4_a
        sec
        sbc #$06
        sta p4_a
_sbc_no_lo_adj:

        ; Check if high nibble needs adjustment
        ; If we had an overall borrow (carry was 0), subtract $60
        lda p4_vec_hi
        bne _sbc_no_hi_adj      ; carry was 1, no borrow
        lda p4_a
        sec
        sbc #$60
        sta p4_a
_sbc_no_hi_adj:

        ; Restore flags and set C/V in p4_p
        plp
        lda p4_p
        and #(~(P_C|P_V)) & $ff
        sta p4_p

        ; Set C from saved carry
        lda p4_vec_hi
        beq _sbc_dec_noc
        lda p4_p
        ora #P_C
        sta p4_p
_sbc_dec_noc:

        ; Set V from binary subtract
        bvc _sbc_dec_nov
        lda p4_p
        ora #P_V
        sta p4_p
_sbc_dec_nov:
        lda p4_a
        ;jsr set_zn_a
        #set_zna
        rts

_do_sbc_bin:
        ; ---------- your original binary SBC ----------
        lda p4_p
        and #P_C
        bne _sbc_c
        clc
        jmp _sbc_go2
_sbc_c:
        sec
_sbc_go2:
        lda p4_a
        sbc p4_tmp
        sta p4_a
        php
        lda p4_p
        and #(~(P_C|P_V)) & $ff
        sta p4_p
        plp
        bcc _sbc_noc2
        lda p4_p
        ora #P_C
        sta p4_p
_sbc_noc2:
        bvc _sbc_nov2
        lda p4_p
        ora #P_V
        sta p4_p
_sbc_nov2:
        lda p4_a
        ;jsr set_zn_a
        #set_zna
        rts

; --- do_cmp ---
do_cmp:
        sta p4_tmp
        lda p4_p
        and #(~(P_C|P_Z|P_N)) & $ff
        sta p4_p
        lda p4_a
        cmp p4_tmp
        bcc _cmp_noc
        lda p4_p
        ora #P_C
        sta p4_p
_cmp_noc:
        lda p4_a
        sec
        sbc p4_tmp
        beq _cmp_z
        bmi _cmp_n
        rts
_cmp_z:
        lda p4_p
        ora #P_Z
        sta p4_p
        rts
_cmp_n:
        lda p4_p
        ora #P_N
        sta p4_p
        rts

; --- do_cpx ---
do_cpx:
        sta p4_tmp
        lda p4_p
        and #(~(P_C|P_Z|P_N)) & $ff
        sta p4_p
        lda p4_x
        cmp p4_tmp
        bcc _cpx_noc
        lda p4_p
        ora #P_C
        sta p4_p
_cpx_noc:
        lda p4_x
        sec
        sbc p4_tmp
        beq _cpx_z
        bmi _cpx_n
        rts
_cpx_z:
        lda p4_p
        ora #P_Z
        sta p4_p
        rts
_cpx_n:
        lda p4_p
        ora #P_N
        sta p4_p
        rts

; --- do_cpy ---
do_cpy:
        sta p4_tmp
        lda p4_p
        and #(~(P_C|P_Z|P_N)) & $ff
        sta p4_p
        lda p4_y
        cmp p4_tmp
        bcc _cpy_noc
        lda p4_p
        ora #P_C
        sta p4_p
_cpy_noc:
        lda p4_y
        sec
        sbc p4_tmp
        beq _cpy_z
        bmi _cpy_n
        rts
_cpy_z:
        lda p4_p
        ora #P_Z
        sta p4_p
        rts
_cpy_n:
        lda p4_p
        ora #P_N
        sta p4_p
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
        lda p4_inst_pc_hi
        sta $7050               ; PC hi where BRK was
        lda p4_inst_pc_lo
        sta $7051               ; PC lo where BRK was
        lda p4_sp
        sta $7052
        lda p4_a
        sta $7053
        lda p4_x
        sta $7054
        lda p4_y
        sta $7055
        lda p4_p
        sta $7056
        lda mmu_cr
        sta $7057

        ; Standard BRK sequence
        ; BRK: increment PC past signature byte
        inw p4_pc_lo
        ; Push PC high
        lda p4_pc_hi
        sta p4_data
        jsr push_data
        ; Push PC low
        lda p4_pc_lo
        sta p4_data
        jsr push_data
        ; Push P with B=1, U=1
        lda p4_p
        ora #P_B                ; Set B flag (distinguishes BRK from IRQ)
        ora #P_U
        sta p4_data
        jsr push_data
        ; Set I flag
        lda p4_p
        ora #P_I
        sta p4_p
        ; Load IRQ vector from $FFFE/$FFFF
        lda #$FE
        sta p4_addr_lo
        lda #$FF
        sta p4_addr_hi
        jsr C128_ReadFast
        sta p4_pc_lo
        lda #$FF
        sta p4_addr_lo
        jsr C128_ReadFast
        sta p4_pc_hi
        lda #0
        sta p4_code_valid
        lda #7
        jmp finish_cycles

; $01 ORA (zp,X)
op_01:
        jsr addr_indx
        jsr C128_ReadFast
        ora p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $05 ORA zp
op_05:
        ; Optimized ORA zp
        jsr fetch8
        tax
        jsr lrb_read_x
        ora p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #3
        #finish_cycles_inline
        rts

; $06 ASL zp
op_06:
        jsr fetch8
        tax
        ; ASL directly in LOW_RAM_BUFFER
        jsr lrb_asl_x
        ; Update carry flag
        lda p4_p
        and #(~P_C) & $ff
        bcc _op06_nc
        ora #P_C
_op06_nc:
        sta p4_p
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $08 PHP
op_08:
        lda p4_p
        ora #(P_B|P_U)
        sta p4_data
        jsr push_data
        lda #3
        #finish_cycles_inline
        rts

; $09 ORA #imm
op_09:
        jsr fetch8
        ora p4_a
        sta p4_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $0A ASL A
op_0a:
        lda p4_a
        asl
        sta p4_a
        php
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        plp
        bcc _op0a_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op0a_nc:
        lda p4_a
        ;jsr set_zn_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $0D ORA abs
op_0d:
        jsr addr_abs
        jsr C128_ReadFast
        ora p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $0E ASL abs
op_0e:
        jsr addr_abs
        jsr C128_ReadFast
        asl
        php
        sta p4_data
        jsr C128_Write
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        plp
        bcc _op0e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op0e_nc:
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $10 BPL
op_10:
        jsr fetch_rel
        bbs 7, p4_p,_op10_nt
        ;lda p4_p
        ;and #P_N
        ;bne _op10_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op10_nt:
        lda #0
        sta p4_xtra
        lda #2
        #finish_cycles_inline
        rts

; $11 ORA (zp),Y
op_11:
        jsr addr_indy
        jsr C128_ReadFast
        ora p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $15 ORA zp,X
op_15:
        ; Optimized ORA zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_read_x
        ora p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $16 ASL zp,X
op_16:
        ; Optimized ASL zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        ; ASL directly in LOW_RAM_BUFFER
        jsr lrb_asl_x
        ; Update carry flag
        lda p4_p
        and #(~P_C) & $ff
        bcc _op16_nc
        ora #P_C
_op16_nc:
        sta p4_p
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $18 CLC
op_18:
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda #2
        #finish_cycles_inline
        rts

; $19 ORA abs,Y
op_19:
        jsr addr_absy
        jsr C128_ReadFast
        ora p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $1D ORA abs,X
op_1d:
        jsr addr_absx
        jsr C128_ReadFast
        ora p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $1E ASL abs,X
op_1e:
        jsr addr_absx
        jsr C128_ReadFast
        asl
        php
        sta p4_data
        jsr C128_Write
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        plp
        bcc _op1e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op1e_nc:
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #7
        #finish_cycles_inline
        rts

; $20 JSR
op_20:
        jsr fetch16_to_addr
        lda p4_addr_lo
        sta p4_vec_lo
        lda p4_addr_hi
        sta p4_vec_hi
        lda p4_pc_lo
        sec
        sbc #1
        sta p4_tmp
        lda p4_pc_hi
        sbc #0
        sta p4_data
        jsr push_data
        lda p4_tmp
        sta p4_data
        jsr push_data
        lda p4_vec_lo
        sta p4_pc_lo
        lda p4_vec_hi
        sta p4_pc_hi
        lda #6
        #finish_cycles_inline
        rts

; $21 AND (zp,X)
op_21:
        jsr addr_indx
        jsr C128_ReadFast
        and p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $24 BIT zp
op_24:
        ; Optimized BIT zp
        jsr fetch8
        tax
        jsr lrb_read_x
        sta p4_tmp
        lda p4_p
        and #(~(P_N|P_V|P_Z)) & $ff
        sta p4_p
        lda p4_tmp
        and #P_N
        ora p4_p
        sta p4_p
        lda p4_tmp
        and #P_V
        ora p4_p
        sta p4_p
        lda p4_tmp
        and p4_a
        bne _op24_nz
        lda p4_p
        ora #P_Z
        sta p4_p
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
        and p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #3
        #finish_cycles_inline
        rts

; $26 ROL zp
op_26:
        jsr fetch8
        tax
        ; Get old carry into bit 0 position
        lda p4_p
        and #P_C
        sta p4_tmp2
        ; Get memory, save bit 7 for new carry
        jsr lrb_read_x
        sta p4_tmp
        ; Shift left and OR in old carry
        asl
        ora p4_tmp2
        jsr lrb_write_x
        ; Update carry from old bit 7
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        bpl _op26_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op26_nc:
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $28 PLP
op_28:
        jsr pull_to_a
        and #(~P_B) & $ff
        ora #P_U
        sta p4_p
        lda #4
        #finish_cycles_inline
        rts

; $29 AND #imm
op_29:
        jsr fetch8
        and p4_a
        sta p4_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $2A ROL A
op_2a:
        lda p4_p
        and #P_C
        sta p4_tmp2
        lda p4_a
        sta p4_tmp
        asl
        ora p4_tmp2
        sta p4_a
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$80
        beq _op2a_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op2a_nc:
        lda p4_a
        ;jsr set_zn_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $2C BIT abs
op_2c:
        jsr addr_abs
        jsr C128_ReadFast
        sta p4_tmp
        lda p4_p
        and #(~(P_N|P_V|P_Z)) & $ff
        sta p4_p
        lda p4_tmp
        and #P_N
        ora p4_p
        sta p4_p
        lda p4_tmp
        and #P_V
        ora p4_p
        sta p4_p
        lda p4_tmp
        and p4_a
        bne _op2c_nz
        lda p4_p
        ora #P_Z
        sta p4_p
_op2c_nz:
        lda #4
        #finish_cycles_inline
        rts

; $2D AND abs
op_2d:
        jsr addr_abs
        jsr C128_ReadFast
        and p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $2E ROL abs
op_2e:
        jsr addr_abs
        jsr C128_ReadFast
        sta p4_tmp
        lda p4_p
        and #P_C
        sta p4_tmp2
        lda p4_tmp
        asl
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$80
        beq _op2e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op2e_nc:
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $30 BMI
op_30:
        jsr fetch_rel
        bbr 7, p4_p, _op30_nt
        ;lda p4_p
        ;and #P_N
        ;beq _op30_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op30_nt:
        lda #0
        sta p4_xtra
        lda #2
        #finish_cycles_inline
        rts

; $31 AND (zp),Y
op_31:
        jsr addr_indy
        jsr C128_ReadFast
        and p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $35 AND zp,X
op_35:
        ; Optimized AND zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_read_x
        and p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $36 ROL zp,X
op_36:
        ; Optimized ROL zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        ; Get old carry into bit 0 position
        lda p4_p
        and #P_C
        sta p4_tmp2
        ; Get memory, save bit 7 for new carry
        jsr lrb_read_x
        sta p4_tmp
        ; Shift left and OR in old carry
        asl
        ora p4_tmp2
        jsr lrb_write_x
        ; Update carry from old bit 7
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        bpl _op36_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op36_nc:
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $38 SEC
op_38:
        lda p4_p
        ora #P_C
        sta p4_p
        lda #2
        #finish_cycles_inline
        rts

; $39 AND abs,Y
op_39:
        jsr addr_absy
        jsr C128_ReadFast
        and p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $3D AND abs,X
op_3d:
        jsr addr_absx
        jsr C128_ReadFast
        and p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $3E ROL abs,X
op_3e:
        jsr addr_absx
        jsr C128_ReadFast
        sta p4_tmp
        lda p4_p
        and #P_C
        sta p4_tmp2
        lda p4_tmp
        asl
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$80
        beq _op3e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op3e_nc:
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #7
        #finish_cycles_inline
        rts

; $40 RTI
op_40:
        jsr pull_to_a
        and #(~P_B) & $ff
        ora #P_U
        sta p4_p
        jsr pull_to_a
        sta p4_pc_lo
        jsr pull_to_a
        sta p4_pc_hi
        lda #6
        #finish_cycles_inline
        rts

; $41 EOR (zp,X)
op_41:
        jsr addr_indx
        jsr C128_ReadFast
        eor p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $45 EOR zp
op_45:
        ; Optimized EOR zp
        jsr fetch8
        tax
        jsr lrb_read_x
        eor p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #3
        #finish_cycles_inline
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
        lda p4_p
        and #(~P_C) & $ff
        bcc _op46_nc
        ora #P_C
_op46_nc:
        sta p4_p
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $48 PHA
op_48:
        lda p4_a
        sta p4_data
        jsr push_data
        lda #3
        #finish_cycles_inline
        rts

; $49 EOR #imm
op_49:
        jsr fetch8
        eor p4_a
        sta p4_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $4A LSR A
op_4a:
        lda p4_a
        sta p4_tmp
        lsr
        sta p4_a
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op4a_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op4a_nc:
        lda p4_a
        ;jsr set_zn_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $4C JMP abs
op_4c:
        jsr fetch16_to_addr
        lda p4_addr_lo
        sta p4_pc_lo
        lda p4_addr_hi
        sta p4_pc_hi
        lda #3
        #finish_cycles_inline
        rts

; $4D EOR abs
op_4d:
        jsr addr_abs
        jsr C128_ReadFast
        eor p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $4E LSR abs
op_4e:
        jsr addr_abs
        jsr C128_ReadFast
        sta p4_tmp
        lsr
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op4e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op4e_nc:
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $50 BVC
op_50:
        jsr fetch_rel
        bbs 6, p4_p, _op50_nt
        ;lda p4_p
        ;and #P_V
        ;bne _op50_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op50_nt:
        lda #0
        sta p4_xtra
        lda #2
        #finish_cycles_inline
        rts

; $51 EOR (zp),Y
op_51:
        jsr addr_indy
        jsr C128_ReadFast
        eor p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $55 EOR zp,X
op_55:
        ; Optimized EOR zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_read_x
        eor p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $56 LSR zp,X
op_56:
        ; Optimized LSR zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        ; Shift right directly
        jsr lrb_read_x
        lsr
        jsr lrb_write_x
        ; Update carry - carry flag already set by LSR
        lda p4_p
        and #(~P_C) & $ff
        bcc _op56_nc
        ora #P_C
_op56_nc:
        sta p4_p
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $58 CLI
op_58:
        lda p4_p
        and #(~P_I) & $ff
        sta p4_p
        lda #2
        #finish_cycles_inline
        rts

; $59 EOR abs,Y
op_59:
        jsr addr_absy
        jsr C128_ReadFast
        eor p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $5D EOR abs,X
op_5d:
        jsr addr_absx
        jsr C128_ReadFast
        eor p4_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $5E LSR abs,X
op_5e:
        jsr addr_absx
        jsr C128_ReadFast
        sta p4_tmp
        lsr
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op5e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op5e_nc:
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #7
        #finish_cycles_inline
        rts

; $60 RTS
op_60:
        jsr pull_to_a
        sta p4_pc_lo
        jsr pull_to_a
        sta p4_pc_hi
        inw p4_pc_lo            ; 16-bit increment
        lda #6
        #finish_cycles_inline
        rts

; $61 ADC (zp,X)
op_61:
        jsr addr_indx
        jsr C128_ReadFast
        jsr do_adc
        lda #6
        #finish_cycles_inline
        rts

; $65 ADC zp
op_65:
        ; Optimized ADC zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_adc
        lda #3
        #finish_cycles_inline
        rts

; $66 ROR zp
op_66:
        jsr fetch8
        tax
        ; Get old carry into bit 7 position
        lda p4_p
        and #P_C
        beq _op66_nci
        lda #$80
        sta p4_tmp2
        bra _op66_do
_op66_nci:
        lda #$00
        sta p4_tmp2
_op66_do:
        ; Get memory, save bit 0 for new carry
        jsr lrb_read_x
        sta p4_tmp
        ; Shift right and OR in old carry
        lsr
        ora p4_tmp2
        jsr lrb_write_x
        ; Update carry from old bit 0
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op66_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op66_nc:
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $68 PLA
op_68:
        jsr pull_to_a
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $69 ADC #imm
op_69:
        jsr fetch8
        jsr do_adc
        lda #2
        #finish_cycles_inline
        rts

; $6A ROR A
op_6a:
        lda p4_p
        and #P_C
        beq _op6a_nci
        lda #$80
        sta p4_tmp2
        jmp _op6a_do
_op6a_nci:
        lda #$00
        sta p4_tmp2
_op6a_do:
        lda p4_a
        sta p4_tmp
        lsr
        ora p4_tmp2
        sta p4_a
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op6a_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op6a_nc:
        lda p4_a
        ;jsr set_zn_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $6C JMP (ind)
op_6c:
        jsr addr_ind_jmp

        lda p4_addr_lo
        sta p4_pc_lo
        lda p4_addr_hi
        sta p4_pc_hi
        lda #5
        #finish_cycles_inline
        rts

; $6D ADC abs
op_6d:
        jsr addr_abs
        jsr C128_ReadFast
        jsr do_adc
        lda #4
        #finish_cycles_inline
        rts

; $6E ROR abs
op_6e:
        jsr addr_abs
        jsr C128_ReadFast
        sta p4_tmp
        lda p4_p
        and #P_C
        beq _op6e_nci
        lda #$80
        sta p4_tmp2
        jmp _op6e_do
_op6e_nci:
        lda #$00
        sta p4_tmp2
_op6e_do:
        lda p4_tmp
        lsr
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op6e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op6e_nc:
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $70 BVS
op_70:
        jsr fetch_rel
        bbr 6, p4_p, _op70_nt
        ;lda p4_p
        ;and #P_V
        ;beq _op70_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op70_nt:
        lda #0
        sta p4_xtra
        lda #2
        #finish_cycles_inline
        rts

; $71 ADC (zp),Y
op_71:
        jsr addr_indy
        jsr C128_ReadFast
        jsr do_adc
        lda #5
        #finish_cycles_inline
        rts

; $75 ADC zp,X
op_75:
        ; Optimized ADC zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_read_x
        jsr do_adc
        lda #4
        #finish_cycles_inline
        rts

; $76 ROR zp,X
op_76:
        ; Optimized ROR zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        ; Get old carry into bit 7 position
        lda p4_p
        and #P_C
        beq _op76_nci
        lda #$80
        sta p4_tmp2
        bra _op76_do
_op76_nci:
        lda #$00
        sta p4_tmp2
_op76_do:
        ; Get memory, save bit 0 for new carry
        jsr lrb_read_x
        sta p4_tmp
        ; Shift right and OR in old carry
        lsr
        ora p4_tmp2
        jsr lrb_write_x
        ; Update carry from old bit 0
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op76_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op76_nc:
        ; Set N/Z from result
        jsr lrb_read_x
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $78 SEI
op_78:
        lda p4_p
        ora #P_I
        sta p4_p
        lda #2
        #finish_cycles_inline
        rts

; $79 ADC abs,Y
op_79:
        jsr addr_absy
        jsr C128_ReadFast
        jsr do_adc
        lda #4
        #finish_cycles_inline
        rts

; $7D ADC abs,X
op_7d:
        jsr addr_absx
        jsr C128_ReadFast
        jsr do_adc
        lda #4
        #finish_cycles_inline
        rts

; $7E ROR abs,X
op_7e:
        jsr addr_absx
        jsr C128_ReadFast
        sta p4_tmp
        lda p4_p
        and #P_C
        beq _op7e_nci
        lda #$80
        sta p4_tmp2
        jmp _op7e_do
_op7e_nci:
        lda #$00
        sta p4_tmp2
_op7e_do:
        lda p4_tmp
        lsr
        ora p4_tmp2
        sta p4_data
        lda p4_p
        and #(~P_C) & $ff
        sta p4_p
        lda p4_tmp
        and #$01
        beq _op7e_nc
        lda p4_p
        ora #P_C
        sta p4_p
_op7e_nc:
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #7
        #finish_cycles_inline
        rts

; $81 STA (zp,X)
op_81:
        jsr addr_indx
        lda p4_a
        sta p4_data
        jsr C128_Write
        lda #6
        #finish_cycles_inline
        rts

; $84 STY zp
op_84:
        ; Optimized STY zp
        jsr fetch8
        tax
        lda p4_y
        jsr lrb_write_x
        lda #3
        #finish_cycles_inline
        rts


; $85 STA zp
op_85:
        jsr fetch8
        tax
        lda p4_a
        jsr lrb_write_x
        lda #3
        #finish_cycles_inline
        rts

; $86 STX zp  
op_86:
        jsr fetch8
        tax
        lda p4_x
        jsr lrb_write_x
        lda #3
        #finish_cycles_inline
        rts

; $88 DEY
op_88:
        dec p4_y
        lda p4_y
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $8A TXA
op_8a:
        lda p4_x
        sta p4_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $8C STY abs
op_8c:
        jsr addr_abs
        lda p4_y
        sta p4_data
        jsr C128_Write
        lda #4
        #finish_cycles_inline
        rts

; $8D STA abs
op_8d:
        jsr addr_abs
        sei
        lda p4_a
        sta p4_data
        cli
        jsr C128_Write
        lda #4
        #finish_cycles_inline
        rts

; $8E STX abs
op_8e:
        jsr addr_abs
        lda p4_x
        sta p4_data
        jsr C128_Write
        lda #4
        #finish_cycles_inline
        rts

; $90 BCC
op_90:
        jsr fetch_rel

        ; P_C is Bit 0. If Bit 0 is Set, we do NOT take the branch (BCC).
        bbs 0, p4_p, _op90_nt 

        ;lda p4_p
        ;and #P_C
        ;bne _op90_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_op90_nt:
        lda #0
        sta p4_xtra
        lda #2
        #finish_cycles_inline
        rts

; $91 STA (zp),Y
op_91:
        jsr addr_indy
        lda p4_a
        sta p4_data
        jsr C128_Write
        lda #6
        #finish_cycles_inline
        rts

; $94 STY zp,X
op_94:
        ; Optimized STY zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        lda p4_y
        jsr lrb_write_x
        lda #4
        #finish_cycles_inline
        rts

; $95 STA zp,X
op_95:
        ; Optimized STA zp,X - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc p4_x
        tax
        lda p4_a
        jsr lrb_write_x
        lda #4
        #finish_cycles_inline
        rts

; $96 STX zp,Y
op_96:
        ; Optimized STX zp,Y - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc p4_y
        tax
        lda p4_x
        jsr lrb_write_x
        lda #4
        #finish_cycles_inline
        rts

; $98 TYA
op_98:
        lda p4_y
        sta p4_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $99 STA abs,Y
op_99:
        jsr addr_absy
        lda p4_a
        sta p4_data
        jsr C128_Write
        lda #5
        #finish_cycles_inline
        rts

; $9A TXS
op_9a:
        lda p4_x
        sta p4_sp
        lda #2
        #finish_cycles_inline
        rts

; $9D STA abs,X
op_9d:
        jsr addr_absx
        lda p4_a
        sta p4_data
        jsr C128_Write
        lda #5
        #finish_cycles_inline
        rts

; $A0 LDY #imm
op_a0:
        jsr fetch8
        sta p4_y
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $A1 LDA (zp,X)
op_a1:
        jsr addr_indx
        jsr C128_ReadFast
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $A2 LDX #imm
op_a2:
        jsr fetch8
        sta p4_x
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $A4 LDY zp
op_a4:
        ; Optimized LDY zp
        jsr fetch8
        tax
        jsr lrb_read_x
        sta p4_y
        #set_zna
        lda #3
        #finish_cycles_inline
        rts

; $A5 LDA zp
op_a5:
        ; Optimized LDA zp - direct access to LOW_RAM_BUFFER
        jsr fetch8              ; Get zero page address in A
        tax
        jsr lrb_read_x
        sta p4_a
        #set_zna
        lda #3
        #finish_cycles_inline
        rts

; $A6 LDX zp
op_a6:
        ; Optimized LDX zp
        jsr fetch8
        tax
        jsr lrb_read_x
        sta p4_x
        #set_zna
        lda #3
        #finish_cycles_inline
        rts

; $A8 TAY
op_a8:
        lda p4_a
        sta p4_y
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $A9 LDA #imm
op_a9:
        jsr fetch8
        sta p4_a
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $AA TAX
op_aa:
        lda p4_a
        sta p4_x
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $AC LDY abs
op_ac:
        jsr addr_abs
        jsr C128_ReadFast
        sta p4_y
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $AD LDA abs
op_ad:
        jsr addr_abs
        jsr C128_ReadFast
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $AE LDX abs
op_ae:
        jsr addr_abs
        jsr C128_ReadFast
        sta p4_x
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $B0 BCS
op_b0:
        jsr fetch_rel
        bbr 0, p4_p, _opb0_nt
        ;lda p4_p
        ;and #P_C
        ;beq _opb0_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_opb0_nt:
        lda #0
        sta p4_xtra
        lda #2
        #finish_cycles_inline
        rts

; $B1 LDA (zp),Y
op_b1:
        jsr addr_indy
        jsr C128_ReadFast
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $B4 LDY zp,X
op_b4:
        ; Optimized LDY zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_read_x
        sta p4_y
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $B5 LDA zp,X
op_b5:
        ; Optimized LDA zp,X - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_read_x
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $B6 LDX zp,Y
op_b6:
        ; Optimized LDX zp,Y - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc p4_y
        tax
        jsr lrb_read_x
        sta p4_x
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $B8 CLV
op_b8:
        lda p4_p
        and #(~P_V) & $ff
        sta p4_p
        lda #2
        #finish_cycles_inline
        rts

; $B9 LDA abs,Y
op_b9:
        jsr addr_absy
        jsr C128_ReadFast
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $BA TSX
op_ba:
        lda p4_sp
        sta p4_x
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $BC LDY abs,X
op_bc:
        jsr addr_absx
        jsr C128_ReadFast
        sta p4_y
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $BD LDA abs,X
op_bd:
        jsr addr_absx
        jsr C128_ReadFast
        sta p4_a
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $BE LDX abs,Y
op_be:
        jsr addr_absy
        jsr C128_ReadFast
        sta p4_x
        ;jsr set_zn_a
        #set_zna
        lda #4
        #finish_cycles_inline
        rts

; $C0 CPY #imm
op_c0:
        jsr fetch8
        jsr do_cpy
        lda #2
        #finish_cycles_inline
        rts

; $C1 CMP (zp,X)
op_c1:
        jsr addr_indx
        jsr C128_ReadFast
        jsr do_cmp
        lda #6
        #finish_cycles_inline
        rts

; $C4 CPY zp
op_c4:
        ; Optimized CPY zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_cpy
        lda #3
        #finish_cycles_inline
        rts

; $C5 CMP zp
op_c5:
        ; Optimized CMP zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_cmp
        lda #3
        #finish_cycles_inline
        rts

; $C6 DEC zp
op_c6:
        ; Optimized DEC zp
        jsr fetch8
        tax
        jsr lrb_dec_x
        jsr lrb_read_x
        ;jsr set_zn_a
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $C8 INY
op_c8:
        inc p4_y
        lda p4_y
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $C9 CMP #imm
op_c9:
        jsr fetch8
        jsr do_cmp
        lda #2
        #finish_cycles_inline
        rts

; $CA DEX
op_ca:
        dec p4_x
        lda p4_x
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $CC CPY abs
op_cc:
        jsr addr_abs
        jsr C128_ReadFast
        jsr do_cpy
        lda #4
        #finish_cycles_inline
        rts

; $CD CMP abs
op_cd:
        jsr addr_abs
        jsr C128_ReadFast
        jsr do_cmp
        lda #4
        #finish_cycles_inline
        rts

; $CE DEC abs
op_ce:
        jsr addr_abs
        jsr C128_ReadFast
        sec
        sbc #1
        sta p4_data
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $D0 BNE
op_d0:
        jsr fetch_rel
        bbs 1, p4_p, _opd0_nt
        ;lda p4_p
        ;and #P_Z
        ;bne _opd0_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_opd0_nt:
        lda #0
        sta p4_xtra
        lda #2
        #finish_cycles_inline
        rts

; $D1 CMP (zp),Y
op_d1:
        jsr addr_indy
        jsr C128_ReadFast
        jsr do_cmp
        lda #5
        #finish_cycles_inline
        rts

; $D5 CMP zp,X
op_d5:
        ; Optimized CMP zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_read_x
        jsr do_cmp
        lda #4
        #finish_cycles_inline
        rts

; $D6 DEC zp,X
op_d6:
        ; Optimized DEC zp,X - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_dec_x
        jsr lrb_read_x
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $D8 CLD
op_d8:
        lda p4_p
        and #(~P_D) & $ff
        sta p4_p
        lda #2
        #finish_cycles_inline
        rts

; $D9 CMP abs,Y
op_d9:
        jsr addr_absy
        jsr C128_ReadFast
        jsr do_cmp
        lda #4
        #finish_cycles_inline
        rts

; $DD CMP abs,X
op_dd:
        jsr addr_absx
        jsr C128_ReadFast
        jsr do_cmp
        lda #4
        #finish_cycles_inline
        rts

; $DE DEC abs,X
op_de:
        jsr addr_absx
        jsr C128_ReadFast
        sec
        sbc #1
        sta p4_data
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #7
        #finish_cycles_inline
        rts

; $E0 CPX #imm
op_e0:
        jsr fetch8
        jsr do_cpx
        lda #2
        #finish_cycles_inline
        rts

; $E1 SBC (zp,X)
op_e1:
        jsr addr_indx
        jsr C128_ReadFast
        jsr do_sbc
        lda #6
        #finish_cycles_inline
        rts

; $E4 CPX zp
op_e4:
        ; Optimized CPX zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_cpx
        lda #3
        #finish_cycles_inline
        rts

; $E5 SBC zp
op_e5:
        ; Optimized SBC zp
        jsr fetch8
        tax
        jsr lrb_read_x
        jsr do_sbc
        lda #3
        #finish_cycles_inline
        rts

; $E6 INC zp
op_e6:
        ; Optimized INC zp
        jsr fetch8
        tax
        jsr lrb_inc_x
        jsr lrb_read_x
        ;jsr set_zn_a
        #set_zna
        lda #5
        #finish_cycles_inline
        rts

; $E8 INX
op_e8:
        inc p4_x
        lda p4_x
        #set_zna
        lda #2
        #finish_cycles_inline
        rts

; $E9 SBC #imm
op_e9:
        jsr fetch8
        jsr do_sbc
        lda #2
        #finish_cycles_inline
        rts

; $EA NOP
op_ea:
        lda #2
        #finish_cycles_inline
        rts

; $EC CPX abs
op_ec:
        jsr addr_abs
        jsr C128_ReadFast
        jsr do_cpx
        lda #4
        #finish_cycles_inline
        rts

; $ED SBC abs
op_ed:
        jsr addr_abs
        jsr C128_ReadFast
        jsr do_sbc
        lda #4
        #finish_cycles_inline
        rts

; $EE INC abs
op_ee:
        jsr addr_abs
        jsr C128_ReadFast
        clc
        adc #1
        sta p4_data
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $F0 BEQ
op_f0:
        jsr fetch_rel
        bbr 1, p4_p, _opf0_nt
        ;lda p4_p
        ;and #P_Z
        ;beq _opf0_nt
        jsr branch_do
        lda #2
        #finish_cycles_inline
        rts
_opf0_nt:
        lda #0
        sta p4_xtra
        lda #2
        #finish_cycles_inline
        rts

; $F1 SBC (zp),Y
op_f1:
        jsr addr_indy
        jsr C128_ReadFast
        jsr do_sbc
        lda #5
        #finish_cycles_inline
        rts

; $F5 SBC zp,X
op_f5:
        ; Optimized SBC zp,X
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_read_x
        jsr do_sbc
        lda #4
        #finish_cycles_inline
        rts

; $F6 INC zp,X
op_f6:
        ; Optimized INC zp,X - direct access to LOW_RAM_BUFFER
        jsr fetch8
        clc
        adc p4_x
        tax
        jsr lrb_inc_x
        jsr lrb_read_x
        ;jsr set_zn_a
        #set_zna
        lda #6
        #finish_cycles_inline
        rts

; $F8 SED
op_f8:
        lda p4_p
        ora #P_D
        sta p4_p
        lda #2
        #finish_cycles_inline
        rts

; $F9 SBC abs,Y
op_f9:
        jsr addr_absy
        jsr C128_ReadFast
        jsr do_sbc
        lda #4
        #finish_cycles_inline
        rts

; $FD SBC abs,X
op_fd:
        jsr addr_absx
        jsr C128_ReadFast
        jsr do_sbc
        lda #4
        #finish_cycles_inline
        rts

; $FE INC abs,X
op_fe:
        jsr addr_absx
        jsr C128_ReadFast
        clc
        adc #1
        sta p4_data
        jsr C128_Write
        lda p4_data
        ;jsr set_zn_a
        #set_zna
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
        #finish_cycles_inline
        rts

; NOP abs (3 bytes) - $0C
op_0c:
        jsr fetch8              ; Skip low byte
        jsr fetch8              ; Skip high byte
        lda #4
        #finish_cycles_inline
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
        #finish_cycles_inline
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
        #finish_cycles_inline
        rts

; NOP implied (1 byte) - $1A, $3A, $5A, $7A, $DA, $FA
op_1a:
op_3a:
op_5a:
op_7a:
op_da:
op_fa:
        lda #2
        #finish_cycles_inline
        rts

; NOP immediate (2 bytes) - $80, $82, $89, $C2, $E2
op_80:
op_82:
op_89:
op_c2:
op_e2:
        jsr fetch8              ; Skip immediate byte
        lda #2
        #finish_cycles_inline
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
        lda p4_a
        and p4_x
        jsr lrb_write_x
        lda #3
        #finish_cycles_inline
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