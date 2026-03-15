; ============================================================
; c128_vdc.asm - VDC 8563 80-column display controller
; Host: MEGA65
; Assembler: 64tass (45GS02)
;
; Emulates the C128's VDC (Video Display Controller, 8563).
; The VDC is accessed via index/data registers at $D600/$D601.
; VDC RAM (16KB) is stored in MEGA65 bank 5 at $50000-$53FFF.
;
; The MEGA65's VIC-IV displays the 80-col screen at $054000.
; VDC screen writes are mirrored to MEGA65 screen RAM, and
; VDC attribute writes are translated (RGBI -> VIC-II color)
; and written to MEGA65 color RAM at $0FF80000.
;
; Key routines:
;   read_vdc_register   - Handle CPU reads from $D600-$D601
;   write_vdc_register  - Handle CPU writes to $D600-$D601
;   VDC_RenderFrame     - Batch copy VDC screen/attrs to MEGA65
;   VDC_UpdateCursor    - Draw/erase blinking cursor
;   VDC_SyncCursor      - Sync C128 ZP cursor from VDC position
;   vdc_print_char      - Direct char output at VDC cursor
; ============================================================

        .cpu "45gs02"

; ============================================================
; VDC RAM location in MEGA65 memory
; ============================================================
; VDC 16KB RAM stored in chip RAM at $50000-$53FFF (bank 5)
; 28-bit address: $50000
;   For DMA: MB = $00, bank = $05, addr = $0000 + offset
;   For 32-bit ZP ptr: +3=$00, +2=$05, +1=addr_hi, +0=addr_lo
; Max VDC addr $3FFF -> $53FFF, stays in bank 5.
VDC_RAM_BANK = $05              ; bank byte (bank 5 = display area)
VDC_RAM_MB   = $00              ; megabyte byte for 32-bit ptr
VDC_RAM_BASE = $0000            ; base offset within bank ($50000)

; ============================================================
; VDC Register State
; ============================================================
vdc_index:       .byte 0          ; Current VDC register index
vdc_regs:        .fill 10, 0     ; R0-R9
                 .byte $60       ; R10 = cursor mode: %11 blink, start line 0
                 .byte $07       ; R11 = cursor end line 7 (full block)
                 .byte $00, $00  ; R12:R13 = screen start ($0000)
                 .byte $00, $00  ; R14:R15 = cursor position
                 .fill 4, 0     ; R16-R19
                 .byte $08, $00  ; R20:R21 = attribute start ($0800)
                 .byte 80        ; R22 = chars per line (80)
                 .byte $08       ; R23 = char height (8 scan lines)
                 .byte $00       ; R24 = VSS (bit 7: 1=copy, 0=fill; bits 4-0: vert scroll)
                 .byte $07       ; R25 = attr mode flags
                 .byte $F0       ; R26 = color: white fg ($F), black bg ($0)
                 .byte 0         ; R27 = row increment
                 .byte $00       ; R28 = charset addr / RAM type
                 .fill 9, 0     ; R29-R37

; ============================================================
; VDC display state flags
; ============================================================
vdc_mode_active:    .byte 0       ; 0=40-col (no VDC render), 1=80-col (VDC active)
vdc_screen_dirty:   .byte 1       ; 1=screen RAM changed, needs DMA copy
vdc_attr_dirty:     .byte 1       ; 1=attribute RAM changed, needs translation
fco_offset_dirty:   .byte 1       ; 1=attr offset cache invalid (R12/R13/R20/R21 changed)

; 32-bit pointer for VDC color RAM writes (must be in zero page for [ptr],z)
vdc_color_ptr = $EC                     ; 4 bytes at $EC-$EF


; ============================================================
; VDC Register Reads ($D600-$D601)
; ============================================================
read_vdc_register:
        lda c128_addr_lo
        and #$01
        beq _rv_d600

        ; $D601: VDC data register - behavior depends on selected register
        ldx vdc_index
        cpx #31
        beq _rv_vdc_data         ; R31 = data read from VDC RAM
        cpx #38
        bcs _rv_vdc_open
        lda vdc_regs,x
        rts

_rv_d600:
        ; $D600: VDC status register
        ; Bit 7: ready (always return ready)
        ; Bit 5: vertical blank
        lda #$A0               ; Ready + VBlank
        rts

_rv_vdc_open:
        lda #$FF
        rts

_rv_vdc_data:
        ; R31: Read byte from VDC RAM at address R18:R19, auto-increment
        lda vdc_regs+19         ; address lo
        sta C128_MEM_PTR+0
        lda vdc_regs+18         ; address hi
        and #$3F                ; mask to 16KB
        clc
        adc #>VDC_RAM_BASE
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK       ; bank 5
        sta C128_MEM_PTR+2
        lda #VDC_RAM_MB         ; $00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        sta vdc_regs+31         ; update shadow with actual value
        ; Auto-increment R18:R19
        inc vdc_regs+19
        bne +
        inc vdc_regs+18
+       rts


; ============================================================
; VDC Register Writes ($D600-$D601)
; ============================================================
write_vdc_register:
        lda c128_addr_lo
        and #$01
        beq _wvdc_index

        ; $D601: Write VDC data register
        ldx vdc_index
        cpx #31
        beq _wvdc_data           ; R31 = data write to VDC RAM
        cpx #30
        beq _wvdc_word_count     ; R30 = word count (triggers block fill/copy)
        cpx #38
        bcs _wvdc_done
        lda c128_saved_data
        sta vdc_regs,x
        ; Mark dirty if screen/attr pointer regs changed
        cpx #12
        beq _wvdc_mark_scr_dirty
        cpx #13
        beq _wvdc_mark_scr_dirty
        cpx #20
        beq _wvdc_mark_attr_dirty
        cpx #21
        beq _wvdc_mark_attr_dirty
        cpx #24
        beq _wvdc_mark_attr_dirty  ; R24 bit 6 = reverse mode, affects attr rendering
        cpx #26
        beq _wvdc_update_colors
_wvdc_done:
        rts
_wvdc_mark_scr_dirty:
        lda #1
        sta vdc_screen_dirty
        sta fco_offset_dirty    ; screen start changed, invalidate attr offset cache
        rts
_wvdc_mark_attr_dirty:
        lda #1
        sta vdc_attr_dirty
        sta fco_offset_dirty    ; attr start changed, invalidate attr offset cache
        rts
_wvdc_update_colors:
        ; R26 written: high nibble = FG, low nibble = BG
        ; Apply BG color to MEGA65 $D021 when in 80-col mode
        ; so COLOR 6 command takes visible effect on VDC display
        lda vdc_mode_active
        beq _wvdc_done          ; 40-col mode: VIC handles its own colors
        lda vdc_regs+26
        and #$0F                ; low nibble = VDC background color (RGBI)
        tax
        lda vdc_to_vic_color,x  ; translate RGBI -> VIC-II color index
        sta $D020               ; border matches background on VDC
        sta $D021               ; apply to MEGA65 background
        rts

_wvdc_index:
        ; $D600: Set VDC register index
        lda c128_saved_data
        and #$3F               ; 6-bit register index
        sta vdc_index
        rts

_wvdc_data:
        ; R31: Write byte to VDC RAM at address R18:R19, auto-increment
        lda c128_saved_data
        sta vdc_regs+31         ; Always update shadow

_wvdc_data_go:
        ; VDC RAM at $50000: real_addr = $50000 + VDC_addr
        lda vdc_regs+19         ; address lo
        sta C128_MEM_PTR+0
        lda vdc_regs+18         ; address hi
        and #$3F                ; mask to 16KB
        clc
        adc #>VDC_RAM_BASE
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK       ; bank 5
        sta C128_MEM_PTR+2
        lda #VDC_RAM_MB         ; $00
        sta C128_MEM_PTR+3
        ldz #0
        lda c128_saved_data
        sta [C128_MEM_PTR],z

        ; Skip MEGA65 screen/color mirroring if in 40-col mode
        ; (VDC RAM is always written above, but MEGA65 display only
        ; needs updating when actively showing 80-col)
        ; Mark dirty so VDC_RenderFrame re-copies when switching to 80-col
        ldx vdc_mode_active
        bne _wvdc_do_mirror
        lda #1
        sta vdc_screen_dirty
        sta vdc_attr_dirty
        jmp _wvdc_data_skip
_wvdc_do_mirror:

        ; --- Inline attribute translation to MEGA65 color RAM ---
        ; Check if this write is to attribute area (R18 >= R20)
        lda vdc_regs+18
        cmp vdc_regs+20
        bcc _wvdc_is_screen
        bne _wvdc_is_attr
        lda vdc_regs+19
        cmp vdc_regs+21
        bcc _wvdc_is_screen

_wvdc_is_attr:
        ; Attribute write: translate color and write to color RAM or buffer
        ; Offset within attr area = (R18:R19) - (R20:R21)
        lda vdc_regs+19
        sec
        sbc vdc_regs+21
        sta vdc_color_ptr+0
        lda vdc_regs+18
        sbc vdc_regs+20
        sta vdc_color_ptr+1
        ; If displaying 40-col, redirect to 80-col save buffer in bank 5
        lda display_showing_80
        bne _wvdc_attr_live
        ; 40-col display: write to save buffer at $05A800 + offset
        lda vdc_color_ptr+1
        clc
        adc #>COLOR_80_ADDR     ; + $A8
        sta vdc_color_ptr+1
        lda #COLOR_80_BANK      ; bank $05
        sta vdc_color_ptr+2
        lda #COLOR_80_MB        ; MB $00
        sta vdc_color_ptr+3
        bra _wvdc_attr_do_write
_wvdc_attr_live:
        ; 80-col display: write to live color RAM at $0FF80000
        lda #$F8
        sta vdc_color_ptr+2
        lda #$0F
        sta vdc_color_ptr+3
_wvdc_attr_do_write:
        ; Translate attribute to color RAM
        ; If bit 6 (reverse) set, use R26 fg color (high nibble)
        lda c128_saved_data
        and #$40
        bne _wvdc_attr_rev
        ; Normal: use attribute low nibble (foreground)
        lda c128_saved_data
        and #$0F
        tax
        bra _wvdc_attr_write_color
_wvdc_attr_rev:
        ; Reverse: use R26 high nibble (foreground)
        lda vdc_regs+26
        lsr
        lsr
        lsr
        lsr
        tax
_wvdc_attr_write_color:
        lda vdc_to_vic_color,x
        ldz #0
        sta [vdc_color_ptr],z
        jmp _wvdc_data_skip

_wvdc_is_screen:
        ; Screen write: copy char directly to MEGA65 screen + offset
        ; Offset = (R18:R19) - (R12:R13)
        lda vdc_regs+19
        sec
        sbc vdc_regs+13
        sta vdc_color_ptr+0     ; Reuse ptr temporarily
        lda vdc_regs+18
        sbc vdc_regs+12
        clc
        adc #>C128_SCREEN_OFF   ; +screen offset high byte
        sta vdc_color_ptr+1
        lda #C128_SCREEN_BANK
        sta vdc_color_ptr+2
        lda #$00
        sta vdc_color_ptr+3
        ldz #0
        lda c128_saved_data
        sta [vdc_color_ptr],z

        ; Auto-increment R18:R19
_wvdc_data_skip:
        inc vdc_regs+19
        bne +
        inc vdc_regs+18
+       rts


_wvdc_word_count:
        ; R30: Writing word count triggers block copy or fill
        ; R30 triggers block fill/copy of additional bytes.
        ; The R31 write already wrote the first byte and advanced R18:R19.
        ; R30 value = number of additional bytes to fill/copy (not +1).
        ; Bit 7 of R24 selects copy (1) vs fill (0).
        lda c128_saved_data
        sta vdc_regs+30

        ; Count = raw value (16-bit: $00 means 0 extra bytes, $FF means 255)
        lda c128_saved_data
        sta _vdc_fill_count
        lda #0
        sta _vdc_fill_count+1

        ; Check bit 7 of R24 (VSS register): 1 = copy, 0 = fill
        ; (Oxyron VDC ref: R24 bit 7 = "Fill/Copy")
        lda vdc_regs+24
        bmi _vdc_block_copy

        ; --- Block fill via DMA ---
        ; Fill (count) bytes at VDC R18:R19 with R31 value

        ; Set fill value in source addr field (little-endian, hi byte=0)
        lda vdc_regs+31
        sta _vdc_fill_val
        lda #$00
        sta _vdc_fill_val+1

        ; Set destination: VDC_RAM_BANK, addr = R18:R19 + VDC_RAM_BASE
        lda vdc_regs+19
        clc
        adc #<VDC_RAM_BASE
        sta _vdc_fill_dst
        lda vdc_regs+18
        and #$3F
        adc #>VDC_RAM_BASE
        sta _vdc_fill_dst+1

        ; Trigger DMA fill
        lda #$00
        sta $D707
        .byte $80, $00          ; src MB = $00
        .byte $81, $00          ; dst MB = $00
        .byte $00               ; end options
        .byte $03               ; fill command
_vdc_fill_count:
        .word $0000             ; count (filled above)
_vdc_fill_val:
        .word $0000             ; fill value in src addr field (filled above)
        .byte $00               ; src bank (ignored for fill)
_vdc_fill_dst:
        .word $0000             ; dst addr (filled above)
        .byte $05               ; dst bank = 5 (VDC RAM)
        .byte $00               ; command high byte
        .word $0000             ; modulo

        ; Save original R18:R19 before advance (needed for MEGA65 mirror)
        lda vdc_regs+18
        sta _vdc_fill_orig_hi
        lda vdc_regs+19
        sta _vdc_fill_orig_lo

        ; Advance R18:R19 by count (as real VDC hardware would)
        clc
        lda vdc_regs+19
        adc _vdc_fill_count
        sta vdc_regs+19
        lda vdc_regs+18
        adc _vdc_fill_count+1
        sta vdc_regs+18

        ; Skip MEGA65 mirroring if in 40-col mode
        lda vdc_mode_active
        beq _wvdc_wc_done

        ; --- Also fill MEGA65 screen/color RAM ---
        ; Check if fill destination is screen or attr area
        lda _vdc_fill_orig_hi
        cmp vdc_regs+20
        bcs _vdc_fill_is_attr

        ; Screen area fill: mirror to MEGA65 screen
        ; Dest offset = original R18:R19 - R12:R13
        lda _vdc_fill_orig_lo
        sec
        sbc vdc_regs+13
        sta _vdc_scr_fill_dst
        lda _vdc_fill_orig_hi
        sbc vdc_regs+12
        clc
        adc #>C128_SCREEN_OFF   ; +screen offset high byte
        sta _vdc_scr_fill_dst+1

        lda _vdc_fill_count
        sta _vdc_scr_fill_cnt
        lda _vdc_fill_count+1
        sta _vdc_scr_fill_cnt+1

        lda vdc_regs+31
        sta _vdc_scr_fill_val
        lda #$00
        sta _vdc_scr_fill_val+1

        lda #$00
        sta $D707
        .byte $80, $00
        .byte $81, $00
        .byte $00
        .byte $03               ; fill command
_vdc_scr_fill_cnt:
        .word $0000
_vdc_scr_fill_val:
        .word $0000
        .byte $00
_vdc_scr_fill_dst:
        .word $0000
        .byte C128_SCREEN_BANK  ; dst bank
        .byte $00
        .word $0000
        rts

_vdc_fill_is_attr:
        ; Attribute area fill: mark dirty for batch processing
        lda #1
        sta vdc_attr_dirty
_wvdc_wc_done:
        rts

_vdc_fill_orig_hi: .byte 0
_vdc_fill_orig_lo: .byte 0

_vdc_block_copy:
        ; --- Block copy via DMA ---
        ; VDC 8563 block copy:
        ; R32:R33 = block copy source address
        ; R18:R19 = update address (destination)
        ; Count was already computed in _vdc_fill_count

        ; Set source: VDC_RAM_BANK, addr = R32:R33 + VDC_RAM_BASE
        lda vdc_regs+33
        clc
        adc #<VDC_RAM_BASE
        sta _vdc_copy_src
        lda vdc_regs+32
        and #$3F
        adc #>VDC_RAM_BASE
        sta _vdc_copy_src+1

        ; Set destination: VDC_RAM_BANK, addr = R18:R19 + VDC_RAM_BASE
        lda vdc_regs+19
        clc
        adc #<VDC_RAM_BASE
        sta _vdc_copy_dst
        lda vdc_regs+18
        and #$3F
        adc #>VDC_RAM_BASE
        sta _vdc_copy_dst+1

        ; Copy count into DMA list
        lda _vdc_fill_count
        sta _vdc_copy_count
        lda _vdc_fill_count+1
        sta _vdc_copy_count+1

        ; Trigger DMA copy
        lda #$00
        sta $D707
        .byte $80, $00          ; src MB = $00
        .byte $81, $00          ; dst MB = $00
        .byte $00               ; end options
        .byte $00               ; copy command (not fill)
_vdc_copy_count:
        .word $0000             ; count
_vdc_copy_src:
        .word $0000             ; src addr
        .byte $05               ; src bank = 5 (VDC RAM)
_vdc_copy_dst:
        .word $0000             ; dst addr
        .byte $05               ; dst bank = 5 (VDC RAM)
        .byte $00               ; command high byte
        .word $0000             ; modulo

        ; --- Also copy within MEGA65 screen RAM so display updates ---
        ; Skip MEGA65 mirroring if in 40-col mode
        lda vdc_mode_active
        beq _vdc_copy_advance

        ; Determine if this is a screen copy or attribute copy
        ; Screen area: R12:R13 to R20:R21-1
        ; Attribute area: R20:R21 onwards
        ; Check destination (R18): if < R20, it's screen area
        lda vdc_regs+18
        cmp vdc_regs+20
        bcs _vdc_copy_attr

        ; Screen area copy: mirror to MEGA65 screen
        ; Source offset from screen start = R32:R33 - R12:R13
        lda vdc_regs+33
        sec
        sbc vdc_regs+13
        sta _vdc_scr_copy_src
        lda vdc_regs+32
        sbc vdc_regs+12
        clc
        adc #>C128_SCREEN_OFF   ; +screen offset high byte
        sta _vdc_scr_copy_src+1

        ; Dest offset from screen start = R18:R19 - R12:R13
        lda vdc_regs+19
        sec
        sbc vdc_regs+13
        sta _vdc_scr_copy_dst
        lda vdc_regs+18
        sbc vdc_regs+12
        clc
        adc #>C128_SCREEN_OFF
        sta _vdc_scr_copy_dst+1

        ; Copy count
        lda _vdc_fill_count
        sta _vdc_scr_copy_cnt
        lda _vdc_fill_count+1
        sta _vdc_scr_copy_cnt+1

        lda #$00
        sta $D707
        .byte $80, $00          ; src MB = $00
        .byte $81, $00          ; dst MB = $00
        .byte $00               ; end options
        .byte $00               ; copy command
_vdc_scr_copy_cnt:
        .word $0000
_vdc_scr_copy_src:
        .word $0000
        .byte C128_SCREEN_BANK  ; src bank
_vdc_scr_copy_dst:
        .word $0000
        .byte C128_SCREEN_BANK  ; dst bank
        .byte $00
        .word $0000

        jmp _vdc_copy_advance

_vdc_copy_attr:
        ; Attribute area copy: translate and copy to MEGA65 color RAM
        ; For now, just mark attr dirty so the batch loop handles it
        lda #1
        sta vdc_attr_dirty

_vdc_copy_advance:
        ; Advance R18:R19 by count (as real VDC hardware would)
        clc
        lda vdc_regs+19
        adc _vdc_fill_count
        sta vdc_regs+19
        lda vdc_regs+18
        adc _vdc_fill_count+1
        sta vdc_regs+18

        ; Advance R32:R33 by count (source also advances)
        clc
        lda vdc_regs+33
        adc _vdc_fill_count
        sta vdc_regs+33
        lda vdc_regs+32
        adc _vdc_fill_count+1
        sta vdc_regs+32
        rts



; ============================================================
; vdc_print_char - Print a screen code at the VDC cursor
;
; Input: A = screen code (not PETSCII)
; Effect: writes to VDC RAM, mirrors to MEGA65 screen + color RAM,
;         advances cursor R14:R15
; Clobbers: A, X, Z
; ============================================================
vdc_print_char:
        sta _vpc_char

        ; Set R18:R19 = R14:R15 (cursor position)
        lda vdc_regs+14
        sta vdc_regs+18
        lda vdc_regs+15
        sta vdc_regs+19

        ; Write char to VDC RAM at R18:R19
        lda vdc_regs+19
        sta C128_MEM_PTR+0
        lda vdc_regs+18
        and #$3F
        clc
        adc #>VDC_RAM_BASE
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda _vpc_char
        sta [C128_MEM_PTR],z

        ; Compute screen offset = R18:R19 - R12:R13
        lda vdc_regs+19
        sec
        sbc vdc_regs+13
        sta _vpc_off_lo
        lda vdc_regs+18
        sbc vdc_regs+12
        sta _vpc_off_hi

        ; Bounds check: offset must be 0..1999
        bmi _vpc_skip_mirror    ; negative = off screen
        cmp #>2000
        bcc _vpc_do_mirror
        bne _vpc_skip_mirror
        lda _vpc_off_lo
        cmp #<2000
        bcs _vpc_skip_mirror

_vpc_do_mirror:
        ; Write to MEGA65 screen at C128_SCREEN_OFF + offset
        lda _vpc_off_lo
        sta C128_MEM_PTR+0
        lda _vpc_off_hi
        clc
        adc #>C128_SCREEN_OFF
        sta C128_MEM_PTR+1
        lda #C128_SCREEN_BANK
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda _vpc_char
        sta [C128_MEM_PTR],z

        ; Write color to color RAM at $0FF80000 + offset
        lda _vpc_off_lo
        sta vdc_color_ptr+0
        lda _vpc_off_hi
        sta vdc_color_ptr+1
        lda #$F8
        sta vdc_color_ptr+2
        lda #$0F
        sta vdc_color_ptr+3
        lda vdc_regs+26
        and #$0F
        tax
        lda vdc_to_vic_color,x
        ldz #0
        sta [vdc_color_ptr],z

_vpc_skip_mirror:
        ; Advance cursor R14:R15
        inc vdc_regs+15
        bne +
        inc vdc_regs+14
+
        ; Auto-increment R18:R19 to match
        inc vdc_regs+19
        bne +
        inc vdc_regs+18
+
        rts

_vpc_char:    .byte 0
_vpc_off_lo:  .byte 0
_vpc_off_hi:  .byte 0


; ============================================================
; VDC_RenderFrame - Copy VDC screen/attributes to MEGA65 display
;
; Called once per frame from VIC_FrameTasks.
; Copies VDC screen RAM to MEGA65 screen at $054000,
; then translates VDC attribute RAM to MEGA65 color RAM
; at $0FF80000, converting VDC RGBI colors to VIC-II indices
; via lookup table.
; ============================================================
VDC_RenderFrame:
        jsr ensure_viciv_regs
        ; Check if VDC is in bitmap mode (R25 bit 7)
        lda vdc_regs+25
        bmi _vdc_render_bitmap

        ; --- TEXT MODE ---
        ; Border/background from R26 low nibble (bg color)
        lda vdc_regs+26
        and #$0F
        tax
        lda vdc_to_vic_color,x
        sta $D020
        sta $D021

        ; --- TEXT MODE RENDERING ---
        ; Ensure VIC-IV is in text mode (no bitmap)
        lda vdc_bitmap_active
        beq _vdc_text_mode_ok
        ; Was in bitmap mode, switch back to text
        lda #$00
        sta vdc_bitmap_active
        lda #$1B
        sta $D011               ; bitmap off
        ; Restore CHARPTR to chargen
        lda #<CHARGEN_OFF
        sta $D068
        lda #>CHARGEN_OFF
        sta $D069
        lda #CHARGEN_BANK
        sta $D06A
        ; Restore SCRNPTR to 80-col text screen
        lda #$00
        sta $D060
        lda #$40
        sta $D061
        lda #$05
        sta $D062
        lda #$00
        sta $D063

_vdc_text_mode_ok:

        ; --- Step 1: DMA copy VDC screen RAM to MEGA65 screen ---
        ; Always copy every frame (FastCHROUT/PRIMM removed,
        ; ROM writes directly to VDC RAM via R31)

        ; Build the DMA source addr = VDC R12:R13 + VDC_RAM_BASE
        lda vdc_regs+13         ; R13 = screen start low
        clc
        adc #<VDC_RAM_BASE
        sta _vdc_dma_scr_src
        lda vdc_regs+12         ; R12 = screen start high
        and #$3F                ; mask to 16KB
        adc #>VDC_RAM_BASE
        sta _vdc_dma_scr_src+1

        lda #$00
        sta $D707               ; Enhanced DMA job
        .byte $80, $00          ; src MB = $00 (chip RAM)
        .byte $81, $00          ; dst MB = $00
        .byte $00               ; end options
        .byte $00               ; copy command
        .word 2000              ; count = 2000 bytes
_vdc_dma_scr_src:
        .word $0000             ; src addr (filled: VDC RAM + R12:R13)
_vdc_dma_scr_bank:
        .byte $05               ; src bank 5 (VDC RAM)
        .word $4000             ; dst addr = $4000
        .byte $05               ; dst bank = 5 ($054000)
        .byte $00               ; command high byte
        .word $0000             ; modulo

_vdc_skip_screen_dma:

        ; --- Step 1b: Screen-wide reverse mode via charset swap ---
        ; Instead of modifying 2000 screen bytes per frame, swap the
        ; charset between normal and pre-inverted versions.
        ; Only triggers on R24 bit 6 transition (not every frame).
        ;
        ; Normal charset at attic $8030000 (8KB)
        ; Reversed charset at attic $8040000 (8KB, EOR #$FF of normal)
        ; Active charset at bank 2 $02A000 (where CHARPTR points)
        lda vdc_regs+24
        and #$40
        cmp _vdc_prev_r24_rev   ; compare with previous state
        beq _vdc_no_rev_change  ; no transition, skip

        sta _vdc_prev_r24_rev   ; save new state (sta doesn't set flags)
        cmp #$40                ; test: reverse or normal?
        beq _vdc_swap_to_rev
        ; Transition to normal: DMA copy normal charset from attic to bank 2
        #dma_copy $80, $03, $0000, $00, $02, $A000, $2000
        bra _vdc_no_rev_change
_vdc_swap_to_rev:
        ; Transition to reverse: DMA copy reversed charset from attic to bank 2
        #dma_copy $80, $04, $0000, $00, $02, $A000, $2000

_vdc_no_rev_change:

        ; --- Step 2: Translate VDC attribute RAM -> MEGA65 color RAM ---
        ; Individual R31 writes are translated inline during writes.
        ; This batch loop only runs after R30 block fill/copy operations
        ; which set vdc_attr_dirty.
        lda vdc_attr_dirty
        beq _vdc_skip_attr

        lda #0
        sta vdc_attr_dirty      ; Clear dirty flag

        ; Source pointer: $50000 + VDC R20:R21
        lda vdc_regs+21
        sta C128_MEM_PTR+0
        lda vdc_regs+20
        and #$3F
        clc
        adc #>VDC_RAM_BASE
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK
        sta C128_MEM_PTR+2
        lda #VDC_RAM_MB
        sta C128_MEM_PTR+3

        ; Dest pointer: color RAM at $0FF80000
        lda #$00
        sta vdc_color_ptr+0
        lda #$00
        sta vdc_color_ptr+1
        lda #$F8
        sta vdc_color_ptr+2
        lda #$0F
        sta vdc_color_ptr+3

        ; Pre-translate R26 fg color for per-character reverse video
        lda vdc_regs+26
        lsr
        lsr
        lsr
        lsr                     ; high nibble = foreground
        tax
        lda vdc_to_vic_color,x
        sta _vdc_rev_bg_color   ; cached VIC-II fg color for reversed cells

        ; Process 7 full pages (1792 bytes) then 208 tail
        lda #7
        sta _vdc_page_count

_vdc_attr_page:
        ldz #0
_vdc_attr_inner:
        lda [C128_MEM_PTR],z    ; Read VDC attribute byte
        bit #$40                ; test bit 6 (per-char reverse)
        bne _vdc_attr_rev
        ; Normal: use attribute low nibble (foreground)
        and #$0F
        tax
        lda vdc_to_vic_color,x
        bra _vdc_attr_store
_vdc_attr_rev:
        ; Reversed: use R26 foreground color (cached)
        lda _vdc_rev_bg_color
_vdc_attr_store:
        sta [vdc_color_ptr],z   ; Write to color RAM
        inz
        bne _vdc_attr_inner

        ; Advance source and dest pointers by 256
        inc C128_MEM_PTR+1
        inc vdc_color_ptr+1

        dec _vdc_page_count
        bne _vdc_attr_page

        ; Remaining: 2000 - 1792 = 208 bytes
        ldz #0
_vdc_attr_tail:
        lda [C128_MEM_PTR],z
        bit #$40
        bne _vdc_attr_tail_rev
        and #$0F
        tax
        lda vdc_to_vic_color,x
        bra _vdc_attr_tail_store
_vdc_attr_tail_rev:
        lda _vdc_rev_bg_color
_vdc_attr_tail_store:
        sta [vdc_color_ptr],z
        inz
        cpz #208
        bne _vdc_attr_tail

_vdc_skip_attr:
        rts

_vdc_page_count:    .byte 0
_vdc_rev_bg_color:  .byte 0
_vdc_prev_r24_rev:  .byte 0         ; previous R24 bit 6 state for transition detection
vdc_bitmap_active: .byte 0     ; 1 = VIC-IV is in bitmap mode for VDC

; ============================================================
; VDC Bitmap Mode Rendering
;
; Converts VDC sequential bitmap (80 bytes/row, 200 rows)
; to VIC-IV character-cell bitmap format (8 consecutive bytes
; per cell, cells arranged left-to-right, top-to-bottom).
;
; VDC byte at row R, column C = VDC_RAM + R*80 + C
; VIC-IV byte for char_col C, char_row CR, pixel_row P =
;   bitmap_base + (CR*80 + C)*8 + P
;
; Source: VDC RAM at bank 5 $050000 (R12:R13 offset)
; Dest: bitmap buffer at bank 5 $054000 (reuse 80-col screen area)
; Screen RAM: needs sequential char indices at $054000... wait,
;   bitmap mode uses CHARPTR for data, SCRNPTR for color info.
;   Actually in VIC-II bitmap mode, CHARPTR = bitmap data.
;   SCRNPTR = screen RAM with color nibbles (hi=fg, lo=bg per cell).
; ============================================================
_vdc_render_bitmap:
        ; Set border/bg from R26 background color (low nibble, same as text mode)
        lda vdc_regs+26
        and #$0F
        tax
        lda vdc_to_vic_color_bmp,x
        sta $D020
        sta $D021

        ; Set up VIC-IV for H640 bitmap mode if not already
        lda vdc_bitmap_active
        bne _vdc_bmp_update

        ; First time entering bitmap mode - configure VIC-IV
        lda #1
        sta vdc_bitmap_active

        ; Enable bitmap mode: $D011 bit 5
        lda #$3B
        sta $D011

        ; Point CHARPTR at bitmap buffer in bank 5: $054000
        lda #$00
        sta $D068
        lda #$40
        sta $D069
        lda #$05
        sta $D06A

        ; Set SCRNPTR to color info area at bank 5: $058000
        lda #$00
        sta $D060
        lda #$80
        sta $D061
        lda #$05
        sta $D062
        lda #$00
        sta $D063

        ; DMA fill color RAM with 0 (only needed once)
        #dma_fill_col $0000, 2000, $00

_vdc_bmp_update:
        ; Check R25 bit 6: attributes enabled?
        lda vdc_regs+25
        and #%01000000
        bne _vdc_bmp_attr_mode

        ; --- Attributes disabled: uniform color from R26 ---
        ; Output: bg<<4 | fg
        lda vdc_regs+26
        lsr
        lsr
        lsr
        lsr                     ; background (OFF bits)
        tax
        lda vdc_to_vic_color_bmp,x
        asl
        asl
        asl
        asl
        sta _vdc_bmp_tmp        ; bg in high nibble
        lda vdc_regs+26
        and #$0F                ; foreground (ON bits)
        tax
        lda vdc_to_vic_color_bmp,x
        ora _vdc_bmp_tmp        ; combine bg<<4 | fg
        sta _vdc_bmp_fill_val

        ; DMA fill screen RAM at $058000 with uniform color byte
        lda #$00
        sta $D707
        .byte $80, $00          ; src MB = $00
        .byte $81, $00          ; dst MB = $00
        .byte $00               ; end options
        .byte $03               ; fill command
        .word 2000              ; count
_vdc_bmp_fill_val:
        .word $0000             ; fill value (patched above)
        .byte $00               ; src bank (ignored for fill)
        .word $8000             ; dst addr = $8000
        .byte $05               ; dst bank = $05 -> $058000
        .byte $00               ; command high byte
        .word $0000             ; modulo
        jmp _vdc_bmp_do_convert

_vdc_bmp_attr_mode:
        ; --- Attributes enabled: translate per-cell from VDC attr RAM ---
        ; VDC bitmap attr: low nibble = fg, high nibble = bg
        ; VIC-II bitmap screen RAM: high nibble = fg, low nibble = bg

        ; Source: VDC RAM at $050000 + R20:R21
        lda vdc_regs+21
        sta C128_MEM_PTR+0
        lda vdc_regs+20
        and #$3F
        clc
        adc #>VDC_RAM_BASE
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3

        ; Dest: screen RAM at $058000 (bank 5)
        lda #$00
        sta vdc_color_ptr+0
        lda #$80
        sta vdc_color_ptr+1
        lda #$05
        sta vdc_color_ptr+2
        lda #$00
        sta vdc_color_ptr+3

        ; Process 7 full pages (1792 bytes) then 208 tail
        lda #7
        sta _vdc_bmp_page_count

_vdc_bmp_attr_page:
        ldz #0
_vdc_bmp_attr_inner:
        lda [C128_MEM_PTR],z    ; read once
        pha                     ; save for low nibble
        ; High nibble = bg color (RGBI) -> VIC high nibble
        lsr
        lsr
        lsr
        lsr
        tax
        lda vdc_to_vic_color_bmp,x
        asl
        asl
        asl
        asl                         ; bg to high nibble
        sta _vdc_bmp_tmp
        ; Low nibble = fg color (RGBI) -> VIC low nibble
        pla                     ; restore original byte
        and #$0F
        tax
        lda vdc_to_vic_color_bmp,x  ; fg stays in low nibble
        ora _vdc_bmp_tmp            ; combine bg<<4 | fg
        sta [vdc_color_ptr],z
        inz
        bne _vdc_bmp_attr_inner

        inc C128_MEM_PTR+1
        inc vdc_color_ptr+1
        dec _vdc_bmp_page_count
        bne _vdc_bmp_attr_page

        ; Remaining 208 bytes
        ldz #0
_vdc_bmp_attr_tail:
        lda [C128_MEM_PTR],z
        pha
        lsr
        lsr
        lsr
        lsr
        tax
        lda vdc_to_vic_color_bmp,x
        asl
        asl
        asl
        asl
        sta _vdc_bmp_tmp
        pla
        and #$0F
        tax
        lda vdc_to_vic_color_bmp,x
        ora _vdc_bmp_tmp
        sta [vdc_color_ptr],z
        inz
        cpz #208
        bne _vdc_bmp_attr_tail

_vdc_bmp_do_convert:
        ; Convert VDC sequential bitmap to VIC-IV cell-interleaved bitmap
        ; VDC: 80 bytes per row, 200 rows, sequential
        ; VIC-IV: 8 bytes per cell (one per pixel row), 80 cells per char row
        ;
        ; For pixel row R (0-199):
        ;   char_row = R / 8
        ;   pix_in_cell = R mod 8
        ;   src = VDC_RAM + R12:R13 + R * 80
        ;   dst_base = $054000 + char_row * 640 + pix_in_cell
        ;   For column C (0-79):
        ;     dst[dst_base + C*8] = src[C]

        ; Source base = VDC RAM + R12:R13
        lda vdc_regs+13
        clc
        adc #<VDC_RAM_BASE
        sta _vbmp_src_lo
        lda vdc_regs+12
        and #$3F
        adc #>VDC_RAM_BASE
        sta _vbmp_src_hi

        ; Dest base = $4000 (within bank 5)
        lda #$00
        sta _vbmp_dst_lo
        lda #$40
        sta _vbmp_dst_hi

        lda #0
        sta _vbmp_pix_in_cell   ; R mod 8

        ; Process 200 pixel rows
        ldx #200
_vbmp_row_loop:
        stx _vbmp_rows_left

        ; Set up source pointer
        lda _vbmp_src_lo
        sta C128_MEM_PTR+0
        lda _vbmp_src_hi
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3

        ; Set up dest pointer = dst_base + pix_in_cell
        lda _vbmp_dst_lo
        clc
        adc _vbmp_pix_in_cell
        sta vdc_color_ptr+0
        lda _vbmp_dst_hi
        adc #0
        sta vdc_color_ptr+1
        lda #$05
        sta vdc_color_ptr+2
        lda #$00
        sta vdc_color_ptr+3

        ; Inner loop: 80 columns
        ; Source: sequential via X index, read with [C128_MEM_PTR],z
        ; Dest: stride 8 via vdc_color_ptr, write with [ptr],z where z=0
        ldx #0
        ldz #0                  ; Z stays 0 for all dest writes
_vbmp_col_loop:
        ; Read source byte: set Z=X temporarily for indexed read
        txa
        taz
        lda [C128_MEM_PTR],z    ; read src[X]
        ldz #0                  ; restore Z=0 for dest write
        sta [vdc_color_ptr],z   ; write to dest

        ; Source advances by 1
        inx

        ; Dest advances by 8
        lda vdc_color_ptr+0
        clc
        adc #8
        sta vdc_color_ptr+0
        bcc +
        inc vdc_color_ptr+1
+
        cpx #80
        bne _vbmp_col_loop

        ; Advance source by 80 for next pixel row
        lda _vbmp_src_lo
        clc
        adc #80
        sta _vbmp_src_lo
        lda _vbmp_src_hi
        adc #0
        sta _vbmp_src_hi

        ; Update pix_in_cell and dst_base
        inc _vbmp_pix_in_cell
        lda _vbmp_pix_in_cell
        cmp #8
        bcc _vbmp_same_char_row

        ; New character row - reset pix_in_cell, advance dst_base by 640
        lda #0
        sta _vbmp_pix_in_cell
        lda _vbmp_dst_lo
        clc
        adc #<640
        sta _vbmp_dst_lo
        lda _vbmp_dst_hi
        adc #>640
        sta _vbmp_dst_hi

_vbmp_same_char_row:
        ldx _vbmp_rows_left
        dex
        bne _vbmp_row_loop

        rts

_vbmp_src_lo:       .byte 0
_vbmp_src_hi:       .byte 0
_vbmp_dst_lo:       .byte 0
_vbmp_dst_hi:       .byte 0
_vbmp_pix_in_cell:  .byte 0
_vbmp_rows_left:    .byte 0
_vdc_bmp_tmp:       .byte 0
_vdc_bmp_page_count: .byte 0


; ============================================================
; VDC_SyncCursor - Sync C128 screen editor cursor from VDC
;
; In 80-col mode, derives ZP $EB (row) and $EC (col) from
; VDC cursor position R14:R15 relative to screen start R12:R13.
; In 40-col mode, does nothing (ROM handles cursor).
; ============================================================
VDC_SyncCursor:
        lda display_showing_80
        beq _vsc_done
        lda vdc_regs+15
        sec
        sbc vdc_regs+13
        sta tmp_lo
        lda vdc_regs+14
        sbc vdc_regs+12
        sta tmp_hi
        ldx #0
_vsc_calcrow:
        lda tmp_lo
        sec
        sbc #80
        tay
        lda tmp_hi
        sbc #0
        bcc _vsc_gotrow
        sta tmp_hi
        sty tmp_lo
        inx
        bra _vsc_calcrow
_vsc_gotrow:
        ; X = row, tmp_lo = col
        stx _vsc_row            ; save row for line pointer calc

        txa
        ldx #$EB
        jsr c128_write_zp_x     ; row -> ZP $EB
        lda tmp_lo
        ldx #$EC
        jsr c128_write_zp_x     ; col -> ZP $EC

        ; --- Compute row * 80 (16-bit result in _vsc_mul_hi:_vsc_mul_lo) ---
        ; row * 80 = row * 64 + row * 16
        ; First compute row * 16 (16-bit)
        lda _vsc_row
        sta _vsc_mul_lo
        lda #0
        sta _vsc_mul_hi
        asl _vsc_mul_lo         ; *2
        rol _vsc_mul_hi
        asl _vsc_mul_lo         ; *4
        rol _vsc_mul_hi
        asl _vsc_mul_lo         ; *8
        rol _vsc_mul_hi
        asl _vsc_mul_lo         ; *16
        rol _vsc_mul_hi
        ; Save row*16
        lda _vsc_mul_lo
        sta _vsc_mul64_lo       ; reuse as temp
        lda _vsc_mul_hi
        sta _vsc_mul64_hi
        ; Continue shifting for row*64
        asl _vsc_mul_lo         ; *32
        rol _vsc_mul_hi
        asl _vsc_mul_lo         ; *64
        rol _vsc_mul_hi
        ; row*80 = row*64 + row*16
        lda _vsc_mul_lo
        clc
        adc _vsc_mul64_lo
        sta _vsc_mul_lo
        lda _vsc_mul_hi
        adc _vsc_mul64_hi
        sta _vsc_mul_hi
        ; $E0:$E1 = R12:R13 + row*80
        lda vdc_regs+13
        clc
        adc _vsc_mul_lo
        ldx #$E0
        jsr c128_write_zp_x     ; $E0 = lo
        lda vdc_regs+12
        adc _vsc_mul_hi
        ldx #$E1
        jsr c128_write_zp_x     ; $E1 = hi
        ; $E2:$E3 = R20:R21 + row*80 (attr line base)
        lda vdc_regs+21
        clc
        adc _vsc_mul_lo
        ldx #$E2
        jsr c128_write_zp_x     ; $E2 = lo
        lda vdc_regs+20
        adc _vsc_mul_hi
        ldx #$E3
        jsr c128_write_zp_x     ; $E3 = hi

_vsc_done:
        rts

_vsc_row:       .byte 0
_vsc_mul_lo:    .byte 0
_vsc_mul_hi:    .byte 0
_vsc_mul64_lo:  .byte 0
_vsc_mul64_hi:  .byte 0


; ============================================================
; VDC_UpdateCursor - Draw/erase cursor on MEGA65 screen
;
; Checks VDC R10 bits 6:5 for cursor mode:
;   %00 = non-blinking (always visible)
;   %01 = cursor disabled
;   %10 = slow blink (toggle every 16 frames, ~1/16 SRF)
;   %11 = fast blink (toggle every 8 frames, ~1/32 SRF)
;
; Manages its own blink counter (_vdc_cur_counter).
; Called once per frame from the main loop.
;
; MEGA65 screen at $054000, VDC screen RAM at $50000 + R12:R13
; ============================================================
VDC_UpdateCursor:
        ; Extract cursor mode from R10 bits 6:5
        lda vdc_regs+10
        and #%01100000
        sta _vdc_cur_mode

        ; Mode %01 ($20) = cursor off
        cmp #%00100000
        bne _vdc_cur_not_off

        ; Cursor disabled - erase if currently drawn, then exit
        lda _vdc_cur_drawn
        beq _vdc_cur_done
        jmp _vdc_cur_do_erase

_vdc_cur_not_off:
        ; Determine if cursor should be visible this frame
        lda _vdc_cur_mode
        beq _vdc_cur_visible    ; %00 = non-blinking, always visible

        ; Blinking mode - increment counter and check threshold
        inc _vdc_cur_counter
        lda _vdc_cur_mode
        cmp #%01000000          ; %10 = slow blink
        beq _vdc_cur_slow

        ; %11 = slow blink: toggle every 16 frames (1/32 SRF)
        lda _vdc_cur_counter
        cmp #16
        bcc _vdc_cur_check_phase
        bra _vdc_cur_toggle

_vdc_cur_slow:
        ; %10 = fast blink: toggle every 8 frames (1/16 SRF)
        lda _vdc_cur_counter
        cmp #8
        bcc _vdc_cur_check_phase

_vdc_cur_toggle:
        ; Reset counter and toggle phase
        lda #0
        sta _vdc_cur_counter
        lda _vdc_cur_phase
        eor #1
        sta _vdc_cur_phase

_vdc_cur_check_phase:
        lda _vdc_cur_phase
        bne _vdc_cur_visible

        ; Phase OFF - erase cursor if drawn
        lda _vdc_cur_drawn
        beq _vdc_cur_done
        jmp _vdc_cur_do_erase

_vdc_cur_visible:
        ; --- Step 1: Erase old cursor at previous position ---
        lda _vdc_cur_drawn
        beq _vdc_cur_no_erase

_vdc_cur_do_erase:
        ; Read original char from VDC screen RAM at old position
        ; VDC RAM addr = R12:R13 + prev_offset, at $50000+
        lda vdc_regs+13
        clc
        adc _vdc_cur_prev
        sta C128_MEM_PTR
        lda vdc_regs+12
        and #$3F
        adc _vdc_cur_prev+1
        clc
        adc #>VDC_RAM_BASE
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK       ; bank 5
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z    ; original char
        pha

        ; Write to MEGA65 screen at old position
        lda _vdc_cur_prev
        clc
        adc #<C128_SCREEN_OFF
        sta C128_MEM_PTR
        lda _vdc_cur_prev+1
        adc #>C128_SCREEN_OFF
        sta C128_MEM_PTR+1
        lda #C128_SCREEN_BANK
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        pla
        ldz #0
        sta [C128_MEM_PTR],z

        lda #0
        sta _vdc_cur_drawn      ; mark as erased

        ; If we got here from cursor-off or phase-off, exit now
        lda _vdc_cur_mode
        cmp #%00100000          ; cursor off?
        beq _vdc_cur_done
        lda _vdc_cur_phase
        beq _vdc_cur_done       ; phase off for blinking mode

_vdc_cur_no_erase:
        ; --- Step 2: Compute new cursor offset ---
        lda vdc_regs+15         ; R15 cursor pos low
        sec
        sbc vdc_regs+13         ; R13 screen start low
        sta _vdc_cur_offset
        lda vdc_regs+14         ; R14 cursor pos high
        sbc vdc_regs+12         ; R12 screen start high
        sta _vdc_cur_offset+1

        ; Bounds check: offset must be 0-1999 ($0000-$07CF)
        cmp #$07
        bcc _vdc_cur_in_bounds  ; high < $07: in bounds
        bne _vdc_cur_done       ; high > $07: out of bounds
        lda _vdc_cur_offset
        cmp #$D0
        bcs _vdc_cur_done       ; low >= $D0: offset >= 2000, out of bounds

_vdc_cur_in_bounds:
        ; --- Step 3: Draw cursor at new position ---
        ; Write reverse space ($A0) to MEGA65 screen
        lda _vdc_cur_offset
        clc
        adc #<C128_SCREEN_OFF
        sta C128_MEM_PTR
        lda _vdc_cur_offset+1
        adc #>C128_SCREEN_OFF
        sta C128_MEM_PTR+1
        lda #C128_SCREEN_BANK
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        lda #$A0               ; reverse space
        ldz #0
        sta [C128_MEM_PTR],z

        lda #1
        sta _vdc_cur_drawn      ; mark as drawn

_vdc_cur_save_prev:
        ; Save current offset as previous for next call
        lda _vdc_cur_offset
        sta _vdc_cur_prev
        lda _vdc_cur_offset+1
        sta _vdc_cur_prev+1

_vdc_cur_done:
        rts

_vdc_cur_offset:  .word 0
_vdc_cur_prev:    .word 0       ; Previous cursor offset (for erase)
_vdc_cur_drawn:   .byte 0       ; 1 = cursor block currently on screen
_vdc_cur_mode:    .byte 0       ; R10 bits 6:5 cached
_vdc_cur_counter: .byte 0       ; Frame counter for blink timing
_vdc_cur_phase:   .byte 1       ; 1 = cursor visible, 0 = hidden


; ============================================================
; VDC RGBI -> VIC-II color lookup table
; ============================================================
; VDC RGBI encoding:  R G B I  (4 bits)
; Index: 0=black 1=dgray 2=dblue 3=lblue 4=dgreen 5=lgreen
;        6=dcyan 7=lcyan 8=dred 9=lred 10=dpurple 11=purple
;        12=dyellow 13=yellow 14=lgray 15=white
;
; Mapping to closest VIC-II colors:
; VDC $0 (0000 black)    -> VIC 0  (black)
; VDC $1 (0001 dk gray)  -> VIC 11 (dark grey)
; VDC $2 (0010 dk blue)  -> VIC 6  (blue)
; VDC $3 (0011 lt blue)  -> VIC 14 (light blue)
; VDC $4 (0100 dk green) -> VIC 5  (green)
; VDC $5 (0101 lt green) -> VIC 13 (light green)
; VDC $6 (0110 dk cyan)  -> VIC 11 (dark grey - closest)
; VDC $7 (0111 lt cyan)  -> VIC 3  (cyan)
; VDC $8 (1000 dk red)   -> VIC 2  (red)
; VDC $9 (1001 lt red)   -> VIC 10 (light red)
; VDC $A (1010 dk purple)-> VIC 4  (purple)
; VDC $B (1011 lt purple)-> VIC 4  (purple)
; VDC $C (1100 dk yellow)-> VIC 9  (brown)
; VDC $D (1101 lt yellow)-> VIC 7  (yellow)
; VDC to VIC-II color mapping for TEXT mode (original working table)
vdc_to_vic_color:
        .byte 0, 11, 6, 14, 5, 13, 11, 3
        .byte 2, 10, 4, 4, 9, 7, 15, 1

; VDC RGBI to VIC-II color mapping for BITMAP mode
; Same RGBI encoding as text mode (confirmed against Z64K):
; $0=black $1=dk gray $2=dk blue $3=lt blue $4=dk green $5=lt green
; $6=dk cyan $7=lt cyan $8=dk red $9=lt red $A=dk purple $B=lt purple
; $C=brown $D=yellow $E=lt gray $F=white
vdc_to_vic_color_bmp:
        .byte 0, 11, 6, 14, 5, 13, 11, 3
        .byte 2, 10, 4, 4, 9, 7, 15, 1