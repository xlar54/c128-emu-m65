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
        ; Translate: extract low nibble, lookup VIC color
        lda c128_saved_data
        and #$0F
        tax
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
        ; The VDC copies/fills (count+1) bytes starting at R18:R19
        ; Bit 5 of R24 selects copy (0) vs fill (1)
        lda c128_saved_data
        sta vdc_regs+30

        ; Count = value + 1 (16-bit because value=$FF -> count=256)
        lda c128_saved_data
        clc
        adc #1
        sta _vdc_fill_count
        lda #0
        adc #0
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

_wvdc_wc_skip:
        ; 40-col mode: Advance R18:R19 by (value + 1)
        clc
        lda vdc_regs+19
        adc c128_saved_data
        sta vdc_regs+19
        lda vdc_regs+18
        adc #$00
        sta vdc_regs+18
        inc vdc_regs+19
        bne +
        inc vdc_regs+18
+       rts


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
        ; Force 80-col border/background colors from VDC register 26
        ; VDC has no separate border color - border matches background
        lda vdc_regs+26
        and #$0F
        tax
        lda vdc_to_vic_color,x
        sta $D020
        sta $D021

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

        ; --- Step 2: Translate VDC attribute RAM -> MEGA65 color RAM ---
        ; Individual R31 writes are translated inline during writes.
        ; This batch loop only runs after R30 block fill/copy operations
        ; which set vdc_attr_dirty.
        lda vdc_attr_dirty
        beq _vdc_skip_attr

        lda #0
        sta vdc_attr_dirty      ; Clear dirty flag
        ; Read each attribute byte from VDC RAM, extract low nibble
        ; (VDC RGBI color), convert via lookup table, write to color RAM.

        ; Source pointer: $50000 + VDC R20:R21
        lda vdc_regs+21         ; R21 = attribute start low
        sta C128_MEM_PTR+0
        lda vdc_regs+20         ; R20 = attribute start high
        and #$3F                ; mask to 16KB
        clc
        adc #>VDC_RAM_BASE
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK       ; bank 5
        sta C128_MEM_PTR+2
        lda #VDC_RAM_MB         ; $00
        sta C128_MEM_PTR+3

        ; Dest pointer: color RAM at $0FF80000
        lda #$00
        sta vdc_color_ptr+0
        lda #$00
        sta vdc_color_ptr+1
        lda #$F8
        sta vdc_color_ptr+2
        lda #$0F
        sta vdc_color_ptr+3     ; -> $0FF80000

        ; Process in pages of 256 bytes using Z as index
        ; First: 7 full pages (7 x 256 = 1792)
        lda #7
        sta _vdc_page_count

_vdc_attr_page:
        ldz #0
_vdc_attr_inner:
        lda [C128_MEM_PTR],z    ; Read VDC attribute byte
        and #$0F                ; Extract foreground color
        tax
        lda vdc_to_vic_color,x  ; Convert to VIC-II color
        sta [vdc_color_ptr],z   ; Write to color RAM
        inz
        bne _vdc_attr_inner     ; Loop 256 times (Z wraps)

        ; Advance source and dest pointers by 256 (increment high byte)
        inc C128_MEM_PTR+1
        inc vdc_color_ptr+1

        dec _vdc_page_count
        bne _vdc_attr_page

        ; Remaining: 2000 - 1792 = 208 bytes
        ldz #0
_vdc_attr_tail:
        lda [C128_MEM_PTR],z
        and #$0F
        tax
        lda vdc_to_vic_color,x
        sta [vdc_color_ptr],z
        inz
        cpz #208
        bne _vdc_attr_tail

_vdc_skip_attr:
        rts

_vdc_page_count: .byte 0


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
        txa
        ldx #$EB
        jsr c128_write_zp_x     ; row -> ZP $EB
        lda tmp_lo
        ldx #$EC
        jsr c128_write_zp_x     ; col -> ZP $EC
_vsc_done:
        rts


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
; VDC $E (1110 lt gray)  -> VIC 15 (light grey)
; VDC $F (1111 white)    -> VIC 1  (white)
vdc_to_vic_color:
        .byte 0, 11, 6, 14, 5, 13, 11, 3
        .byte 2, 10, 4, 4, 9, 7, 15, 1