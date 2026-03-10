; ============================================================
; c128_hooks.asm - BASIC/KERNAL hook points for C128 emulator
; Host: MEGA65
; Assembler: 64tass (45GS02)
;
; This file contains the hook dispatcher and non-DOS hooks:
;   - FastCHROUT (80-column screen output)
;   - PRIMM (print immediate)
;   - GONE (BASIC token dispatch)
;   - CHRGET (fast CHRGET replacement)
;   - IRQ (fast IRQ handler)
;   - Crunch (BASIC tokenizer)
;   - IOINIT skip
;   - scroll_screen_up
;
; DOS hooks (SETNAM, SETLFS, LOAD, SAVE, OPEN, CLOSE, etc.)
; are in c128_hooks_dos.asm.
; ============================================================

        .cpu "45gs02"

; c128_hook_pc_changed is defined in c128_cpu_8502.asm at ZP $E7
; Hooks set this flag when they modify PC, so the threaded
; interpreter knows to invalidate the code cache.


; ============================================================
; Include DOS hooks (stubbed out, ready for reimplementation)
; ============================================================
        .include "c128_hooks_dos.asm"


; ============================================================
; C128Hook_Reset - Reset all hook state variables
; Called during emulator reset
; ============================================================
C128Hook_Reset:
        jsr C128Hook_DOS_Reset
        rts

C128Hook_CheckAndRun:
        lda c128_pc_hi

        ; ----- Check for $FFxx addresses (KERNAL calls) -----
        cmp #$FF
        bne _not_ff

        ; Save registers only when we have a potential match
        pha
        txa
        pha
        tya
        pha
        
        lda c128_pc_lo
        cmp #$BD                        ; SETNAM = $FFBD
        beq _do_setnam
        cmp #$BA                        ; SETLFS = $FFBA
        beq _do_setlfs
        cmp #$D8                        ; SAVE = $FFD8
        beq _do_save
        cmp #$D5                        ; LOAD = $FFD5
        beq _do_load_direct
        ; Sequential file I/O hooks
        cmp #$C0                        ; OPEN = $FFC0
        beq _do_open
        cmp #$C3                        ; CLOSE = $FFC3
        beq _do_close
        cmp #$C6                        ; CHKIN = $FFC6
        beq _do_chkin
        cmp #$C9                        ; CHKOUT = $FFC9
        beq _do_chkout
        cmp #$CC                        ; CLRCHN = $FFCC
        beq _do_clrchn
        cmp #$CF                        ; CHRIN = $FFCF
        beq _do_chrin
        cmp #$D2                        ; CHROUT = $FFD2
        beq _do_chrout
        cmp #$E4                        ; GETIN = $FFE4
        beq _do_getin
        cmp #$68                        ; SETBNK = $FF68
        beq _do_setbnk
        jmp _done                       ; No match in $FF page

_do_setnam:
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnSETNAM
        jmp _done

_do_setlfs:
        jsr C128Hook_OnSETLFS
        jmp _done

_do_setbnk:
        jsr C128Hook_OnSETBNK
        jmp _done

_do_save:
        lda c128_file_op_active
        bne _done_load_skip     ; reentrancy guard
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnSAVE
        jmp _done

_do_open:
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnOPEN
        jmp _done

_do_close:
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnCLOSE
        jmp _done

_do_chkin:
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnCHKIN
        jmp _done

_do_chkout:
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnCHKOUT
        jmp _done

_do_clrchn:
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnCLRCHN
        jmp _done

_do_chrin:
        lda seq_input_slot
        cmp #$FF
        beq _done               ; No file input - let ROM handle
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnBASIN
        jmp _done

_do_chrout:
        lda seq_output_slot
        cmp #$FF
        bne _do_chrout_file
        ; Screen output: try fast path for 80-col
        ; Check C128's actual mode flag, not our cached state
        lda #$D7
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z     ; C128 $D7: $00=40-col, $80=80-col
        bpl _done               ; 40-col: let ROM handle
        ; FastCHROUT disabled - ROM handles natively
        jmp _done
_do_chrout_file:
        jsr C128Hook_OnCHROUT
        jmp _done

_do_getin:
        lda seq_input_slot
        cmp #$FF
        beq _done               ; No file input - let ROM handle
        jsr C128Hook_OnGETIN
        jmp _done


_do_chrget:
        jsr C128Hook_CHRGET
        jmp _done


_do_load_direct:
        ; Guard: if already inside a file operation, skip
        lda c128_file_op_active
        bne _done_load_skip
        ; Handle all LOAD calls (BASIC, Monitor, etc.) via OnLOAD.
        ; OnLOAD returns to the caller via RTS_Guest (pops the
        ; return address pushed by JSR $FFD5 from the guest stack).
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnLOAD
        jmp _done

_not_ff:
        ; ----- Check for IRQ handler at $FA65 (fires 60x/sec) -----
        cmp #$FA
        bne _check_f8
        lda c128_pc_lo
        cmp #$65
        bne _done_fast
        ; In 40-col mode AND displaying 40-col, let ROM handle IRQ
        ; (screen editor does cursor blink for 40-col)
        ; But if C128 is in 80-col mode ($D7=$80), keep fast hook
        ; even if we're peeking at 40-col via TAB
        lda vdc_mode_active
        ora display_showing_80
        bne _do_irq_hook

        ; NEW: Also check C128 $D7 directly
        lda #$D7
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        bmi _do_irq_hook        ; C128 is in 80-col mode, use fast hook

        jmp _done_fast
_do_irq_hook:
        ; Match - save regs and handle
        pha
        txa
        pha
        tya
        pha
        jsr C128Hook_IRQ
        jmp _done

_check_f8:
        ; ----- Check for $F8xx (auto-boot) -----
        cmp #$F8
        bne _check_f9
        lda c128_pc_lo
        cmp #$67                        ; $F867 = IOINIT entry
        bne _done_fast
        ; Match - save regs and handle
        pha
        txa
        pha
        tya
        pha
        ; Skip IOINIT entirely - no serial bus or disk drive
        ; But we must initialize CIA1 Timer A for keyboard scanning IRQ
        lda #$25
        sta cia1_timer_a_latch_lo
        sta cia1_timer_a_lo
        lda #$40
        sta cia1_timer_a_latch_hi
        sta cia1_timer_a_hi
        lda #$81
        sta cia1_icr_mask
        lda #$01
        sta cia1_timer_a_ctrl
        lda #$FF
        sta $DC00
        jsr C128Hook_RTS_Guest
        jmp _done

_check_f9:
        ; ----- Check for $A0xx (DIRECTORY at $A07E) -----
        cmp #$A0
        bne _check_a8
        lda c128_pc_lo
        cmp #$7E
        bne _done_fast
        ; Match - save regs and handle
        pha
        txa
        pha
        tya
        pha
        jsr C128Hook_SyncLowRAM
        jsr C128Hook_OnDIRECTORY
        jmp _done

_check_a8:
        ; Crunch/GONE disabled - no $A8xx hooks active
        cmp #$A8
        beq _done_fast

        ; ----- Check for $E24B (GO64 mode) -----
        cmp #$E2
        bne _done_fast
        lda c128_pc_lo
        cmp #$4B
        bne _done_fast
        ; GO64 detected - save regs and handle
        pha
        txa
        pha
        tya
        pha
        jsr C128Hook_GO64
        jmp _done

; Fast exit - no registers were saved, just return
_done_fast:
        rts

_done_load_skip:
        ; File operation in progress (reentrancy guard) - let ROM handle
_done:
        pla
        tay
        pla
        tax
        pla
        rts

C128Hook_RTS_Guest:
        ldy c128_sp
        iny
        sty c128_sp
        lda #$01
        sta c128_addr_hi
        tya
        sta c128_addr_lo
        jsr C128_Read
        sta tmp_lo

        ldy c128_sp
        iny
        sty c128_sp
        lda #$01
        sta c128_addr_hi
        tya
        sta c128_addr_lo
        jsr C128_Read
        sta tmp_hi

        clc
        lda tmp_lo
        adc #$01
        sta c128_pc_lo
        lda tmp_hi
        adc #$00
        sta c128_pc_hi
        lda #1
        sta c128_hook_pc_changed    ; Signal to threaded interpreter
        rts


; FastCHROUT removed to reduce code size
; (ROM handles 80-col output natively via VDC writes + RenderFrame)

; PRIMM removed to reduce code size
; (ROM handles PRIMM natively, vdc_screen_dirty set by RenderFrame)




; ============================================================
; C128Hook_CHRGET - Fast CHRGET replacement
;
; Hooked via JMP $FF42 patched into CHRGET at $0380.
; Called via JSR $0380 from BASIC interpreter (very frequently).
;
; CHRGET: increment text pointer, read byte from bank 0 RAM,
; skip spaces, set flags for token/digit/colon detection.
;
; On exit: A = byte read, Y = 0
;   Flags: Z=1 if byte is $00 or $3A (colon)
;          C=1 if byte >= $3A (not a digit)
;          C=0 if byte is $30-$39 (digit)
; ============================================================
C128Hook_CHRGET:
        ; DISABLED - redirect to original CHRGET code at $0383
        ; $0380 = JMP $FF42 (our hook)
        ; $0383 = original $02 (was BNE offset)
        ; We need to do what CHRGET does: INC $3D, BNE +2, INC $3E
        ; then fall into CHRGOT at $0386
        ; Simplest: do the INC ourselves and set PC to $0386 (CHRGOT)
        lda #$3D
        sta c128_zp_ptr
        ldz #0
        lda [c128_zp_ptr],z     ; read $3D
        clc
        adc #1
        sta [c128_zp_ptr],z     ; write $3D
        bne _chrget_disabled_no_carry
        ldz #1
        lda [c128_zp_ptr],z     ; read $3E
        clc
        adc #1
        sta [c128_zp_ptr],z     ; write $3E
_chrget_disabled_no_carry:
        ; Set PC to CHRGOT ($0386) and let ROM handle the rest
        lda #$86
        sta c128_pc_lo
        lda #$03
        sta c128_pc_hi
        lda #1
        sta c128_hook_pc_changed
        rts

; ============================================================
; C128Hook_IRQ - Fast IRQ handler
;
; Hooked at $FA65 (KERNAL IRQ entry point).
; The real IRQ does:
;   1. JSR $C024 - Screen editor (keyboard scan, cursor) 
;   2. JSR $F5F8 - Jiffy clock update
;   3. JSR $EED0 - Tape motor interlock
;   4. LDA $DC0D - Acknowledge CIA interrupt
;   5. JSR $4006 - BASIC IRQ (sprites, music, collision)
;   6. JMP $FF33 - RTI
;
; We skip everything except jiffy clock and CIA acknowledge.
; Keyboard/cursor already handled natively by MEGA65.
; ============================================================
C128Hook_IRQ:
        ; Slow down 40-col cursor blink (ZP $CC = blink speed)
        lda #$28
        ldx #$CC
        jsr c128_write_zp_x

        ; --- Update jiffy clock at $A0-$A2 ---
        ; $A2 = low byte, $A1 = mid, $A0 = high
        ; Increment the 3-byte counter
        lda #$A2
        sta c128_zp_ptr
        ldz #0
        lda [c128_zp_ptr],z     ; read $A2 (low)
        clc
        adc #1
        sta [c128_zp_ptr],z     ; write $A2
        bcc _irq_no_carry1

        ; Carry to $A1
        lda #$A1
        sta c128_zp_ptr
        lda [c128_zp_ptr],z     ; read $A1 (mid)
        clc
        adc #1
        sta [c128_zp_ptr],z     ; write $A1
        bcc _irq_no_carry1

        ; Carry to $A0
        lda #$A0
        sta c128_zp_ptr
        lda [c128_zp_ptr],z     ; read $A0 (high)
        clc
        adc #1
        sta [c128_zp_ptr],z     ; write $A0

_irq_no_carry1:
        ; Check for 24-hour rollover: 5184000 = $4F1A00
        ; $A0=$4F, $A1=$1A, $A2=$01 means we've hit 5184001
        ; (ROM checks for $4F1A01 and resets to 0)
        lda #$A0
        sta c128_zp_ptr
        ldz #0
        lda [c128_zp_ptr],z     ; $A0
        cmp #$4F
        bne _irq_clock_done
        ldz #1
        lda [c128_zp_ptr],z     ; $A1
        cmp #$1A
        bne _irq_clock_done
        ldz #2
        lda [c128_zp_ptr],z     ; $A2
        cmp #$01
        bcc _irq_clock_done

        ; Reset clock to 0
        lda #0
        ldz #0
        sta [c128_zp_ptr],z     ; $A0 = 0
        ldz #1
        sta [c128_zp_ptr],z     ; $A1 = 0
        ldz #2
        sta [c128_zp_ptr],z     ; $A2 = 0

_irq_clock_done:
        ; --- Acknowledge CIA1 interrupt ---
        ; The ROM reads $DC0D to clear the interrupt flag
        lda cia1_icr_data
        lda #0
        sta cia1_icr_data       ; Clear pending interrupt flags

        ; --- Call BASIC IRQ extension at $4006 ---
        ; This handles PLAY (music), MOVSPR (sprite motion), COLLISION.
        ; Push return address ($FF33-1 = $FF32) onto guest stack so
        ; RTS from $4006 goes to $FF33 (KERNAL IRQ exit/RTI).
        lda #$FF
        sta c128_data
        jsr push_data           ; push high byte of $FF33
        lda #$32
        sta c128_data
        jsr push_data           ; push low byte ($33-1=$32 for RTS convention)

        ; Set PC to $4006 - BASIC IRQ handler
        lda #$06
        sta c128_pc_lo
        lda #$40
        sta c128_pc_hi

        lda #1
        sta c128_hook_pc_changed

        rts

; ============================================================
; scroll_screen_up - Scroll all screen buffers up one line
; Scrolls VDC screen RAM, VDC attr RAM, MEGA65 screen, and
; MEGA65 color RAM. Clears bottom row (row 24) in all four.
; ============================================================
scroll_screen_up:
        ; --- Scroll VDC screen RAM up one line (1920 bytes) ---
        lda vdc_regs+13
        clc
        adc #80
        sta _scr_scroll_src
        lda vdc_regs+12
        adc #0
        and #$3F
        clc
        adc #>VDC_RAM_BASE
        sta _scr_scroll_src+1

        lda vdc_regs+13
        clc
        adc #<VDC_RAM_BASE
        sta _scr_scroll_dst
        lda vdc_regs+12
        and #$3F
        adc #>VDC_RAM_BASE
        sta _scr_scroll_dst+1

        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00               ; copy
        .word 1920
_scr_scroll_src:
        .word $0000
        .byte $05               ; bank 5 (VDC RAM)
_scr_scroll_dst:
        .word $0000
        .byte $05               ; bank 5 (VDC RAM)
        .byte $00
        .word $0000

        ; --- Scroll VDC attr RAM up one line ---
        lda vdc_regs+21
        clc
        adc #80
        sta _attr_scroll_src
        lda vdc_regs+20
        adc #0
        and #$3F
        clc
        adc #>VDC_RAM_BASE
        sta _attr_scroll_src+1

        lda vdc_regs+21
        clc
        adc #<VDC_RAM_BASE
        sta _attr_scroll_dst
        lda vdc_regs+20
        and #$3F
        adc #>VDC_RAM_BASE
        sta _attr_scroll_dst+1

        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00               ; copy
        .word 1920
_attr_scroll_src:
        .word $0000
        .byte $05               ; bank 5 (VDC RAM)
_attr_scroll_dst:
        .word $0000
        .byte $05               ; bank 5 (VDC RAM)
        .byte $00
        .word $0000

        ; --- Scroll MEGA65 screen up one line ---
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00               ; copy
        .word 1920
        .word $4050             ; src = $054050
        .byte $05
        .word $4000             ; dst = $054000
        .byte $05
        .byte $00
        .word $0000

        ; --- Scroll MEGA65 color RAM up one line ---
        lda #$00
        sta $D707
        .byte $80, $FF, $81, $FF, $00
        .byte $00               ; copy
        .word 1920
        .word $0050             ; src = offset 80
        .byte $08
        .word $0000             ; dst = offset 0
        .byte $08
        .byte $00
        .word $0000

        ; --- Clear bottom row in VDC screen RAM ---
        lda vdc_regs+13
        clc
        adc #<1920
        sta _scr_clr_dst
        lda vdc_regs+12
        adc #>1920
        and #$3F
        clc
        adc #>VDC_RAM_BASE
        sta _scr_clr_dst+1

        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03               ; fill
        .word 80
        .word $0020             ; space
        .byte $00
_scr_clr_dst:
        .word $0000
        .byte $05               ; bank 5 (VDC RAM)
        .byte $00
        .word $0000

        ; --- Clear bottom row in VDC attr RAM ---
        lda vdc_regs+21
        clc
        adc #<1920
        sta _attr_clr_dst
        lda vdc_regs+20
        adc #>1920
        and #$3F
        clc
        adc #>VDC_RAM_BASE
        sta _attr_clr_dst+1

        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03               ; fill
        .word 80
        .word $0007             ; default attr (light cyan)
        .byte $00
_attr_clr_dst:
        .word $0000
        .byte $05               ; bank 5 (VDC RAM)
        .byte $00
        .word $0000

        ; --- Clear bottom row in MEGA65 screen ---
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03               ; fill
        .word 80
        .word $0020             ; space
        .byte $00
        .word $4000+1920        ; $054780
        .byte $05
        .byte $00
        .word $0000

        ; --- Clear bottom row in MEGA65 color RAM ---
        lda #$00
        sta $D707
        .byte $80, $FF, $81, $FF, $00
        .byte $03               ; fill
        .word 80
        .word $0003             ; cyan (VIC color 3)
        .byte $00
        .word 1920              ; offset 1920
        .byte $08
        .byte $00
        .word $0000

        ; Mark dirty
        lda #1
        sta vdc_screen_dirty
        sta vdc_attr_dirty
        rts

; Crunch tokenizer removed to reduce code size
; (BASIC tokenizer runs natively through ROM)


; ============================================================
; C128Hook_GO64 - Handle GO64 command
;
; Restores the C65 ROM at $2A000 (8KB chargen area that was
; overwritten during boot), then calls the MEGA65's native
; GO64 routine at $FF53 to switch to C64 mode.
; ============================================================
C128Hook_GO64:
        ; Restore C65 ROM: DMA copy 8KB from attic $8020000 to $2A000
        ;#dma_copy $80, $02, $0000, $00, $02, $A000, $2000

        ; Enable HOTREG so GO64 can reconfigure VIC-IV properly
        ;lda $D05D
        ;ora #$80
        ;sta $D05D

        ; Call MEGA65 native GO64 (now C65 KERNAL is mapped at $E000+)
        ;jmp $FF53

        ; Disable interrupts during transition
        sei

        ; Restore C65 ROM: DMA copy 8KB from attic $8020000 to $2A000
        #dma_copy $80, $02, $0000, $00, $02, $A000, $2000

        ; Restore C65 KERNAL mapping at $E000-$FFFF only.
        ; We cannot map $2000-$7FFF (MAPLO) because our emulator code lives there.
        ; MAPHI: map $8000-$BFFF + $E000-$FFFF to bank 3, offset $30000
        ; MAPLO: unmapped (leave our code accessible)
        lda #$00
        ldx #$00                ; MAPLO: nothing mapped, no offset
        ldy #$00
        ldz #$B3                ; MAPHI: $8000-$BFFF + $E000-$FFFF, offset $30000
        map

        ; Bank I/O in via C64 mechanism
        lda #$35
        sta $01

        ; Do MEGA65 / VIC-IV I/O knock
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F

        ; End MAP sequence
        eom

        ; Do Hypervisor call to re-write-protect the ROM area
        lda #$71
        sta $D640
        clv

        ; Reset VIC-IV to safe defaults
        lda #$00
        sta $D031               ; H640 off, no VIC-III features
        sta $D054               ; No MEGA65 extended features

        ; Reset SCRNPTR to default ($0400 in bank 0)
        sta $D060
        lda #$04
        sta $D061
        lda #$00
        sta $D062
        sta $D063

        ; Reset CHARPTR to default ($1000 in bank 0 = char ROM)
        sta $D068
        lda #$10
        sta $D069
        lda #$00
        sta $D06A

        ; Disable SPRPTR16, reset SPRPTRADR
        sta $D06E
        lda #$F8
        sta $D06C
        lda #$07
        sta $D06D

        ; Enable HOTREG so GO64 can reconfigure VIC-IV properly
        lda $D05D
        ora #$80
        sta $D05D

        ; Reset VIC-II registers to defaults
        lda #$1B
        sta $D011
        lda #$C8
        sta $D016
        lda #$14
        sta $D018
        lda #$00
        sta $D015               ; sprites off
        lda #$0E
        sta $D020               ; light blue border
        lda #$06
        sta $D021               ; blue background

        ; Call MEGA65 native GO64 (now C65 KERNAL is mapped at $E000+)
        jmp $FF53