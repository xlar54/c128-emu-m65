; ============================================================
; c128_host.asm - Host-side routines for C128 emulator
; 
; Screen output via injected CHROUT calls.
;
; How it works:
;   1. Caller sets p4h_print_char with character to print
;   2. Caller sets p4h_print_pending = 1
;   3. Main emulation loop checks the flag
;   4. If set: save RAM at inject point, save CPU state,
;      inject LDA #char / JSR $FFD2 / JMP trap, set PC
;   5. Run until PC hits trap address
;   6. Restore RAM and CPU state, clear flag
;
; IMPORTANT: The C128 CPU reads from physical bank 4 (BANK_RAM0),
; but LOW_RAM_BUFFER is a mirror in host bank 0. After writing
; injected code to LOW_RAM_BUFFER, we DMA-copy it to bank 4 so
; the emulated CPU actually sees it. Same for restore.
;
; Host: MEGA65
; Assembler: 64tass (45GS02)
; ============================================================

        .cpu "45gs02"

; ============================================================
; Configuration
; ============================================================

; Address in guest RAM where we inject our print code
; $0334 is in the C128 KERNAL indirect vector area, but we
; save/restore the bytes so it's safe as long as we sync to
; physical bank 4.
P4_INJECT_ADDR  = $0334

; Trap address - we'll jump here when done and detect PC=this
P4_TRAP_ADDR    = $033C         ; Just after our injected code

; Size of injected code:
;   LDA #$xx    ; 2 bytes  ($0334-$0335)
;   JSR $FFD2   ; 3 bytes  ($0336-$0338)
;   JMP $033C   ; 3 bytes  ($0339-$033B)
P4_INJECT_SIZE  = 8             ; Save/restore 8 bytes to be safe

; Plus/4 KERNAL CHROUT
P4_CHROUT       = $FFD2

; Zero page pointer for string printing
P4H_STR_PTR     = $FB           ; 2 bytes


; ============================================================
; Public variables - set these to request a print
; ============================================================

; Set to 1 to request printing, cleared when done
p4h_print_pending:  .byte 0

; Character to print (PETSCII)
p4h_print_char:     .byte 0

; Flag indicating we're in the middle of printing (for emulation loop)
p4h_print_active:   .byte 0


; ============================================================
; Saved state storage (global labels so they're accessible)
; ============================================================
p4h_saved_ram:
        .fill P4_INJECT_SIZE, 0

p4h_save_a:     .byte 0
p4h_save_x:     .byte 0
p4h_save_y:     .byte 0
p4h_save_p:     .byte 0
p4h_save_sp:    .byte 0
p4h_save_pc_lo: .byte 0
p4h_save_pc_hi: .byte 0


; ============================================================
; P4Host_StartPrint - Called by emulation loop when pending=1
;
; Saves state, injects code, sets PC to begin printing.
; After writing to LOW_RAM_BUFFER, DMA-copies the injected
; bytes to physical bank 4 so the CPU can read them.
; ============================================================
P4Host_StartPrint:
        ; Save original bytes from injection point (LOW_RAM_BUFFER copy)
        ldx #0
p4h_sp_save_loop:
        lda LOW_RAM_BUFFER + P4_INJECT_ADDR,x
        sta p4h_saved_ram,x
        inx
        cpx #P4_INJECT_SIZE
        bne p4h_sp_save_loop
        
        ; Save emulated CPU state
        lda p4_a
        sta p4h_save_a
        lda p4_x
        sta p4h_save_x
        lda p4_y
        sta p4h_save_y
        lda p4_p
        sta p4h_save_p
        lda p4_sp
        sta p4h_save_sp
        lda p4_pc_lo
        sta p4h_save_pc_lo
        lda p4_pc_hi
        sta p4h_save_pc_hi
        
        ; Inject the code: LDA #char / JSR $FFD2 / JMP trap
        ; $0334: A9 xx      LDA #char
        lda #$A9                        ; LDA immediate
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR
        lda p4h_print_char
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR + 1
        ; $0336: 20 D2 FF   JSR $FFD2
        lda #$20                        ; JSR
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR + 2
        lda #<P4_CHROUT
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR + 3
        lda #>P4_CHROUT
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR + 4
        ; $0339: 4C 3C 03   JMP $033C
        lda #$4C                        ; JMP
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR + 5
        lda #<P4_TRAP_ADDR
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR + 6
        lda #>P4_TRAP_ADDR
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR + 7
        
        ; DMA copy injected code from LOW_RAM_BUFFER to physical bank 4
        jsr p4h_sync_inject_to_bank4
        
        ; Invalidate code cache since we modified guest RAM
        lda #0
        sta p4_code_valid
        
        ; Set PC to injected code (no stack manipulation needed!)
        lda #<P4_INJECT_ADDR
        sta p4_pc_lo
        lda #>P4_INJECT_ADDR
        sta p4_pc_hi
        
        ; Mark as active, clear pending
        lda #1
        sta p4h_print_active
        lda #0
        sta p4h_print_pending
        
        rts


; ============================================================
; P4Host_CheckPrintDone - Called by emulation loop each step
;
; Returns: C=1 if print is done, C=0 if still running
; When done, restores everything
; ============================================================
P4Host_CheckPrintDone:
        ; Check if we've hit the trap address
        lda p4_pc_hi
        cmp #>P4_TRAP_ADDR
        bne p4h_cpd_not_done
        lda p4_pc_lo
        cmp #<P4_TRAP_ADDR
        bne p4h_cpd_not_done
        
        ; Done! Restore everything
        jsr P4Host_EndPrint
        sec                             ; C=1 = done
        rts
        
p4h_cpd_not_done:
        clc                             ; C=0 = still running
        rts


; ============================================================
; P4Host_EndPrint - Restore RAM and CPU state after print
;
; Restores saved bytes to LOW_RAM_BUFFER, then DMA-copies
; them back to physical bank 4.
; ============================================================
P4Host_EndPrint:
        ; Restore original RAM to LOW_RAM_BUFFER
        ldx #0
p4h_ep_restore_loop:
        lda p4h_saved_ram,x
        sta LOW_RAM_BUFFER + P4_INJECT_ADDR,x
        inx
        cpx #P4_INJECT_SIZE
        bne p4h_ep_restore_loop
        
        ; DMA copy restored bytes from LOW_RAM_BUFFER to physical bank 4
        jsr p4h_sync_inject_to_bank4
        
        ; Invalidate code cache since we modified guest RAM
        lda #0
        sta p4_code_valid
        
        ; Restore CPU state
        lda p4h_save_a
        sta p4_a
        lda p4h_save_x
        sta p4_x
        lda p4h_save_y
        sta p4_y
        lda p4h_save_p
        sta p4_p
        lda p4h_save_sp
        sta p4_sp
        lda p4h_save_pc_lo
        sta p4_pc_lo
        lda p4h_save_pc_hi
        sta p4_pc_hi
        
        ; Clear active flag
        lda #0
        sta p4h_print_active
        
        rts


; ============================================================
; _p4h_sync_inject_to_bank4 - DMA copy inject region from
; LOW_RAM_BUFFER to physical C128 RAM bank 4
;
; Copies P4_INJECT_SIZE bytes from:
;   Bank 0: LOW_RAM_BUFFER + P4_INJECT_ADDR
; To:
;   Bank 4: P4_INJECT_ADDR
; ============================================================
p4h_sync_inject_to_bank4:
        lda #$00
        sta $D707                       ; DMA list in bank 0, mega-list
        ; Enhanced DMA list follows inline
        .byte $80,$00                   ; src skip rate = 0 (normal)
        .byte $81,$00                   ; dst skip rate = 0 (normal)
        .byte $00                       ; end of options
        .byte $00                       ; command: COPY
        .byte P4_INJECT_SIZE            ; count lo
        .byte $00                       ; count hi
        .byte <(LOW_RAM_BUFFER + P4_INJECT_ADDR)  ; src lo
        .byte >(LOW_RAM_BUFFER + P4_INJECT_ADDR)  ; src hi
        .byte $00                       ; src bank 0
        .byte <P4_INJECT_ADDR          ; dst lo
        .byte >P4_INJECT_ADDR          ; dst hi
        .byte BANK_RAM0                 ; dst bank 4
        .byte $00                       ; sub-command
        .word $0000                     ; modulo
        rts


; ============================================================
; P4Host_PrintCharSync - Synchronous print (blocks until done)
;
; Input: A = character to print
;
; This is a convenience wrapper that sets the flag and waits.
; Use this from hook code that needs to print immediately.
; ============================================================
P4Host_PrintCharSync:
        sta p4h_print_char
        lda #1
        sta p4h_print_pending
        
        ; Start the print
        jsr P4Host_StartPrint
        
        ; Run emulator until print completes
p4h_pcs_loop:
        jsr P4CPU_Step
        jsr P4Host_CheckPrintDone
        bcc p4h_pcs_loop                ; C=0 means not done yet
        
        rts

; Alias for compatibility
P4Host_PutChar = P4Host_PrintCharSync


; ============================================================
; P4Host_PrintString - Print null-terminated string
;
; Input:  A/X = pointer to string (lo/hi)
; ============================================================
P4Host_PrintString:
        sta P4H_STR_PTR
        stx P4H_STR_PTR+1
        ldy #0
p4h_pstr_loop:
        lda (P4H_STR_PTR),y
        beq p4h_pstr_done
        phy
        jsr P4Host_PrintCharSync
        ply
        iny
        bne p4h_pstr_loop
        inc P4H_STR_PTR+1
        bne p4h_pstr_loop
p4h_pstr_done:
        rts


; ============================================================
; P4Host_NewLine - Print carriage return
; ============================================================
P4Host_NewLine:
        lda #$0d
        jmp P4Host_PrintCharSync


; ============================================================
; Convenience messages (lowercase for Plus/4 default mode)
; ============================================================
P4Host_Msg_Searching:
        .byte $0d                       ; Newline first
        .text "searching for "
        .byte 0

P4Host_Msg_Loading:
        .byte $0d
        .text "loading"
        .byte 0

P4Host_Msg_Saving:
        .byte $0d
        .text "saving "
        .byte 0

P4Host_Msg_Ready:
        .byte $0d
        .text "ready."
        .byte $0d, 0