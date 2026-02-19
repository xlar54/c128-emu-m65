; ============================================================
; c128_host.asm - Host-side routines for C128 emulator
; 
; Screen output via injected CHROUT calls.
;
; How it works:
;   1. Caller sets c128h_print_char with character to print
;   2. Caller sets c128h_print_pending = 1
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
C128_INJECT_ADDR  = $0334

; Trap address - we'll jump here when done and detect PC=this
C128_TRAP_ADDR    = $033C         ; Just after our injected code

; Size of injected code:
;   LDA #$xx    ; 2 bytes  ($0334-$0335)
;   JSR $FFD2   ; 3 bytes  ($0336-$0338)
;   JMP $033C   ; 3 bytes  ($0339-$033B)
C128_INJECT_SIZE  = 8             ; Save/restore 8 bytes to be safe

; C128 KERNAL CHROUT
C128_CHROUT       = $FFD2

; Zero page pointer for string printing
C128H_STR_PTR     = $FB           ; 2 bytes


; ============================================================
; Public variables - set these to request a print
; ============================================================

; Set to 1 to request printing, cleared when done
c128h_print_pending:  .byte 0

; Character to print (PETSCII)
c128h_print_char:     .byte 0

; Flag indicating we're in the middle of printing (for emulation loop)
c128h_print_active:   .byte 0


; ============================================================
; Saved state storage (global labels so they're accessible)
; ============================================================
c128h_saved_ram:
        .fill C128_INJECT_SIZE, 0

c128h_save_a:     .byte 0
c128h_save_x:     .byte 0
c128h_save_y:     .byte 0
c128h_save_p:     .byte 0
c128h_save_sp:    .byte 0
c128h_save_pc_lo: .byte 0
c128h_save_pc_hi: .byte 0


; ============================================================
; C128Host_StartPrint - Called by emulation loop when pending=1
;
; Saves state, injects code, sets PC to begin printing.
; After writing to LOW_RAM_BUFFER, DMA-copies the injected
; bytes to physical bank 4 so the CPU can read them.
; ============================================================
C128Host_StartPrint:
        ; Save original bytes from injection point (LOW_RAM_BUFFER copy)
        ldx #0
c128h_sp_save_loop:
        lda LOW_RAM_BUFFER + C128_INJECT_ADDR,x
        sta c128h_saved_ram,x
        inx
        cpx #C128_INJECT_SIZE
        bne c128h_sp_save_loop
        
        ; Save emulated CPU state
        lda c128_a
        sta c128h_save_a
        lda c128_x
        sta c128h_save_x
        lda c128_y
        sta c128h_save_y
        lda c128_p
        sta c128h_save_p
        lda c128_sp
        sta c128h_save_sp
        lda c128_pc_lo
        sta c128h_save_pc_lo
        lda c128_pc_hi
        sta c128h_save_pc_hi
        
        ; Inject the code: LDA #char / JSR $FFD2 / JMP trap
        ; $0334: A9 xx      LDA #char
        lda #$A9                        ; LDA immediate
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR
        lda c128h_print_char
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR + 1
        ; $0336: 20 D2 FF   JSR $FFD2
        lda #$20                        ; JSR
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR + 2
        lda #<C128_CHROUT
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR + 3
        lda #>C128_CHROUT
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR + 4
        ; $0339: 4C 3C 03   JMP $033C
        lda #$4C                        ; JMP
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR + 5
        lda #<C128_TRAP_ADDR
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR + 6
        lda #>C128_TRAP_ADDR
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR + 7
        
        ; DMA copy injected code from LOW_RAM_BUFFER to physical bank 4
        jsr c128h_sync_inject_to_bank4
        
        ; Invalidate code cache since we modified guest RAM
        lda #0
        sta c128_code_valid
        
        ; Set PC to injected code (no stack manipulation needed!)
        lda #<C128_INJECT_ADDR
        sta c128_pc_lo
        lda #>C128_INJECT_ADDR
        sta c128_pc_hi
        
        ; Mark as active, clear pending
        lda #1
        sta c128h_print_active
        lda #0
        sta c128h_print_pending
        
        rts


; ============================================================
; C128Host_CheckPrintDone - Called by emulation loop each step
;
; Returns: C=1 if print is done, C=0 if still running
; When done, restores everything
; ============================================================
C128Host_CheckPrintDone:
        ; Check if we've hit the trap address
        lda c128_pc_hi
        cmp #>C128_TRAP_ADDR
        bne c128h_cpd_not_done
        lda c128_pc_lo
        cmp #<C128_TRAP_ADDR
        bne c128h_cpd_not_done
        
        ; Done! Restore everything
        jsr C128Host_EndPrint
        sec                             ; C=1 = done
        rts
        
c128h_cpd_not_done:
        clc                             ; C=0 = still running
        rts


; ============================================================
; C128Host_EndPrint - Restore RAM and CPU state after print
;
; Restores saved bytes to LOW_RAM_BUFFER, then DMA-copies
; them back to physical bank 4.
; ============================================================
C128Host_EndPrint:
        ; Restore original RAM to LOW_RAM_BUFFER
        ldx #0
c128h_ep_restore_loop:
        lda c128h_saved_ram,x
        sta LOW_RAM_BUFFER + C128_INJECT_ADDR,x
        inx
        cpx #C128_INJECT_SIZE
        bne c128h_ep_restore_loop
        
        ; DMA copy restored bytes from LOW_RAM_BUFFER to physical bank 4
        jsr c128h_sync_inject_to_bank4
        
        ; Invalidate code cache since we modified guest RAM
        lda #0
        sta c128_code_valid
        
        ; Restore CPU state
        lda c128h_save_a
        sta c128_a
        lda c128h_save_x
        sta c128_x
        lda c128h_save_y
        sta c128_y
        lda c128h_save_p
        sta c128_p
        lda c128h_save_sp
        sta c128_sp
        lda c128h_save_pc_lo
        sta c128_pc_lo
        lda c128h_save_pc_hi
        sta c128_pc_hi
        
        ; Clear active flag
        lda #0
        sta c128h_print_active
        
        rts


; ============================================================
; _c128h_sync_inject_to_bank4 - DMA copy inject region from
; LOW_RAM_BUFFER to physical C128 RAM bank 4
;
; Copies C128_INJECT_SIZE bytes from:
;   Bank 0: LOW_RAM_BUFFER + C128_INJECT_ADDR
; To:
;   Bank 4: C128_INJECT_ADDR
; ============================================================
c128h_sync_inject_to_bank4:
        lda #$00
        sta $D707                       ; DMA list in bank 0, mega-list
        ; Enhanced DMA list follows inline
        .byte $80,$00                   ; src skip rate = 0 (normal)
        .byte $81,$00                   ; dst skip rate = 0 (normal)
        .byte $00                       ; end of options
        .byte $00                       ; command: COPY
        .byte C128_INJECT_SIZE            ; count lo
        .byte $00                       ; count hi
        .byte <(LOW_RAM_BUFFER + C128_INJECT_ADDR)  ; src lo
        .byte >(LOW_RAM_BUFFER + C128_INJECT_ADDR)  ; src hi
        .byte $00                       ; src bank 0
        .byte <C128_INJECT_ADDR          ; dst lo
        .byte >C128_INJECT_ADDR          ; dst hi
        .byte BANK_RAM0                 ; dst bank 4
        .byte $00                       ; sub-command
        .word $0000                     ; modulo
        rts


; ============================================================
; C128Host_PrintCharSync - Synchronous print (blocks until done)
;
; Input: A = character to print
;
; This is a convenience wrapper that sets the flag and waits.
; Use this from hook code that needs to print immediately.
; ============================================================
C128Host_PrintCharSync:
        sta c128h_print_char
        lda #1
        sta c128h_print_pending
        
        ; Start the print
        jsr C128Host_StartPrint
        
        ; Run emulator until print completes
c128h_pcs_loop:
        jsr C128CPU_Step
        jsr C128Host_CheckPrintDone
        bcc c128h_pcs_loop                ; C=0 means not done yet
        
        rts

; Alias for compatibility
C128Host_PutChar = C128Host_PrintCharSync


; ============================================================
; C128Host_PrintString - Print null-terminated string
;
; Input:  A/X = pointer to string (lo/hi)
; ============================================================
C128Host_PrintString:
        sta C128H_STR_PTR
        stx C128H_STR_PTR+1
        ldy #0
c128h_pstr_loop:
        lda (C128H_STR_PTR),y
        beq c128h_pstr_done
        phy
        jsr C128Host_PrintCharSync
        ply
        iny
        bne c128h_pstr_loop
        inc C128H_STR_PTR+1
        bne c128h_pstr_loop
c128h_pstr_done:
        rts


; ============================================================
; C128Host_NewLine - Print carriage return
; ============================================================
C128Host_NewLine:
        lda #$0d
        jmp C128Host_PrintCharSync


; ============================================================
; Convenience messages (lowercase for C128 default mode)
; ============================================================
C128Host_Msg_Searching:
        .byte $0d                       ; Newline first
        .text "searching for "
        .byte 0

C128Host_Msg_Loading:
        .byte $0d
        .text "loading"
        .byte 0

C128Host_Msg_Saving:
        .byte $0d
        .text "saving "
        .byte 0

C128Host_Msg_Ready:
        .byte $0d
        .text "ready."
        .byte $0d, 0