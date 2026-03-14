; ============================================================
; c128_hooks_dos.asm - DOS/File I/O hooks for C128 emulator
; Host: MEGA65
; Assembler: 64tass (45GS02)
;
; All DOS-related KERNAL intercepts: SETNAM, SETLFS, LOAD,
; SAVE, OPEN, CLOSE, CHKIN, CHKOUT, CLRCHN, BASIN (file),
; CHROUT (file), GETIN (file), and DIRECTORY.
;
; STATUS: STUBBED OUT - all hooks return immediately, letting
; the C128 ROM handle the call natively.
; Each hook has a TODO marker for future C128-specific
; implementation.
; ============================================================

        .cpu "45gs02"


; Switch to KERNAL ZP (page $00) - call before host KERNAL JSRs
; Clobbers A.

; Switch back to emulator ZP - call after host KERNAL returns
; Preserves A and processor flags (carry etc).


; ------------------------------------------------------------
; Hooked guest PC locations (KERNAL jump table)
; ------------------------------------------------------------
C128_SETNAM               = $FFBD
C128_SETLFS               = $FFBA
C128_LOAD                 = $FFD5
C128_SAVE                 = $FFD8
C128_OPEN                 = $FFC0
C128_CLOSE                = $FFC3
C128_CHKIN                = $FFC6
C128_CHKOUT               = $FFC9
C128_CLRCHN               = $FFCC
C128_BASIN                = $FFCF
; C128_CHROUT defined in c128_host.asm
C128_READST               = $FFB7
C128_SETBNK               = $FF68         ; SETBNK entry point

; ------------------------------------------------------------
; C128 BASIC ROM addresses for hook points
; ------------------------------------------------------------
; Inside BASIC LOAD handler ($912C): the JSR $FFD5 instruction
C128Hook_ROM_LOAD_KERNAL_CALL      = $913C
C128Hook_ROM_AFTER_KERNAL_CALL     = $913F

; Inside BASIC SAVE handler
C128Hook_ROM_SAVE_KERNAL_CALL      = $A851
C128Hook_ROM_AFTER_SAVE_CALL       = $A854

; DIRECTORY keyword handler entry in BASIC ROM
C128Hook_ROM_DIRECTORY             = $A07E

; Sequential file I/O constants
MAX_SEQ_FILES           = 10
SEQ_LFN_BASE            = 10

; ------------------------------------------------------------
; Directory / staging buffer configuration
; ------------------------------------------------------------
C128_BANK_RAM     = $04

; MEGA65 80-col screen location (must match c128_mem.asm)
C128_SCREEN_BANK = $05      ; Bank byte for 80-col screen
C128_SCREEN_OFF  = $4000    ; Offset within bank for 80-col screen           ; C128 RAM bank 0 = MEGA65 bank 4
C128_DIR_BUF      = $9400

; MEGA65 KERNAL SAVE
SAVE            = $FFD8

DEBUG_BASE = $A200

; Zero page pointer for directory parsing
DIR_PTR         = $F5


; ============================================================
; DOS Hook Variables
;
; These are kept so that other modules that reference them
; continue to assemble. They will be populated once the hooks
; are properly implemented.
; ============================================================

; SETNAM capture
c128_setnam_len:    .byte 0
c128_setnam_ptr_lo: .byte 0
c128_setnam_ptr_hi: .byte 0
c128_setnam_valid:  .byte 0

; SETLFS capture
c128_setlfs_dev:    .byte 8       ; Default device 8
c128_setlfs_sa:     .byte 0

; SETBNK capture
c128_setbnk_data:   .byte 0       ; C128 bank for load/save data (0 or 1)
c128_setbnk_fname:  .byte 0       ; C128 bank for filename (0 or 1)

; File load buffer
c128_fl_buf:      .fill 17, 0
c128_fl_len:      .byte 0
c128_fl_end_lo:   .byte 0
c128_fl_end_hi:   .byte 0
c128_load_verify_flag: .byte 0

; SAVE variables
c128_save_start_lo: .byte 0
c128_save_start_hi: .byte 0
c128_save_end_lo:   .byte 0
c128_save_end_hi:   .byte 0

; Monitor LOAD flag
c128_monitor_load:  .byte 0

; PRG header (load address)
c128_prg_header_lo: .byte 0
c128_prg_header_hi: .byte 0

; Directory length/destination
c128_dir_len_lo:  .byte 0
c128_dir_len_hi:  .byte 0
c128_dir_dest_lo: .byte 0
c128_dir_dest_hi: .byte 0

tmp_lo: .byte 0
tmp_hi: .byte 0

; Sequential File I/O tables
seq_slot_lfn:     .fill MAX_SEQ_FILES, $FF
seq_slot_dev:     .fill MAX_SEQ_FILES, 0
seq_slot_sa:      .fill MAX_SEQ_FILES, 0
seq_slot_status:  .fill MAX_SEQ_FILES, 0
seq_slot_open:    .fill MAX_SEQ_FILES, 0

seq_input_slot:   .byte $FF
seq_output_slot:  .byte $FF

seq_filename:     .fill 17, 0
seq_filename_len: .byte 0

dos_err_code:       .byte 0

; ============================================================
; C128Hook_DOS_Reset - Reset all DOS hook state variables
; Called during emulator reset
; ============================================================
C128Hook_DOS_Reset:
        lda #0
        sta c128_setnam_valid
        sta c128_setnam_len
        sta c128_setlfs_sa
        sta c128_monitor_load
        sta c128_fl_len
        sta c128_setbnk_data    ; Default bank 0 for data
        sta c128_setbnk_fname   ; Default bank 0 for filename
        lda #8
        sta c128_setlfs_dev
        ; Reset sequential I/O state
        lda #$FF
        sta seq_input_slot
        sta seq_output_slot
        ldx #0
-       sta seq_slot_lfn,x
        inx
        cpx #MAX_SEQ_FILES
        bcc -
        rts


; ============================================================
; dos_print_filename - Print "SEARCHING FOR " + filename
;
; Prints the search message followed by the filename in
; c128_fl_buf (c128_fl_len bytes). Uses emu_print_string
; and emu_chrout.
; ============================================================
dos_print_filename:
        lda #<C128Host_Msg_Searching
        ldx #>C128Host_Msg_Searching
        jsr emu_print_string

        ; Fall through to print just the filename
dos_print_filename_only:
        ldy #0
_dpf_loop:
        cpy c128_fl_len
        beq _dpf_done
        lda c128_fl_buf,y
        phy
        jsr emu_chrout
        ply
        iny
        bne _dpf_loop
_dpf_done:
        rts


; ============================================================
; C128Hook_OnSETNAM - Capture filename parameters
; Intercept at $FFBD
;
; Guest registers on entry:
;   A = filename length
;   X = filename pointer low byte
;   Y = filename pointer high byte
;
; We capture the parameters AND copy the actual filename bytes
; into c128_fl_buf, since the guest pointer may reference
; banked RAM that becomes inaccessible later.
;
; The ROM SETNAM continues normally after we return (we don't
; redirect PC), so the C128 KERNAL ZP ($B7, $BB, $BC) also
; gets written by the ROM.
; ============================================================
C128Hook_OnSETNAM:
        ; Only capture if length > 0 (LOAD calls SETNAM twice:
        ; first with length=0 to clear, then with actual name)
        lda c128_a
        bne _setnam_capture
        ; Length 0: clear our filename state so stale data isn't used
        sta c128_fl_len
        sta c128_setnam_valid
        rts
_setnam_capture:
        ; Save pointer and length only - defer actual byte copy
        ; to OPEN/LOAD/SAVE handlers when c128_setbnk_fname is correct.
        sta c128_setnam_len

        lda c128_x
        sta c128_setnam_ptr_lo

        lda c128_y
        sta c128_setnam_ptr_hi

        ; Mark that we have valid SETNAM data
        lda #1
        sta c128_setnam_valid

        ; Let the ROM SETNAM continue normally
        rts


; ============================================================
; dos_copy_filename - Copy filename bytes from guest RAM
;
; Called from OPEN/LOAD/SAVE hooks AFTER SETBNK has been called.
; Reads c128_setnam_len bytes from c128_setnam_ptr in the bank
; specified by c128_setbnk_fname, stores into c128_fl_buf.
;
; Bank 0 filenames: read from MEGA65 bank 4 via C128_MEM_PTR
; Bank 1 filenames: read from attic via DMA (attic_read_byte)
;
; D-command filenames (ptr in $1000-$1FFF range) are always
; in bank 0 regardless of c128_setbnk_fname setting.
; ============================================================
dos_copy_filename:
        lda c128_setnam_len
        beq _dcf_done           ; Nothing to copy

        lda c128_setbnk_fname
        beq _dcf_use_bank0
        ; setbnk says bank 1 - but check if address suggests bank 0
        lda c128_setnam_ptr_hi
        cmp #$20                ; below $2000? DOS command area = bank 0
        bcc _dcf_use_bank0
        ; Genuine bank 1 filename (e.g. LOAD from input buffer at $FEFD)
        jmp _dcf_copy_attic

_dcf_use_bank0:
        lda #BANK_RAM0
        sta C128_MEM_PTR+2
        lda #BANK_RAM0_MB
        sta C128_MEM_PTR+3
        lda c128_setnam_ptr_lo
        sta C128_MEM_PTR+0
        lda c128_setnam_ptr_hi
        sta C128_MEM_PTR+1

        ldy #0
_dcf_copy_loop:
        cpy c128_setnam_len
        beq _dcf_done
        tya
        taz
        lda [C128_MEM_PTR],z
        sta c128_fl_buf,y
        iny
        cpy #16
        bcc _dcf_copy_loop
        bra _dcf_done

_dcf_copy_attic:
        ; Read filename from attic RAM bank 1 via DMA
        lda c128_setnam_ptr_lo
        sta c128_addr_lo
        lda c128_setnam_ptr_hi
        ldy #0
_dcf_attic_loop:
        cpy c128_setnam_len
        beq _dcf_done
        phy
        lda c128_setnam_ptr_hi
        jsr attic_read_byte
        ply
        sta c128_fl_buf,y
        inc c128_addr_lo
        bne +
        inc c128_setnam_ptr_hi
+       iny
        cpy #16
        bcc _dcf_attic_loop

_dcf_done:
        lda #0
        sta c128_fl_buf,y       ; Null terminate
        sty c128_fl_len
        rts


; ============================================================
; C128Hook_OnSETLFS - Capture device number and secondary address
; Intercept at $FFBA
;
; Guest registers on entry:
;   A = logical file number
;   X = device number
;   Y = secondary address
;
; We capture the values for our own use AND let the ROM
; continue to write them to KERNAL ZP ($B8, $B9, $BA).
; ============================================================
C128Hook_OnSETLFS:
        lda c128_x
        sta c128_setlfs_dev

        lda c128_y
        sta c128_setlfs_sa

        ; Let the ROM SETLFS continue normally
        rts


; ============================================================
; C128Hook_OnSETBNK - Capture bank numbers for load/save
; Intercept at $FF68
;
; Guest registers on entry:
;   A = bank number for load/save data (stored to $C6)
;   X = bank number for filename (stored to $C7)
;
; The C128 ROM SETBNK at $F73F does: STA $C6 / STX $C7 / RTS
; We capture A and X directly from the guest registers so we
; have reliable values regardless of LOW_RAM_BUFFER sync state.
; The ROM continues normally after we return.
; ============================================================
C128Hook_OnSETBNK:
        lda c128_a
        sta c128_setbnk_data

        lda c128_x
        sta c128_setbnk_fname

        ; Let the ROM SETBNK continue normally
        rts


; ============================================================
; C128Hook_OnLOAD - Handle file loading
;
; Called from two dispatch paths:
;   1. $A800 hook (BASIC LOAD) - dispatcher already set
;      c128_file_op_active=1 and advanced PC to $A803
;   2. $FFD5 hook (Monitor/direct) - _do_load_monitor path
;
; On entry to C128 KERNAL LOAD ($FFD5):
;   A = 0 for load, nonzero for verify
;   X/Y = load address (if relocating load, SA bit 0 = 0)
;
; C128 KERNAL ZP (already set by ROM SETBNK/SETNAM/SETLFS):
;   $B7 = filename length
;   $B8 = logical file number
;   $B9 = secondary address
;   $BA = device number
;   $BB/$BC = filename pointer
;   $C6 = load bank (0 or 1)
;   $C7 = filename bank
;
; Strategy:
;   - Use our captured filename from c128_fl_buf (set by OnSETNAM)
;   - Determine destination: SA bit 0 clear -> guest X/Y,
;     SA bit 0 set -> read 2-byte PRG header from file
;   - Map C128 bank ($C6) to MEGA65 bank (4 or 5)
;   - Use host MEGA65 KERNAL LOAD with SA=0 to load directly
;     into the correct MEGA65 bank at the destination address
;   - If load touched $0000-$0FFF, sync LOW_RAM_BUFFER
;   - Return end address in guest X/Y, status in carry
; ============================================================
C128Hook_OnLOAD:
        ; Copy filename bytes now that SETBNK is correct
        jsr dos_copy_filename

        ; Check for valid filename from our SETNAM capture
        lda c128_fl_len
        bne _load_has_name

        ; No filename - MISSING FILE NAME error
        lda c128_p
        ora #P_C
        sta c128_p
        lda #$08
        sta c128_a
        jsr c128_write_status
        jsr C128Hook_RTS_Guest
        rts

_load_has_name:

        ; Only intercept device 8 (disk)
        lda c128_setlfs_dev
        cmp #$08
        beq _load_is_disk
        ; Not device 8 - let ROM handle
        rts

_load_is_disk:
        ; Save the load/verify flag (A=0 for load, A!=0 for verify)
        lda c128_a
        sta c128_load_verify_flag

        ; Clear the valid SETNAM flag (consumed)
        lda #0
        sta c128_setnam_valid

_load_file:
        ; All callers (BASIC, Monitor, etc.) return via RTS_Guest
        ; which pops the JSR $FFD5 return address from the guest stack.

_load_continue:
        ; Print "SEARCHING FOR " + filename
        jsr dos_print_filename

        ; Determine the MEGA65 bank for the load destination.
        ; Use our captured c128_setbnk_data (set by OnSETBNK hook).
        ; C128 bank 0 -> MEGA65 bank 4, C128 bank 1 -> MEGA65 bank 5.
        lda c128_setbnk_data
        beq _load_dest_bank0
        lda #$05
        bra _load_dest_bank_set
_load_dest_bank0:
        lda #$04
_load_dest_bank_set:
        sta load_mega65_bank

        ; Determine the destination address based on secondary address.
        ; SA bit 0 = 0: relocating load -> use guest X/Y as dest
        ; SA bit 0 = 1: absolute load -> read dest from PRG header
        lda c128_setlfs_sa
        and #$01
        bne _load_absolute

        ; --- Relocating load (SA bit 0 = 0) ---
        ; Destination = guest X (lo) / Y (hi)
        lda c128_x
        sta c128_dir_dest_lo
        lda c128_y
        sta c128_dir_dest_hi
        jmp _load_do_host_load

_load_absolute:
        ; --- Absolute load (SA bit 0 = 1) ---
        ; Need to read the 2-byte PRG header to get load address.
        ; OPEN file, read 2 bytes via BASIN, CLOSE, then LOAD.

        ; Mark file op active (if not already) to prevent video switching
        lda #1
        sta c128_file_op_active


        jsr CLRCHN
        lda #$0f
        jsr CLOSE

        ; Set up host KERNAL for header read
        lda #$00
        ldx #$00
        jsr SETBNK

        ldx #<c128_fl_buf
        ldy #>c128_fl_buf
        lda c128_fl_len
        jsr SETNAM

        ; SETLFS: lfn=15, device 8, sa=0 (sequential read)
        lda #$0f
        ldx #$08
        ldy #$00
        jsr SETLFS

        jsr OPEN
        bcc _do_CHKIN
        
        sta dos_err_code    ; Save return address for error path
        jmp _load_abs_header_err

_do_CHKIN:
        ; CHKIN to file 15
        ldx #$0f
        jsr CHKIN

;_loopy:
;inc $d020
;jmp _loopy
        bcc _do_BASIN

        sta dos_err_code
        jmp _load_abs_header_err2

_do_BASIN:
        ; Read 2-byte PRG header (lo, hi)
        jsr C128_BASIN
        bcc _got_header_lo

        sta dos_err_code
        jmp _load_abs_header_err2

_got_header_lo:
        sta c128_prg_header_lo

        jsr $FFB7 ; Read KERNAL status byte (e.g. EOF)
        sta dos_err_code

        jsr C128_BASIN
        bcc _got_header_hi

        sta dos_err_code
        jmp _load_abs_header_err2

_got_header_hi:
        sta c128_prg_header_hi

        ; Clean up
        jsr CLRCHN
        lda #$0F
        jsr CLOSE


        ; Use header address as destination
        lda c128_prg_header_lo
        sta c128_dir_dest_lo
        lda c128_prg_header_hi
        sta c128_dir_dest_hi

        jmp _load_do_host_load

_load_abs_header_err2:
        jsr CLRCHN
_load_abs_header_err:
        lda #$0F
        jsr CLOSE
        jmp _load_error

        ; --------------------------------------------------------
        ; _load_do_host_load - Actually load the file
        ;
        ; At this point:
        ;   c128_dir_dest = target address in guest RAM
        ;   load_mega65_bank = MEGA65 bank (4 or 5)
        ;   c128_fl_buf/c128_fl_len = filename
        ;
        ; We use host MEGA65 KERNAL LOAD with SA=0 so we control
        ; the destination address. SETBNK points at the correct
        ; MEGA65 bank so data goes directly into guest RAM.
        ; --------------------------------------------------------
_load_do_host_load:
        ; Check destination won't corrupt MMU config at $FF00
        lda c128_dir_dest_hi
        cmp #$FE
        bcc _load_dest_ok
        ; Dest >= $FE00 - could overwrite $FF00, abort
        jmp _load_error
_load_dest_ok:

        lda #1
        sta c128_file_op_active

        ; Check if this is a VERIFY instead of a LOAD
        lda c128_load_verify_flag
        beq _do_host_load_native
        jmp _do_manual_verify

_do_host_load_native:
        ; DEBUG: store filename length and first 4 chars at $A1F0
        lda c128_fl_len
        sta $A1F0
        lda c128_fl_buf+0
        sta $A1F1
        lda c128_fl_buf+1
        sta $A1F2
        lda c128_fl_buf+2
        sta $A1F3
        lda c128_fl_buf+3
        sta $A1F4
        ; DEBUG: store dest address and bank at $A1F5
        lda c128_dir_dest_lo
        sta $A1F5
        lda c128_dir_dest_hi
        sta $A1F6
        lda load_mega65_bank
        sta $A1F7
        ; DEBUG: store SA and captured banks at $A1F8
        lda c128_setlfs_sa
        sta $A1F8
        lda c128_setbnk_data
        sta $A1FC
        lda c128_setbnk_fname
        sta $A1FD


        ; Ensure B register = 0 for KERNAL calls
        lda #$00
        tab

        ; SETBNK: data bank = guest's MEGA65 bank, filename bank = 0
        lda load_mega65_bank   ; 4 or 5
        ldx #$00                ; Filename is in our c128_fl_buf in bank 0
        jsr SETBNK

        ; Set filename from our captured buffer
        ldx #<c128_fl_buf
        ldy #>c128_fl_buf
        lda c128_fl_len
        jsr SETNAM

        ; SETLFS: lfn=1, device 8, sa=0 (we provide address via X/Y)
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS

        ; DEBUG: mark before LOAD
        lda #$AA
        sta $A1F9

        ; LOAD: A=0 (load) or A!=0 (verify), X/Y = destination address
        ; Data goes directly into MEGA65 bank 4 or 5 (guest RAM)
        lda c128_load_verify_flag
        ldx c128_dir_dest_lo
        ldy c128_dir_dest_hi
        jsr LOAD
        bcc _load_host_ok

        sta dos_err_code        ; Capture error code (e.g. 27/28 for verify error)
        
        ; DEBUG: store error info
        sta $A1FA               ; A = error code from LOAD
        lda #$EE                ; marker = error path
        sta $A1FB
        jmp _load_host_failed
_load_host_ok:
        ; DEBUG: mark success
        lda #$01
        sta $A1FB

        ; Success! X/Y hold end address (first byte after loaded data)
        stx c128_fl_end_lo
        sty c128_fl_end_hi

        ; Close and clean up host KERNAL state
        lda #$01
        jsr CLOSE
        jsr CLRCHN

        ; Reset SETBNK
        lda #$00
        ldx #$00
        jsr SETBNK

_load_host_ok_shared:
        ; Print "LOADING" or "VERIFYING"
        lda c128_load_verify_flag
        bne _load_print_verify
        lda #<C128Host_Msg_Loading
        ldx #>C128Host_Msg_Loading
        bra _load_print_msg
_load_print_verify:
        lda #<C128Host_Msg_Verifying
        ldx #>C128Host_Msg_Verifying
_load_print_msg:
        jsr emu_print_string

        ; If load destination was in $0000-$0FFF, sync LOW_RAM_BUFFER
        lda c128_dir_dest_hi
        cmp #$10
        bcs _load_no_lrb_sync

        jsr C128Hook_SyncLowRAM

_load_no_lrb_sync:
        ; Clear KERNAL status in guest
        lda #$00
        jsr c128_write_status

        ; Set end address in guest X/Y registers
        lda c128_fl_end_lo
        sta c128_x
        lda c128_fl_end_hi
        sta c128_y

        ; Clear carry = success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p

        jmp _load_return

_load_host_failed:
        ; Host LOAD returned error - clean up
        lda #$01
        jsr CLOSE
        jsr CLRCHN
        lda #$00
        ldx #$00
        jsr SETBNK
        ; Fall through to error

_load_error:
        ; Set carry = error
        lda c128_p
        ora #P_C
        sta c128_p
        ; Default to FILE NOT FOUND if no specific error
        lda dos_err_code
        bne +
        lda #$04
+       sta c128_a
        jsr c128_write_status

        ; Print error message on 80-col screen
        ;lda #<_load_msg_fnf
        ;ldx #>_load_msg_fnf
        ;jsr C128Host_PrintString

_load_return:
        ; Sync C128 cursor ZP from VDC cursor position (80-col only)
        jsr VDC_SyncCursor

        ; Restore video state if error occurred (80-col only)
        lda display_showing_80
        beq +
        jsr C128Hook_PostFileOpVideoFix
+

        ; Clear file operation flag and consumed filename
        lda #0
        sta c128_file_op_active
        sta c128_fl_len

        ; Return to guest via RTS - pops JSR $FFD5 return address
        jsr C128Hook_RTS_Guest
        rts


_do_manual_verify:
        ; Ensure B register = 0 for KERNAL calls
        lda #$00
        tab

        ; Set up host KERNAL for reading the file
        lda #$00
        ldx #$00
        jsr SETBNK

        ldx #<c128_fl_buf
        ldy #>c128_fl_buf
        lda c128_fl_len
        jsr SETNAM

        ; SETLFS: lfn=1, device 8, sa=0 (sequential read)
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS

        jsr OPEN
        bcc _verify_open_ok
        
        lda #$04                ; FILE NOT FOUND
        sta dos_err_code
        jmp _verify_fail

_verify_open_ok:
        ldx #$01
        jsr CHKIN
        bcc _verify_chkin_ok

        lda #$04
        sta dos_err_code
        jmp _verify_fail_close

_verify_chkin_ok:
        ; Read 2-byte PRG header and discard it
        jsr CHRIN
        jsr CHRIN

        ; Setup pointer to guest RAM
        lda c128_dir_dest_lo
        sta C128_MEM_PTR+0
        lda c128_dir_dest_hi
        sta C128_MEM_PTR+1
        lda load_mega65_bank
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3

        ; Check if the file was empty (EOF hit during header)
        jsr READST
        and #$40
        bne _verify_success

_verify_byte_loop:
        ; Read byte from file
        jsr CHRIN
        sta tmp_lo              ; save byte from file

        ; Check host status for EOF or errors
        jsr READST
        sta tmp_hi

        ; Read byte from guest RAM
        ldz #0
        lda [C128_MEM_PTR],z

        ; Compare
        cmp tmp_lo
        bne _verify_mismatch

        ; Increment pointer
        inc C128_MEM_PTR+0
        bne +
        inc C128_MEM_PTR+1
+
        ; Was this the last byte?
        lda tmp_hi
        and #$40                ; EOF?
        bne _verify_success
        
        bra _verify_byte_loop

_verify_mismatch:
        ; Mismatch! Set error code 28 for VERIFY ERROR
        lda #28
        sta dos_err_code
        bra _verify_fail_close

_verify_success:
        ; Success! Save end address in X/Y for return
        ldx C128_MEM_PTR+0
        ldy C128_MEM_PTR+1
        stx c128_fl_end_lo
        sty c128_fl_end_hi

        jsr CLRCHN
        lda #$01
        jsr CLOSE

        ; Reset SETBNK
        lda #$00
        ldx #$00
        jsr SETBNK

        jmp _load_host_ok_shared

_verify_fail_close:
        jsr CLRCHN
        lda #$01
        jsr CLOSE
_verify_fail:
        lda #$00
        ldx #$00
        jsr SETBNK
        jmp _load_error


_load_let_rom:
        ; Can't handle - let ROM deal with it
        rts

load_mega65_bank: .byte $04    ; MEGA65 bank for load destination

_load_msg_fnf:
        .byte $0d
        .text "?file not found  error"
        .byte $0d, $00


; ============================================================
; STUB: C128Hook_OnSAVE
; Intercept at $FFD8 - handle file saving
;
; Guest registers on entry:
;   A = zero page address containing start address pointer
;   X = end address low byte
;   Y = end address high byte
;
; TODO: Copy filename from guest input buffer, read start
;       address from the ZP pointer in A, DMA guest RAM to
;       staging buffer (with 2-byte PRG header), call host
;       KERNAL SAVE with bank swap, handle errors, return
;       via RTS_Guest.
; ============================================================
C128Hook_OnSAVE:
        ; Copy filename bytes now that SETBNK is correct
        jsr dos_copy_filename

        ; Only intercept device 8
        lda c128_setlfs_dev
        cmp #$08
        beq _save_check
        rts

_save_check:
        ; Check for valid filename
        lda c128_fl_len
        bne _save_have_name

        ; No filename - MISSING FILE NAME error
        lda c128_p
        ora #P_C
        sta c128_p
        lda #$08
        sta c128_a
        jsr c128_write_status
        jsr C128Hook_RTS_Guest
        rts

_save_have_name:
        ; Get end address from X/Y
        lda c128_x
        sta c128_save_end_lo
        lda c128_y
        sta c128_save_end_hi

        ; Get start address from ZP pointer in A
        lda c128_a
        tax
        lda LOW_RAM_BUFFER,x
        sta c128_save_start_lo
        inx
        lda LOW_RAM_BUFFER,x
        sta c128_save_start_hi

        ; Calculate data length
        lda c128_save_end_lo
        sec
        sbc c128_save_start_lo
        sta c128_dir_len_lo
        lda c128_save_end_hi
        sbc c128_save_start_hi
        sta c128_dir_len_hi

        ; Check for zero or negative length
        bmi _save_error
        lda c128_dir_len_lo
        ora c128_dir_len_hi
        beq _save_error

        ; Mark file operation active
        lda #1
        sta c128_file_op_active

        ; Print "SAVING " + filename
        lda #<C128Host_Msg_Saving
        ldx #>C128Host_Msg_Saving
        jsr emu_print_string
        jsr dos_print_filename_only

        ; DMA copy from guest RAM (bank 4) to staging buffer+2
        ; First 2 bytes = PRG load address header
        lda c128_save_start_lo
        sta C128_DIR_BUF
        lda c128_save_start_hi
        sta C128_DIR_BUF+1

        lda c128_dir_len_lo
        sta _save_dma_len
        lda c128_dir_len_hi
        sta _save_dma_len+1
        lda c128_save_start_lo
        sta _save_dma_src
        lda c128_save_start_hi
        sta _save_dma_src+1
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $00               ; copy
_save_dma_len:
        .word $0000
_save_dma_src:
        .word $0000
        .byte BANK_RAM0         ; src bank 4
        .word C128_DIR_BUF+2    ; dst
        .byte $00               ; dst bank 0
        .byte $00
        .word $0000

        ; Set up host KERNAL
        lda #$00
        ldx #$00
        jsr SETBNK

        ldx #<c128_fl_buf
        ldy #>c128_fl_buf
        lda c128_fl_len
        jsr SETNAM

        lda #$01
        ldx #$08
        ldy #$01
        jsr SETLFS

        jsr OPEN
        bcs _save_open_error

        ldx #$01
        jsr CHKOUT
        bcs _save_chkout_error

        ; Total = data + 2 byte header
        clc
        lda c128_dir_len_lo
        adc #2
        sta _save_total_lo
        lda c128_dir_len_hi
        adc #0
        sta _save_total_hi

        ; Output all bytes
        lda #<C128_DIR_BUF
        sta DIR_PTR
        lda #>C128_DIR_BUF
        sta DIR_PTR+1

        ldy #0
_save_loop:
        lda _save_total_lo
        ora _save_total_hi
        beq _save_loop_done

        lda (DIR_PTR),y
        jsr CHROUT

        iny
        bne +
        inc DIR_PTR+1
+
        lda _save_total_lo
        bne +
        dec _save_total_hi
+       dec _save_total_lo
        bra _save_loop

_save_loop_done:
        jsr CLRCHN
        lda #$01
        jsr CLOSE

        ; Success
        lda #$00
        jsr c128_write_status
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        lda #$00
        sta c128_a
        jmp _save_return

_save_open_error:
_save_chkout_error:
        jsr CLRCHN
        lda #$01
        jsr CLOSE

_save_error:
        lda c128_p
        ora #P_C
        sta c128_p
        lda #$05
        sta c128_a
        jsr c128_write_status

_save_return:
        ; Sync C128 cursor ZP from VDC cursor position (80-col only)
        jsr VDC_SyncCursor

        lda #0
        sta c128_file_op_active
        sta c128_fl_len
        jsr C128Hook_RTS_Guest
        rts

_save_total_lo: .byte 0
_save_total_hi: .byte 0

; ============================================================
; emu_chrout - Print a PETSCII character to the emulated screen
; Input: A = PETSCII character
; Clobbers: A, X, Y, Z
; ============================================================
emu_chrout:
        ; 40-col mode: use vic_print_char
        ldx display_showing_80
        bne _ec_80col
        cmp #$0D
        beq _ec_newline_40
        jsr emu_petscii_to_screen
        jsr vic_print_char
        ; Check if row hit 25 -> scroll
        lda #$EB
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        cmp #25
        bcc +
        jsr vic_scroll_up
        lda #24
        ldx #$EB
        jsr c128_write_zp_x
+       rts

_ec_80col:
        cmp #$0D
        beq _ec_newline

        ; Convert PETSCII to screen code
        jsr emu_petscii_to_screen

        ; Print via vdc_print_char (VDC RAM + MEGA65 screen + color)
        jsr vdc_print_char

        ; Mark dirty so VDC_RenderFrame updates display
        lda #1
        sta vdc_screen_dirty
        sta vdc_attr_dirty

        ; Track column for newline
        inc emu_col
        lda emu_col
        cmp #80
        bcc +
        lda #0
        sta emu_col
+       rts

_ec_newline:
        ; 40-col newline handled above
        ldx display_showing_80
        bne _ec_nl_80
_ec_newline_40:
        ; Set column to 0, advance row
        lda #0
        ldx #$EC
        jsr c128_write_zp_x
        lda #$EB
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        inc a
        cmp #25
        bcc _ec_nl40_noscroll
        jsr vic_scroll_up
        lda #24
        ldx #$EB
        jsr c128_write_zp_x
        ; After scroll, $E0:$E1 already points to correct line
        ; (content shifted up, so same address = new bottom row)
        rts
_ec_nl40_noscroll:
        ldx #$EB
        jsr c128_write_zp_x
        ; Advance screen line base $E0:$E1 += 40
        lda #$E0
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        clc
        adc #40
        ldx #$E0
        jsr c128_write_zp_x
        ldz #1
        lda [c128_zp_ptr],z
        adc #0
        ldx #$E1
        jsr c128_write_zp_x
        ; Advance color line base $E2:$E3 += 40
        lda #$E2
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        clc
        adc #40
        ldx #$E2
        jsr c128_write_zp_x
        ldz #1
        lda [c128_zp_ptr],z
        adc #0
        ldx #$E3
        jsr c128_write_zp_x
        rts
_ec_nl_80:
        ; Advance cursor to start of next row
        lda #80
        sec
        sbc emu_col
        clc
        adc vdc_regs+15
        sta vdc_regs+15
        bcc +
        inc vdc_regs+14
+
        lda #0
        sta emu_col

        ; Check scroll (offset from screen start >= 2000)
        lda vdc_regs+15
        sec
        sbc vdc_regs+13
        tax
        lda vdc_regs+14
        sbc vdc_regs+12
        cmp #>2000
        bcc _ec_nl_done
        bne _ec_scroll
        cpx #<2000
        bcc _ec_nl_done
_ec_scroll:
        lda vdc_regs+15
        sec
        sbc #80
        sta vdc_regs+15
        bcs +
        dec vdc_regs+14
+
        jsr scroll_screen_up
_ec_nl_done:
        rts

emu_col:    .byte 0
emu_col_hi: .byte 0

; --- PETSCII to screen code ---
emu_petscii_to_screen:
        cmp #$20
        bcc _epts_done
        cmp #$40
        bcc _epts_done
        cmp #$60
        bcc _epts_at
        cmp #$80
        bcc _epts_lower
        cmp #$A0
        bcc _epts_done
        cmp #$C0
        bcc _epts_shift
        cmp #$FF
        bcc _epts_c0
        lda #$5E
        rts
_epts_at:
        sec
        sbc #$40
        rts
_epts_lower:
        sec
        sbc #$20
        rts
_epts_shift:
        sec
        sbc #$40
        rts
_epts_c0:
        sec
        sbc #$80
_epts_done:
        rts

; --- Print null-terminated string ---
; Input: A = lo, X = hi
emu_print_string:
        pha
        jsr emu_print_init
        pla
        sta DIR_PTR
        stx DIR_PTR+1
        ldy #0
_eps_loop:
        lda (DIR_PTR),y
        beq _eps_done
        phy
        jsr emu_chrout
        ply
        iny
        bne _eps_loop
        inc DIR_PTR+1
        bra _eps_loop
_eps_done:
        rts

; --- Print 16-bit decimal number ---
; Input: epn_val set by caller
emu_print_number:
        lda #0
        sta epn_leading
        ldx #0
-       lda epn_val
        sec
        sbc #<10000
        tay
        lda epn_val+1
        sbc #>10000
        bcc +
        sta epn_val+1
        sty epn_val
        inx
        bra -
+       jsr epn_digit
        ldx #0
-       lda epn_val
        sec
        sbc #<1000
        tay
        lda epn_val+1
        sbc #>1000
        bcc +
        sta epn_val+1
        sty epn_val
        inx
        bra -
+       jsr epn_digit
        ldx #0
-       lda epn_val
        sec
        sbc #100
        tay
        lda epn_val+1
        sbc #0
        bcc +
        sta epn_val+1
        sty epn_val
        inx
        bra -
+       jsr epn_digit
        ldx #0
-       lda epn_val
        sec
        sbc #10
        tay
        lda epn_val+1
        sbc #0
        bcc +
        sta epn_val+1
        sty epn_val
        inx
        bra -
+       jsr epn_digit
        lda epn_val
        ora #$30
        jsr emu_chrout
        rts
epn_digit:
        cpx #0
        bne epn_nonzero
        lda epn_leading
        beq epn_skip
epn_nonzero:
        lda #1
        sta epn_leading
        txa
        ora #$30
        jsr emu_chrout
epn_skip:
        rts
epn_val:       .word 0
epn_leading:   .byte 0

; --- Init column tracker from VDC cursor ---
emu_print_init:
        lda display_showing_80
        bne _epi_80col
        ; 40-col: read column from C128 ZP $EC
        lda #$EC
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        sta emu_col
        rts
_epi_80col:
        ; 80-col: compute from VDC R14:R15 - R12:R13 mod 80
        lda vdc_regs+15
        sec
        sbc vdc_regs+13
        sta emu_col
        lda vdc_regs+14
        sbc vdc_regs+12
        sta emu_col_hi
_epi_mod:
        lda emu_col
        sec
        sbc #80
        tay
        lda emu_col_hi
        sbc #0
        bcc _epi_done
        sta emu_col_hi
        sty emu_col
        bra _epi_mod
_epi_done:
        rts


; ============================================================
; STUB: C128Hook_OnOPEN
; Intercept at $FFC0 - sequential file OPEN
;
; Guest state: $AC=LFN, $AD=SA, $AE=device (set by SETLFS)
;              $AB=namelen, $AF/$B0=nameptr (set by SETNAM)
;
; TODO: Only intercept device 8. Find free slot, copy filename
;       from guest RAM (may be in low RAM or bank 5), call
;       host OPEN, update C128 KERNAL file tables (LAT/FAT/SAT
;       at $0509+, LDTND at $97), return via RTS_Guest with
;       carry clear on success.
; ============================================================
C128Hook_OnOPEN:
        ; Copy filename bytes now that SETBNK is correct
        jsr dos_copy_filename

        ; Check if this is device 8
        lda LOW_RAM_BUFFER + $BA        ; Device number
        cmp #$08
        beq _open_disk
        ; Not disk - let ROM handle it
        rts

_open_disk:
        ; Use filename already captured by OnSETNAM
        lda c128_fl_len
        cmp #17
        bcs _open_error_no_file         ; Too long
        sta seq_filename_len
        bne _open_has_filename

        ; Empty filename - only allowed for SA >= $0F (command channel)
        lda LOW_RAM_BUFFER + $B9
        and #$0F
        cmp #$0F
        bne _open_error_no_file

_open_has_filename:
        ; Get guest LFN
        lda LOW_RAM_BUFFER + $B8
        sta _open_guest_lfn

        ; Find a free slot
        ldx #0
_open_find_slot:
        lda seq_slot_lfn,x
        cmp #$FF
        beq _open_found_slot
        cmp _open_guest_lfn
        beq _open_error_file_open
        inx
        cpx #MAX_SEQ_FILES
        bcc _open_find_slot
        jmp _open_error_no_file         ; No free slots

_open_found_slot:
        stx _open_slot

        ; Copy filename from c128_fl_buf to seq_filename
        ldy #0
_open_copy_name:
        cpy c128_fl_len
        beq _open_copy_done
        lda c128_fl_buf,y
        sta seq_filename,y
        iny
        cpy #17
        bcc _open_copy_name
_open_copy_done:
        lda #0
        sta seq_filename,y              ; Null terminate

        ; Set up host KERNAL
        lda #$00
        ldx #$00
        jsr SETBNK

        ; Set filename for host
        ldx #<seq_filename
        ldy #>seq_filename
        lda seq_filename_len
        jsr SETNAM

        ; Calculate host LFN = SEQ_LFN_BASE + slot
        lda _open_slot
        clc
        adc #SEQ_LFN_BASE
        sta _open_host_lfn

        ; SETLFS for host: host_lfn, device 8, guest SA
        lda _open_host_lfn
        ldx #$08
        ldy LOW_RAM_BUFFER + $B9        ; Secondary address
        jsr SETLFS

        ; Call host OPEN
        jsr OPEN
        bcs _open_error_host

        ; Success - record in slot table
        ldx _open_slot
        lda _open_guest_lfn
        sta seq_slot_lfn,x
        lda #$08
        sta seq_slot_dev,x
        lda LOW_RAM_BUFFER + $B9
        sta seq_slot_sa,x
        lda #0
        sta seq_slot_status,x
        lda #1
        sta seq_slot_open,x

        ; Update C128 KERNAL file tables
        ; LDTND at $98, LAT at $0362, FAT at $036C, SAT at $0376
        ldx LOW_RAM_BUFFER + $98        ; Current file count
        cpx #10
        bcs _open_table_full

        lda _open_guest_lfn
        sta LOW_RAM_BUFFER + $0362,x    ; LAT[x] = LFN
        lda #$08
        sta LOW_RAM_BUFFER + $036C,x    ; FAT[x] = device
        lda LOW_RAM_BUFFER + $B9        ; SA
        ora #$60                        ; Set bits 5+6 like KERNAL does
        sta LOW_RAM_BUFFER + $B9        ; Update ZP too
        sta LOW_RAM_BUFFER + $0376,x    ; SAT[x] = SA | $60

        ; Increment file count
        inc LOW_RAM_BUFFER + $98

_open_table_full:
        ; Sync updated tables back to bank 4
        jsr C128Hook_SyncLowRAMBack

        ; Set success status
        lda #$00
        jsr c128_write_status
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p

        jsr C128Hook_RTS_Guest
        rts

_open_error_host:
        lda _open_host_lfn
        jsr CLOSE
        jmp _open_error_no_file

_open_error_file_open:
        lda #$02                        ; FILE ALREADY OPEN
        jmp _open_set_error

_open_error_no_file:
        lda #$04                        ; FILE NOT FOUND

_open_set_error:
        sta c128_a
        jsr c128_write_status
        lda c128_p
        ora #P_C
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_open_guest_lfn: .byte 0
_open_host_lfn:  .byte 0
_open_slot:      .byte 0



; ============================================================
; STUB: C128Hook_OnCLOSE
; Intercept at $FFC3 - close a logical file
;
; Guest A register = LFN to close
;
; TODO: Find slot by LFN, close on host, clear slot, remove
;       entry from C128 KERNAL file tables (shift subsequent
;       entries down, decrement LDTND), return via RTS_Guest.
; ============================================================
C128Hook_OnCLOSE:
        ; Get LFN from guest A
        lda c128_a
        sta _close_lfn

        ; Find slot with this LFN
        ldx #0
_close_find:
        lda seq_slot_lfn,x
        cmp _close_lfn
        beq _close_found
        inx
        cpx #MAX_SEQ_FILES
        bcc _close_find
        ; Not found in our table - let ROM handle (might be non-disk)
        rts

_close_found:
        stx _close_slot

        ; Check if it's actually open
        lda seq_slot_open,x
        beq _close_not_open

        ; Clear host channels before closing
        jsr CLRCHN

        ; Close on host
        lda _close_slot
        clc
        adc #SEQ_LFN_BASE
        jsr CLOSE

        ; Clear slot
        ldx _close_slot
        lda #$FF
        sta seq_slot_lfn,x
        lda #0
        sta seq_slot_open,x
        sta seq_slot_status,x

        ; If this was current input/output, clear that too
        cpx seq_input_slot
        bne +
        lda #$FF
        sta seq_input_slot
+       cpx seq_output_slot
        bne +
        lda #$FF
        sta seq_output_slot
+
        ; Remove entry from C128 KERNAL file tables
        ; LDTND=$98, LAT=$0362, FAT=$036C, SAT=$0376
        ldx #0
        lda LOW_RAM_BUFFER + $98
        beq _close_table_done

_close_find_lat:
        lda LOW_RAM_BUFFER + $0362,x    ; LAT[x]
        cmp _close_lfn
        beq _close_found_lat
        inx
        cpx LOW_RAM_BUFFER + $98
        bcc _close_find_lat
        bra _close_table_done

_close_found_lat:
        ; Found at index X - shift subsequent entries down
        dec LOW_RAM_BUFFER + $98

_close_shift_loop:
        inx
        cpx LOW_RAM_BUFFER + $98
        beq _close_copy_last
        bcs _close_table_done

        ; Copy entry X to X-1
        lda LOW_RAM_BUFFER + $0362,x
        sta LOW_RAM_BUFFER + $0361,x    ; LAT[x-1] = LAT[x]
        lda LOW_RAM_BUFFER + $036C,x
        sta LOW_RAM_BUFFER + $036B,x    ; FAT[x-1] = FAT[x]
        lda LOW_RAM_BUFFER + $0376,x
        sta LOW_RAM_BUFFER + $0375,x    ; SAT[x-1] = SAT[x]
        bra _close_shift_loop

_close_copy_last:
        lda LOW_RAM_BUFFER + $0362,x
        sta LOW_RAM_BUFFER + $0361,x
        lda LOW_RAM_BUFFER + $036C,x
        sta LOW_RAM_BUFFER + $036B,x
        lda LOW_RAM_BUFFER + $0376,x
        sta LOW_RAM_BUFFER + $0375,x

_close_table_done:
        ; Sync tables back to bank 4
        jsr C128Hook_SyncLowRAMBack

_close_not_open:
        ; Success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_close_lfn:  .byte 0
_close_slot: .byte 0



; ============================================================
; STUB: C128Hook_OnCHKIN
; Intercept at $FFC6 - set input channel
;
; Guest X register = LFN
;
; TODO: Find slot by LFN, verify it's open, set as current
;       input (seq_input_slot) on both our side and host side,
;       return via RTS_Guest.
; ============================================================
C128Hook_OnCHKIN:
        ; Get LFN from guest X
        lda c128_x
        sta _chkin_lfn

        ; Find slot with this LFN
        ldx #0
_chkin_find:
        lda seq_slot_lfn,x
        cmp _chkin_lfn
        beq _chkin_found
        inx
        cpx #MAX_SEQ_FILES
        bcc _chkin_find
        ; Not in our table - let ROM handle
        rts

_chkin_found:
        ; Check if open
        lda seq_slot_open,x
        beq _chkin_not_open

        ; Set as current input
        stx seq_input_slot

        ; Also set on host side
        txa
        clc
        adc #SEQ_LFN_BASE
        tax
        jsr CHKIN

        ; Success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_chkin_not_open:
        ; File not open error
        lda #$03
        sta c128_a
        jsr c128_write_status
        lda c128_p
        ora #P_C
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_chkin_lfn: .byte 0



; ============================================================
; STUB: C128Hook_OnCHKOUT
; Intercept at $FFC9 - set output channel
;
; Guest X register = LFN
;
; TODO: Find slot by LFN, verify it's open, set as current
;       output (seq_output_slot) on both our side and host side,
;       return via RTS_Guest.
; ============================================================
C128Hook_OnCHKOUT:
        ; Get LFN from guest X
        lda c128_x
        sta _chkout_lfn

        ; LFN 0 = screen (let ROM handle)
        beq _chkout_rom

        ; Find slot with this LFN
        ldx #0
_chkout_find:
        lda seq_slot_lfn,x
        cmp _chkout_lfn
        beq _chkout_found
        inx
        cpx #MAX_SEQ_FILES
        bcc _chkout_find
        ; Not in our table - let ROM handle
_chkout_rom:
        rts

_chkout_found:
        ; Check if open
        lda seq_slot_open,x
        beq _chkout_not_open

        ; Set as current output
        stx seq_output_slot

        ; Also set on host side
        txa
        clc
        adc #SEQ_LFN_BASE
        tax
        jsr CHKOUT

        ; Success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_chkout_not_open:
        lda #$03
        sta c128_a
        jsr c128_write_status
        lda c128_p
        ora #P_C
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_chkout_lfn: .byte 0



; ============================================================
; STUB: C128Hook_OnCLRCHN
; Intercept at $FFCC - clear I/O channels
;
; TODO: If we had an active input/output channel, clear it on
;       host side too. Reset seq_input_slot / seq_output_slot
;       to $FF. Let ROM also run to reset its state.
; ============================================================
C128Hook_OnCLRCHN:
        ; Check if we have any active channels
        lda seq_input_slot
        cmp #$FF
        beq _clrchn_check_out
        ; We had input channel - clear on host too
        jsr CLRCHN

_clrchn_check_out:
        ; Clear our tracking
        lda #$FF
        sta seq_input_slot
        sta seq_output_slot

        ; Let ROM also run to reset its state
        rts



; ============================================================
; STUB: C128Hook_OnBASIN
; Intercept at $FFCF - character input (file path)
;
; Only called when seq_input_slot != $FF (dispatcher checks).
;
; TODO: Check EOF status, read byte from host via BASIN,
;       update slot status, update guest status byte,
;       return byte in guest A, return via RTS_Guest.
; ============================================================
C128Hook_OnBASIN:
        ; Check if we have an active input channel
        lda seq_input_slot
        cmp #$FF
        beq _basin_rom

        ; We're reading from a file
        ldx seq_input_slot

        ; Check status - if EOF already, return with status
        lda seq_slot_status,x
        and #$40
        bne _basin_eof

        ; Read from host
        jsr CHRIN
        sta _basin_byte

        ; Check host status
        jsr READST
        sta _basin_status

        ; Update our status
        ldx seq_input_slot
        ora seq_slot_status,x
        sta seq_slot_status,x

        ; Update guest status byte
        jsr c128_write_status

        ; Return the byte in guest A
        lda _basin_byte
        sta c128_a

        ; Clear carry for success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p

        jsr C128Hook_RTS_Guest
        rts

_basin_eof:
        lda #$00
        sta c128_a
        lda #$40
        jsr c128_write_status
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_basin_rom:
        rts

_basin_byte:   .byte 0
_basin_status: .byte 0



; ============================================================
; STUB: C128Hook_OnCHROUT
; Character output to file (not screen)
;
; Only called when seq_output_slot != $FF (dispatcher checks).
;
; TODO: Get byte from guest A, write to host via CHROUT,
;       update slot status, update guest status byte,
;       return via RTS_Guest.
; ============================================================
C128Hook_OnCHROUT:
        ; Check if we have an active output channel to a file
        lda seq_output_slot
        cmp #$FF
        beq _fchrout_rom

        ; We're writing to a file - get byte from guest A
        lda c128_a
        jsr CHROUT

        ; Check status
        jsr READST
        ldx seq_output_slot
        ora seq_slot_status,x
        sta seq_slot_status,x
        jsr c128_write_status

        ; Clear carry for success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p

        jsr C128Hook_RTS_Guest
        rts

_fchrout_rom:
        rts



; ============================================================
; STUB: C128Hook_OnGETIN
; Intercept at $FFE4 - get character (file path)
;
; Only called when seq_input_slot != $FF (dispatcher checks).
;
; TODO: Same as BASIN for file input. Check EOF, read byte,
;       update status, return in guest A via RTS_Guest.
; ============================================================
C128Hook_OnGETIN:
        ; Check if we have an active input channel
        lda seq_input_slot
        cmp #$FF
        beq _getin_rom

        ; Reading from a file
        ldx seq_input_slot

        ; Check EOF
        lda seq_slot_status,x
        and #$40
        bne _getin_eof

        ; Read from host
        jsr CHRIN
        sta _getin_byte

        ; Check host status
        jsr READST
        sta _getin_status

        ; Update our status
        ldx seq_input_slot
        ora seq_slot_status,x
        sta seq_slot_status,x

        ; Update guest status
        jsr c128_write_status

        ; Return byte in guest A
        lda _getin_byte
        sta c128_a

        ; Clear carry
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p

        jsr C128Hook_RTS_Guest
        rts

_getin_eof:
        lda #$00
        sta c128_a
        lda #$40
        jsr c128_write_status
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_getin_rom:
        rts

_getin_byte:   .byte 0
_getin_status: .byte 0



; ============================================================
; Host KERNAL bank swap helpers
;
; The MEGA65 host KERNAL needs its original ROM in banks 2-3.
; The emulator overwrites these banks with VDC RAM etc.
; Before any host KERNAL file I/O, swap in the originals;
; after, swap back.
;
; Attic RAM layout:
;   $80020000 = saved original bank 2 (at init)
;   $80030000 = saved original bank 3 (at init)
;   $80060000 = saved emulator bank 2 (during swap)
;   $80070000 = saved emulator bank 3 (during swap)
; ============================================================






; ============================================================
; DMA / sync helpers
; ============================================================

; ------------------------------------------------------------
; C128Hook_SyncLowRAM - Sync LOW_RAM_BUFFER from bank 4
; Fast ZP opcodes write directly to bank 4, bypassing the
; LOW_RAM_BUFFER mirror. This DMA brings the mirror up to date.
; ------------------------------------------------------------
C128Hook_SyncLowRAM:
        lda #$00
        sta $D707
        .byte $80, $00
        .byte $81, $00
        .byte $00
        .byte $00               ; copy
        .word $1000             ; 4KB
        .word $0000             ; src = bank 4 : $0000
        .byte $04
        .word LOW_RAM_BUFFER    ; dst = bank 0 : $B000
        .byte $00
        .byte $00
        .word $0000
        rts

; ------------------------------------------------------------
; C128Hook_SyncLowRAMBack - Copy LOW_RAM_BUFFER back to bank 4
; Called after hooks that modify file tables at $0500+
; ------------------------------------------------------------
C128Hook_SyncLowRAMBack:
        lda #$00
        sta $D707
        .byte $80, $00
        .byte $81, $00
        .byte $00
        .byte $00               ; copy
        .word $0600             ; pages 0-5 (1.5KB)
        .word LOW_RAM_BUFFER    ; src = $B000
        .byte $00
        .word $0000             ; dst = bank 4 : $0000
        .byte $04
        .byte $00
        .word $0000
        rts


; ============================================================
; Helpers: Write to BOTH LOW_RAM_BUFFER and bank 4
; ============================================================

; Write A to status byte ($0090) in both places
; Preserves: A, Y
c128_write_status:
        sta LOW_RAM_BUFFER + $90
        phx
        ldx #$90
        stx c128_zp_ptr+0
        ldz #0
        sta [c128_zp_ptr],z
        plx
        rts

; Write A to ZP offset X in both places
; Preserves: A
c128_write_zp_x:
        sta LOW_RAM_BUFFER,x
        stx c128_zp_ptr+0
        ldz #0
        sta [c128_zp_ptr],z
        rts

; Inc ZP offset X in both places
; Clobbers: A
c128_inc_zp_x:
        inc LOW_RAM_BUFFER,x
        stx c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        inc a
        sta [c128_zp_ptr],z
        rts

; Dec ZP offset X in both places
; Clobbers: A
c128_dec_zp_x:
        dec LOW_RAM_BUFFER,x
        stx c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        dec a
        sta [c128_zp_ptr],z
        rts


; ============================================================
; C128Hook_PostFileOpVideoFix
; If the last host file operation set Carry in c128_p (error),
; force emulator back to text mode to recover host video state.
; ============================================================
C128Hook_PostFileOpVideoFix:
        lda c128_p
        and #P_C
        beq _pfov_done

        lda #0
        jsr C128Vid_DisableHostBitmap
        jsr C128_VideoInit

_pfov_done:
        rts