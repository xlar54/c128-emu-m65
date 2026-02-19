; ============================================================
; c128_hooks.asm - BASIC/KERNAL hook points for C128 emulator
; Host: MEGA65
; Assembler: 64tass (45GS02)
;
; Intercepts SETNAM/SETLFS to capture filename parameters
; directly from the emulated CPU registers, then uses them when
; the LOAD or SAVE KERNAL calls happen.
;
; ============================================================

        .cpu "45gs02"

; c128_hook_pc_changed is defined in c128_cpu_8502.asm at ZP $E7
; Hooks set this flag when they modify PC, so the threaded
; interpreter knows to invalidate the code cache.

; ------------------------------------------------------------
; Hooked guest PC locations
; ------------------------------------------------------------
; C128 KERNAL jump table entries
C128_SETNAM               = $FFBD         ; SETNAM entry point
C128_SETLFS               = $FFBA         ; SETLFS entry point  
C128_LOAD                 = $FFD5         ; LOAD entry point
C128_SAVE                 = $FFD8         ; SAVE entry point
; Additional KERNAL entries used for DIRECTORY (host-side)
C128_OPEN                 = $FFC0         ; OPEN
C128_CLOSE                = $FFC3         ; CLOSE
C128_CHKIN                = $FFC6         ; CHKIN
C128_CHKOUT               = $FFC9         ; CHKOUT  
C128_CLRCHN               = $FFCC         ; CLRCHN
C128_CHRIN                = $FFCF         ; CHRIN
; C128_CHROUT defined in c128_host.asm
C128_READST               = $FFB7         ; READST


; MEGA65 KERNAL routines are defined in main.asm:
; OPEN, CLOSE, CHKIN, CHKOUT, CLRCHN, CHRIN, CHROUT, READST

; Sequential file I/O constants
MAX_SEQ_FILES           = 10            ; Maximum simultaneous open files (matches C128 KERNAL)
SEQ_LFN_BASE            = 10            ; Host logical file numbers start here

; Inside BASIC LOAD handler: the JSR $FFD5 instruction
C128Hook_ROM_LOAD_KERNAL_CALL      = $A800
C128Hook_ROM_AFTER_KERNAL_CALL     = $A803

; Inside BASIC SAVE handler: the JSR $FFD8 instruction
; We'll find this by hooking KERNAL SAVE directly
C128Hook_ROM_SAVE_KERNAL_CALL      = $A851  ; Approximate - we hook $FFD8 instead
C128Hook_ROM_AFTER_SAVE_CALL       = $A854  ; Return address after SAVE

; DIRECTORY keyword handler - actual entry point in BASIC ROM
C128Hook_ROM_DIRECTORY             = $C8BC

; DIRECTORY keyword handler entry (from BASIC keyword vector table) - old
C128Hook_ROM_PERFORM_DIRECTORY     = $ED7F

; LIST keyword handler entry (from BASIC keyword vector table)
C128Hook_ROM_PERFORM_LIST          = $8AFE

; A "fake" return address used to regain control after LIST prints.
C128Hook_DIR_RESTORE_TRAP          = $02F6

; ------------------------------------------------------------
; C128 BASIC low-RAM pointers we need
; ------------------------------------------------------------
ZP_TXTTAB_LO = $2B
ZP_TXTTAB_HI = $2C
ZP_VARTAB_LO = $2D
ZP_VARTAB_HI = $2E
ZP_TOPMEM_LO = $37
ZP_TOPMEM_HI = $38

ZP_PTRS_BASE  = $2B
ZP_PTRS_COUNT = 10

; ------------------------------------------------------------
; Directory load configuration
; ------------------------------------------------------------
C128_BANK_RAM     = $05           ; emulated C128 RAM lives in MEGA65 bank 5

; Host-side staging buffer (bank 0 address)
C128_DIR_BUF      = $6000         ; 4KB staging buffer in bank 0

; MEGA65 KERNAL SAVE (may not be in main.asm)
SAVE            = $FFD8

c128_dir_len_lo:  .byte 0
c128_dir_len_hi:  .byte 0

c128_dir_dest_lo: .byte 0
c128_dir_dest_hi: .byte 0

c128_saved_basic_ptrs:
        .fill ZP_PTRS_COUNT, 0

c128_dir_temp_lo: .byte 0
c128_dir_temp_hi: .byte 0

tmp_lo: .byte 0
tmp_hi: .byte 0

; Filename "$" for CBM directory listing
c128_dir_name:
        .byte '$'

; ------------------------------------------------------------
; SETNAM capture variables - this is the key new addition!
; ------------------------------------------------------------
c128_setnam_len:    .byte 0       ; Captured filename length
c128_setnam_ptr_lo: .byte 0       ; Captured filename pointer lo
c128_setnam_ptr_hi: .byte 0       ; Captured filename pointer hi
c128_setnam_valid:  .byte 0       ; Flag: 1 = we have valid SETNAM data

; SETLFS capture variables
c128_setlfs_dev:    .byte 8       ; Device number (default 8)
c128_setlfs_sa:     .byte 0       ; Secondary address

; File load buffer
c128_fl_buf:      .fill 17, 0     ; Filename buffer (16 chars + null)
c128_fl_len:      .byte 0
c128_fl_end_lo:   .byte 0
c128_fl_end_hi:   .byte 0

; SAVE variables
c128_save_start_lo: .byte 0       ; Start address of data to save
c128_save_start_hi: .byte 0
c128_save_end_lo:   .byte 0       ; End address + 1
c128_save_end_hi:   .byte 0

; Monitor LOAD flag - if set, return via RTS not jump to BASIC
c128_monitor_load:  .byte 0

; ------------------------------------------------------------
; Sequential File I/O Variables
; ------------------------------------------------------------
; We support up to MAX_SEQ_FILES (10) simultaneous open files.
; Each slot tracks: guest LFN, device, SA, host LFN, status
;
; Guest LFN (logical file number) maps to our internal slot.
; Host LFN is what we use with MEGA65 KERNAL (SEQ_LFN_BASE + slot).

; File slot table - $FF = unused, other values = guest LFN using this slot
seq_slot_lfn:     .fill MAX_SEQ_FILES, $FF   ; Guest LFN for each slot ($FF = unused)
seq_slot_dev:     .fill MAX_SEQ_FILES, 0    ; Device number
seq_slot_sa:      .fill MAX_SEQ_FILES, 0    ; Secondary address
seq_slot_status:  .fill MAX_SEQ_FILES, 0    ; Status (EOF, error flags)
seq_slot_open:    .fill MAX_SEQ_FILES, 0    ; 1 if slot is open on host

; Current I/O channel state
seq_input_slot:   .byte $FF             ; Current input slot ($FF = none/keyboard)
seq_output_slot:  .byte $FF             ; Current output slot ($FF = none/screen)

; Temp storage for filename during OPEN
seq_filename:     .fill 17, 0           ; Filename buffer for sequential files
seq_filename_len: .byte 0

; ------------------------------------------------------------
; C128Hook_Reset - Reset all hook state variables
; Called during emulator reset
; ------------------------------------------------------------
C128Hook_Reset:
        lda #0
        sta c128_setnam_valid
        sta c128_setnam_len
        sta c128_setlfs_sa
        sta c128_monitor_load
        sta c128_fl_len
        lda #8
        sta c128_setlfs_dev       ; Default device 8
        rts

; ------------------------------------------------------------
; C128Hook_CheckAndRun
;   Called once per emulated instruction, right before opcode fetch.
; ------------------------------------------------------------
C128Hook_CheckAndRun:
        pha
        txa
        pha
        tya
        pha

        lda c128_pc_hi

        ; ----- Check for $FFxx addresses (KERNAL calls) -----
        cmp #$FF
        bne _not_ff
        
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
        jmp _check_other

_do_setnam:
        jsr C128Hook_OnSETNAM
        jmp _done

_do_setlfs:
        jsr C128Hook_OnSETLFS
        jmp _done

_do_save:
        jsr C128Hook_OnSAVE
        jmp _done

_do_open:
        jsr C128Hook_OnOPEN
        jmp _done

_do_close:
        jsr C128Hook_OnCLOSE
        jmp _done

_do_chkin:
        jsr C128Hook_OnCHKIN
        jmp _done

_do_chkout:
        jsr C128Hook_OnCHKOUT
        jmp _done

_do_clrchn:
        jsr C128Hook_OnCLRCHN
        jmp _done

_do_chrin:
        lda seq_input_slot
        cmp #$FF
        beq _done               ; No file input - let ROM handle
        jsr C128Hook_OnCHRIN
        jmp _done

_do_chrout:
        lda seq_output_slot
        cmp #$FF
        beq _done               ; Let guest ROM handle it
        jsr C128Hook_OnCHROUT
        jmp _done

_do_getin:
        lda seq_input_slot
        cmp #$FF
        beq _done               ; No file input - let ROM handle
        jsr C128Hook_OnGETIN
        jmp _done

_do_load_direct:
        ; HACK: Distinguish BASIC LOAD from Monitor L command
        ; 
        ; Problem: BASIC LOAD is hooked at $A800 (JSR $FFD5 inside BASIC).
        ; Monitor L command calls $FFD5 directly, bypassing $A800.
        ; We need to intercept monitor but not interfere with BASIC.
        ;
        ; Solution: Check the return address on the 6502 stack.
        ; - BASIC: JSR $FFD5 at $A800 pushes return addr $A802 (addr-1)
        ; - Monitor: Return address will be somewhere else (monitor ROM)
        ;
        ; If return address is $A802, skip - the $A800 hook handles BASIC.
        ; Otherwise, this is the monitor - handle LOAD here.
        ;
        ldy c128_sp
        iny                             ; Point to return address lo on stack
        lda #$01
        sta c128_addr_hi
        sty c128_addr_lo
        jsr C128_Read
        cmp #$02                        ; Return lo = $02?
        bne _do_load_monitor
        iny
        sty c128_addr_lo
        jsr C128_Read
        cmp #$A8                        ; Return hi = $A8?
        bne _do_load_monitor
        ; Return addr is $A802 = BASIC calling $FFD5, skip
        jmp _done
        
_do_load_monitor:
        ; Monitor or other non-BASIC caller - handle LOAD here
        jsr C128Hook_OnLOAD
        jmp _done

_not_ff:
        ; ----- Check for $F8xx (auto-boot) -----
        cmp #$F8
        bne _check_f9
        lda c128_pc_lo
        cmp #$67                        ; $F867 = IOINIT entry (serial init + auto-boot)
        bne _f8_done
        ; Skip IOINIT entirely - no serial bus or disk drive
        ; But we must initialize CIA1 Timer A for keyboard scanning IRQ
        ; C128 KERNAL sets Timer A to $4025 (~60Hz on PAL)
        lda #$25
        sta cia1_timer_a_latch_lo
        sta cia1_timer_a_lo
        lda #$40
        sta cia1_timer_a_latch_hi
        sta cia1_timer_a_hi
        ; Enable Timer A IRQ (bit 0 of ICR mask)
        lda #$81                        ; Set bit 0 (Timer A)
        sta cia1_icr_mask
        ; Start Timer A: continuous mode
        lda #$01                        ; bit 0 = start
        sta cia1_timer_a_ctrl
        ; Write keyboard column select to $DC00 (all columns = $FF)
        lda #$FF
        sta $DC00
        ; Simulate RTS to return to caller (who did JSR $FF56)
        jsr C128Hook_RTS_Guest
_f8_done:
        jmp _done

_check_f9:
        cmp #$F9
        bne _check_fa
        jmp _done

_check_fa:
        cmp #$FA
        bne _check_c4_rom
        jmp _done

_check_c4_rom:
_check_c4_done:
        ; Reload c128_pc_hi for next check
        lda c128_pc_hi

        ; ----- Check for $C8xx (DIRECTORY at $C8BC) -----
_check_c8:
        cmp #$C8
        bne _check_a8
        lda c128_pc_lo
        cmp #$BC                        ; DIRECTORY = $C8BC
        bne _done
        jsr C128Hook_OnDIRECTORY
        jmp _done

_check_a8:
        ; ----- Check for LOAD hook at $A800 -----
        cmp #$A8
        bne _done
        lda c128_pc_lo
        cmp #$00
        bne _done

_check_other:
        lda c128_pc_hi
        cmp #$A8
        bne _done
        lda c128_pc_lo
        bne _done
        
        ; We're at $A800 - the JSR $FFD5 inside BASIC LOAD
        jsr C128Hook_OnLOAD

_done:
        pla
        tay
        pla
        tax
        pla
        rts


; ============================================================
; Hook: SETNAM - Capture filename parameters from registers
; Called when guest PC == $FFBD
; 
; On entry to SETNAM:
;   A = filename length
;   X = filename pointer low byte
;   Y = filename pointer high byte
;
; NOTE: LOAD calls SETNAM twice:
;   1st call: length=0 to clear filename
;   2nd call: length>0 with actual filename, pointer to RAM UNDER ROM
; ============================================================
C128Hook_OnSETNAM:
        ; Only capture if length > 0 (skip the clearing call)
        lda c128_a
        beq _setnam_skip
        
        ; Capture the parameters from emulated CPU registers
        sta c128_setnam_len
        
        lda c128_x
        sta c128_setnam_ptr_lo
        
        lda c128_y
        sta c128_setnam_ptr_hi
        
        ; Mark that we have valid SETNAM data
        lda #1
        sta c128_setnam_valid
        
_setnam_skip:
        ; Let the ROM SETNAM continue normally
        rts


; ============================================================
; Hook: SETLFS - Capture device number and secondary address
; Called when guest PC == $FFBA
;
; On entry to SETLFS:
;   A = logical file number
;   X = device number
;   Y = secondary address
; ============================================================
C128Hook_OnSETLFS:
        ; Capture device and secondary address
        lda c128_x
        sta c128_setlfs_dev
        
        lda c128_y
        sta c128_setlfs_sa
        
        ; Let the ROM SETLFS continue normally
        rts


; ============================================================
; Hook: LOAD - Handle the actual file loading
; Called when guest PC == $A800 (JSR $FFD5 inside BASIC LOAD)
; ============================================================
C128Hook_OnLOAD:
        ; Check if we have valid SETNAM data
        lda c128_setnam_valid
        beq _load_no_setnam
        
        ; Check filename length - if 1, might be "$" for directory
        lda c128_setnam_len
        cmp #1
        bne _load_file
        
        ; Check if it's "$" for directory
        ; Read from RAM under ROM using 32-bit addressing
        lda c128_setnam_ptr_lo
        sta C128_MEM_PTR
        lda c128_setnam_ptr_hi
        sta C128_MEM_PTR+1
        lda #BANK_RAM
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        
        ldz #0
        lda [C128_MEM_PTR],z
        cmp #'$'
        beq _load_directory
        
        ; Single character filename that's not "$" - load as file
        jmp _load_file

_load_no_setnam:
        ; No SETNAM called - check if monitor set filename directly
        ; Monitor stores: $AB = length, $AF/$B0 = pointer (pointing to $025D)
        lda LOW_RAM_BUFFER + $AB        ; Filename length
        beq _load_no_setnam_rts         ; No filename, let ROM handle
        cmp #17
        bcs _load_no_setnam_rts         ; Too long, let ROM handle
        
        ; Mark this as a monitor load - needs RTS return, not jump to BASIC
        pha
        lda #1
        sta c128_monitor_load
        pla
        
        ; Get SETLFS values from KERNAL variables
        ; $AC = logical file, $AD = secondary address, $AE = device
        lda LOW_RAM_BUFFER + $AD        ; Secondary address
        sta c128_setlfs_sa
        lda LOW_RAM_BUFFER + $AE        ; Device number
        sta c128_setlfs_dev
        
        ; Use KERNAL variables - copy filename from LOW_RAM_BUFFER
        ; since monitor stores filename at $025D which is in low RAM
        lda LOW_RAM_BUFFER + $AB        ; Reload filename length
        sta c128_fl_len
        
        ; Set up pointer: LOW_RAM_BUFFER + $AF/$B0 value
        lda LOW_RAM_BUFFER + $AF        ; Pointer lo ($5D)
        sta $FB
        clc
        lda LOW_RAM_BUFFER + $B0        ; Pointer hi ($02)
        adc #>LOW_RAM_BUFFER            ; Add $A0 -> $A2
        sta $FC
        
        ; Copy filename bytes
        ldy #0
_load_copy_fn:
        cpy c128_fl_len
        beq _load_copy_done
        lda ($FB),y
        sta c128_fl_buf,y
        iny
        cpy #17
        bcc _load_copy_fn
_load_copy_done:
        lda #0
        sta c128_fl_buf,y                 ; Null terminate
        jmp _load_do_it                 ; Skip to loading

_load_no_setnam_rts:
        rts

_load_directory:
        ; Check device number - only intercept device 8 (disk)
        lda c128_setlfs_dev
        cmp #$08
        beq _load_dir_disk
        ; Not device 8 - let ROM handle
        lda #0
        sta c128_setnam_valid
        rts
        
_load_dir_disk:
        ; Clear the valid flag
        lda #0
        sta c128_setnam_valid
        
        ; Print "SEARCHING FOR "
        lda #<C128Host_Msg_Searching
        ldx #>C128Host_Msg_Searching
        jsr C128Host_PrintString
        
        ; Print the filename
        lda #'$'
        jsr C128Host_PutChar
        
        lda #<C128Host_Msg_Loading
        ldx #>C128Host_Msg_Loading
        jsr C128Host_PrintString

        ; Load directory listing
        jsr C128Hook_LoadDirectory
        rts

_load_file:
        ; Check device number - only intercept device 8 (disk)
        lda c128_setlfs_dev
        cmp #$08
        beq _load_file_disk
        ; Not device 8 - let ROM handle (tape, or no device specified)
        ; Clear the valid flag first
        lda #0
        sta c128_setnam_valid
        rts
        
_load_file_disk:
        ; Clear the valid flag
        lda #0
        sta c128_setnam_valid
        
        ; Check if we were called from the $A800 hook (BASIC) or $FFD5 hook (direct)
        ; If PC is currently $A800, we're in BASIC path
        lda c128_pc_hi
        cmp #$A8
        bne _lfd_not_basic
        lda c128_pc_lo
        bne _lfd_not_basic
        
        ; Caller is BASIC (PC = $A800)
        lda #0
        sta c128_monitor_load
        jmp _lfd_continue
        
_lfd_not_basic:
        ; Caller is NOT BASIC - set monitor flag for RTS return
        lda #1
        sta c128_monitor_load
        
_lfd_continue:
        ; Copy filename from guest RAM to our buffer
        jsr C128Hook_CopyFilename
        bcs _load_file_error
        
_load_do_it:
        ; Print "SEARCHING FOR "
        lda #<C128Host_Msg_Searching
        ldx #>C128Host_Msg_Searching
        jsr C128Host_PrintString
        
        ; Print the filename
        ldy #0
    _print_name:
        cpy c128_fl_len
        beq _print_done
        lda c128_fl_buf,y
        phy
        jsr C128Host_PutChar          ; (alias for C128Host_PrintCharSync)
        ply
        iny
        bne _print_name
    _print_done:
        
        ; Now load the file using host KERNAL
        jsr C128Hook_DoHostLoad
        rts

_load_file_error:
        ; Set carry and error code
        lda c128_p
        ora #P_C
        sta c128_p
        lda #$04                        ; FILE NOT FOUND
        sta c128_a
        lda #$42
        sta LOW_RAM_BUFFER + $90        ; Also set STATUS byte!
        
        ; Check if this was a monitor/direct load or BASIC load
        lda c128_monitor_load
        beq _lfe_basic
        
        ; Monitor/direct load - return via RTS
        lda #0
        sta c128_monitor_load
        jsr C128Hook_RTS_Guest
        rts
        
_lfe_basic:
        ; BASIC load - skip to after JSR $FFD5
        lda #<C128Hook_ROM_AFTER_KERNAL_CALL
        sta c128_pc_lo
        lda #>C128Hook_ROM_AFTER_KERNAL_CALL
        sta c128_pc_hi
        lda #1
        sta c128_hook_pc_changed    ; Signal to threaded interpreter
        rts

; ============================================================
; C128Hook_CopyFilename - Copy filename from guest RAM to buffer
; 
; NOTE: The filename pointer points to RAM UNDER ROM, so we must
; read directly from Bank 5 (guest RAM), NOT through C128_Read
; which would see the ROM overlay.
;
; Returns: C=0 success, C=1 error
; ============================================================
C128Hook_CopyFilename:
        lda c128_setnam_len
        beq _cf_error                   ; No filename
        cmp #17
        bcs _cf_error                   ; Too long
        sta c128_fl_len
        
        ; Check if filename is in low RAM ($0000-$0FFF) or bank 5 ($1000+)
        lda c128_setnam_ptr_hi
        cmp #$10
        bcs _cf_bank5                   ; >= $1000, use bank 5
        
        ; Filename is in low RAM - read from LOW_RAM_BUFFER
        lda c128_setnam_ptr_lo
        sta $FB
        clc
        lda c128_setnam_ptr_hi
        adc #>LOW_RAM_BUFFER            ; Add $A0
        sta $FC
        
        ldy #0
_cf_low_loop:
        cpy c128_fl_len
        beq _cf_done
        lda ($FB),y
        sta c128_fl_buf,y
        iny
        cpy #17
        bcc _cf_low_loop
        bra _cf_done
        
_cf_bank5:
        ; Set up 32-bit pointer to read from Bank 5 (guest RAM)
        ; C128_MEM_PTR is at $F0-$F3
        lda c128_setnam_ptr_lo
        sta C128_MEM_PTR
        lda c128_setnam_ptr_hi
        sta C128_MEM_PTR+1
        lda #BANK_RAM                   ; Bank 5 = guest RAM
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        
        ; Copy bytes using 32-bit indirect addressing
        ldy #0
_cf_loop:
        cpy c128_fl_len
        beq _cf_done
        
        ; Read from [C128_MEM_PTR],y - this reads from Bank 5 directly
        ldz #0
        tya
        taz                             ; Z = Y (offset)
        lda [C128_MEM_PTR],z
        
        ; Store in buffer
        sta c128_fl_buf,y
        
        iny
        bne _cf_loop                    ; Always branch (Y < 17)
        
_cf_done:
        ; Null terminate
        lda #0
        sta c128_fl_buf,y
        clc
        rts
        
_cf_error:
        sec
        rts


; Old print routines removed - now using C128Host_* routines from c128_host.asm


; ============================================================
; C128Hook_DoHostLoad - Load file using host MEGA65 KERNAL
;
; The MEGA65 KERNAL LOAD with SA=0 reads the 2-byte header but then
; loads to the address we specify in X/Y (ignoring header address).
; The header bytes are NOT placed in our buffer.
;
; For SA=0 (load to TXTTAB): We just use LOAD directly, data goes 
; straight to our buffer, then DMA to TXTTAB.
;
; For SA!=0 (load to file's address): We need to know the header
; address. We first OPEN/CHRIN to read the 2-byte header, close,
; then use LOAD to get the data.
; ============================================================

; Storage for the captured PRG header (load address)
c128_prg_header_lo: .byte 0
c128_prg_header_hi: .byte 0

; Saved video mode across host file operations (for error recovery)
c128_prev_video_mode: .byte 0

C128Hook_DoHostLoad:
        ; Mark file operation in progress (prevents video mode switching)
        lda #1
        sta c128_file_op_active

        ; Remember current C128 video mode (for error recovery)
        lda c128_video_mode
        sta c128_prev_video_mode
        
        ; Set up host KERNAL for file operations
        lda #$00
        ldx #$00
        jsr SETBNK
        
        ; Check if we need the header address (SA != 0)
        lda c128_setlfs_sa
        beq _dhl_do_load            ; SA=0, skip header reading
        
        ; --------------------------------------------------------
        ; SA != 0: Need to read header address first
        ; --------------------------------------------------------
        ; Set filename for OPEN
        ldx #<c128_fl_buf
        ldy #>c128_fl_buf
        lda c128_fl_len
        jsr SETNAM
        
        ; SETLFS: lfn=15, device, sa=15 (command channel style read)
        lda #$0F                        ; Logical file number 15
        ldx c128_setlfs_dev
        bne +
        ldx #$08                        ; Default to device 8
+       ldy #$00                        ; SA=0 for sequential
        jsr SETLFS
        
        jsr OPEN
        bcs _dhl_header_error
        
        ; CHKIN to file 15
        ldx #$0F
        jsr CHKIN
        bcs _dhl_header_chkin_error
        
        ; Read header bytes
        jsr CHRIN
        sta c128_prg_header_lo
        jsr CHRIN
        sta c128_prg_header_hi
        
        ; Close
        jsr CLRCHN
        lda #$0F
        jsr CLOSE
        jsr C128Hook_UnlockVIC            ; Just re-unlock VIC, don't change mode
        jmp _dhl_do_load_after_header

_dhl_header_error:
_dhl_header_chkin_error:
        jsr CLRCHN
        lda #$0F
        jsr CLOSE
        jsr C128Hook_UnlockVIC            ; Just re-unlock VIC, don't change mode
        jmp _dhl_error_set

_dhl_do_load_after_header:
_dhl_do_load:
        ; --------------------------------------------------------
        ; Load file directly to guest RAM (bank 5)
        ; Skip the staging buffer to handle large files
        ; --------------------------------------------------------
        
        ; First we need to know the destination address
        ; SA=0: use TXTTAB, SA!=0: use file header
        lda c128_setlfs_sa
        bne _dhl_use_header_addr
        
        ; SA=0: destination = TXTTAB
        lda LOW_RAM_BUFFER + ZP_TXTTAB_LO
        sta c128_dir_dest_lo
        lda LOW_RAM_BUFFER + ZP_TXTTAB_HI
        sta c128_dir_dest_hi
        jmp _dhl_setup_load
        
_dhl_use_header_addr:
        ; SA!=0: use the header address we captured earlier
        lda c128_prg_header_lo
        sta c128_dir_dest_lo
        lda c128_prg_header_hi
        sta c128_dir_dest_hi
        
_dhl_setup_load:
        ; Set filename
        ldx #<c128_fl_buf
        ldy #>c128_fl_buf
        lda c128_fl_len
        jsr SETNAM
        
        ; SETLFS: lfn=1, device, sa=0 (we provide our own address)
        lda #$01                        ; Logical file number
        ldx c128_setlfs_dev
        bne +
        ldx #$08                        ; Default to device 8
+       ldy #$00                        ; SA=0: use X/Y address, not file header
        jsr SETLFS
        
        ; Set destination bank to 5 (guest RAM)
        lda #C128_BANK_RAM                ; Bank 5 for LOAD destination
        ldx #$00                        ; Bank 0 for filename
        jsr SETBNK
        
        ; Load directly to guest RAM in bank 5
        lda #$00                        ; 0 = LOAD (not verify)
        ldx c128_dir_dest_lo
        ldy c128_dir_dest_hi
        jsr LOAD
        
        ; Save end address from LOAD (in X/Y)
        stx c128_fl_end_lo
        sty c128_fl_end_hi
        php                             ; Save carry flag (error status from LOAD)
        
        ; Close the logical file to clean up KERNAL state
        lda #$01                        ; LFN we used
        jsr CLOSE
        jsr CLRCHN                      ; Clear channels
        
        ; Reset SETBNK back to bank 0 for subsequent operations
        lda #$00                        ; Bank 0 for both
        ldx #$00
        jsr SETBNK
        
        jsr C128Hook_UnlockVIC            ; Just re-unlock VIC, don't change mode
        
        plp                             ; Restore carry flag from LOAD
        bcc _dhl_load_ok
        
        ; Load failed
        jmp _dhl_error_set

_dhl_load_ok:
        ; Print "LOADING" message
        lda #<C128Host_Msg_Loading
        ldx #>C128Host_Msg_Loading
        jsr C128Host_PrintString
        
        ; Calculate loaded length
        ; We loaded directly to destination, so length = end - dest
        lda c128_fl_end_lo
        sec
        sbc c128_dir_dest_lo
        sta c128_dir_len_lo
        lda c128_fl_end_hi
        sbc c128_dir_dest_hi
        sta c128_dir_len_hi
        
        ; Need at least 1 byte of data
        lda c128_dir_len_hi
        bne _dhl_has_data
        lda c128_dir_len_lo
        beq _dhl_error_set

_dhl_has_data:
        ; We loaded directly to bank 5, so we need to sync LOW_RAM_BUFFER
        ; for addresses $0000-$0FFF if the load touched that area
        jsr C128Hook_SyncLowRAMFromBank5
        
        ; Clear KERNAL status
        lda #$00
        sta LOW_RAM_BUFFER + $90
        
        ; Set end address in X/Y registers
        clc
        lda c128_dir_dest_lo
        adc c128_dir_len_lo
        sta c128_x
        lda c128_dir_dest_hi
        adc c128_dir_len_hi
        sta c128_y
        
        ; Clear carry = success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        jmp _dhl_set_pc

_dhl_error_set:
        ; Clean up KERNAL state before error handling
        lda #$01                        ; LFN we may have used
        jsr CLOSE
        jsr CLRCHN
        lda #$00
        ldx #$00
        jsr SETBNK                      ; Reset to bank 0
        jsr C128Hook_UnlockVIC
        
        ; Print appropriate error message based on caller
        lda c128_monitor_load
        bne _dhl_error_monitor
        jmp _dhl_error_set_status

_dhl_error_monitor:
       ; Since we are bypassing the kernal load routine,
       ; if its the monitor thats failing to load, we 
       ; need to print its error message
        lda #<C128Host_Msg_Monitor_FileNotFound
        ldx #>C128Host_Msg_Monitor_FileNotFound
        jsr C128Host_PrintString

_dhl_error_set_status:
        lda c128_p
        ora #P_C
        sta c128_p
        lda #$04                        ; FILE NOT FOUND
        sta c128_a
        lda #$42
        sta LOW_RAM_BUFFER + $90        ; set STATUS byte

_dhl_set_pc:
        ; Clear file operation flag - allow video mode switching again
        lda #0
        sta c128_file_op_active

        ; If LOAD failed, force text mode to recover host video state
        jsr C128Hook_PostFileOpVideoFix
        
        ; Check if this was a monitor load
        lda c128_monitor_load
        beq _dhl_basic_return
        
        ; Monitor load - return via RTS to caller
        lda #0
        sta c128_monitor_load             ; Clear the flag
        jsr C128Hook_RTS_Guest
        rts
        
_dhl_basic_return:
        ; BASIC load - skip past the JSR $FFD5
        lda #<C128Hook_ROM_AFTER_KERNAL_CALL
        sta c128_pc_lo
        lda #>C128Hook_ROM_AFTER_KERNAL_CALL
        sta c128_pc_hi
        
        lda #1
        sta c128_hook_pc_changed    ; Signal to threaded interpreter
        rts

C128Host_Msg_Monitor_FileNotFound:
        .byte $0d
        .text "i/o error #4"
        .byte $00

; ============================================================
; Hook: SAVE - Handle file saving
; Called when guest PC == $FFD8 (KERNAL SAVE entry)
;
; On entry to KERNAL SAVE:
;   A = zero page address containing start address pointer
;   X = end address low byte
;   Y = end address high byte
;
; The start address is read from the ZP location pointed to by A.
; End address is the first byte NOT to save (exclusive).
; ============================================================
C128Hook_OnSAVE:
        ; Check device number - only intercept device 8 (disk)
        lda c128_setlfs_dev
        cmp #$08
        beq _save_check_setnam
        ; Not device 8 - let ROM handle (tape, or no device specified)
        rts
        
_save_check_setnam:
        ; Check if we have valid SETNAM data
        lda c128_setnam_valid
        bne _save_have_setnam
        
        ; No SETNAM called - check if monitor set filename directly
        ; Monitor stores: $AB = length, $AF/$B0 = pointer (pointing to $025D)
        lda LOW_RAM_BUFFER + $AB        ; Filename length
        beq _save_no_setnam             ; No filename, let ROM handle
        cmp #17
        bcs _save_no_setnam             ; Too long, let ROM handle
        
        ; Use KERNAL variables - copy filename from LOW_RAM_BUFFER
        ; since monitor stores filename at $025D which is in low RAM
        sta c128_fl_len
        
        ; Set up pointer: LOW_RAM_BUFFER + $AF/$B0 value
        lda LOW_RAM_BUFFER + $AF        ; Pointer lo ($5D)
        sta $FB
        clc
        lda LOW_RAM_BUFFER + $B0        ; Pointer hi ($02)
        adc #>LOW_RAM_BUFFER            ; Add $A0 -> $A2
        sta $FC
        
        ; Copy filename bytes
        ldy #0
_save_copy_fn:
        cpy c128_fl_len
        beq _save_copy_done
        lda ($FB),y
        sta c128_fl_buf,y
        iny
        cpy #17
        bcc _save_copy_fn
_save_copy_done:
        lda #0
        sta c128_fl_buf,y                 ; Null terminate
        bra _save_do_it
        
_save_have_setnam:
        ; Clear the valid flag
        lda #0
        sta c128_setnam_valid
        
        ; Copy filename from guest RAM to buffer (uses bank 5)
        jsr C128Hook_CopyFilename
        bcs _save_error
        
_save_do_it:
        ; Get end address from X/Y registers
        lda c128_x
        sta c128_save_end_lo
        lda c128_y
        sta c128_save_end_hi
        
        ; Get start address - A register contains ZP address
        ; The ZP location contains a 2-byte pointer to start of data
        ; ZP is in LOW_RAM_BUFFER, not Bank 5!
        lda c128_a                        ; ZP address (e.g., $2B for TXTTAB)
        tax
        
        ; Read start address from LOW_RAM_BUFFER (ZP is in low RAM)
        jsr lrb_read_x  ; Start lo
        sta c128_save_start_lo
        inx
        jsr lrb_read_x  ; Start hi
        sta c128_save_start_hi
        
        ; Perform the save
        jsr C128Hook_DoHostSave
        rts

_save_no_setnam:
        ; No filename set - let ROM handle error
        rts

_save_error:
        ; Filename copy error
        jsr C128Hook_SaveSetError
        rts


; ============================================================
; C128Hook_DoHostSave - Save file using host MEGA65 KERNAL
; ============================================================
C128Hook_DoHostSave:
        ; Mark file operation in progress (prevents video mode switching)
        lda #1
        sta c128_file_op_active

        ; Remember current C128 video mode (for error recovery)
        lda c128_video_mode
        sta c128_prev_video_mode
        
        ; Calculate data length
        lda c128_save_end_lo
        sec
        sbc c128_save_start_lo
        sta c128_dir_len_lo
        lda c128_save_end_hi
        sbc c128_save_start_hi
        sta c128_dir_len_hi
        
        ; Check for zero or negative length
        lda c128_dir_len_hi
        bmi _dhs_error                  ; Negative = error
        ora c128_dir_len_lo
        beq _dhs_error                  ; Zero length = error
        
        ; DMA copy from guest RAM to staging buffer
        ; First 2 bytes = load address header
        lda c128_save_start_lo
        sta C128_DIR_BUF
        lda c128_save_start_hi
        sta C128_DIR_BUF+1
        
        ; Copy program data from guest RAM (bank 5) to staging buffer+2
        jsr C128Hook_DMACopyGuestToHost


        ; Print "SAVING "
        lda #<C128Host_Msg_Saving
        ldx #>C128Host_Msg_Saving
        jsr C128Host_PrintString
        
        ; Print the filename
        ldy #0
    _print_name:
        cpy c128_fl_len
        beq _print_done
        lda c128_fl_buf,y
        phy
        jsr C128Host_PutChar          ; (alias for C128Host_PrintCharSync)
        ply
        iny
        bne _print_name
    _print_done:
        
        ; Set up host KERNAL for save
        lda #$00
        ldx #$00
        jsr SETBNK
        
        ; Set filename
        ldx #<c128_fl_buf
        ldy #>c128_fl_buf
        lda c128_fl_len
        jsr SETNAM
        
        ; Set device - use OPEN/CHKOUT/CHROUT method
        lda #$01                        ; Logical file number
        ldx c128_setlfs_dev
        bne +
        ldx #$08                        ; Default to device 8
+       ldy #$01                        ; SA=1 for save with relocate address
        jsr SETLFS
        
        ; OPEN the file
        jsr OPEN
        bcs _dhs_open_error
        
        ; Set output channel to file
        ldx #$01                        ; Logical file number
        jsr CHKOUT
        bcs _dhs_chkout_error
        
        ; Calculate total length including 2-byte header
        clc
        lda c128_dir_len_lo
        adc #2
        sta _save_total_lo
        lda c128_dir_len_hi
        adc #0
        sta _save_total_hi
        
        ; Output all bytes using CHROUT
        lda #<C128_DIR_BUF
        sta $FB
        lda #>C128_DIR_BUF
        sta $FC
        
        ldy #0
_save_loop:
        ; Check if done
        lda _save_total_lo
        ora _save_total_hi
        beq _save_loop_done
        
        ; Output one byte
        lda ($FB),y
        jsr CHROUT
        
        ; Advance pointer
        iny
        bne +
        inc $FC
+
        ; Decrement count
        lda _save_total_lo
        bne +
        dec _save_total_hi
+       dec _save_total_lo
        
        bra _save_loop
        
_save_loop_done:
        ; Close file
        jsr CLRCHN
        lda #$01
        jsr CLOSE
        jsr C128Hook_UnlockVIC            ; Just re-unlock VIC, don't change mode
        
        jmp _dhs_ok

_dhs_open_error:
_dhs_chkout_error:
        jsr CLRCHN
        lda #$01
        jsr CLOSE
        jsr C128Hook_UnlockVIC            ; Just re-unlock VIC, don't change mode
        jmp _dhs_error

_dhs_ok:
        ; Clear KERNAL status in guest
        lda #$00
        sta LOW_RAM_BUFFER + $90
        
        ; Clear carry = success in guest P register
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        
        ; Set A to 0 (no error)
        lda #$00
        sta c128_a
        
        ; Clear file operation flag - allow video mode switching again
        lda #0
        sta c128_file_op_active

        ; If SAVE failed, force text mode to recover host video state
        jsr C128Hook_PostFileOpVideoFix
        
        ; We intercepted at $FFD8 (KERNAL SAVE entry)
        ; Pop the return address from guest stack and set PC to return
        jsr C128Hook_RTS_Guest
        rts

_dhs_error:
        ; Clear file operation flag even on error
        lda #0
        sta c128_file_op_active

        ; If SAVE failed, force text mode to recover host video state
        jsr C128Hook_PostFileOpVideoFix
        jsr C128Hook_SaveSetError
        rts

_save_total_lo: .byte 0
_save_total_hi: .byte 0

; ============================================================
; C128Hook_SaveSetError - Set error status for failed SAVE
; ============================================================
C128Hook_SaveSetError:
        lda c128_p
        ora #P_C
        sta c128_p
        lda #$05                        ; DEVICE NOT PRESENT or similar
        sta c128_a
        
        ; Pop return address and return from SAVE
        jsr C128Hook_RTS_Guest
        rts


; ============================================================
; C128Hook_DMACopyGuestToHost - Copy from guest RAM to staging buffer
; Source: Bank 5 at p4_save_start
; Dest: C128_DIR_BUF+2 (after load address header)
; Length: p4_dir_len
; ============================================================
C128Hook_DMACopyGuestToHost:
        lda c128_dir_len_lo
        sta _dma_g2h_len_lo
        lda c128_dir_len_hi
        sta _dma_g2h_len_hi
        lda c128_save_start_lo
        sta _dma_g2h_src_lo
        lda c128_save_start_hi
        sta _dma_g2h_src_hi

        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00                       ; copy
_dma_g2h_len_lo:
        .byte $00
_dma_g2h_len_hi:
        .byte $00
_dma_g2h_src_lo:
        .byte $00
_dma_g2h_src_hi:
        .byte $00
        .byte BANK_RAM                  ; src bank 5 (guest RAM)
        .byte <(C128_DIR_BUF+2)           ; dest lo
        .byte >(C128_DIR_BUF+2)           ; dest hi
        .byte $00                       ; dest bank 0 (host RAM)
        .byte $00
        .word $0000
        rts


; ============================================================
; C128Hook_LoadDirectory - Load "$" directory as BASIC program
; ============================================================
C128Hook_LoadDirectory:
        ; Set up host KERNAL
        lda #$00
        ldx #$00
        jsr SETBNK
        
        ; SETNAM("$", len=1)
        ldx #<c128_dir_name
        ldy #>c128_dir_name
        lda #$01
        jsr SETNAM
        
        ; SETLFS(lfn=1, device=8, sa=0)
        lda #$01
        ldx #$08
        ldy #$00
        jsr SETLFS
        
        ; LOAD to staging buffer
        lda #$00
        ldx #<C128_DIR_BUF
        ldy #>C128_DIR_BUF
        jsr LOAD
        bcc _ld_ok
        
        ; Load failed - let ROM handle the error
        rts

_ld_ok:
        ; Store end address
        stx _ld_end_lo
        sty _ld_end_hi
        
        ; Calculate length
        lda _ld_end_lo
        sec
        sbc #<C128_DIR_BUF
        sta c128_dir_len_lo
        lda _ld_end_hi
        sbc #>C128_DIR_BUF
        sta c128_dir_len_hi
        
        ; Check for zero length
        lda c128_dir_len_lo
        ora c128_dir_len_hi
        beq _ld_bypass
        
        ; Destination = TXTTAB
        jsr C128Hook_SetDestFromTXTTAB
        
        ; Clear first byte
        jsr C128Hook_ClearGuestDest1
        
        ; DMA copy
        jsr C128Hook_DMACopyDirToGuest

_ld_bypass:
        ; Clear KERNAL status
        lda #$00
        sta c128_addr_hi
        lda #$90
        sta c128_addr_lo
        jsr C128_WriteA0
        
        ; Set end address in X/Y
        clc
        lda c128_dir_dest_lo
        adc c128_dir_len_lo
        sta c128_x
        lda c128_dir_dest_hi
        adc c128_dir_len_hi
        sta c128_y
        
        ; Clear carry
        lda c128_p
        and #((~P_C) & $ff)
        sta c128_p
        
        ; Skip JSR $FFD5
        lda #<C128Hook_ROM_AFTER_KERNAL_CALL
        sta c128_pc_lo
        lda #>C128Hook_ROM_AFTER_KERNAL_CALL
        sta c128_pc_hi
        lda #1
        sta c128_hook_pc_changed    ; Signal to threaded interpreter
        rts

_ld_end_lo: .byte 0
_ld_end_hi: .byte 0


; ============================================================
; DMA copy file data to guest RAM
; Source: C128_DIR_BUF (staging buffer - data only, no header)
; Dest: p4_dir_dest in Bank 5 (guest RAM)
; Length: p4_dir_len
; ============================================================
C128Hook_DMACopyFileToGuest:
        lda c128_dir_len_lo
        sta _dma_fl_len_lo
        lda c128_dir_len_hi
        sta _dma_fl_len_hi
        lda c128_dir_dest_lo
        sta _dma_fl_dst_lo
        lda c128_dir_dest_hi
        sta _dma_fl_dst_hi

        ; --------------------------------------------------------
        ; FIRST: Sync LOW_RAM_BUFFER -> Bank 5 so any guest writes
        ; to low RAM are preserved before we potentially overwrite
        ; --------------------------------------------------------
        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00                       ; copy
        .byte $00                       ; len lo = $1000 (4KB)
        .byte $10                       ; len hi
        .byte <LOW_RAM_BUFFER           ; src lo
        .byte >LOW_RAM_BUFFER           ; src hi
        .byte $00                       ; src bank 0
        .byte $00                       ; dest lo
        .byte $00                       ; dest hi  
        .byte C128_BANK_RAM               ; dest bank 5
        .byte $00
        .word $0000

        ; --------------------------------------------------------
        ; Now DMA the file data to guest RAM (bank 5)
        ; --------------------------------------------------------
        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00                       ; copy
_dma_fl_len_lo:
        .byte $00
_dma_fl_len_hi:
        .byte $00
        .byte <C128_DIR_BUF               ; src lo (buffer contains data only)
        .byte >C128_DIR_BUF               ; src hi
        .byte $00                       ; src bank 0
_dma_fl_dst_lo:
        .byte $00
_dma_fl_dst_hi:
        .byte $00
        .byte C128_BANK_RAM               ; dest bank 5
        .byte $00
        .word $0000

        ; --------------------------------------------------------
        ; Check if load destination overlaps LOW_RAM_BUFFER range ($0000-$0FFF)
        ; If dest_hi >= $10, no overlap - we're done
        ; --------------------------------------------------------
        lda c128_dir_dest_hi
        cmp #$10
        bcs _dma_fl_done                ; dest >= $1000, no overlap with low RAM

        ; --------------------------------------------------------
        ; Calculate end of loaded data, clamped to $1000
        ; --------------------------------------------------------
        clc
        lda c128_dir_dest_lo
        adc c128_dir_len_lo
        sta _sync_end_lo
        lda c128_dir_dest_hi
        adc c128_dir_len_hi
        sta _sync_end_hi
        
        ; Clamp end to $1000 if it exceeds
        lda _sync_end_hi
        cmp #$10
        bcc _sync_calc_range            ; end < $1000, use as-is
        lda #$00
        sta _sync_end_lo
        lda #$10
        sta _sync_end_hi

_sync_calc_range:
        ; Calculate start of sync: use actual dest address
        ; We MUST sync stack page ($0100-$01FF) because depackers run from there!
        ; The original code skipped $0000-$01FF but this breaks depackers
        lda c128_dir_dest_lo
        sta _sync_start_lo
        lda c128_dir_dest_hi
        sta _sync_start_hi

_sync_check_range:
        ; If start >= end, nothing to sync
        lda _sync_start_hi
        cmp _sync_end_hi
        bcc _do_sync                    ; start_hi < end_hi, sync needed
        bne _dma_fl_done                ; start_hi > end_hi, nothing to sync
        lda _sync_start_lo
        cmp _sync_end_lo
        bcs _dma_fl_done                ; start_lo >= end_lo, nothing to sync

_do_sync:
        ; Calculate length: end - start
        sec
        lda _sync_end_lo
        sbc _sync_start_lo
        sta _dma_sync_len_lo
        lda _sync_end_hi
        sbc _sync_start_hi
        sta _dma_sync_len_hi
        
        ; Set up source address in bank 5
        lda _sync_start_lo
        sta _dma_sync_src_lo
        lda _sync_start_hi
        sta _dma_sync_src_hi
        
        ; Set up dest address in LOW_RAM_BUFFER
        clc
        lda _sync_start_lo
        sta _dma_sync_dst_lo
        lda _sync_start_hi
        adc #>LOW_RAM_BUFFER
        sta _dma_sync_dst_hi
        
        ; DMA sync: Bank 5 -> LOW_RAM_BUFFER (only the loaded portion)
        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00                       ; copy
_dma_sync_len_lo:
        .byte $00
_dma_sync_len_hi:
        .byte $00
_dma_sync_src_lo:
        .byte $00
_dma_sync_src_hi:
        .byte $00
        .byte C128_BANK_RAM               ; src bank 5
_dma_sync_dst_lo:
        .byte $00
_dma_sync_dst_hi:
        .byte $00
        .byte $00                       ; dest bank 0
        .byte $00
        .word $0000

_dma_fl_done:
        rts

_sync_start_lo: .byte 0
_sync_start_hi: .byte 0
_sync_end_lo:   .byte 0
_sync_end_hi:   .byte 0

; ============================================================
; Helper routines (from original)
; ============================================================

C128Hook_SetDestFromTXTTAB:
        lda #$00
        sta c128_addr_hi
        lda #ZP_TXTTAB_LO
        sta c128_addr_lo
        jsr C128_Read
        sta c128_dir_dest_lo
        lda #ZP_TXTTAB_HI
        sta c128_addr_lo
        jsr C128_Read
        sta c128_dir_dest_hi
        rts

C128Hook_DMACopyDirToGuest:
        lda c128_dir_len_lo
        sta _dma_count_lo
        lda c128_dir_len_hi
        sta _dma_count_hi
        lda c128_dir_dest_lo
        sta _dma_dst_lo
        lda c128_dir_dest_hi
        sta _dma_dst_hi

        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00
_dma_count_lo:
        .byte $00
_dma_count_hi:
        .byte $00
        .byte <C128_DIR_BUF
        .byte >C128_DIR_BUF
        .byte $00
_dma_dst_lo:
        .byte $00
_dma_dst_hi:
        .byte $00
        .byte C128_BANK_RAM
        .byte $00
        .word $0000
        rts

C128Hook_ClearGuestDest1:
        lda c128_dir_dest_lo
        sta _fill_dst_lo
        lda c128_dir_dest_hi
        sta _fill_dst_hi

        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $03
        .word $0001
        .word $0000
        .byte $00
_fill_dst_lo:
        .byte $00
_fill_dst_hi:
        .byte $00
        .byte C128_BANK_RAM
        .byte $00
        .word $0000
        rts

; --- Memory write helpers ---
C128_WriteA:
        sta c128_data
        jsr C128_Write
        rts

C128_WriteA0:
        lda #$00
        sta c128_data
        jsr C128_Write
        rts


; ============================================================
; DIRECTORY support - NEW implementation
; ============================================================

; Called when PC = $C8BC (DIRECTORY keyword entry)
C128Hook_OnDIRECTORY:
        ; ------------------------------------------------------------
        ; DIRECTORY hook (print-only, do not alter BASIC program memory)
        ;
        ; We mimic the ROM strategy: OPEN a directory channel and read
        ; bytes via CHRIN, sending them to the guest console.
        ;
        ; We try secondary address $60 first (what the C128 KERNAL uses),
        ; and fall back to $00 if needed on the host filesystem.
        ; ------------------------------------------------------------

        ; Reset any stale bitmap mode flags to prevent graphics glitch
        lda #0
        sta c128_gfx_dirty
        
        ; Ensure host KERNAL I/O uses bank 0
        lda #$00
        ldx #$00
        jsr SETBNK

        ; Name = "$"
        ldx #<c128_dir_name
        ldy #>c128_dir_name
        lda #$01
        jsr SETNAM

        ; Try OPEN with SA=$60 (C128 style)
        lda #$02                        ; logical file #
        ldx #$08                        ; device 8
        ldy #$60                        ; secondary for directory
        jsr SETLFS
        jsr C128_OPEN

        jsr C128_READST
        beq _dir_open_ok

        ; Fall back: try OPEN with SA=$00
        lda #$02
        jsr C128_CLOSE

        lda #$02
        ldx #$08
        ldy #$00
        jsr SETLFS
        jsr C128_OPEN

        jsr C128_READST
        bne _dir_open_fail

_dir_open_ok:
        ; Make it the current input channel
        ldx #$02
        jsr C128_CHKIN

        ; ---- Directory stream is a BASIC "program" ----
        ; First two bytes are the LOAD address ($0401 typically). Discard them.
        jsr C128_CHRIN
        jsr C128_CHRIN

_dir_line_loop:
        ; If EOF flagged, bail (safety)
        jsr C128_READST
        and #$40                        ; EOF?
        bne _dir_done

        ; Read next-line pointer (lo/hi)
        jsr C128_CHRIN
        sta _dir_nextptr
        jsr C128_CHRIN
        sta _dir_nextptr+1

        ; 0000 means end-of-program
        lda _dir_nextptr
        ora _dir_nextptr+1
        beq _dir_done

        ; Read "line number" (lo/hi) which is blocks used/free
        jsr C128_CHRIN
        sta _dir_blocks
        jsr C128_CHRIN
        sta _dir_blocks+1

        jsr _dir_print_blocks_u16_left4_sp   ; prints number + pads + trailing space


        ; Print text bytes until $00 (end of line)
_dir_text_loop:
        jsr C128_CHRIN
        beq _dir_eol

        ; D81/1581 padding uses $A0 - treat it like space
        cmp #$A0
        bne _dir_put
        lda #$20
_dir_put:
        jsr C128Host_PutChar
        jmp _dir_text_loop

_dir_eol:
        ; End of "line" -> newline
        lda #$0D
        jsr C128Host_PutChar
        jmp _dir_line_loop


; ------------------------------------------------------------
; Helpers / locals (place these near your hook code)
; ------------------------------------------------------------

; Storage (not ZP, avoids collisions)
_dir_nextptr:   .word 0
_dir_blocks:    .word 0

; ------------------------------------------------------------
; Print _dir_blocks as decimal, LEFT-justified in 4 columns,
; then print one trailing space (total width = 5).
; Examples:
;   0    -> "0   "
;   39   -> "39  "
;   65   -> "65  "
;   2991 -> "2991"
; then it always adds one extra space after the 4-col field.
; ------------------------------------------------------------
_dir_print_blocks_u16_left4_sp:
        ; Copy blocks to work
        lda _dir_blocks
        sta _dir_work
        lda _dir_blocks+1
        sta _dir_work+1

        lda #0
        sta _dir_ndig

        ; Print digits (no leading spaces). Use repeated subtraction like before.
        ; 1000s
        lda #<1000
        sta _dir_div
        lda #>1000
        sta _dir_div+1
        jsr _dir_emit_digit_nolead

        ; 100s
        lda #<100
        sta _dir_div
        lda #>100
        sta _dir_div+1
        jsr _dir_emit_digit_nolead

        ; 10s
        lda #<10
        sta _dir_div
        lda #>10
        sta _dir_div+1
        jsr _dir_emit_digit_nolead

        ; 1s (always printed)
        lda #<1
        sta _dir_div
        lda #>1
        sta _dir_div+1
        jsr _dir_emit_last_digit_counted

        ; Pad to 4 columns (left-justified)
        lda _dir_ndig
        cmp #1
        bcs _dir_pad_done
_dir_pad_loop:
        lda #$20
        jsr C128Host_PutChar
        inc _dir_ndig
        lda _dir_ndig
        cmp #4
        bcc _dir_pad_loop
_dir_pad_done:
        ; One trailing separator space
        lda #$20
        jmp C128Host_PutChar


_dir_ndig:      .byte 0

; Like your existing digit subtractor, but:
; - does NOT output anything for leading zeros
; - increments _dir_ndig when it prints a digit
_dir_emit_digit_nolead:
        ldy #0
_dir_sub_loop_nl:
        lda _dir_work+1
        cmp _dir_div+1
        bcc _dir_sub_done_nl
        bne _dir_can_sub_nl
        lda _dir_work
        cmp _dir_div
        bcc _dir_sub_done_nl
_dir_can_sub_nl:
        lda _dir_work
        sec
        sbc _dir_div
        sta _dir_work
        lda _dir_work+1
        sbc _dir_div+1
        sta _dir_work+1
        iny
        cpy #10
        bne _dir_sub_loop_nl
_dir_sub_done_nl:
        ; If digit is zero and no digits printed yet, output nothing
        tya
        bne _dir_print_digit_nl
        lda _dir_ndig
        beq _dir_emit_digit_nl_rts
        lda #'0'
        jsr C128Host_PutChar
        inc _dir_ndig
        rts
_dir_print_digit_nl:
        tya
        clc
        adc #'0'
        jsr C128Host_PutChar
        inc _dir_ndig
_dir_emit_digit_nl_rts:
        rts

; Ones place: always print, and count it
_dir_emit_last_digit_counted:
        ldy #0
_dir_sub_loop_1c:
        lda _dir_work+1
        cmp _dir_div+1
        bcc _dir_sub_done_1c
        bne _dir_can_sub_1c
        lda _dir_work
        cmp _dir_div
        bcc _dir_sub_done_1c
_dir_can_sub_1c:
        lda _dir_work
        sec
        sbc _dir_div
        sta _dir_work
        lda _dir_work+1
        sbc _dir_div+1
        sta _dir_work+1
        iny
        cpy #10
        bne _dir_sub_loop_1c
_dir_sub_done_1c:
        tya
        clc
        adc #'0'
        jsr C128Host_PutChar
        inc _dir_ndig
        rts

_dir_work:
        .byte $00, $00
_dir_div:
        .byte $00, $00

_dir_done:
        jsr C128_CLRCHN
        lda #$02
        jsr C128_CLOSE

        ; Re-unlock VIC-IV after file operations (don't reset video mode)
        jsr C128Hook_UnlockVIC
        
        ; Return to BASIC via RTS
        jsr C128Hook_RTS_Guest
        rts

_tmplinectr:
        .byte $00

_dir_open_fail:
        ; Couldn't open directory channel. Just return to BASIC.
        jsr C128Hook_UnlockVIC
        jsr C128Hook_RTS_Guest
        rts


 .byte 0
c128dir_end_hi: .byte 0
c128dir_end_lo: .byte 0
c128dir_len_lo: .byte 0
c128dir_len_hi: .byte 0


; Zero page pointer for directory parsing
DIR_PTR         = $FB           ; 2 bytes (shared with GC_PTR when not printing)

; ============================================================
; C128Hook_PrintDirectory - Parse and print CBM directory format
; 
; CBM directory format in memory:
;   First line: load address (2 bytes), link (2 bytes), line# (2 bytes), 
;               then reversed disk name, etc.
;   Each entry: link (2 bytes), line# (2 bytes = blocks), 
;               filename in quotes, type
;   Last line: "BLOCKS FREE"
; ============================================================
C128Hook_PrintDirectory:
        ; Set up pointer to start of directory data
        lda #<C128_DIR_BUF
        sta DIR_PTR
        lda #>C128_DIR_BUF
        sta DIR_PTR+1
        
        ; Skip load address (2 bytes)
        jsr _dir_skip2
        
_dir_next_line:
        ; Check if we've reached end of data
        lda DIR_PTR+1
        cmp c128dir_end_hi
        bcc _dir_process_line
        bne _dir_done
        lda DIR_PTR
        cmp c128dir_end_lo
        bcs _dir_done
        
_dir_process_line:
        ; Read link pointer (2 bytes) - if zero, end of listing
        jsr _dir_read_byte
        sta _dir_link_lo
        jsr _dir_read_byte
        sta _dir_link_hi
        
        ; Check for end (link = 0)
        lda _dir_link_lo
        ora _dir_link_hi
        beq _dir_done
        
        ; Read line number (block count) - 2 bytes little-endian
        jsr _dir_read_byte
        sta _dir_blocks_lo
        jsr _dir_read_byte
        sta _dir_blocks_hi
        
        ; Print block count (as decimal number)
        jsr _dir_print_blocks
        
        ; Print space
        lda #' '
        jsr C128Host_PutChar
        
        ; Print rest of line until null
_dir_print_chars:
        jsr _dir_read_byte
        beq _dir_line_done              ; Null = end of line
        jsr C128Host_PutChar
        jmp _dir_print_chars
        
_dir_line_done:
        ; Print newline
        lda #$0d
        jsr C128Host_PutChar
        
        ; Next line
        jmp _dir_next_line

_dir_done:
        rts

; Skip 2 bytes
_dir_skip2:
        jsr _dir_read_byte
        jsr _dir_read_byte
        rts

; Read byte from directory buffer and advance pointer
_dir_read_byte:
        ldy #0
        lda (DIR_PTR),y
        inc DIR_PTR
        bne +
        inc DIR_PTR+1
+       rts

; Print block count (16-bit number in _dir_blocks)
_dir_print_blocks:
        ; Convert to decimal and print
        ; Simple approach: print as 5-digit number with leading space suppression
        lda _dir_blocks_hi
        ldx _dir_blocks_lo
        
        ; Use simple decimal conversion
        ldy #0                          ; Leading zero flag
        
        ; 10000s place
        lda #0
        sta _dir_digit
_div_10000:
        lda _dir_blocks_lo
        sec
        sbc #<10000
        tax
        lda _dir_blocks_hi
        sbc #>10000
        bcc _print_10000
        sta _dir_blocks_hi
        stx _dir_blocks_lo
        inc _dir_digit
        jmp _div_10000
_print_10000:
        lda _dir_digit
        bne _print_10000_digit
        cpy #0
        beq _skip_10000
_print_10000_digit:
        clc
        adc #'0'
        jsr C128Host_PutChar
        ldy #1                          ; No longer leading
_skip_10000:

        ; 1000s place
        lda #0
        sta _dir_digit
_div_1000:
        lda _dir_blocks_lo
        sec
        sbc #<1000
        tax
        lda _dir_blocks_hi
        sbc #>1000
        bcc _print_1000
        sta _dir_blocks_hi
        stx _dir_blocks_lo
        inc _dir_digit
        jmp _div_1000
_print_1000:
        lda _dir_digit
        bne _print_1000_digit
        cpy #0
        beq _skip_1000
_print_1000_digit:
        clc
        adc #'0'
        jsr C128Host_PutChar
        ldy #1
_skip_1000:

        ; 100s place
        lda #0
        sta _dir_digit
_div_100:
        lda _dir_blocks_lo
        sec
        sbc #100
        bcc _print_100
        sta _dir_blocks_lo
        inc _dir_digit
        jmp _div_100
_print_100:
        lda _dir_digit
        bne _print_100_digit
        cpy #0
        beq _skip_100
_print_100_digit:
        clc
        adc #'0'
        jsr C128Host_PutChar
        ldy #1
_skip_100:

        ; 10s place
        lda #0
        sta _dir_digit
_div_10:
        lda _dir_blocks_lo
        sec
        sbc #10
        bcc _print_10
        sta _dir_blocks_lo
        inc _dir_digit
        jmp _div_10
_print_10:
        lda _dir_digit
        bne _print_10_digit
        cpy #0
        beq _skip_10
_print_10_digit:
        clc
        adc #'0'
        jsr C128Host_PutChar
_skip_10:

        ; 1s place - always print
        lda _dir_blocks_lo
        clc
        adc #'0'
        jsr C128Host_PutChar
        
        rts

_dir_link_lo:   .byte 0
_dir_link_hi:   .byte 0
_dir_blocks_lo: .byte 0
_dir_blocks_hi: .byte 0
_dir_digit:     .byte 0





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


; ============================================================
; Sequential File I/O Handlers
; ============================================================
; These routines intercept OPEN, CLOSE, CHKIN, CHKOUT, CLRCHN,
; CHRIN, and CHROUT to provide sequential file access to the
; SD card through the host MEGA65 KERNAL.
;
; Only device 8 (disk) is intercepted. Other devices (keyboard,
; screen, serial, etc.) are passed through to the C128 ROM.
; ============================================================


; ============================================================
; VIC State Save/Restore for Host KERNAL Calls
; ============================================================
; The MEGA65 host KERNAL can change VIC-IV registers during file
; operations. We just re-unlock VIC-IV after each call.

C128Hook_SaveVIC:
        rts

C128Hook_RestoreVIC:
        ; Just re-unlock VIC-IV, don't reinitialize entire video
        jsr C128Hook_UnlockVIC
        rts

; Lighter version - just re-unlock VIC-IV without changing mode
; Use this when we want to preserve graphics mode
C128Hook_UnlockVIC:
        ; Re-unlock VIC-III
        lda #$A5
        sta $D02F
        lda #$96
        sta $D02F
        ; Re-unlock VIC-IV
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F
        
        ; Restore border/background colors from VIC-II shadow registers
        lda vic_regs+$20
        sta $D020
        lda vic_regs+$21
        sta $D021
        
        rts


; ============================================================
; C128Hook_OnOPEN - Handle OPEN command
; 
; Guest state on entry:
;   SETLFS has set: $AC=LFN, $AD=SA, $AE=device
;   SETNAM has set: $AB=namelen, $AF/$B0=nameptr
;
; We intercept device 8 only. Other devices fall through to ROM.
; ============================================================
C128Hook_OnOPEN:
        ; Check if this is device 8
        lda LOW_RAM_BUFFER + $AE        ; Device number
        cmp #$08
        beq _open_disk
        ; Not disk - let ROM handle it
        rts

_open_disk:
        ; Reset stale graphics flags before file operations
        lda #0
        sta c128_gfx_dirty
        
        ; Check filename length
        ; Empty filename (length 0) is allowed for command channel reads
        lda LOW_RAM_BUFFER + $AB
        cmp #17
        bcs _open_error_no_file         ; Filename too long
        sta seq_filename_len
        bne _open_has_filename          ; Non-empty filename, continue
        
        ; Empty filename - only allowed for SA >= $0F (command channel)
        lda LOW_RAM_BUFFER + $AD        ; Secondary address
        and #$0F
        cmp #$0F
        bne _open_error_no_file         ; Empty name but not command channel
        
_open_has_filename:
        ; Get guest LFN
        lda LOW_RAM_BUFFER + $AC        ; Logical file number
        sta _open_guest_lfn
        ; Note: LFN 0 is valid for command/error channel reads
        
        ; Find a free slot
        ldx #0
_open_find_slot:
        lda seq_slot_lfn,x
        cmp #$FF
        beq _open_found_slot            ; Empty slot ($FF = unused)
        cmp _open_guest_lfn             ; Already open with this LFN?
        beq _open_error_file_open
        inx
        cpx #MAX_SEQ_FILES
        bcc _open_find_slot
        ; No free slots
        jmp _open_error_no_file

_open_found_slot:
        stx _open_slot                  ; Save slot number
        
        ; Copy filename from guest RAM to our buffer
        ; Filename pointer is at $AF/$B0 in low RAM
        ; The filename might be in low RAM (<$1000) or in bank 5 (>=$1000)
        lda LOW_RAM_BUFFER + $B0        ; High byte of pointer
        cmp #$10
        bcs _open_copy_bank5            ; >= $1000, use bank 5
        
        ; Filename is in low RAM - read from LOW_RAM_BUFFER
        lda LOW_RAM_BUFFER + $AF
        sta $FB
        clc
        lda LOW_RAM_BUFFER + $B0
        adc #>LOW_RAM_BUFFER            ; Add $A0
        sta $FC
        
        ldy #0
_open_copy_low:
        cpy seq_filename_len
        beq _open_copy_done
        lda ($FB),y
        sta seq_filename,y
        iny
        cpy #17
        bcc _open_copy_low
        bra _open_copy_done

_open_copy_bank5:
        ; Filename is in bank 5 (RAM under ROM or higher RAM)
        lda LOW_RAM_BUFFER + $AF
        sta C128_MEM_PTR
        lda LOW_RAM_BUFFER + $B0
        sta C128_MEM_PTR+1
        lda #BANK_RAM
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        
        ldy #0
_open_copy_b5_loop:
        cpy seq_filename_len
        beq _open_copy_done
        tya
        taz
        lda [C128_MEM_PTR],z
        sta seq_filename,y
        iny
        cpy #17
        bcc _open_copy_b5_loop
        
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
        ldy LOW_RAM_BUFFER + $AD        ; Secondary address from guest
        jsr SETLFS
        
        ; Save VIC state before host KERNAL call
        jsr C128Hook_SaveVIC
        
        ; Call host OPEN
        jsr OPEN
        php                             ; Save carry (error flag)
        
        ; Restore VIC state after host call
        jsr C128Hook_RestoreVIC
        
        plp                             ; Restore carry
        bcs _open_error_host
        
        ; Success - record in slot table
        ldx _open_slot
        lda _open_guest_lfn
        sta seq_slot_lfn,x
        lda #$08
        sta seq_slot_dev,x
        lda LOW_RAM_BUFFER + $AD
        sta seq_slot_sa,x
        lda #0
        sta seq_slot_status,x
        lda #1
        sta seq_slot_open,x
        
        ; ============================================================
        ; IMPORTANT: Update C128 KERNAL file tables so ROM knows
        ; the file is open. Without this, CLOSE will fail with
        ; "FILE NOT OPEN" error.
        ;
        ; C128 file table locations:
        ;   $97 = LDTND (number of open files, index for next entry)
        ;   $0509-$0512 = LAT (Logical file numbers) - 10 entries
        ;   $0513-$051C = FAT (Device numbers) - 10 entries
        ;   $051D-$0526 = SAT (Secondary addresses) - 10 entries
        ; ============================================================
        ldx LOW_RAM_BUFFER + $97        ; Get current file count
        cpx #10
        bcs _open_table_full            ; Max 10 files
        
        ; Store in C128 tables (these are in low RAM, not ZP)
        lda _open_guest_lfn
        sta LOW_RAM_BUFFER + $0509,x    ; LAT[x] = LFN
        lda #$08
        sta LOW_RAM_BUFFER + $0513,x    ; FAT[x] = device
        lda LOW_RAM_BUFFER + $AD        ; SA from SETLFS
        ora #$60                        ; Set bits 5+6 like KERNAL does
        sta LOW_RAM_BUFFER + $AD        ; Update SETLFS work area too
        sta LOW_RAM_BUFFER + $051D,x    ; SAT[x] = secondary address
        
        ; Increment file count
        inc LOW_RAM_BUFFER + $97
        
_open_table_full:
        ; Set success status in guest
        lda #$00
        sta LOW_RAM_BUFFER + $90        ; Clear status
        lda c128_p
        and #((~P_C) & $FF)             ; Clear carry
        sta c128_p
        
        ; Return via RTS to guest
        jsr C128Hook_RTS_Guest
        rts

_open_error_host:
        ; Host OPEN failed - close and report error
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
        sta LOW_RAM_BUFFER + $90        ; Set status
        lda c128_p
        ora #P_C                        ; Set carry
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_open_guest_lfn: .byte 0
_open_host_lfn:  .byte 0
_open_slot:      .byte 0


; ============================================================
; C128Hook_OnCLOSE - Handle CLOSE command
;
; Guest A register contains LFN to close
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
        
        ; Save VIC state before host KERNAL call
        jsr C128Hook_SaveVIC
        
        ; Clear host channels before closing
        jsr CLRCHN
        
        ; Close on host
        lda _close_slot
        clc
        adc #SEQ_LFN_BASE
        jsr CLOSE
        
        ; Restore VIC state after host call
        jsr C128Hook_RestoreVIC
        
        ; Clear slot
        ldx _close_slot
        lda #$FF
        sta seq_slot_lfn,x              ; $FF = unused
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
        ; ============================================================
        ; Remove entry from C128 KERNAL file tables
        ; We need to find the entry with matching LFN and remove it
        ; by shifting all subsequent entries down
        ;
        ; C128 file table locations:
        ;   $97 = LDTND (file count)
        ;   $0509-$0512 = LAT (Logical file numbers)
        ;   $0513-$051C = FAT (Device numbers)
        ;   $051D-$0526 = SAT (Secondary addresses)
        ; ============================================================
        ldx #0
        lda LOW_RAM_BUFFER + $97        ; File count
        beq _close_table_done           ; No files open
        
_close_find_lat:
        lda LOW_RAM_BUFFER + $0509,x    ; LAT[x]
        cmp _close_lfn
        beq _close_found_lat
        inx
        cpx LOW_RAM_BUFFER + $97
        bcc _close_find_lat
        bra _close_table_done           ; Not found in table
        
_close_found_lat:
        ; Found at index X - shift subsequent entries down
        ; First decrement file count
        dec LOW_RAM_BUFFER + $97
        
_close_shift_loop:
        inx
        cpx LOW_RAM_BUFFER + $97
        beq _close_copy_last
        bcs _close_table_done
        
        ; Copy entry X to X-1
        lda LOW_RAM_BUFFER + $0509,x
        sta LOW_RAM_BUFFER + $0508,x    ; LAT[x-1] = LAT[x]
        lda LOW_RAM_BUFFER + $0513,x
        sta LOW_RAM_BUFFER + $0512,x    ; FAT[x-1] = FAT[x]
        lda LOW_RAM_BUFFER + $051D,x
        sta LOW_RAM_BUFFER + $051C,x    ; SAT[x-1] = SAT[x]
        bra _close_shift_loop
        
_close_copy_last:
        ; Copy the last entry
        lda LOW_RAM_BUFFER + $0509,x
        sta LOW_RAM_BUFFER + $0508,x
        lda LOW_RAM_BUFFER + $0513,x
        sta LOW_RAM_BUFFER + $0512,x
        lda LOW_RAM_BUFFER + $051D,x
        sta LOW_RAM_BUFFER + $051C,x
        
_close_table_done:
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
; C128Hook_OnCHKIN - Set input channel
;
; Guest X register contains LFN
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
        sta LOW_RAM_BUFFER + $90
        lda c128_p
        ora #P_C
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_chkin_lfn: .byte 0


; ============================================================
; C128Hook_OnCHKOUT - Set output channel
;
; Guest X register contains LFN
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
        ; File not open error
        lda #$03
        sta c128_a
        sta LOW_RAM_BUFFER + $90
        lda c128_p
        ora #P_C
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_chkout_lfn: .byte 0


; ============================================================
; C128Hook_OnCLRCHN - Clear channels (reset to keyboard/screen)
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
; C128Hook_OnPRIMM - Handle PRIMM (print immediate string)
;
; PRIMM is called via JSR $FA17 (or JMP $FA17 from $FF7D).
; The string follows inline after the JSR. The return address
; on the stack points to the byte BEFORE the string.
;
; We need to:
; 1. Read return address from guest stack (points to string-1)
; 2. Add 1 to get string start
; 3. Read bytes, output via guest CHROUT (write to screen RAM)
; 4. Update return address to point past null terminator
; 5. Simulate RTS
; ============================================================
C128Hook_OnPRIMM:
        ; Get return address from guest stack
        ; SP points below the return address
        ; But PRIMM pushes A, X, Y first: 3 bytes
        ; Actually - we catch at $FA17 BEFORE any pushes happen
        ; So the stack has the JSR return address at SP+1/SP+2
        lda c128_sp
        clc
        adc #1
        sta c128_addr_lo
        lda #$01
        sta c128_addr_hi
        jsr C128_ReadFast       ; return addr lo
        sta _primm_ptr_lo

        lda c128_sp
        clc
        adc #2
        sta c128_addr_lo
        jsr C128_ReadFast       ; return addr hi
        sta _primm_ptr_hi

        ; Return address from JSR is (target-1), so add 1 for string start
        inc _primm_ptr_lo
        bne _primm_loop
        inc _primm_ptr_hi

_primm_loop:
        ; Read next string byte
        lda _primm_ptr_lo
        sta c128_addr_lo
        lda _primm_ptr_hi
        sta c128_addr_hi
        jsr C128_ReadFast
        
        ; Null terminator?
        beq _primm_end

        ; Output character via screen write to guest screen RAM
        ; Store char in guest A and call the screen editor output
        sta c128_a
        
        ; Write to C128 screen RAM directly
        ; Use the guest's cursor position from ZP $EB (row) and $EC (col)
        ; Actually, simpler: just call guest CHROUT by letting ROM handle it
        ; For now, skip the character output - just advance past the string
        ; The important thing is to get past the string without crashing

        ; Advance pointer
        inc _primm_ptr_lo
        bne _primm_loop
        inc _primm_ptr_hi
        jmp _primm_loop

_primm_end:
        ; _primm_ptr now points to the null terminator
        ; Set return address to null terminator address
        ; (RTS will add 1, so execution continues after the null)
        lda c128_sp
        clc
        adc #1
        sta c128_addr_lo
        lda #$01
        sta c128_addr_hi
        lda _primm_ptr_lo
        sta c128_data
        jsr C128_Write          ; update return addr lo

        lda c128_sp
        clc
        adc #2
        sta c128_addr_lo
        lda _primm_ptr_hi
        sta c128_data
        jsr C128_Write          ; update return addr hi

        ; Now simulate RTS - pop return address and jump there+1
        jsr C128Hook_RTS_Guest
        rts

_primm_ptr_lo: .byte 0
_primm_ptr_hi: .byte 0


; ============================================================
; C128Hook_OnCHRIN - Character input
;
; If input is from a file we manage, read from host.
; Otherwise let ROM handle (keyboard input).
; ============================================================
C128Hook_OnCHRIN:

        ; Reset stale graphics flags before file operations
        lda #0
        sta c128_gfx_dirty
        
        ; We're reading from a file
        ldx seq_input_slot
        
        ; Check status - if EOF already, return with status
        lda seq_slot_status,x
        and #$40                        ; EOF flag
        bne _chrin_eof
        
        ; Save VIC state before host KERNAL call
        jsr C128Hook_SaveVIC
        
        ; Read from host
        jsr CHRIN
        sta _chrin_byte
        
        ; Restore VIC state after host KERNAL call
        jsr C128Hook_RestoreVIC
        
        ; Check host status
        jsr READST
        sta _chrin_status
        
        ; Update our status
        ldx seq_input_slot
        ora seq_slot_status,x
        sta seq_slot_status,x
        
        ; Also update guest status byte
        sta LOW_RAM_BUFFER + $90
        
        ; Return the byte in guest A
        lda _chrin_byte
        sta c128_a
        
        ; Clear carry for success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        
        jsr C128Hook_RTS_Guest
        rts

_chrin_eof:
        ; Already at EOF - return 0 with status
        lda #$00
        sta c128_a
        lda #$40
        sta LOW_RAM_BUFFER + $90
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts

_chrin_byte:   .byte 0
_chrin_status: .byte 0


; ============================================================
; C128Hook_OnCHROUT - Character output
;
; If output is to a file we manage, write to host.
; Otherwise let ROM handle (screen output).
; ============================================================
C128Hook_OnCHROUT:
        ; We're writing to a file (dispatcher already verified seq_output_slot != $FF)
        
        ; Reset stale graphics flags before file operations
        lda #0
        sta c128_gfx_dirty
        
        ; Save VIC state before host KERNAL call
        jsr C128Hook_SaveVIC
        
        ; Get byte from guest A
        lda c128_a
        
        ; Write to host
        jsr CHROUT
        
        ; Restore VIC state after host call
        jsr C128Hook_RestoreVIC
        
        ; Check status
        jsr READST
        ldx seq_output_slot
        ora seq_slot_status,x
        sta seq_slot_status,x
        sta LOW_RAM_BUFFER + $90
        
        ; Clear carry for success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        
        jsr C128Hook_RTS_Guest
        rts


; ============================================================
; C128Hook_OnGETIN - Get character input (used by GET#)
;
; If input is from a file we manage, read from host.
; Otherwise let ROM handle (keyboard input).
;
; GETIN differs from CHRIN in that it's non-blocking for keyboard
; and returns 0 if no key is pressed. For files, it works the same.
; ============================================================
C128Hook_OnGETIN:

        ; Reset stale graphics flags before file operations
        lda #0
        sta c128_gfx_dirty
        
        ; We're reading from a file - same as CHRIN
        ldx seq_input_slot
        
        ; Check status - if EOF already, return 0 with status
        lda seq_slot_status,x
        and #$40                        ; EOF flag
        bne _getin_eof
        
        ; Save VIC state before host KERNAL call
        jsr C128Hook_SaveVIC
        
        ; Read from host using CHRIN (GETIN uses same mechanism for files)
        jsr CHRIN
        sta _getin_byte
        
        ; Restore VIC state after host KERNAL call
        jsr C128Hook_RestoreVIC
        
        ; Check host status
        jsr READST
        sta _getin_status
        
        ; Update our status
        ldx seq_input_slot
        ora seq_slot_status,x
        sta seq_slot_status,x
        
        ; Also update guest status byte
        sta LOW_RAM_BUFFER + $90
        
        ; Return the byte in guest A
        lda _getin_byte
        sta c128_a
        
        ; Clear carry for success
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        
        jsr C128Hook_RTS_Guest
        rts

_getin_eof:
        ; Already at EOF - return 0 with status
        lda #$00
        sta c128_a
        lda #$40
        sta LOW_RAM_BUFFER + $90
        lda c128_p
        and #((~P_C) & $FF)
        sta c128_p
        jsr C128Hook_RTS_Guest
        rts


_getin_byte:   .byte 0
_getin_status: .byte 0

; ============================================================
; C128Hook_PostFileOpVideoFix
; If the last host file operation set Carry in c128_p (error),
; force emulator back to text mode. On success, do nothing.
; This specifically fixes host KERNAL error-path leaving VIC-IV
; in a different mode after ?FILE NOT FOUND, etc.
; ============================================================
C128Hook_PostFileOpVideoFix:
        lda c128_p
        and #P_C
        beq _pfov_done

        ; Error: force text mode
        lda #0
        sta c128_video_mode
        jsr C128Vid_DisableHostBitmap
        jsr C128_VideoInit

_pfov_done:
        rts

; ============================================================
; C128Hook_SyncLowRAMFromBank5 - Sync LOW_RAM_BUFFER from bank 5
; Called after loading directly to bank 5 to update the cached
; copy of $0000-$0FFF in LOW_RAM_BUFFER
; ============================================================
C128Hook_SyncLowRAMFromBank5:
        ; Check if load touched low RAM area ($0000-$0FFF)
        ; If dest >= $1000, no need to sync
        lda c128_dir_dest_hi
        cmp #$10
        bcs _sync_b5_done               ; dest >= $1000, nothing to sync
        
        ; Calculate the range to sync
        ; Start = dest address (clamped to $0000)
        lda c128_dir_dest_lo
        sta _sync_b5_start_lo
        lda c128_dir_dest_hi
        sta _sync_b5_start_hi
        
        ; End = min(dest + len, $1000)
        clc
        lda c128_dir_dest_lo
        adc c128_dir_len_lo
        sta _sync_b5_end_lo
        lda c128_dir_dest_hi
        adc c128_dir_len_hi
        sta _sync_b5_end_hi
        
        ; Clamp end to $1000
        lda _sync_b5_end_hi
        cmp #$10
        bcc _sync_b5_calc_len
        lda #$00
        sta _sync_b5_end_lo
        lda #$10
        sta _sync_b5_end_hi
        
_sync_b5_calc_len:
        ; Calculate length = end - start
        lda _sync_b5_end_lo
        sec
        sbc _sync_b5_start_lo
        sta _sync_b5_len_lo
        lda _sync_b5_end_hi
        sbc _sync_b5_start_hi
        sta _sync_b5_len_hi
        
        ; If length <= 0, nothing to sync
        ora _sync_b5_len_lo
        beq _sync_b5_done
        
        ; Fill in DMA list
        ; Count
        lda _sync_b5_len_lo
        sta _sync_b5_dma_count
        lda _sync_b5_len_hi
        sta _sync_b5_dma_count+1
        
        ; Source address (bank 5)
        lda _sync_b5_start_lo
        sta _sync_b5_dma_src
        lda _sync_b5_start_hi
        sta _sync_b5_dma_src+1
        
        ; Dest address (LOW_RAM_BUFFER + start offset)
        clc
        lda _sync_b5_start_lo
        adc #<LOW_RAM_BUFFER
        sta _sync_b5_dma_dst
        lda _sync_b5_start_hi
        adc #>LOW_RAM_BUFFER
        sta _sync_b5_dma_dst+1
        
        ; DMA copy from bank 5 to LOW_RAM_BUFFER
        lda #$00
        sta $D702                       ; DMA list bank 0
        lda #>_sync_b5_dma_list
        sta $D701                       ; High byte
        lda #<_sync_b5_dma_list
        sta $D700                       ; Low byte triggers DMA
        
_sync_b5_done:
        rts

_sync_b5_start_lo: .byte 0
_sync_b5_start_hi: .byte 0
_sync_b5_end_lo:   .byte 0
_sync_b5_end_hi:   .byte 0
_sync_b5_len_lo:   .byte 0
_sync_b5_len_hi:   .byte 0

_sync_b5_dma_list:
        .byte $00                       ; Command: COPY
_sync_b5_dma_count:
        .word $0000                     ; Count (filled in)
_sync_b5_dma_src:
        .word $0000                     ; Source address (filled in)
        .byte C128_BANK_RAM               ; Source bank 5
_sync_b5_dma_dst:
        .word $0000                     ; Dest address (filled in = LOW_RAM_BUFFER + offset)
        .byte $00                       ; Dest bank 0
        .byte $00                       ; Sub-command
        .word $0000                     ; Modulo