; ============================================================
; c128_mem.asm - Memory system for C128 emulator
; Host: MEGA65
; Assembler: 64tass (with 45GS02 support)
; ============================================================
;
; C128 Memory Architecture:
;   Two 64KB RAM banks (bank 0 and bank 1)
;   MMU (8722) controls what CPU sees at each address range
;   VIC-II sees RAM bank selected by $D506 bits 7-6
;   $FF00-$FF04 ALWAYS mapped to MMU (not RAM/ROM)
;
; MEGA65 Host Memory Layout:
;   Bank 1 ($10000-$1FFFF): C128 ROMs (48KB)
;     $10000-$11FFF: Character ROM (chargen, 8KB)
;     $14000-$17FFF: BASIC LO ROM  (C128 $4000-$7FFF)
;     $18000-$1BFFF: BASIC HI ROM  (C128 $8000-$BFFF)
;     $1C000-$1FFFF: KERNAL ROM    (C128 $C000-$FFFF)
;   Bank 4 ($40000-$4FFFF): C128 RAM Bank 0 (64KB)
;   Bank 5 ($50000-$5FFFF): C128 RAM Bank 1 (64KB)
;
; C128 MMU Configuration Register ($D500 / $FF00):
;   Bit 7-6: RAM bank select (00=bank0, 01=bank1)
;            (bits 10/11 for expansion banks - not emulated)
;   Bit 5-4: High ROM area ($C000-$FFFF)
;            00=System ROM (KERNAL)
;            01=Internal Function ROM (not emulated)
;            10=External Function ROM (not emulated)
;            11=RAM
;   Bit 3-2: Mid ROM area ($8000-$BFFF)
;            00=System ROM (BASIC HI)
;            01=Internal Function ROM (not emulated)
;            10=External Function ROM (not emulated)
;            11=RAM
;   Bit 1:   Low ROM ($4000-$7FFF)
;            0=System ROM (BASIC LO)
;            1=RAM
;   Bit 0:   I/O / ROM select for $D000-$DFFF
;            0=I/O block visible
;            1=ROM or RAM (based on bits 5-4)
;
; C128 RAM Configuration Register ($D506):
;   Bit 7-6: VIC-II RAM bank (which bank VIC sees)
;   Bit 3-0: Shared RAM configuration
;     Bits 1-0: Shared RAM size
;       00 = 1KB  ($0000-$03FF / $FC00-$FFFF)
;       01 = 4KB  ($0000-$0FFF / $F000-$FFFF)
;       10 = 8KB  ($0000-$1FFF / $E000-$FFFF)
;       11 = 16KB ($0000-$3FFF / $C000-$FFFF)
;     Bit 2: Share bottom of RAM (1=shared from bank 0)
;     Bit 3: Share top of RAM (1=shared from bank 0)
;
; ============================================================

        .cpu "45gs02"

; ============================================================
; Bank numbers (MEGA65 banks)
; ============================================================
BANK_ROM    = $01                       ; C128 ROMs in MEGA65 bank 1
BANK_RAM0   = $04                       ; C128 RAM bank 0 in MEGA65 bank 4
BANK_RAM1   = $05                       ; C128 RAM bank 1 in MEGA65 bank 5

; KERNAL $F800-$FFFF is relocated to $12000-$127FF in bank 1
; because $1F800-$1FFFF is hijacked by the MEGA65 color RAM window
KERNAL_F800_RELOC_HI = $20             ; hi byte of relocated address ($2000)

; Character ROM location in bank 0
; chargen.bin (8KB) loaded at $08000-$09FFF (stays at staging area)
; First 4KB ($08000-$08FFF) = C64 charset
; Second 4KB ($09000-$09FFF) = C128 charset
CHARGEN_BASE = $08000                   ; MEGA65 flat address of chargen ROM

; Low RAM buffer in bank 0 host RAM for fast screen/color mirroring
LOW_RAM_BUFFER  = $A000                 ; 4KB - C128 low RAM $0000-$0FFF mirror

; Screen base in low RAM buffer
C128_SCREEN_BASE = LOW_RAM_BUFFER + $0400  ; Default C128 screen at $0400

; 32-bit pointer for direct memory access
C128_MEM_PTR    = $F0                   ; 4 bytes at $F0-$F3
c128_saved_data = $F4                   ; Saved data byte at write entry

; ============================================================
; MMU Register State
; ============================================================
mmu_cr:          .byte $00     ; Configuration Register ($D500/$FF00)
                               ; Power-on default: $00 (all ROMs, bank 0, I/O)
mmu_pcr_a:       .byte $00     ; Pre-configuration Register A ($D501/$FF01)
mmu_pcr_b:       .byte $00     ; Pre-configuration Register B ($D502/$FF02)
mmu_pcr_c:       .byte $00     ; Pre-configuration Register C ($D503/$FF03)
mmu_pcr_d:       .byte $00     ; Pre-configuration Register D ($D504/$FF04)
mmu_mcr:         .byte $B6     ; Mode Configuration Register ($D505)
                               ; Bit 7: 40/80 key (1=released = 40 col mode)
                               ; Bit 6: C64 mode (0=C128)
                               ; Bit 5: EXROM (1=C128 mode)
                               ; Bit 4: GAME (1=C128 mode)
                               ; Bits 2-1: always set
                               ; Bit 0: CPU select (0=Z80 initially)
                               ; $B6 = 10110110 = 40col, C128, EXROM=1, GAME=1
mmu_rcr:         .byte $00     ; RAM Configuration Register ($D506)
                               ; Bit 7-6: VIC bank (00=bank 0)
                               ; Bit 3-0: Shared RAM config
mmu_p0l:         .byte $00     ; Page 0 pointer low ($D507)
mmu_p0h:         .byte $00     ; Page 0 pointer high ($D508)
mmu_p1l:         .byte $01     ; Page 1 pointer low ($D509) - default $01 (page 1)
mmu_p1h:         .byte $00     ; Page 1 pointer high ($D50A)

MMU_VERSION      = $20         ; Version register ($D50B) - read only

; ============================================================
; Derived state (updated when MMU CR changes)
; These are pre-computed for fast read/write dispatch
; ============================================================
mmu_ram_bank:    .byte BANK_RAM0  ; MEGA65 bank for current RAM bank (4 or 5)
mmu_io_visible:  .byte 1          ; 1 = I/O at $D000-$DFFF, 0 = ROM/RAM
mmu_basic_lo_rom:.byte 1          ; 1 = BASIC LO ROM at $4000-$7FFF
mmu_basic_hi_rom:.byte 1          ; 1 = BASIC HI ROM at $8000-$BFFF (00 in bits 3-2)
mmu_kernal_rom:  .byte 1          ; 1 = KERNAL ROM at $C000-$FFFF (00 in bits 5-4)
mmu_hi_is_ram:   .byte 0          ; 1 = bits 5-4 are 11 (RAM, not ROM)
mmu_mid_is_ram:  .byte 0          ; 1 = bits 3-2 are 11 (RAM, not ROM)

; Shared RAM masks (derived from $D506)
shared_bottom_mask: .byte $00     ; Top address byte of shared bottom region (0=disabled)
shared_top_start:   .byte $FF     ; Start page of shared top region ($FF=disabled)
shared_bottom_on:   .byte 0       ; 1 = bottom sharing enabled
shared_top_on:      .byte 0       ; 1 = top sharing enabled

; ============================================================
; VIC-II register shadow (for reads)
; The C128's VIC-IIe is at $D000-$D3FF
; On MEGA65 we let the real VIC-IV handle display,
; but we track register writes for emulation
; ============================================================
vic_regs:        .fill 64, 0      ; 64 VIC-II registers

; CIA shadows
cia1_regs:       .fill 16, 0      ; CIA1 $DC00-$DC0F
cia2_regs:       .fill 16, 0      ; CIA2 $DD00-$DD0F

; VDC registers (8563 - 80 column controller)
; Accessed through index/data at $D600/$D601
vdc_index:       .byte 0          ; Current VDC register index
vdc_regs:        .fill 38, 0      ; VDC internal registers (R0-R37)
; VDC 16KB RAM stored in ATTIC RAM at $8000000
VDC_RAM_BANK = $00              ; low bank byte for ATTIC RAM
VDC_RAM_ATTIC_HI = $08          ; high byte ($08xxxxxx = ATTIC RAM)

; SID registers are passed through to real hardware

; ============================================================
; Video state
; ============================================================
c128_charset_dirty: .byte 0
c128_video_mode:    .byte 0       ; 0=text, 1=bitmap
c128_file_op_active: .byte 0

; ============================================================
; C128_MemInit - Initialize memory system
; ============================================================
C128_MemInit:
        ; Set MMU to default power-on state
        lda #$00
        sta mmu_cr              ; All ROMs visible, bank 0, I/O
        sta mmu_pcr_a
        sta mmu_pcr_b
        sta mmu_pcr_c
        sta mmu_pcr_d
        lda #$B6                ; 40-col, C128 mode, EXROM=1, GAME=1, bits 2-1 set
        sta mmu_mcr
        lda #$00
        sta mmu_rcr             ; VIC bank 0, no shared RAM
        sta mmu_p0l             ; Page 0 at $0000
        sta mmu_p0h
        lda #$01
        sta mmu_p1l             ; Page 1 at $0100
        lda #$00
        sta mmu_p1h

        ; Compute derived state from MMU registers (initial setup)
        ; NOTE: Done here first so vic_regs clear uses correct bank
        jsr mmu_update_derived

        ; Initialize VIC-II shadow registers to defaults
        ldx #63
        lda #0
_clear_vic:
        sta vic_regs,x
        dex
        bpl _clear_vic

        ; Initialize CIA shadows
        ldx #15
        lda #$FF
_clear_cia:
        sta cia1_regs,x
        sta cia2_regs,x
        dex
        bpl _clear_cia

        ; Initialize SID for sound
        jsr C128_SndInit

        ; Clear C128 RAM bank 0 (already done in main.asm via DMA)
        ; Clear C128 RAM bank 1 (already done in main.asm via DMA)

        ; Clear local low RAM buffer
        jsr clear_low_ram_buffer

        ; Re-derive MMU state AFTER clear_low_ram_buffer
        ; (DMA may have zeroed our variables if they overlap $A000-$AFFF)
        jsr mmu_update_derived

        rts

; ============================================================
; C128_VideoInit - Set up display for C128 emulation
; VIC-IV must stay unlocked for SCRNPTR/CHARPTR to work.
; We disable VIC-III extended attributes to keep VIC-II color
; behavior. Everything else uses standard VIC-II registers.
; ============================================================
C128_VideoInit:
        ; Unlock VIC-IV
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F

        ; Make sure HOTREG is OFF so VIC-II changes dont effect VIC-IV registers
        lda $D05D
        and #$7F                ; Clear bit 7 (HOTREG disable)
        sta $D05D


        
        ; Switch to 40-column mode (H640 off, VIC-III attributes off)
        ; Clear all: H640=0 (bit 7), ATTR=0 (bit 5)
        lda #$00
        sta $D031               

        ; Standard VIC-II setup
        ;lda #$1B
        ;sta $D011               ; 25-row text mode
        ;lda #$14
        ;sta $D018               ; default charset layout

        ; Point screen at C128 RAM bank 4 + $0400 = $040400
        ; $040400 is within VIC-IV's 384KB range
        lda #$00
        sta $D060               ; SCRNPTR[7:0]   = $00
        ;sta $D063               ; SCRNPTR[31:24] = $00
        lda #$04
        sta $D061               ; SCRNPTR[15:8]  = $04
        lda #BANK_RAM0
        sta $D062               ; SCRNPTR[23:16] = $04 -> $040400


        ; Point charset at C128 character ROM in bank 1
        ; C128 charset is in second 4KB half: $0000-$0FFF
        ; Default is uppercase/graphics
        lda #$00
        sta $D068               ; CHARPTR LSB
        lda #$90 ;00
        sta $D069               ; CHARPTR byte 1
        lda #$00 ;01
        sta $D06A               ; CHARPTR byte 2 -> $010000

        ; Standard 40-column, virtual row width = 40
        ; Necesssary!
        lda #40
        sta $D058
        lda #0
        sta $D059

        ; CHR16 OFF (standard 1-byte screen codes)
        ;lda $D054
        ;and #$FC                ; Clear bits 0-1 (CHR16, FCM)
        ;sta $D054
        lda #$d7                ; same thing?
        trb $d054

        ; MCM off
        ; unnecessary
        ;lda $D016
        ;and #$EF
        ;;lda #$08
        ;sta $D016               ; 40 columns, no scroll, MCM off

        ; Default C128 colors
        lda #13
        sta $D020               ; border = light green
        lda #11
        sta $D021               ; background = dark grey


        ; Clear color RAM to light green (13)
;        ldx #0
;        lda #$0D
;_clr_color:
;        sta $D800,x
;        sta $D900,x
;        sta $DA00,x
;        sta $DB00,x
;        inx
;        bne _clr_color



        ; Color RAM pointer - set FIRST before anything else
        ; MEGA65 color RAM is at fixed $FF80000
        ; Set COLPTR to physical color RAM at $FF8(0000)
        ; so writes to $D800 go to actual color RAM, not ROM
        lda #$00
        sta $D064               ; COLPTR byte 0
        lda #$00
        sta $D065               ; COLPTR byte 1
        lda #$F8
        sta $D066               ; COLPTR[23:16] = $F8
        lda #$0F
        sta $D067               ; COLPTR[31:24] = $0F  -> $0FF80800



        rts

; ============================================================
; C128_MemClearRAM - Clear a RAM bank via DMA
; X = MEGA65 bank number (4 or 5)
; ============================================================
C128_MemClearRAM:
        stx _clr_bank+0
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00   ; enhanced DMA options
        .byte $03                       ; fill
        .word $0000                     ; count (0 = 64K)
        .word $0000                     ; fill with $00
        .byte $00                       ; unused
        .word $0000                     ; dest start
_clr_bank:
        .byte $04                       ; dest bank (modified)
        .byte $00
        .word $0000
        rts

; ============================================================
; clear_low_ram_buffer - Clear the 4KB low RAM buffer
; ============================================================
clear_low_ram_buffer:
        lda #$00
        sta $D707
        .byte $80, $00, $81, $00, $00
        .byte $03                       ; fill
        .word $1000                     ; 4KB
        .word $0000                     ; fill with $00
        .byte $00
        .byte <LOW_RAM_BUFFER
        .byte >LOW_RAM_BUFFER
        .byte $00                       ; bank 0
        .byte $00
        .word $0000
        rts

; ============================================================
; mmu_update_derived - Recompute derived MMU state from CR/RCR
; Called whenever MMU CR or RCR changes
; ============================================================
mmu_update_derived:
        lda mmu_cr

        ; Bit 6: RAM bank select (we only support 0 and 1)
        ; Bit 7 is for expansion banks, treat as bit 6
        and #$40
        beq _mmu_bank0
        lda #BANK_RAM1
        bne _mmu_bank_set       ; always taken
_mmu_bank0:
        lda #BANK_RAM0
_mmu_bank_set:
        sta mmu_ram_bank

        ; Bit 0: I/O vs ROM/RAM at $D000-$DFFF
        lda mmu_cr
        and #$01
        eor #$01                ; Invert: bit0=0 means I/O visible
        sta mmu_io_visible

        ; Bit 1: BASIC LO ROM at $4000-$7FFF
        lda mmu_cr
        and #$02
        eor #$02                ; bit1=0 means ROM visible
        lsr                     ; shift to bit 0
        sta mmu_basic_lo_rom

        ; Bits 3-2: Mid area ($8000-$BFFF)
        lda mmu_cr
        and #$0C
        ; 00 = BASIC HI ROM, 11 = RAM, others = function ROM (treat as RAM)
        beq _mmu_mid_rom
        lda #0
        sta mmu_basic_hi_rom
        lda mmu_cr
        and #$0C
        cmp #$0C
        bne _mmu_mid_done
        lda #1
        sta mmu_mid_is_ram
        jmp _mmu_mid_done
_mmu_mid_rom:
        lda #1
        sta mmu_basic_hi_rom
        lda #0
        sta mmu_mid_is_ram
_mmu_mid_done:

        ; Bits 5-4: High area ($C000-$FFFF)
        lda mmu_cr
        and #$30
        ; 00 = KERNAL ROM, 11 = RAM, others = function ROM (treat as RAM)
        beq _mmu_hi_rom
        lda #0
        sta mmu_kernal_rom
        lda mmu_cr
        and #$30
        cmp #$30
        bne _mmu_hi_done
        lda #1
        sta mmu_hi_is_ram
        jmp _mmu_hi_done
_mmu_hi_rom:
        lda #1
        sta mmu_kernal_rom
        lda #0
        sta mmu_hi_is_ram
_mmu_hi_done:

        ; Update shared RAM from RCR ($D506)
        jsr mmu_update_shared
        rts

; ============================================================
; mmu_update_shared - Compute shared RAM boundaries from $D506
; ============================================================
mmu_update_shared:
        ; Bits 1-0 of RCR: shared size
        ;   00 = 1KB,  01 = 4KB,  10 = 8KB,  11 = 16KB
        ; Bit 2: share bottom (1 = bottom shared from bank 0)
        ; Bit 3: share top (1 = top shared from bank 0)
        lda mmu_rcr
        and #$04
        beq _no_bottom_share
        lda #1
        sta shared_bottom_on
        ; Compute bottom shared size
        lda mmu_rcr
        and #$03
        tax
        lda _shared_bottom_masks,x
        sta shared_bottom_mask
        jmp _check_top_share
_no_bottom_share:
        lda #0
        sta shared_bottom_on
        sta shared_bottom_mask

_check_top_share:
        lda mmu_rcr
        and #$08
        beq _no_top_share
        lda #1
        sta shared_top_on
        lda mmu_rcr
        and #$03
        tax
        lda _shared_top_starts,x
        sta shared_top_start
        rts
_no_top_share:
        lda #0
        sta shared_top_on
        lda #$FF
        sta shared_top_start
        rts

; Shared bottom: max page for bottom shared region
_shared_bottom_masks:
        .byte $04               ; 1KB  = $0000-$03FF -> pages $00-$03
        .byte $10               ; 4KB  = $0000-$0FFF -> pages $00-$0F
        .byte $20               ; 8KB  = $0000-$1FFF -> pages $00-$1F
        .byte $40               ; 16KB = $0000-$3FFF -> pages $00-$3F

; Shared top: start page of top shared region
_shared_top_starts:
        .byte $FC               ; 1KB  = $FC00-$FFFF -> page $FC+
        .byte $F0               ; 4KB  = $F000-$FFFF -> page $F0+
        .byte $E0               ; 8KB  = $E000-$FFFF -> page $E0+
        .byte $C0               ; 16KB = $C000-$FFFF -> page $C0+


; ============================================================
; get_physical_bank - Determine the MEGA65 bank for a C128 address
; Input: p4_addr_hi (page number)
; Output: A = MEGA65 bank number (BANK_RAM0 or BANK_RAM1)
; Uses shared RAM logic - if address is in shared region,
; always returns BANK_RAM0 regardless of MMU bank select
; ============================================================
get_physical_bank:
        ; Check if in bottom shared region
        lda shared_bottom_on
        beq _gpb_check_top
        lda p4_addr_hi
        cmp shared_bottom_mask
        bcc _gpb_shared          ; Address < shared boundary -> bank 0

_gpb_check_top:
        ; Check if in top shared region
        lda shared_top_on
        beq _gpb_selected
        lda p4_addr_hi
        cmp shared_top_start
        bcs _gpb_shared          ; Address >= shared top start -> bank 0

_gpb_selected:
        ; Use the MMU-selected bank
        lda mmu_ram_bank
        rts

_gpb_shared:
        lda #BANK_RAM0
        rts


; ============================================================
; C128_ReadFast - Fast path for memory reads
; Only handles simple RAM cases ($0000-$3FFF), falls through
; to full C128_Read for anything complex ($4000+)
; ============================================================
C128_ReadFast:
        lda p4_addr_hi
        cmp #$40
        bcs C128_Read           ; $4000+ needs full handler (ROM/IO possible)

        ; $0000-$3FFF: RAM - read from physical bank via 32-bit pointer
        jsr get_physical_bank
        sta C128_MEM_PTR+2
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda p4_addr_hi
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        rts

; ============================================================
; C128_Read - Full memory read handler
; Input: p4_addr_hi:p4_addr_lo = C128 address
; Output: A = byte read
; ============================================================
C128_Read:
        lda p4_addr_hi

        ; $0000-$0FFF: RAM - read from physical bank
        ; (get_physical_bank handles shared RAM)
        cmp #$10
        bcs _rd_not_low
        ; Special case: $0000/$0001 = CPU port registers
        cmp #$00
        bne _rd_low_ram
        lda p4_addr_lo
        cmp #$02
        bcs _rd_low_ram
        jmp read_zp             ; Use port register handler for $00/$01
_rd_low_ram:
        jsr get_physical_bank
        sta C128_MEM_PTR+2
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda p4_addr_hi
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        rts

_rd_not_low:
        ; $FF00-$FF04: ALWAYS MMU registers (regardless of config)
        cmp #$FF
        bne _rd_not_ff
        lda p4_addr_lo
        cmp #$05
        bcc read_mmu_register   ; $FF00-$FF04 -> MMU
        ; $FF05-$FFFF: KERNAL ROM or RAM
        ; Check mmu_cr bits 5-4 directly (00 = KERNAL ROM)
        lda mmu_cr
        and #$30
        bne _ff_read_ram        ; nonzero = RAM or function ROM
        jmp read_from_kernal
_ff_read_ram:
        jmp read_ram_direct

_rd_not_ff:
        ; $D000-$DFFF: I/O or ROM/RAM
        lda p4_addr_hi
        cmp #$D0
        bcc _rd_not_io
        cmp #$E0
        bcs _rd_check_hi_rom

        ; $D000-$DFFF: Check if I/O is visible
        lda mmu_io_visible
        bne _rd_io_dispatch     ; I/O visible -> dispatch to I/O handlers
        ; I/O not visible -> character ROM or RAM
        ; When I/O is off and ROM is mapped, $D000-$DFFF shows CHARACTER ROM
        ; (not KERNAL — that's only $E000-$FFFF)
        lda mmu_kernal_rom
        bne read_from_chargen   ; If ROM mapped, show char ROM at $D000-$DFFF
        jmp read_ram_direct     ; Otherwise RAM

_rd_io_dispatch:
        ; Dispatch I/O reads by page
        lda p4_addr_hi
        cmp #$D0
        beq _rd_vic             ; $D000-$D0FF -> VIC-II
        cmp #$D4
        beq _rd_sid             ; $D400-$D4FF -> SID
        cmp #$D5
        beq _rd_mmu_io          ; $D500-$D5FF -> MMU registers
        cmp #$D6
        beq _rd_vdc             ; $D600-$D6FF -> VDC
        cmp #$D8
        bcc _rd_io_other        ; $D700-$D7FF -> other
        cmp #$DC
        bcc _rd_color_ram       ; $D800-$DBFF -> Color RAM
        cmp #$DD
        bcc _rd_cia1            ; $DC00-$DCFF -> CIA1
        cmp #$DE
        bcc _rd_cia2            ; $DD00-$DDFF -> CIA2
        jmp read_ram_direct     ; $DE00-$DFFF -> expansion I/O (return open bus)

_rd_vic:
        jmp read_vic_register

_rd_sid:
        ; SID read - most registers are write-only
        ; Return from real MEGA65 SID for voice 3 / paddle reads
        lda p4_addr_lo
        and #$1F
        cmp #$1B
        bcs _rd_sid_readable
        lda #$00               ; Write-only registers read as 0
        rts
_rd_sid_readable:
        ; $D41B-$D41C: Voice 3 waveform/envelope output
        ; $D419-$D41A: Potentiometer X/Y
        tax
        lda $D400,x            ; Read from real MEGA65 SID
        rts

_rd_mmu_io:
        ; $D500-$D50B: MMU registers via I/O
        lda p4_addr_lo
        cmp #$0C
        bcs _rd_mmu_io_open     ; $D50C+ = open bus
        jmp read_mmu_d500

_rd_mmu_io_open:
        lda #$FF
        rts

_rd_vdc:
        jmp read_vdc_register

_rd_io_other:
        lda #$FF               ; Open bus
        rts

_rd_color_ram:
        ; Color RAM at $D800-$DBFF
        ; Read from MEGA65's actual color RAM
        lda p4_addr_hi
        sec
        sbc #$D8
        clc
        adc #$D8                ; Map to real $D800
        sta _rd_col+2
        ldx p4_addr_lo
_rd_col:
        lda $D800,x
        and #$0F               ; Color RAM is 4 bits
        rts

_rd_cia1:
        jmp read_cia1_register

_rd_cia2:
        jmp read_cia2_register

_rd_not_io:
        ; $4000-$CFFF: Check ROM visibility
        lda p4_addr_hi
        cmp #$C0
        bcs _rd_check_hi_rom
        cmp #$80
        bcs _rd_check_mid_rom
        cmp #$40
        bcs _rd_check_lo_rom
        ; $1000-$3FFF: Always RAM
        jmp read_ram_direct

_rd_check_lo_rom:
        ; $4000-$7FFF: BASIC LO ROM or RAM
        lda mmu_basic_lo_rom
        bne read_from_basic_lo
        jmp read_ram_direct

_rd_check_mid_rom:
        ; $8000-$BFFF: BASIC HI ROM or RAM
        lda mmu_basic_hi_rom
        bne read_from_basic_hi
        jmp read_ram_direct

_rd_check_hi_rom:
        ; $C000-$FFFF (except I/O and $FF00-$FF04): KERNAL ROM or RAM
        lda mmu_kernal_rom
        bne read_from_kernal
        jmp read_ram_direct


; ============================================================
; ROM read routines - 32-bit flat addressing into bank 1
; ============================================================

; Read from KERNAL ROM: C128 $C000-$FFFF -> MEGA65 $1C000-$1FFFF
; Exception: $F800-$FFFF is redirected to $12000-$127FF
; because $1F800-$1FFFF is the MEGA65 color RAM window
read_from_kernal:
        lda p4_addr_hi
        cmp #$F8
        bcc _rfk_normal
        ; $F800-$FFFF: read from relocated area at $12000
        ; Map $F8xx -> $20xx, $F9xx -> $21xx, etc.
        sec
        sbc #$D8                ; $F8->$20, $F9->$21, ..., $FF->$27
        sta C128_MEM_PTR+1
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda #BANK_ROM
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts
_rfk_normal:
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda p4_addr_hi
        sta C128_MEM_PTR+1
        lda #BANK_ROM           ; Bank 1
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts

; Read from BASIC LO: C128 $4000-$7FFF -> MEGA65 $14000-$17FFF
read_from_basic_lo:
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda p4_addr_hi
        sta C128_MEM_PTR+1
        lda #BANK_ROM
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts

; Read from BASIC HI: C128 $8000-$BFFF -> MEGA65 $18000-$1BFFF
read_from_basic_hi:
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda p4_addr_hi
        sta C128_MEM_PTR+1
        lda #BANK_ROM
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts

; Read from Character ROM: C128 $D000-$DFFF -> MEGA65 $08000-$09FFF
; The char ROM is 8KB. C128 address $D000 maps to chargen offset $0000.
; MEGA65 address = $08000 + (addr - $D000)
; Since chargen is at $08000 in bank 0, and addr_hi=$D0-$DF:
;   ptr = $08000 + ((addr_hi - $D0) << 8) + addr_lo
;       = bank 0, high = $80 + (addr_hi - $D0), low = addr_lo
read_from_chargen:
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda p4_addr_hi
        sec
        sbc #$D0                ; Offset from $D000 base
        clc
        adc #$80                ; Add $80 base -> $80xx within bank 0
        sta C128_MEM_PTR+1
        lda #$00                ; Bank 0
        sta C128_MEM_PTR+2
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts

; ============================================================
; read_ram_direct - Read from physical RAM with shared logic
; ============================================================
read_ram_direct:
        jsr get_physical_bank
        sta C128_MEM_PTR+2
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda p4_addr_hi
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts


; ============================================================
; MMU Register Reads
; ============================================================

; Read MMU via $FF00-$FF04 (always visible)
read_mmu_register:
        lda p4_addr_lo
        cmp #$00
        beq rmmu_cr
        cmp #$01
        beq rmmu_pcra
        cmp #$02
        beq rmmu_pcrb
        cmp #$03
        beq rmmu_pcrc
        cmp #$04
        beq rmmu_pcrd
        ; $FF05+: KERNAL ROM or RAM (should not reach here)
        jmp read_from_kernal
rmmu_cr:
        lda mmu_cr
        rts
rmmu_pcra:
        lda mmu_pcr_a
        rts
rmmu_pcrb:
        lda mmu_pcr_b
        rts
rmmu_pcrc:
        lda mmu_pcr_c
        rts
rmmu_pcrd:
        lda mmu_pcr_d
        rts

; Read MMU via $D500-$D50B (I/O mapped)
read_mmu_d500:
        lda p4_addr_lo
        cmp #$00
        beq rmmu_cr
        cmp #$01
        beq rmmu_d501
        cmp #$02
        beq rmmu_d502
        cmp #$03
        beq rmmu_d503
        cmp #$04
        beq rmmu_d504
        cmp #$05
        beq rmmu_d505
        cmp #$06
        beq rmmu_d506
        cmp #$07
        beq rmmu_d507
        cmp #$08
        beq rmmu_d508
        cmp #$09
        beq rmmu_d509
        cmp #$0A
        beq rmmu_d50a
        cmp #$0B
        beq rmmu_d50b
        lda #$FF
        rts
rmmu_d501:
        lda mmu_pcr_a
        rts
rmmu_d502:
        lda mmu_pcr_b
        rts
rmmu_d503:
        lda mmu_pcr_c
        rts
rmmu_d504:
        lda mmu_pcr_d
        rts
rmmu_d505:
        lda mmu_mcr
        ora #$06                ; Bits 2-1 always read as 1
        rts
rmmu_d506:
        lda mmu_rcr
        rts
rmmu_d507:
        lda mmu_p0l
        rts
rmmu_d508:
        lda mmu_p0h
        rts
rmmu_d509:
        lda mmu_p1l
        rts
rmmu_d50a:
        lda mmu_p1h
        rts
rmmu_d50b:
        lda #MMU_VERSION        ; Version register
        rts


; ============================================================
; VIC-II Register Reads ($D000-$D03F, mirrored every 64 bytes)
; ============================================================
read_vic_register:
        lda p4_addr_lo
        and #$3F                ; Mirror every 64 bytes
        tax

        ; For most registers, return the shadow
        cpx #$11
        beq _rv_d011
        cpx #$12
        beq _rv_raster
        cpx #$19
        beq _rv_irq
        cpx #$1E
        beq _rv_sprite_collision
        cpx #$1F
        beq _rv_sprite_collision

        ; Default: return shadow register
        lda vic_regs,x
        rts

_rv_d011:
        ; $D011: bits 6-0 = shadow, bit 7 = raster counter bit 8
        lda vic_regs+$11
        and #$7F                ; Clear bit 7
        ldx vic_raster_hi
        beq +
        ora #$80                ; Set bit 7 if raster >= 256
+       rts

_rv_raster:
        ; $D012: Current raster line low byte (from our emulated counter)
        lda vic_raster_lo
        rts

_rv_irq:
        ; $D019: IRQ flags
        lda vic_regs+$19
        rts

_rv_sprite_collision:
        ; Sprite collision registers - cleared on read
        lda vic_regs,x
        pha
        lda #0
        sta vic_regs,x
        pla
        rts


; ============================================================
; CIA1 Register Reads ($DC00-$DC0F)
; ============================================================
read_cia1_register:
        lda p4_addr_lo
        and #$0F
        tax

        ; $DC00: Keyboard column select (directly use MEGA65 CIA)
        cpx #$00
        beq _rc1_port_a
        cpx #$01
        beq _rc1_port_b
        cpx #$0D
        beq _rc1_icr

        ; Other CIA1 registers - return shadow
        lda cia1_regs,x
        rts

_rc1_port_a:
        lda $DC00              ; Read from real CIA
        rts

_rc1_port_b:
        ; $DC01: Keyboard row read
        lda $DC01              ; Read from real MEGA65 CIA
        rts

_rc1_icr:
        ; $DC0D: Interrupt control - cleared on read
        lda cia1_icr_data
        pha
        lda #0
        sta cia1_icr_data
        sta p4_irq_pending      ; Clear pending IRQ
        pla
        rts


; ============================================================
; CIA2 Register Reads ($DD00-$DD0F)
; ============================================================
read_cia2_register:
        lda p4_addr_lo
        and #$0F
        tax

        cpx #$00
        beq _rc2_port_a

        ; Other CIA2 registers - return shadow
        lda cia2_regs,x
        rts

_rc2_port_a:
        ; $DD00: VIC bank select + serial bus
        lda cia2_regs
        rts


; ============================================================
; VDC Register Reads ($D600-$D601)
; ============================================================
read_vdc_register:
        lda p4_addr_lo
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
        ; VDC RAM is in ATTIC RAM at $8000000
        lda vdc_regs+19         ; address lo
        sta C128_MEM_PTR+0
        lda vdc_regs+18         ; address hi
        and #$3F                ; mask to 16KB
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK
        sta C128_MEM_PTR+2
        lda #VDC_RAM_ATTIC_HI
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        pha
        ; Auto-increment R18:R19
        inc vdc_regs+19
        bne +
        inc vdc_regs+18
+       pla
        rts


; ============================================================
; C128_Write - Full memory write handler
; Input: p4_addr_hi:p4_addr_lo = C128 address
;        p4_data = byte to write
; ============================================================
C128_Write:
        lda p4_data
        sta c128_saved_data

        lda p4_addr_hi

        ; $FF00-$FF04: ALWAYS MMU registers
        cmp #$FF
        bne _wr_not_ff
        lda p4_addr_lo
        cmp #$05
        bcc write_mmu_register  ; $FF00-$FF04 -> MMU
        ; $FF05-$FFFF: Write to RAM under ROM
        jmp write_ram_direct

_wr_not_ff:
        ; $D000-$DFFF: I/O or RAM
        lda p4_addr_hi
        cmp #$D0
        bcc _wr_not_io
        cmp #$E0
        bcs _wr_to_ram

        ; $D000-$DFFF: Check I/O visibility
        lda mmu_io_visible
        bne _wr_io_dispatch
        jmp write_ram_direct    ; I/O not visible, write to RAM

_wr_io_dispatch:
        lda p4_addr_hi
        cmp #$D0
        beq _wr_vic
        cmp #$D4
        beq _wr_sid
        cmp #$D5
        beq _wr_mmu_io
        cmp #$D6
        beq _wr_vdc
        cmp #$D8
        bcc _wr_to_ram          ; $D700-$D7FF
        cmp #$DC
        bcc _wr_color_ram       ; $D800-$DBFF
        cmp #$DD
        bcc _wr_cia1            ; $DC00-$DCFF
        cmp #$DE
        bcc _wr_cia2            ; $DD00-$DDFF
        jmp write_ram_direct    ; $DE00-$DFFF expansion I/O

_wr_vic:
        jmp write_vic_register

_wr_sid:
        ; SID write - pass through to real MEGA65 SID
        lda p4_addr_lo
        and #$1F
        tax
        lda c128_saved_data
        sta $D400,x            ; Write directly to MEGA65 SID
        rts

_wr_mmu_io:
        ; $D500-$D50B: MMU via I/O
        lda p4_addr_lo
        cmp #$0C
        bcs _wr_to_ram          ; $D50C+ = open
        jmp write_mmu_d500

_wr_vdc:
        jmp write_vdc_register

_wr_color_ram:
        ; Color RAM $D800-$DBFF
        ; Write to MEGA65's real color RAM
        lda p4_addr_hi
        sec
        sbc #$D8
        clc
        adc #$D8
        sta _wr_col+2
        ldx p4_addr_lo
        lda c128_saved_data
        and #$0F               ; Color RAM is 4 bits
_wr_col:
        sta $D800,x
        rts

_wr_cia1:
        jmp write_cia1_register

_wr_cia2:
        jmp write_cia2_register

_wr_not_io:
        ; $1000-$CFFF / $E000-$FEFF: Write always goes to RAM
        ; (ROM is read-only, writes go to underlying RAM)
_wr_to_ram:
        jmp write_ram_direct


; ============================================================
; write_ram_direct - Write to physical RAM via 32-bit pointer
; ============================================================
; write_ram_direct - Write to C128 RAM using physical bank
; Also mirrors pages $00-$0F to LOW_RAM_BUFFER so the CPU's
; fast ZP/stack reads stay in sync
; ============================================================
write_ram_direct:
        ; Invalidate code cache if writing to current code page
        lda p4_addr_hi
        cmp p4_code_page_hi
        bne _wrd_no_inv
        lda #0
        sta p4_code_valid
_wrd_no_inv:

        jsr get_physical_bank
        sta C128_MEM_PTR+2
        sta _wrd_saved_bank     ; Save bank for mirror check
        lda p4_addr_lo
        sta C128_MEM_PTR
        lda p4_addr_hi
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda c128_saved_data
        sta [C128_MEM_PTR],z

        ; Mirror writes to pages $00-$0F in bank 4 to LOW_RAM_BUFFER
        ; so CPU fast ZP/stack reads see the correct data
        ; Rebuilds C128_MEM_PTR each time
        lda _wrd_saved_bank
        cmp #BANK_RAM0
        bne _wrd_done
        lda p4_addr_hi
        cmp #$10
        bcs _wrd_done
        ; Set C128_MEM_PTR to LOW_RAM_BUFFER + p4_addr_hi page
        clc
        adc #>LOW_RAM_BUFFER
        sta C128_MEM_PTR+1
        lda #<LOW_RAM_BUFFER
        sta C128_MEM_PTR+0
        lda #$00
        sta C128_MEM_PTR+2
        sta C128_MEM_PTR+3
        lda p4_addr_lo
        taz
        lda c128_saved_data
        sta [C128_MEM_PTR],z
_wrd_done:
        rts

_wrd_saved_bank: .byte 0


; ============================================================
; MMU Register Writes
; ============================================================

; Write via $FF00-$FF04 (always accessible)
write_mmu_register:
        lda p4_addr_lo
        cmp #$00
        beq wmmu_cr
        cmp #$01
        beq wmmu_lcra          ; Load Config Reg A (write = load PCR-A into CR)
        cmp #$02
        beq wmmu_lcrb
        cmp #$03
        beq wmmu_lcrc
        cmp #$04
        beq wmmu_lcrd
        rts

wmmu_cr:
        ; Write to Configuration Register - immediate effect
        lda c128_saved_data
        sta mmu_cr
        jsr mmu_update_derived
        rts

wmmu_lcra:
        ; Any write to $FF01 loads PCR-A into CR
        lda mmu_pcr_a
        sta mmu_cr
        jsr mmu_update_derived
        rts

wmmu_lcrb:
        lda mmu_pcr_b
        sta mmu_cr
        jsr mmu_update_derived
        rts

wmmu_lcrc:
        lda mmu_pcr_c
        sta mmu_cr
        jsr mmu_update_derived
        rts

wmmu_lcrd:
        lda mmu_pcr_d
        sta mmu_cr
        jsr mmu_update_derived
        rts

; Write via $D500-$D50B (I/O mapped)
write_mmu_d500:
        lda p4_addr_lo
        cmp #$00
        beq wmmu_cr
        cmp #$01
        beq wmmu_d501
        cmp #$02
        beq wmmu_d502
        cmp #$03
        beq wmmu_d503
        cmp #$04
        beq wmmu_d504
        cmp #$05
        beq wmmu_d505
        cmp #$06
        beq wmmu_d506
        cmp #$07
        beq wmmu_d507
        cmp #$08
        beq wmmu_d508
        cmp #$09
        beq wmmu_d509
        cmp #$0A
        beq wmmu_d50a
        rts                     ; $D50B is read-only (version)

wmmu_d501:
        lda c128_saved_data
        sta mmu_pcr_a
        rts
wmmu_d502:
        lda c128_saved_data
        sta mmu_pcr_b
        rts
wmmu_d503:
        lda c128_saved_data
        sta mmu_pcr_c
        rts
wmmu_d504:
        lda c128_saved_data
        sta mmu_pcr_d
        rts
wmmu_d505:
        lda c128_saved_data
        sta mmu_mcr
        ; Check for C64 mode switch
        and #$40
        bne wmmu_d505_done
        ; TODO: Switch to C64 mode
wmmu_d505_done:
        rts
wmmu_d506:
        lda c128_saved_data
        sta mmu_rcr
        jsr mmu_update_shared
        rts
wmmu_d507:
        lda c128_saved_data
        sta mmu_p0l
        rts
wmmu_d508:
        lda c128_saved_data
        sta mmu_p0h
        rts
wmmu_d509:
        lda c128_saved_data
        sta mmu_p1l
        rts
wmmu_d50a:
        lda c128_saved_data
        sta mmu_p1h
        rts


; ============================================================
; VIC-II Register Writes ($D000-$D03F)
; ============================================================
write_vic_register:
        lda p4_addr_lo
        and #$3F
        tax

        ; Store to shadow
        lda c128_saved_data
        sta vic_regs,x

        ; Handle side effects
        cpx #$11
        beq _wv_d011
        cpx #$16
        beq _wv_d016
        cpx #$18
        beq _wv_d018
        cpx #$19
        beq _wv_d019
        cpx #$20
        beq _wv_d020
        cpx #$21
        beq _wv_d021
        ; Other VIC registers: also write to real VIC-IV
        ; (sprite regs, scroll, etc.)
        ;lda c128_saved_data
        ;sta $D000,x
        ;rts
        ; Other VIC registers: also write to real VIC-IV
        ; (sprite regs, scroll, etc.)
        cpx #$2F
        beq _wv_ignore
        cpx #$30
        beq _wv_ignore
        lda c128_saved_data
        sta $D000,x
        rts
_wv_ignore:
        rts

_wv_d011:
        ; $D011: screen control
        lda c128_saved_data
        sta $D011
        rts

_wv_d016:
        ; $D016: screen control 2
        lda c128_saved_data
        sta $D016
        rts

_wv_d018:
        ; $D018: VIC-II memory control register
        ; Bits 4-7: Screen base offset (x $0400)
        ; Bits 1-3: Charset base offset (x $0800)
        ;
        ; VIC-II CHARACTER ROM SHADOW:
        ; In VIC bank 0 ($0000-$3FFF) or bank 2 ($8000-$BFFF),
        ; charset offsets $1000-$1FFF and $1800-$1FFF read from
        ; character ROM instead of RAM. This is a VIC-II hardware
        ; feature. We emulate this by pointing CHARPTR at our
        ; chargen ROM in MEGA65 bank 0 at $0A000.

        ; --- Update CHARPTR ---
        lda c128_saved_data
        and #$0E                ; Isolate bits 3-1
        asl
        asl                     ; A = charset offset high byte
        sta _d018_char_hi

        ; Check for character ROM shadow: offset $10 or $18
        cmp #$10
        beq _d018_charrom
        cmp #$18
        beq _d018_charrom

        ; Not a char ROM shadow address - use RAM
        lda #$00
        sta $D068
        lda _d018_char_hi
        sta $D069
        lda #BANK_RAM0
        sta $D06A
        jmp _d018_screen

;_d018_charrom:
;        ; Point at chargen in bank 1 at $10000
;        lda #$00
;        sta $D068
;        lda _d018_char_hi
;        sec
;        sbc #$10                ; $10 -> $00, $18 -> $08
;        sta $D069
;        lda #$01
;        sta $D06A

_d018_charrom:
        ; Point at chargen at $9000 in bank 0
        ; $D018 charset offset $10 -> $9000 (uppercase/graphics)
        ; $D018 charset offset $18 -> $9800 (lowercase/uppercase)
        lda #$00
        sta $D068
        lda _d018_char_hi
        clc
        adc #$80                ; $10 -> $90, $18 -> $98
        sta $D069
        lda #$00
        sta $D06A

_d018_screen:
        ; --- Update SCRNPTR ---
        lda c128_saved_data
        lsr
        lsr
        and #$3C
        sta _d018_scrn_hi

        lda #$00
        sta $D060
        lda _d018_scrn_hi
        sta $D061
        lda #BANK_RAM0
        sta $D062

        ; Debug: log D018 writes
        lda c128_saved_data
        sta $7750               ; raw D018 value
        lda _d018_scrn_hi
        sta $7751               ; computed screen hi byte
        lda _d018_char_hi
        sta $7752               ; computed char hi byte

        rts

_d018_char_hi: .byte 0
_d018_scrn_hi: .byte 0

_wv_d019:
        ; $D019: IRQ flag register - write 1 to acknowledge
        ; Writing a 1 bit clears that flag
        lda c128_saved_data
        and vic_regs+$19
        eor vic_regs+$19
        ; Recalculate bit 7: set if any remaining flags match enable mask
        and #$0F                ; keep only individual flags (bits 0-3)
        sta p4_tmp
        lda vic_regs+$1A
        and p4_tmp              ; AND with enable mask
        beq _wv_d019_no_irq
        ; Still have active IRQ sources
        lda p4_tmp
        ora #$80
        sta vic_regs+$19
        rts
_wv_d019_no_irq:
        lda p4_tmp
        sta vic_regs+$19       ; bit 7 clear
        lda #0
        sta p4_irq_pending
        rts

_wv_d020:
        ; $D020: Border color
        lda c128_saved_data
        and #$0F
        sta vic_regs+$20
        sta $D020
        rts

_wv_d021:
        ; $D021: Background color
        lda c128_saved_data
        and #$0F
        sta vic_regs+$21
        sta $D021
        rts


; ============================================================
; CIA1 Register Writes ($DC00-$DC0F)
; ============================================================
write_cia1_register:
        lda p4_addr_lo
        and #$0F
        tax

        lda c128_saved_data
        sta cia1_regs,x

        ; $DC00: Keyboard column select - write to real CIA
        cpx #$00
        beq _wc1_dc00
        ; $DC04: Timer A latch low
        cpx #$04
        beq _wc1_ta_lo
        ; $DC05: Timer A latch high
        cpx #$05
        beq _wc1_ta_hi
        ; $DC0D: ICR mask
        cpx #$0D
        beq _wc1_icr
        ; $DC0E: Timer A control
        cpx #$0E
        beq _wc1_ta_ctrl
        rts

_wc1_dc00:
        lda c128_saved_data
        sta $DC00
        rts

_wc1_ta_lo:
        lda c128_saved_data
        sta cia1_timer_a_latch_lo
        rts

_wc1_ta_hi:
        ; Writing high byte also reloads timer if stopped
        lda c128_saved_data
        sta cia1_timer_a_latch_hi
        ; If timer not running, reload counter from latch
        lda cia1_timer_a_ctrl
        and #$01
        bne +
        lda cia1_timer_a_latch_lo
        sta cia1_timer_a_lo
        lda cia1_timer_a_latch_hi
        sta cia1_timer_a_hi
+       rts

_wc1_icr:
        ; $DC0D write: bit 7 = set/clear flag
        ; If bit 7 = 1, set the bits specified by bits 0-4
        ; If bit 7 = 0, clear the bits specified by bits 0-4
        lda c128_saved_data
        bmi _wc1_icr_set
        ; Clear: mask &= ~data
        eor #$FF
        and cia1_icr_mask
        sta cia1_icr_mask
        rts
_wc1_icr_set:
        ; Set: mask |= data (bits 0-4 only)
        and #$1F
        ora cia1_icr_mask
        sta cia1_icr_mask
        rts

_wc1_ta_ctrl:
        lda c128_saved_data
        sta cia1_timer_a_ctrl
        ; Bit 4 = force reload
        and #$10
        beq +
        lda cia1_timer_a_latch_lo
        sta cia1_timer_a_lo
        lda cia1_timer_a_latch_hi
        sta cia1_timer_a_hi
+       rts


; ============================================================
; CIA2 Register Writes ($DD00-$DD0F)
; ============================================================
write_cia2_register:
        lda p4_addr_lo
        and #$0F
        tax

        lda c128_saved_data
        sta cia2_regs,x

        ; $DD00: VIC bank select
        cpx #$00
        bne _wc2_not_00
        ; TODO: Update VIC bank selection
        rts
_wc2_not_00:
        rts


; ============================================================
; VDC Register Writes ($D600-$D601)
; ============================================================
write_vdc_register:
        lda p4_addr_lo
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
_wvdc_done:
        rts

_wvdc_index:
        ; $D600: Set VDC register index
        lda c128_saved_data
        and #$3F               ; 6-bit register index
        sta vdc_index
        rts

_wvdc_data:
        ; R31: Write byte to VDC RAM at address R18:R19, auto-increment
        ; VDC RAM is in ATTIC RAM at $8000000
        lda vdc_regs+19         ; address lo
        sta C128_MEM_PTR+0
        lda vdc_regs+18         ; address hi
        and #$3F                ; mask to 16KB
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK
        sta C128_MEM_PTR+2
        lda #VDC_RAM_ATTIC_HI
        sta C128_MEM_PTR+3
        ldz #0
        lda c128_saved_data
        sta [C128_MEM_PTR],z
        sta vdc_regs+31         ; also update shadow
        ; Auto-increment R18:R19
        inc vdc_regs+19
        bne +
        inc vdc_regs+18
+       rts

;_wvdc_word_count:
;        ; R30: Writing word count triggers block copy or fill
;        ; The VDC copies/fills (count+1) bytes starting at R18:R19
;        ; Bit 5 of R24 selects copy (0) vs fill (1)
;        ; For now: just advance R18:R19 by (count+1) so address is correct after
;        lda c128_saved_data
;        sta vdc_regs+30
;        ; Advance address by (value + 1)
;        clc
;        lda vdc_regs+19
;        adc c128_saved_data
;        sta vdc_regs+19
;        lda vdc_regs+18
;        adc #$00
;        sta vdc_regs+18
        ; +1 more
;        inc vdc_regs+19
;        bne +
;        inc vdc_regs+18
;+       rts


_wvdc_word_count:
        ; R30: Writing word count triggers block copy or fill
        ; The VDC copies/fills (count+1) bytes starting at R18:R19
        ; Bit 5 of R24 selects copy (0) vs fill (1)
        lda c128_saved_data
        sta vdc_regs+30

        ; Count = value + 1
        lda c128_saved_data
        clc
        adc #1
        sta _vdc_count
        lda #0
        adc #0
        sta _vdc_count+1

        ; Check bit 5 of R24: 1 = fill, 0 = copy
        lda vdc_regs+24
        and #$20
        beq _vdc_block_copy

        ; --- Block fill: fill (count) bytes with R31 value ---
        lda vdc_regs+31
        sta _vdc_fill_byte

_vdc_fill_loop:
        lda vdc_regs+19
        sta C128_MEM_PTR+0
        lda vdc_regs+18
        and #$3F
        sta C128_MEM_PTR+1
        lda #VDC_RAM_BANK
        sta C128_MEM_PTR+2
        lda #VDC_RAM_ATTIC_HI
        sta C128_MEM_PTR+3
        ldz #0
        lda _vdc_fill_byte
        sta [C128_MEM_PTR],z

        ; Increment R18:R19
        inc vdc_regs+19
        bne +
        inc vdc_regs+18
+
        ; Decrement count
        lda _vdc_count
        bne +
        dec _vdc_count+1
+       dec _vdc_count
        lda _vdc_count
        ora _vdc_count+1
        bne _vdc_fill_loop
        rts

_vdc_block_copy:
        ; --- Block copy: not implemented yet, just advance address ---
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

_vdc_count:   .word 0
_vdc_fill_byte: .byte 0

; ============================================================
; Utility: print_hex8 - Print byte in A as 2 hex chars
; ============================================================
print_hex8:
        pha
        lsr
        lsr
        lsr
        lsr
        tax
        lda _hex_chars,x
        jsr CHROUT
        pla
        and #$0F
        tax
        lda _hex_chars,x
        jsr CHROUT
        rts

_hex_chars:
        .text "0123456789abcdef"


; ============================================================
; Compatibility aliases for code that still uses P4 names
; (These will be removed as we update all files)
; ============================================================
P4MEM_Read      = C128_Read
P4MEM_ReadFast  = C128_ReadFast
P4MEM_Write     = C128_Write
P4MEM_Init      = C128_MemInit
P4MEM_InitVideo = C128_VideoInit
P4MEM_ClearRAM  = C128_MemClearRAM
BANK_RAM        = BANK_RAM0             ; Default RAM bank alias
P4_MEM_PTR      = C128_MEM_PTR          ; ZP pointer alias

; Aliases for Plus/4 variable names used in hooks/host
p4_file_op_active = c128_file_op_active
p4_video_mode     = c128_video_mode

; Stub variables referenced by hooks (will be replaced)
p4_gfx_dirty:     .byte 0
p4_multicolor:    .byte 0
p4_host_bmp_on:   .byte 0
p4_bitmap_was_active: .byte 0
p4_screen_fill_pending: .byte 0

; Video stub routines referenced by hooks (will be replaced)
P4VID_DisableHostBitmap:
P4VID_GfxConfigChanged:
P4VID_CharsetChanged:
P4VID_UpdateCursor:
P4VID_Frame:
P4VID_InitPalette:
P4_CheckCursorKeys:
        rts

; Aliases for TED references in existing code (temporary)
ted_regs        = vic_regs              ; VIC regs in place of TED
ted_timer1_lo:  .byte $FF
ted_timer1_hi:  .byte $FF
ted_raster_lo:  .byte 0
ted_raster_hi:  .byte 0
ted_cycle_accum: .byte 0