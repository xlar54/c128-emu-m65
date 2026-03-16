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
;     $10000-$11FFF: RESERVED - C65 KERNAL DOS variables (do not use!)
;     $12000-$127FF: KERNAL $F800-$FFFF relocated (2KB, avoids color RAM window)
;     $14000-$17FFF: BASIC LO ROM  (C128 $4000-$7FFF)
;     $18000-$1BFFF: BASIC HI ROM  (C128 $8000-$BFFF)
;     $1C000-$1F7FF: KERNAL ROM    (C128 $C000-$F7FF, 14KB accessible)
;     $1F800-$1FFFF: (blocked by MEGA65 color RAM window)
;   Bank 2 ($20000-$2FFFF): C65 ROM (write-protect off after boot)
;     $2A000-$2BFFF: Character ROM (chargen, 8KB, loaded last)
;   Bank 4 ($40000-$4FFFF): C128 RAM Bank 0 (64KB)
;   Bank 5 ($50000-$5FFFF): VDC display RAM + 80-col screen + color save buffers
;   Attic ($8000000-$800FFFF): C128 RAM Bank 1 (64KB, DMA-only access)
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
BANK_RAM1   = $00                       ; C128 RAM bank 1 in attic ($8000000)
BANK_RAM1_MB = $80                      ; Megabyte byte for RAM bank 1 (attic)
BANK_RAM0_MB = $00                      ; Megabyte byte for RAM bank 0 (chip RAM)

; KERNAL $F800-$FFFF is relocated to $12000-$127FF in bank 1
; because $1F800-$1FFFF is hijacked by the MEGA65 color RAM window
; KERNAL_F800_RELOC_HI = $20           ; (unused - code uses hardcoded sbc #$D8)

; Character ROM location in bank 2
; chargen.bin (8KB) loaded to bank 2 ($2A000-$2BFFF)
; First 4KB ($2A000-$2AFFF) = C64 charset
; Second 4KB ($2B000-$2BFFF) = C128 charset
;
; NOTE: Chargen is ONLY in bank 2. It is NOT copied to bank 0.
; $08000-$09FFF in bank 0 is emulator code - do NOT write there.
;
; Emulated CPU reads from $D000-$DFFF use read_from_chargen which
; reads from bank 2 via 32-bit pointer.
;
; VIC-IV CHARPTR points to $009000 (bank 0) for 40-col display.
; This works because the VIC-IV hardware reads through the ROM
; shadow mapping, which makes the chargen data visible at $9000
; in bank 0 even though the physical RAM there contains code.
; DO NOT change CHARPTR to bank 1 - it breaks VDC text rendering.
;
; The _d018_charrom handler also uses bank 0 addresses ($9000/$9800)
; for the same reason. DO NOT change it to bank 1.
CHARGEN_BASE = $2A000                   ; MEGA65 flat address of chargen ROM (bank 2)
CHARGEN_BANK = $02                      ; Bank byte for chargen
CHARGEN_OFF  = $A000                    ; Offset within bank for chargen
SID_BASE     = $D400                    ; Base address for SID registers (passed through to real hardware)

; Low RAM buffer in bank 0 host RAM, DMA-synced from bank 4 before hooks
LOW_RAM_BUFFER  = $A000                 ; 4KB - C128 low RAM $0000-$0FFF cache
; Synced from bank 4 via DMA before hooks that read it.
; Hook writes use c128_write_status / c128_write_zp_x to update both places.
; File table writes ($0500+) synced back via C128Hook_SyncLowRAMBack.

; Screen base in low RAM buffer
C128_SCREEN_BASE = $054000  ; MEGA65 80-col screen in bank 5 (display buffer area)

; 32-bit pointer for direct memory access
C128_MEM_PTR    = $F0                   ; 4 bytes at $F0-$F3
c128_saved_data = $F4                   ; Saved data byte at write entry

INITIAL_VIDEO_MODE = $37                ; initial mode - $B7 = 40 col / $37 = 80 col
                                        ; Bit 0 = 1: 8502 active (not Z80)

; ============================================================
; MMU Register State
; ============================================================
mmu_cr:          .byte $00     ; Configuration Register ($D500/$FF00)
                               ; Power-on default: $00 (all ROMs, bank 0, I/O)
mmu_pcr_a:       .byte $00     ; Pre-configuration Register A ($D501/$FF01)
mmu_pcr_b:       .byte $00     ; Pre-configuration Register B ($D502/$FF02)
mmu_pcr_c:       .byte $00     ; Pre-configuration Register C ($D503/$FF03)
mmu_pcr_d:       .byte $00     ; Pre-configuration Register D ($D504/$FF04)
mmu_mcr:         .byte INITIAL_VIDEO_MODE     ; Mode Configuration Register ($D505)
                               ; Bit 7: 40/80 key (0=pressed = 80 col mode)
                               ; Bit 6: C64 mode (0=C128)
                               ; Bit 5: EXROM (1=C128 mode)
                               ; Bit 4: GAME (1=C128 mode)
                               ; Bits 2-1: always set
                               ; Bit 0: CPU select (1=8502 active)
                               ; $B7 = 10110111 = 40col, C128, EXROM=1, GAME=1, 8502
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
mmu_ram_bank:    .byte BANK_RAM0  ; MEGA65 bank for current RAM bank
mmu_ram_mb:      .byte BANK_RAM0_MB ; Megabyte byte for current RAM bank
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

; SID registers are passed through to real hardware

; ============================================================
; Video state
; ============================================================
c128_file_op_active: .byte 0

; ============================================================
; C128_MemInit - Initialize memory system
; ============================================================
C128_MemInit:

        ; Initialize dedicated stack pointer (c128_stack_ptr $E8-$EB)
        ; Byte +0 stays $00 permanently; push/pull use Z register = SP
        lda #$00
        sta c128_stack_ptr+0    ; Always 0 - Z register provides offset
        lda #$01
        sta c128_stack_ptr+1    ; Page $01 (stack page)
        lda #BANK_RAM0
        sta c128_stack_ptr+2    ; Always bank 4
        lda #$00
        sta c128_stack_ptr+3    ; Megabyte 0

        ; Initialize dedicated ZP pointer (c128_zp_ptr $14-$17)
        lda #$00
        sta c128_zp_ptr+0       ; Will be set before each use
        sta c128_zp_ptr+1       ; Page $00
        sta c128_zp_ptr+3       ; Megabyte 0
        lda #BANK_RAM0
        sta c128_zp_ptr+2       ; Bank 4 

        ; Initialize C128_RAM_PTR ($F8-$FB) - dedicated fast path pointer
        ; This pointer is ONLY used by read/write_data_fast macros
        ; and is NEVER modified by anything else.
        lda #$00
        sta C128_RAM_PTR+0      ; Low byte always 0 (Z used as offset)
        sta C128_RAM_PTR+3      ; Megabyte always 0
        lda #BANK_RAM0
        sta C128_RAM_PTR+2      ; Bank always 4 (C128 RAM bank 0)

        ; Set MMU to default power-on state
        lda #$00
        sta mmu_cr              ; All ROMs visible, bank 0, I/O
        sta mmu_pcr_a
        sta mmu_pcr_b
        sta mmu_pcr_c
        sta mmu_pcr_d
        lda #INITIAL_VIDEO_MODE                ; 80-col, C128 mode, EXROM=1, GAME=1, 8502 active
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
        ; Silence all SID voices
        ldx #$18
        lda #$00
_snd_clr:
        sta SID_BASE,x
        dex
        bpl _snd_clr
        
        ; Set default volume to 0
        lda #$00
        sta SID_BASE+$18        ; Filter/volume register

        ; Clear C128 RAM bank 0 (already done in main.asm via DMA)
        ; Clear C128 RAM bank 1 (already done in main.asm via DMA)

        ; Clear local low RAM buffer
        jsr clear_low_ram_buffer

        ; Re-derive MMU state AFTER clear_low_ram_buffer
        ; (DMA may have zeroed our variables if they overlap $A000-$AFFF)
        jsr mmu_update_derived

        ; Initialize 40/80 col color save buffers
        jsr init_color_buffers

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

        ; Disable HOTREG so VIC-II register writes don't auto-update VIC-IV
        lda #$80
        trb $D05D               ; Clear bit 7 = disable HOTREG

        ; Point screen at bank 5 + $4000 = $054000
        lda #$00
        sta $D060               ; SCRNPTR[7:0]   = $00
        lda #$40
        sta $D061               ; SCRNPTR[15:8]  = $40
        lda #$05
        sta $D062               ; SCRNPTR[23:16] = $05
        lda #$00
        sta $D063               ; SCRNPTR[31:24] = $00 -> $054000

        ; Point charset at C128 character ROM in bank 2
        lda #$00
        sta $D068               ; CHARPTR LSB
        lda #>CHARGEN_OFF
        sta $D069               ; CHARPTR byte 1
        lda #CHARGEN_BANK
        sta $D06A               ; CHARPTR byte 2 -> $02A000

        ; CHR16 OFF (standard 1-byte screen codes)
        lda #$d7
        trb $d054

        ; Color RAM pointer at default $0FF80000
        ; We access color RAM via 32-bit pointers (not $D800 window),
        ; so the window at $1F800-$1FFFF doesn't matter to us.
        ; KERNAL $F800-$FFFF must be relocated to $12000 to avoid it.
        lda #$00
        sta $D064               ; COLPTR byte 0
        lda #$00
        sta $D065               ; COLPTR byte 1

        ; --- Check 40/80 column mode ---
        lda mmu_mcr
        bmi _vi_40col           ; Bit 7 set = 40-column mode

        ; === 80-column mode ===
        ; H640 on for 80-column display
        lda #$80
        sta $D031

        ; Virtual row width = 80
        lda #80
        sta $D058
        lda #0
        sta $D059

        ; 80-column colors from VDC register 26 (border matches background)
        lda vdc_regs+26
        and #$0F
        tax
        lda vdc_to_vic_color,x
        sta $D020
        sta $D021

        lda #1
        sta vdc_mode_active     ; Flag: VDC rendering active
        rts

_vi_40col:
        ; === 40-column mode ===
        ; H640 off, VIC-III attributes off
        lda #$00
        sta $D031

        ; Virtual row width = 40
        lda #40
        sta $D058
        lda #0
        sta $D059

        ; 40-column colors: light green border, dark grey background
        lda #13
        sta $D020               ; border = light green
        lda #11
        sta $D021               ; background = dark grey

        lda #0
        sta vdc_mode_active     ; Flag: VDC rendering inactive
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
; Color RAM save/restore buffers in bank 5:
;   $05A000 = 40-col color save (1000 bytes)
;   $05A800 = 80-col color save (2000 bytes)
; Color RAM is at $0FF80000 = MB $FF, bank $08, addr $0000
; ============================================================
COLOR_40_ADDR = $A000
COLOR_40_BANK = $05
COLOR_40_MB   = $00
COLOR_80_ADDR = $A800
COLOR_80_BANK = $05
COLOR_80_MB   = $00

; ============================================================
; init_color_buffers - Fill 40/80 col color save buffers
; 40-col: light green (13) x 1000 at $05A000 (bank 5)
; 80-col: white (1) x 2000 at $05A800 (bank 5)
; ============================================================
init_color_buffers:
        ; Fill 40-col buffer with light green (13)
        lda #$00
        sta $D707
        .byte $80, $00
        .byte $81, COLOR_40_MB  ; dst MB = $00
        .byte $00
        .byte $03               ; fill
        .word 1000
        .word 13                ; fill with 13 (light green)
        .byte $00
        .word COLOR_40_ADDR     ; dst = $A000
        .byte COLOR_40_BANK     ; dst bank $05 -> $05A000
        .byte $00
        .word $0000
        ; Fill 80-col buffer with white (1)
        lda #$00
        sta $D707
        .byte $80, $00
        .byte $81, COLOR_80_MB  ; dst MB = $00
        .byte $00
        .byte $03               ; fill
        .word 2000
        .word 1                 ; fill with 1 (white)
        .byte $00
        .word COLOR_80_ADDR     ; dst = $A800
        .byte COLOR_80_BANK     ; dst bank $05 -> $05A800
        .byte $00
        .word $0000
        rts

; ============================================================
; display_show_40col - Display-only: show 40-col VIC screen
; Does NOT change vdc_mode_active (emulation state unchanged)
; ============================================================
; ============================================================
; ensure_viciv_regs - Ensure VIC-IV registers are accessible
; Unlocks VIC-IV mode and disables HOTREG.
; Must be called before any write to $D060+ registers.
; ============================================================
ensure_viciv_regs:
        lda #$47
        sta $D02F
        lda #$53
        sta $D02F
        lda #$80
        trb $D05D               ; Clear bit 7 = disable HOTREG
        rts

display_show_40col:
        jsr ensure_viciv_regs
        ; Save 80-col colors: $0FF80000 -> $05A800 (bank 5, 2000 bytes)
        lda #$00
        sta $D707
        .byte $80, $FF          ; src MB = $FF (color RAM)
        .byte $81, COLOR_80_MB  ; dst MB = $00 (chip RAM)
        .byte $00
        .byte $00               ; copy
        .word 2000
        .word $0000             ; src addr ($0FF80000)
        .byte $08               ; src bank $08 in MB $FF = color RAM
        .word COLOR_80_ADDR     ; dst = $A800
        .byte COLOR_80_BANK     ; dst bank $05 -> $05A800
        .byte $00
        .word $0000
        ; Restore 40-col colors: $05A000 (bank 5) -> $0FF80000 (1000 bytes)
        lda #$00
        sta $D707
        .byte $80, COLOR_40_MB  ; src MB = $00 (chip RAM)
        .byte $81, $FF          ; dst MB = $FF (color RAM)
        .byte $00
        .byte $00               ; copy
        .word 1000
        .word COLOR_40_ADDR     ; src = $A000
        .byte COLOR_40_BANK     ; src bank $05 -> $05A000
        .word $0000             ; dst addr ($0FF80000)
        .byte $08               ; dst bank $08 in MB $FF = color RAM
        .byte $00
        .word $0000

        lda #$00
        sta $D031               ; H640 off

        ; If VDC was in bitmap mode, restore VIC-IV to text mode
        ; Clear vdc_bitmap_active so switching back to 80-col
        ; re-runs the full bitmap VIC-IV setup
        lda vdc_bitmap_active
        beq _ds40_no_bmp
        lda #$1B
        sta $D011               ; bitmap off
        ; Restore CHARPTR via $D018 re-evaluation (uses bank 4 RAM)
        ; Just fall through to _ds40_no_bmp which calls write_vic_register
        lda #0
        sta vdc_bitmap_active
_ds40_no_bmp:

        lda #40
        sta $D058
        lda #0
        sta $D059
        ; Restore D011 from shadow
        lda vic_regs+$11
        sta $D011
        ; Reconfigure SCRNPTR and CHARPTR from current $D018 and VIC bank
        lda vic_regs+$18
        sta c128_saved_data
        lda #$18
        sta c128_addr_lo
        jsr write_vic_register

        ; Enable SPRPTR16 mode and point SPRPTRADR at our 16-byte
        ; pointer table in host RAM. In SPRPTR16 mode, sprite data
        ; address = pointer_value × 64 (no VIC bank involved),
        ; allowing sprites to reference data in bank 4 ($40000+).
        lda #<spr16_ptrs
        sta $D06C               ; SPRPTRADRLSB
        lda #>spr16_ptrs
        sta $D06D               ; SPRPTRADRMSB
        lda #$80                ; bit 7 = SPRPTR16 enable, bank = 0
        sta $D06E               ; SPRPTRBNK + SPRPTR16

        ; Restore 40-col border/background from VIC-II shadow registers
        lda vic_regs+$20
        sta $D020
        lda vic_regs+$21
        sta $D021

        ; Restore sprites that were saved when switching to 80-col
        lda saved_sprite_enable
        sta $D015
        rts

saved_sprite_enable: .byte 0

; ============================================================
; display_show_80col - Display-only: show 80-col VDC screen
; Does NOT change vdc_mode_active (emulation state unchanged)
; ============================================================
display_show_80col:
        jsr ensure_viciv_regs
        ; Save 40-col colors: $0FF80000 -> $05A000 (bank 5, 1000 bytes)
        lda #$00
        sta $D707
        .byte $80, $FF          ; src MB = $FF (color RAM)
        .byte $81, COLOR_40_MB  ; dst MB = $00 (chip RAM)
        .byte $00
        .byte $00               ; copy
        .word 1000
        .word $0000             ; src addr ($0FF80000)
        .byte $08               ; src bank $08 in MB $FF = color RAM
        .word COLOR_40_ADDR     ; dst = $A000
        .byte COLOR_40_BANK     ; dst bank $05 -> $05A000
        .byte $00
        .word $0000
        ; Restore 80-col colors: $05A800 (bank 5) -> $0FF80000 (2000 bytes)
        lda #$00
        sta $D707
        .byte $80, COLOR_80_MB  ; src MB = $00 (chip RAM)
        .byte $81, $FF          ; dst MB = $FF (color RAM)
        .byte $00
        .byte $00               ; copy
        .word 2000
        .word COLOR_80_ADDR     ; src = $A800
        .byte COLOR_80_BANK     ; src bank $05 -> $05A800
        .word $0000             ; dst addr ($0FF80000)
        .byte $08               ; dst bank $08 in MB $FF = color RAM
        .byte $00
        .word $0000

        lda #$80
        sta $D031               ; H640 on
        lda #$1B
        sta $D011               ; Restore standard display: DEN on, 25 rows, no bitmap
        lda #80
        sta $D058
        lda #0
        sta $D059

        ; Save and disable sprites for 80-col display
        lda vic_regs+$15
        sta saved_sprite_enable
        lda #$00
        sta $D015
        ; Point SCRNPTR at bank 5 screen: $054000
        lda #$00
        sta $D060               ; SCRNPTR[7:0]
        lda #$40
        sta $D061               ; SCRNPTR[15:8]
        lda #$05
        sta $D062               ; SCRNPTR[23:16] = $05
        lda #$00
        sta $D063               ; -> $054000
        ; Force immediate VDC render (sets border/bg from VDC R26)
        lda #1
        sta vdc_screen_dirty
        sta vdc_attr_dirty
        jsr VDC_RenderFrame
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
        ldx #BANK_RAM1_MB
        bne _mmu_bank_set       ; always taken
_mmu_bank0:
        lda #BANK_RAM0
        ldx #BANK_RAM0_MB
_mmu_bank_set:
        sta mmu_ram_bank
        stx mmu_ram_mb

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

        ; Invalidate code cache so fetch8 picks up new ROM/RAM mapping
        lda #0
        sta c128_code_valid
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
; Input: c128_addr_hi (page number)
; Output: A = MEGA65 bank number (BANK_RAM0 or BANK_RAM1)
; Uses shared RAM logic - if address is in shared region,
; always returns BANK_RAM0 regardless of MMU bank select
; ============================================================
get_physical_bank:
        ; Check if in bottom shared region
        lda shared_bottom_on
        beq _gpb_check_top
        lda c128_addr_hi
        cmp shared_bottom_mask
        bcc _gpb_shared          ; Address < shared boundary -> bank 0

_gpb_check_top:
        ; Check if in top shared region
        lda shared_top_on
        beq _gpb_selected
        lda c128_addr_hi
        cmp shared_top_start
        bcs _gpb_shared          ; Address >= shared top start -> bank 0

_gpb_selected:
        ; Use the MMU-selected bank
        lda mmu_ram_bank
        ldx mmu_ram_mb
        rts

_gpb_shared:
        lda #BANK_RAM0
        ldx #BANK_RAM0_MB
        rts


; ============================================================
; C128_ReadFast - Fast path for memory reads
; Only handles simple RAM cases ($0000-$3FFF), falls through
; to full C128_Read for anything complex ($4000+)
; ============================================================
C128_ReadFast:
        lda c128_addr_hi
        cmp #$40
        bcs C128_Read           ; $4000+ needs full handler (ROM/IO possible)

        ; $0000-$3FFF: Almost always BANK_RAM0
        lda mmu_ram_bank
        cmp #BANK_RAM0
        bne _rf_need_bank_check

        ; Fast path: bank is RAM0, no shared check needed
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3      ; MB = 0 for bank 0
_rf_do_read:
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda c128_addr_hi
        sta C128_MEM_PTR+1
        ldz #0
        lda [C128_MEM_PTR],z
        rts

_rf_need_bank_check:
        ; Bank 1 selected - must check shared regions
        jsr get_physical_bank   ; A=bank, X=megabyte
        cpx #$00
        bne _rf_attic           ; MB != 0 -> attic RAM, use DMA
        sta C128_MEM_PTR+2
        jmp _rf_do_read
_rf_attic:
        ; Read from attic via DMA
        lda c128_addr_hi
        jsr attic_read_byte
        rts

; ============================================================
; C128_Read - Full memory read handler
; Input: c128_addr_hi:c128_addr_lo = C128 address
; Output: A = byte read
; ============================================================
C128_Read:
        lda c128_addr_hi

        ; $0000-$0FFF: RAM - read from physical bank
        ; (get_physical_bank handles shared RAM)
        cmp #$10
        bcs _rd_not_low
        ; Special case: $0000/$0001 = CPU port registers
        cmp #$00
        bne _rd_low_ram
        lda c128_addr_lo
        cmp #$02
        bcs _rd_low_ram
        jmp read_zp             ; Use port register handler for $00/$01
_rd_low_ram:
        jsr get_physical_bank
        sta C128_MEM_PTR+2
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda c128_addr_hi
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
        lda c128_addr_lo
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
        lda c128_addr_hi
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
        lda c128_addr_hi
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
        lda c128_addr_lo
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
        lda c128_addr_lo
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
        ; Color RAM at $D800-$DBFF (always 40-col VIC color reads)
        ; If displaying 80-col, read from 40-col save buffer instead
        lda display_showing_80
        bne _rd_color_from_buffer

        ; Displaying 40-col: read from live color RAM $0FF80000
        lda c128_addr_lo
        sta C128_MEM_PTR+0
        lda c128_addr_hi
        sec
        sbc #$D8                ; $D8->$00, $D9->$01, $DA->$02, $DB->$03
        sta C128_MEM_PTR+1
        lda #$F8
        sta C128_MEM_PTR+2
        lda #$0F
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        and #$0F                ; Color RAM is 4 bits
        rts

_rd_color_from_buffer:
        ; Displaying 80-col: read from 40-col save buffer in attic $8010000
        lda c128_addr_lo
        sta C128_MEM_PTR+0
        lda c128_addr_hi
        sec
        sbc #$D8
        clc
        adc #>COLOR_40_ADDR     ; + $00
        sta C128_MEM_PTR+1
        lda #COLOR_40_BANK      ; bank $01
        sta C128_MEM_PTR+2
        lda #COLOR_40_MB        ; MB $08
        sta C128_MEM_PTR+3
        ldz #0
        lda [C128_MEM_PTR],z
        and #$0F
        rts

_rd_cia1:
        jmp read_cia1_register

_rd_cia2:
        jmp read_cia2_register

_rd_not_io:
        ; $4000-$CFFF: Check ROM visibility
        lda c128_addr_hi
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
        lda c128_addr_hi
        cmp #$F8
        bcc read_from_rom_bank1
        ; $F800-$FFFF: read from relocated area at $12000
        ; Map $F8xx -> $20xx, $F9xx -> $21xx, etc.
        sec
        sbc #$D8                ; $F8->$20, $F9->$21, ..., $FF->$27
        sta C128_MEM_PTR+1
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda #BANK_ROM
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts

; Read from ROM bank 1: C128 address maps directly to MEGA65 bank 1
; Used for BASIC LO ($4000-$7FFF), BASIC HI ($8000-$BFFF),
; and KERNAL ($C000-$F7FF) — all at same offset in bank 1
read_from_basic_lo:
read_from_basic_hi:
read_from_rom_bank1:
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda c128_addr_hi
        sta C128_MEM_PTR+1
        lda #BANK_ROM           ; Bank 1
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts

; Read from Character ROM: C128 $D000-$DFFF -> MEGA65 $10000-$11FFF (bank 1)
; The char ROM is 8KB. C128 address $D000 maps to chargen offset $0000.
; MEGA65 address = $10000 + (addr - $D000)
; Since chargen is at $10000 in bank 1:
;   ptr = bank 1, high = (addr_hi - $D0), low = addr_lo
;
; NOTE: This is for emulated CPU reads only (e.g. PEEK($D000)).
; VIC-IV display uses CHARPTR at bank 0 $9000 (ROM shadow) - see VideoInit.
read_from_chargen:
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda c128_addr_hi
        sec
        sbc #$D0                ; Offset from $D000 base
        clc
        adc #>CHARGEN_OFF       ; + $A0 -> maps to $2A000 base
        sta C128_MEM_PTR+1
        lda #CHARGEN_BANK       ; Bank 2
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts

; ============================================================
; read_ram_direct - Read from physical RAM with shared logic
; ============================================================
read_ram_direct:
        lda mmu_ram_bank
        cmp #BANK_RAM0
        bne _rrd_need_bank
        sta C128_MEM_PTR+2
_rrd_do_read:
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda c128_addr_hi
        sta C128_MEM_PTR+1
        lda #$00
        sta C128_MEM_PTR+3
        ldz #$00
        lda [C128_MEM_PTR],z
        rts
_rrd_need_bank:
        jsr get_physical_bank   ; A=bank, X=megabyte
        cpx #$00
        bne _rrd_attic          ; MB != 0 -> attic RAM, use DMA
        sta C128_MEM_PTR+2
        jmp _rrd_do_read
_rrd_attic:
        lda c128_addr_hi
        jsr attic_read_byte
        rts


; ============================================================
; MMU Register Reads
; ============================================================

; Read MMU via $FF00-$FF04 (always visible)
read_mmu_register:
        lda c128_addr_lo
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
        lda c128_addr_lo
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
        lda c128_addr_lo
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
        ; $D019: IRQ flags - merge emulated raster IRQ with real collision IRQs
        lda $D019               ; read real hardware for collision bits
        and #$06                ; keep only collision flags
        ora vic_regs+$19        ; merge with emulated flags (raster, etc.)
        rts

_rv_sprite_collision:
        ; Sprite collision registers - read from real MEGA65 VIC-IV hardware
        ; The VIC-IV detects collisions in hardware and stores results
        ; in the real $D01E/$D01F registers. Reading clears them.
        lda $D000,x             ; read real hardware register
        rts


; ============================================================
; CIA1 Register Reads ($DC00-$DC0F)
; ============================================================
read_cia1_register:
        lda c128_addr_lo
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
        ; Normally return $FF (no keys) - keyboard input is handled
        ; by injecting PETSCII codes into C128 keyboard buffer.
        ; Exception: when $DC00 = $7F (column 7 selected), check
        ; RUN/STOP key via MEGA65 keyboard matrix scan so the
        ; KERNAL's RUN/STOP detection at $F63D works.
        lda $DC00               ; what column is selected?
        cmp #$7F                ; column 7? (%01111111)
        bne _rc1_pb_nokey

        ; Column 7 selected - scan RUN/STOP (col 7, row 7)
        lda #$07
        sta $D614               ; select column 7
        lda $D613               ; read rows
        and #$80                ; bit 7 = row 7 (RUN/STOP)
        bne _rc1_pb_nokey       ; bit set = not pressed
        ; RUN/STOP pressed - return with bit 7 clear
        lda #$7F                ; %01111111 = bit 7 clear
        rts

_rc1_pb_nokey:
        lda #$FF                ; no keys pressed
        rts

_rc1_icr:
        ; $DC0D: Interrupt control - cleared on read
        lda cia1_icr_data
        pha
        lda #0
        sta cia1_icr_data
        sta c128_irq_pending      ; Clear pending IRQ
        pla
        rts


; ============================================================
; CIA2 Register Reads ($DD00-$DD0F)
; ============================================================
read_cia2_register:
        lda c128_addr_lo
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

; ============================================================
; C128_Write - Full memory write handler
; Input: c128_addr_hi:c128_addr_lo = C128 address
;        c128_data = byte to write
; ============================================================
C128_Write:
        lda c128_data
        sta c128_saved_data

        lda c128_addr_hi

        ; $FF00-$FF04: ALWAYS MMU registers
        cmp #$FF
        bne _wr_not_ff
        lda c128_addr_lo
        cmp #$05
        bcc write_mmu_register  ; $FF00-$FF04 -> MMU
        ; $FF05-$FFFF: Write to RAM under ROM
        jmp write_ram_direct

_wr_not_ff:
        ; $D000-$DFFF: I/O or RAM
        lda c128_addr_hi
        cmp #$D0
        bcc _wr_not_io
        cmp #$E0
        bcs _wr_to_ram

        ; $D000-$DFFF: Check I/O visibility
        lda mmu_io_visible
        bne _wr_io_dispatch
        jmp write_ram_direct    ; I/O not visible, write to RAM

_wr_io_dispatch:
        lda c128_addr_hi
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
        lda c128_addr_lo
        and #$1F
        tax
        lda c128_saved_data
        sta $D400,x            ; Write directly to MEGA65 SID
        rts

_wr_mmu_io:
        ; $D500-$D50B: MMU via I/O
        lda c128_addr_lo
        cmp #$0C
        bcs _wr_to_ram          ; $D50C+ = open
        jmp write_mmu_d500

_wr_vdc:
        jmp write_vdc_register

_wr_color_ram:
        ; Color RAM $D800-$DBFF (always 40-col VIC color writes)
        ; If currently displaying 80-col, redirect to 40-col save buffer
        ; so we don't corrupt the live color RAM (used by 80-col display)
        lda display_showing_80
        bne _wr_color_to_buffer

        ; Displaying 40-col: write directly to color RAM $0FF80000
        lda c128_addr_lo
        sta C128_MEM_PTR+0
        lda c128_addr_hi
        sec
        sbc #$D8                ; $D8->$00, $D9->$01, $DA->$02, $DB->$03
        sta C128_MEM_PTR+1
        lda #$F8
        sta C128_MEM_PTR+2
        lda #$0F
        sta C128_MEM_PTR+3
        ldz #0
        lda c128_saved_data
        and #$0F                ; Color RAM is 4 bits
        sta [C128_MEM_PTR],z
        rts

_wr_color_to_buffer:
        ; Displaying 80-col: write to 40-col save buffer in attic $8010000
        lda c128_addr_lo
        sta C128_MEM_PTR+0
        lda c128_addr_hi
        sec
        sbc #$D8
        clc
        adc #>COLOR_40_ADDR     ; + $00
        sta C128_MEM_PTR+1
        lda #COLOR_40_BANK      ; bank $01
        sta C128_MEM_PTR+2
        lda #COLOR_40_MB        ; MB $08
        sta C128_MEM_PTR+3
        ldz #0
        lda c128_saved_data
        and #$0F
        sta [C128_MEM_PTR],z
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
; LOW_RAM_BUFFER sync is handled by forward DMA before hooks.
; ============================================================
write_ram_direct:
        ; Invalidate code cache if writing to current code page
        lda c128_addr_hi
        cmp c128_code_page_hi
        bne _wrd_no_inv
        lda #0
        sta c128_code_valid
_wrd_no_inv:

        ; Fast path: when bank 0 selected (99%+ of the time),
        ; skip get_physical_bank entirely
        lda mmu_ram_bank
        cmp #BANK_RAM0
        bne _wrd_need_bank
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3      ; MB = 0 for bank 0
_wrd_do_write:
        lda c128_addr_lo
        sta C128_MEM_PTR
        lda c128_addr_hi
        sta C128_MEM_PTR+1
        ldz #$00
        lda c128_saved_data
        sta [C128_MEM_PTR],z
_wrd_done:
        rts

_wrd_need_bank:
        jsr get_physical_bank   ; A=bank, X=megabyte
        cpx #$00
        bne _wrd_attic          ; MB != 0 -> attic RAM, use DMA
        sta C128_MEM_PTR+2
        jmp _wrd_do_write
_wrd_attic:
        lda c128_addr_hi
        jsr attic_write_byte
        rts


; ============================================================
; MMU Register Writes
; ============================================================

; Write via $FF00-$FF04 (always accessible)
write_mmu_register:
        lda c128_addr_lo
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
        lda c128_addr_lo
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
        ; D505 write: bit 7 is READ-ONLY (40/80 key), preserve it
        lda mmu_mcr
        and #$80                ; keep current bit 7
        sta c128_tmp
        lda c128_saved_data
        and #$7F                ; mask off bit 7 from written value
        ora c128_tmp            ; combine with preserved bit 7
        sta mmu_mcr
        ; Check for C64 mode switch
        and #$40
        bne wmmu_d505_done
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
        lda c128_addr_lo
        and #$3F
        tax

        ; Store to shadow
        lda c128_saved_data
        sta vic_regs,x

        ; Handle side effects
        cpx #$11
        beq _wv_d011
        cpx #$12
        beq _wv_d012
        cpx #$15
        beq _wv_d015
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
        ; Other VIC registers: write to real VIC-IV (sprite regs, scroll, etc.)
        ; Skip $D02F/$D030 (VIC-III/IV extended regs, not present on VIC-II)
        cpx #$2F
        beq _wv_ignore
        cpx #$30
        beq _wv_ignore
        ; Only write to real hardware for $D000-$D02E (standard VIC-II range)
        ; Writes above $D030 would hit VIC-IV extended registers (SCRNPTR etc.)
        cpx #$31
        bcs _wv_ignore          ; skip any register >= $31
        lda c128_saved_data
        sta $D000,x
        rts
_wv_ignore:
        rts

_wv_d011:
        ; $D011: screen control
        ; Bit 7 = raster compare high bit (write), current raster high bit (read)
        ; Bit 5 = BMM (bitmap mode), Bit 4 = DEN (display enable)
        ; Bit 3 = RSEL (row select), Bits 2-0 = YSCROLL
        lda c128_saved_data
        sta vic_regs+$11        ; Always update shadow

        ; Update raster compare high bit from bit 7
        and #$80
        beq _wv_d011_rc_lo
        lda #$01
_wv_d011_rc_lo:
        sta vic_raster_compare_hi

        lda c128_saved_data
        ldx vdc_mode_active
        bne _wv_d011_done       ; In 80-col mode, don't touch real VIC-IV
        sta $D011

        ; Check if bitmap mode changed - reconfigure CHARPTR
        ; In bitmap mode, CHARPTR must point at bitmap data
        ; In character mode, CHARPTR points at charset
        and #$20                ; isolate BMM bit
        cmp _d011_prev_bmm
        beq _wv_d011_done       ; no change
        sta _d011_prev_bmm

        ; BMM changed - reconfigure CHARPTR from current $D018
        lda vic_regs+$18
        sta c128_saved_data     ; set up for _d018 handler
        jmp _wv_d018_update     ; reconfigure CHARPTR/SCRNPTR

_wv_d011_done:
        rts

_d011_prev_bmm: .byte 0        ; previous BMM state

_wv_d015:
        ; $D015: Sprite enable register
        ; In 80-col mode, don't write to real VIC-IV (sprites hidden)
        ; Shadow is already updated at line 1681
        lda display_showing_80
        bne +
        lda c128_saved_data
        sta $D015
+       rts

_wv_d016:
        ; $D016: screen control 2
        lda c128_saved_data
        sta $D016
        rts

_wv_d012:
        ; $D012: Raster compare register (write = set compare value)
        ; On read, returns current raster line (handled in read_vic_register)
        lda c128_saved_data
        sta vic_raster_compare_lo
        ; Don't write to real MEGA65 $D012 - we handle raster compare internally
        rts

_wv_d018:
        ; $D018: VIC-II memory control register
        ; Bits 4-7: Screen base offset (x $0400)
        ; Bits 1-3: Charset base offset (x $0800)
        ;           In bitmap mode, bit 3 selects bitmap base ($0000 or $2000)
        ;
        ; In 80-col display mode, save to shadow only - don't update SCRNPTR/CHARPTR
        ; since VDC_RenderFrame handles the display
        lda display_showing_80
        bne _d018_shadow_only

_wv_d018_update:
        jsr ensure_viciv_regs
        ; Check if bitmap mode is active ($D011 bit 5)
        lda vic_regs+$11
        and #$20
        bne _d018_bitmap

        ; --- Character mode: CHARPTR = charset base ---
        ;
        ; VIC-II CHARACTER ROM SHADOW:
        ; In VIC bank 0 ($0000-$3FFF) or bank 2 ($8000-$BFFF),
        ; charset offsets $1000-$1FFF and $1800-$1FFF read from
        ; character ROM instead of RAM. This is a VIC-II hardware
        ; feature. We emulate this by pointing CHARPTR at the
        ; chargen ROM shadow in MEGA65 bank 0 at $09000.

        ; --- Update CHARPTR ---
        ; C128 VIC-IIe: character ROM shadow is always active.
        ; $D018 bit 1 selects which 2KB half of the 4KB chargen ROM:
        ;   bit 1 = 0 -> first 2KB at $02A000 (uppercase/graphics)
        ;   bit 1 = 1 -> second 2KB at $02A800 (lowercase/uppercase)
        lda c128_saved_data
        and #$02                ; isolate bit 1
        beq _d018_charset_lo
        lda #$A8                ; second 2KB: $02A800
        bra _d018_charset_set
_d018_charset_lo:
        lda #$A0                ; first 2KB: $02A000
_d018_charset_set:
        sta $D069
        lda #$00
        sta $D068
        lda #CHARGEN_BANK
        sta $D06A
        jmp _d018_screen

_d018_bitmap:
        ; --- Bitmap mode: CHARPTR = bitmap data base ---
        ; $D018 bit 3: 0 = bitmap at VIC bank+$0000, 1 = bitmap at VIC bank+$2000
        lda c128_saved_data
        and #$08                ; isolate bit 3
        beq _d018_bmp_lo
        ; Bit 3 set: bitmap at VIC bank + $2000
        lda #$00
        sta $D068
        lda #$20
        clc
        adc vic_bank_base
        sta $D069
        lda #BANK_RAM0
        sta $D06A
        jmp _d018_screen
_d018_bmp_lo:
        ; Bit 3 clear: bitmap at VIC bank + $0000
        lda #$00
        sta $D068
        lda vic_bank_base
        sta $D069
        lda #BANK_RAM0
        sta $D06A

_d018_screen:
        ; --- Update SCRNPTR ---
        ; Screen addr = VIC bank base + ($D018 bits 7-4) * $0400
        lda c128_saved_data
        lsr
        lsr
        and #$3C
        clc
        adc vic_bank_base       ; add VIC bank offset
        sta _d018_scrn_hi

        lda #$00
        sta $D060
        lda _d018_scrn_hi
        sta $D061
        lda #BANK_RAM0
        sta $D062
        lda #$00
        sta $D063               ; megabyte = 0

        rts

_d018_shadow_only:
        ; 80-col mode: just save to shadow, don't touch VIC-IV
        rts


_d018_scrn_hi: .byte 0
vic_bank_base: .byte 0            ; VIC bank base high byte ($00/$40/$80/$C0)
vic_bank_spr:  .byte 0            ; VIC bank SPRPTR16 high byte offset ($00/$01/$02/$03)
vic_bank_has_charrom: .byte 1     ; 1=char ROM shadow visible (C128: always, unless CHAREN set)

_wv_d019:
        ; $D019: IRQ flag register - write 1 to acknowledge
        ; Writing a 1 bit clears that flag
        ; Also acknowledge on real VIC-IV hardware using ASL (RMW)
        ; which triggers the MEGA65's write-back hack to clear flags
        asl $D019
        lda c128_saved_data
        and vic_regs+$19
        eor vic_regs+$19
        ; Recalculate bit 7: set if any remaining flags match enable mask
        and #$0F                ; keep only individual flags (bits 0-3)
        sta c128_tmp
        lda vic_regs+$1A
        and c128_tmp              ; AND with enable mask
        beq _wv_d019_no_irq
        ; Still have active IRQ sources
        lda c128_tmp
        ora #$80
        sta vic_regs+$19
        rts
_wv_d019_no_irq:
        lda c128_tmp
        sta vic_regs+$19       ; bit 7 clear
        lda #0
        sta c128_irq_pending
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
        lda c128_addr_lo
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
        lda c128_addr_lo
        and #$0F
        tax

        lda c128_saved_data
        sta cia2_regs,x

        ; $DD00: VIC bank select
        cpx #$00
        bne _wc2_not_00
        ; Bits 0-1 select VIC bank (inverted):
        ;   %11 = bank 0 ($0000), %10 = bank 1 ($4000)
        ;   %01 = bank 2 ($8000), %00 = bank 3 ($C000)
        ; Compute VIC bank base high byte for MEGA65 addressing
        lda c128_saved_data
        and #$03
        eor #$03                ; invert: 3->0, 2->1, 1->2, 0->3
        ; Multiply by $40 to get base high byte ($00/$40/$80/$C0)
        asl
        asl
        asl
        asl
        asl
        asl
        sta vic_bank_base       ; high byte of VIC bank within C128 RAM
        ; Compute sprite pointer offset for SPRPTR16 mode
        ; SPRPTR16 address = pointer * 64, so bank offset = vic_bank_base * 256 / 64
        ; = vic_bank_base / 64 * 256 = vic_bank_base >> 6
        ; $00->$00, $40->$01, $80->$02, $C0->$03
        lsr
        lsr
        lsr
        lsr
        lsr
        lsr
        sta vic_bank_spr        ; SPRPTR16 high byte offset for VIC bank
        ; C128 VIC-IIe: character ROM shadow is visible in ALL four VIC banks
        ; (unlike C64 VIC-II which only has it in banks 0 and 2).
        ; The CHAREN bit ($01 bit 2) controls whether it's enabled.
        ; Default is enabled (CHAREN=0), so always set has_charrom=1.
        lda #1
        sta vic_bank_has_charrom
        ; Re-run $D018 update with new bank by simulating a write
        lda vic_regs+$18
        sta c128_saved_data
        lda #$18
        sta c128_addr_lo
        jsr write_vic_register
        rts
_wc2_not_00:
        rts


; ============================================================
; ============================================================
; Aliases used by hooks and host code
; ============================================================
BANK_RAM        = BANK_RAM0             ; Default RAM bank alias

; Stub routines (bitmap/graphics not yet implemented for C128)
C128Vid_DisableHostBitmap:
        rts

; ============================================================
; vic_print_char - Print a screen code at the 40-col cursor
;
; Input: A = screen code (not PETSCII)
; Uses C128 ZP: $E0-$E1 (screen line base), $E2-$E3 (color line base)
;               $EC (cursor column), $EB (cursor row)
;               $E4 (bottom margin), $E6 (left margin)
;               $F1 (current text color)
; Clobbers: A, X, Y, Z
; ============================================================
vic_print_char:
        sta _vipc_char

        ; Read screen line base $E0-$E1 from C128 ZP
        lda #$E0
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z     ; $E0 = screen line lo
        sta _vipc_line_lo
        ldz #1
        lda [c128_zp_ptr],z     ; $E1 = screen line hi
        sta _vipc_line_hi

        ; Read cursor column from $EC
        lda #$EC
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        sta _vipc_col

        ; Screen address = $E0:$E1 + $EC, in bank 4
        lda _vipc_line_lo
        clc
        adc _vipc_col
        sta C128_MEM_PTR+0
        lda _vipc_line_hi
        adc #0
        sta C128_MEM_PTR+1
        lda #BANK_RAM0
        sta C128_MEM_PTR+2
        lda #$00
        sta C128_MEM_PTR+3
        ldz #0
        lda _vipc_char
        sta [C128_MEM_PTR],z

        ; Advance column $EC
        inc _vipc_col
        lda _vipc_col
        cmp #40
        bcc _vipc_update_col
        ; Wrap: column back to left margin, advance row
        lda #$E6
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z     ; left margin
        sta _vipc_col
        ; Advance row $EB
        lda #$EB
        sta c128_zp_ptr+0
        lda [c128_zp_ptr],z
        inc a
        ldx #$EB
        jsr c128_write_zp_x
        ; Update screen/color line base for new row
        ; $E0:$E1 += 40
        lda _vipc_line_lo
        clc
        adc #40
        tax
        lda _vipc_line_hi
        adc #0
        ldx #$E1
        jsr c128_write_zp_x
        txa
        ldx #$E0
        jsr c128_write_zp_x
        ; $E2:$E3 += 40 (color line)
        lda #$E2
        sta c128_zp_ptr+0
        ldz #0
        lda [c128_zp_ptr],z
        clc
        adc #40
        tax
        ldz #1
        lda [c128_zp_ptr],z
        adc #0
        pha
        txa
        ldx #$E2
        jsr c128_write_zp_x
        pla
        ldx #$E3
        jsr c128_write_zp_x

_vipc_update_col:
        lda _vipc_col
        ldx #$EC
        jsr c128_write_zp_x
        rts

_vipc_char:    .byte 0
_vipc_line_lo: .byte 0
_vipc_line_hi: .byte 0
_vipc_col:     .byte 0

; ============================================================
; vic_scroll_up - Scroll 40-col screen up one row
; ============================================================
vic_scroll_up:
        ; Scroll screen RAM (24 rows * 40 = 960 bytes)
        #dma_copy_chip $04, $0428, $04, $0400, 960

        ; Scroll color RAM
        #dma_copy $FF, $08, $0028, $FF, $08, $0000, 960

        ; Clear bottom row of screen with spaces
        #dma_fill_chip $04, $07C0, 40, $20

        ; Clear bottom row of color with current text color from ZP $F1
        lda #$F1
        sta c128_zp_ptr+0
        ldz #0
        lda[c128_zp_ptr],z
        and #$0F                ; Mask color bits (Color RAM is 4 bits)
        sta _vsc_color
        
        lda #<960
        sta C128_MEM_PTR+0
        lda #>960
        sta C128_MEM_PTR+1
        lda #$F8
        sta C128_MEM_PTR+2
        lda #$0F
        sta C128_MEM_PTR+3
        ldz #0
        lda _vsc_color
_vsc_loop:
        sta [C128_MEM_PTR],z
        inz
        cpz #40
        bne _vsc_loop
        rts

_vsc_color: .byte 0


; VDC_RenderFrame, VDC_UpdateCursor, vdc_to_vic_color, and
; vdc_color_ptr are now in c128_vdc.asm