// The ideas used in this code are based off bootjacker:
// http://oneweekwonder.blogspot.com.au/2014/07/bootjacker-amazing-avr-bootloader-hack.html

#include <stdint.h>
#include <stdbool.h>

#include <avr/io.h>
#include <avr/boot.h>
#include <avr/pgmspace.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <util/atomic.h>
#include <util/delay.h>

#define kSpmCsr (_SFR_IO_ADDR(SPMCSR))
#define kSpmCsrMem (kSpmCsr+0x20)

#define IOAddrInsMask(aPort) (((aPort&0x30)<<5)|(aPort&7))

#define BOOTLOADER_START 0x7000
#define BOOTLOADER_END 0x8000
#define BOOTLOADER_SIZE (BOOTLOADER_END-BOOTLOADER_START)
// We prefer to write our custom SPM instruction in the last page of the
// bootloader. If we can't write to that block (because the SPM instruction
// itself is inside it, then write to the first block).
#define IDEAL_BLOCK_ADDR (BOOTLOADER_END - SPM_PAGESIZE)

#define kStsIns 0x9200
#define kStsRegMask 0x01f0
#define kOutSpmCsrIns (0xb800 + IOAddrInsMask(kSpmCsr))
#define kOutSpmCsrRegMask 0x01f0
#define kSpmIns 0x95e8

#define kFlashPageSize (1<<kFlashPageSizeBits)
#define kFlashPageSizeInWords (1<<(kFlashPageSizeBits-1))
#define kFlashSpmEnMask (1<<0)
#define kFlashSpmEraseMask (1<<1)
#define kFlashSpmWritePageMask (1<<2)
#define kFlashSpmBlbSetMask (1<<3)
#define kFlashSpmRwwsReMask (1<<4)
#define kFlashSpmRwwsBusyMask (1<<6)

#define kFlashSpmEn kFlashSpmEnMask
#define kFlashSpmErase (kFlashSpmEraseMask|kFlashSpmEnMask)
#define kFlashSpmWritePage (kFlashSpmWritePageMask|kFlashSpmEnMask)
#define kFlashSpmBlbSet (kFlashSpmBlbSetMask|kFlashSpmEnMask)
#define kFlashSpmRwws (kFlashSpmRwwsReMask|kFlashSpmEnMask)
#define kFlashSpmRwwsBusy (kFlashSpmRwwsBusyMask)

// NOTE: Uses even numbers for ideal configurations, odd numbers for suboptimal
typedef enum {
    SPM_TYPE_STS_IDEAL = 0,
    SPM_TYPE_STS_SECONDARY = 1,
    SPM_TYPE_OUT_IDEAL = 2,
    SPM_TYPE_OUT_SECONARY = 3,
    SPM_TYPE_NONE = 7
} spm_type;

uint16_t gSpmSequenceAddr;

const uint8_t gBootloaderJmpVector[] = {
    0x57, 0xbf, 0xe8, 0x95, // An out, spm command.
    0x00, 0x00,             // nop
    0xFF, 0xCF,             // rjmp -1 (infinite loop)
};

extern const uint16_t bootloader_size;
extern const __flash uint8_t bootloader_data[];

uint8_t find_spm(void) {
    uint8_t spmType = SPM_TYPE_NONE;
    uint16_t addr;

    for( addr=BOOTLOADER_START; addr < BOOTLOADER_END; addr+=2) {
        if ((spmType & 1) == 0) {
            break;
        }

        const uint16_t word_0 = pgm_read_word(addr+0);
        const uint16_t word_1 = pgm_read_word(addr+2);
        const uint16_t word_2 = pgm_read_word(addr+4);

        if (
            (word_0 & ~kStsRegMask) == kStsIns &&
            word_1 == kSpmCsrMem &&
            word_2 == kSpmIns
        ) {
            if (addr+8 < IDEAL_BLOCK_ADDR) {
                spmType = SPM_TYPE_STS_IDEAL;
                gSpmSequenceAddr = addr;
                break;
            } else {
                spmType = SPM_TYPE_STS_SECONDARY;
            }
        }


        // kSpmCsr (_SFR_IO_ADDR(SPMCSR))
        // kSpmCsrMem (kSpmCsr+0x20)
        // IOAddrInsMask(aPort) (((aPort&0x30)<<5)|(aPort&7))
        // kOutSpmCsrIns (0xb800 + IOAddrInsMask(kSpmCsr))
        // kOutSpmCsrRegMask 0x01f0
        // kSpmIns 0x95e8
        //
        // OUT A, Rr:
        // 16-bit Opcode
        // 1011 1AAr rrrr AAAA
        // AA.....AAAA ->
        // ..rrrrr.... ->
        //
        // A  = SPMCSR
        // Rr = register (r20 atmel DFU)

        // out SPMCSR, R20; argument 2 decides function (r18)
        // SPM                ;Store program memory
        if (
            (word_0 & ~kOutSpmCsrRegMask) == kOutSpmCsrIns &&
            word_1 == kSpmIns
        ) {
            if (addr+6 < IDEAL_BLOCK_ADDR) {
                spmType = SPM_TYPE_OUT_IDEAL;
                gSpmSequenceAddr = addr;
                break;
            } else {
                spmType = SPM_TYPE_OUT_SECONARY;
            }
        }

        // Save the SPM address
        if (spmType != SPM_TYPE_NONE) {
            gSpmSequenceAddr = addr;
        }
    }

    return spmType;
}

#define BLB1_MASK 0x30 // controls access of bootloader section
#define BLB0_MASK 0x0C // controls access of application section
#define BLB_MASK 0x03 // controls access from external progarmmer

// Check lock bits. We need to be able to do the following things:
// 1. Use the SPM instruction to write to the bootloader.
// 2. From the application section, be allowed to read the bootloader code.
// 3. When executing from the bootloader, allow interrupt code stored in the
//    application section to execute
//
// This means we need full access to the bootloader (i.e. both bits
// a BLB1 need to be unprogrammed).
uint8_t check_bootloader_lock_bits(void) {
    // uint8_t lfuse;
    // uint8_t hfuse;
    // uint8_t efuse;
    uint8_t lock_bits;

    ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
        // TODO: check other fuse bits
        // lfuse = boot_lock_fuse_bits_get(GET_LOW_FUSE_BITS);
        // hfuse = boot_lock_fuse_bits_get(GET_HIGH_FUSE_BITS);
        // efuse = boot_lock_fuse_bits_get(GET_EXTENDED_FUSE_BITS);
        lock_bits = boot_lock_fuse_bits_get(GET_LOCK_BITS);
    }

    if( (lock_bits & BLB1_MASK) != BLB1_MASK) {
        return -1;
    }

    return 0;
}

void setup_timer0(uint8_t cycles) {

#if defined (__AVR_ATmega32U4__)
    TCCR0B = 0;       // stop the timer.
    TCCR0A = 0;       // mode 0, no OCR outputs.
    TCNT0 = 0;        // reset the timer
    TIFR0 = (1<<OCF0B) | (1<<OCF0A) | (1<<TOV0); // clear all pending timer0 interrupts.
    OCR0B = cycles; // number of cycles to wait
    TIMSK0 = (1<<OCIE0B); // OCR0B interrupt enabled.
#elif defined (__AVR_ATmega168__)
    // Note: Pretty sure Fignition which bootjacker was made for uses ATmega168
    TCCR0B = 0;       // stop the timer.
    TCCR0A = 0;       // mode 0, no OCR outputs.
    TCNT0 = 0;        // reset the timer
    TIFR0 = (1<<OCF0B) | (1<<OCF0A) | (1<<TOV0);  // clear all pending timer0 interrupts.
    OCR0B = cycles;   // 40 clocks from now (40 in test, 31 for real).
    TIMSK0 = (1<<OCIE0B);     // OCR0B interrupt enabled.
#else
#error "Current microcontroller is unsupported or untested."
#endif

}

// Found this value by experimentation.
// This value should be chosen so the timer0 interrupt will be called on the
// instruction immediately following the SPM instruction.
#define SPM_LEAP_CYCLES_USING_STS 40
#define SPM_LEAP_CYCLES_USING_OUT 39

// This function will call the SPM instruction we found in the bootloader
// while setting `R0:R1` <- `optValue` and `Z` <- `addr`.
//
// Note:  I'm not sure if this function will work correctly if it tries to
// modify a value in the application space.
// If the SPM instruction targets the bootloader section, then the CPU will
// be halted while she SPM operation is executed, however if the SPM instruction
// targets the application space, then code will continue executing in the
// bootloader. However the application code cannot be read while the SPM
// operation is executing, therefore it is necessary for the code to wait for
// the SPM instruction to finish executing before returning to the application
// space.
//
// We only want to update code in the bootloader, should not be an issue, but
// if we did want to do this, we would need to write our own SPM instruction
// into the bootloader space which includes a loop to wait for the SPM
// instruction to finish.
void spm_leap_cmd(uint16_t addr, uint8_t spmCmd, uint16_t optValue) {
    uint8_t cmdReg, tmp=0;

    const uint8_t spmaddr_zl = (gSpmSequenceAddr >> 0) & 0xff;
    const uint8_t spmaddr_zh = (gSpmSequenceAddr >> 8) & 0xff;
    // Will probably need this for devices that have more than 128kb flash
    // const uint8_t spmaddr_rz = (gSpmSequenceAddr >> 16) & 0xff;
    const uint8_t spmaddr_rz = 0;

    // Assume that the instruction before SPM is `OUT SPMCSR, rXX`
    // Or it is using STS SPMCSR, rXX.  Need to extract the rXX from
    // the instruction.  It's in the same place for both opcodes:
    //1001 001d dddd 0000
    cmdReg = (uint8_t)(
        (pgm_read_word(gSpmSequenceAddr)>>4) & 0x1f
    );

    PINC|=(1<<4);
    asm volatile(
        "push r0\n"
        "push r1\n"             // needed for opt command.
        "push %[tmp]\n"
        "push r30\n"
        "push r31\n"

        //wait for spm operation complete.
        "SpmLeapCmdWaitSpm: in %[tmp], %[SPM_CSR]\n" //
        "sbrc %[tmp],0\n"
        "rjmp SpmLeapCmdWaitSpm\n"

        // start the TCCR0B timer
        "ldi %[tmp],1\n"     // timer 0 start at fClk
        "out %[_TCCR0B],%[tmp]\n"    // set TCCR0B so off we go. This is time 0c.

        // To call the smp instruction, we first need to write to the SPMCSR
        // register. The variable cmdReg holds the register that will be
        // written to the SPMCSR. We save the current value of cmdReg now
        // and push it to the stack to restore later
        "movw r0, %[optValue] \n"   // set the value to be written. (1c)
        "mov r30, %[cmdReg] \n"   // get the register used by the sequence's spm command. (1c)
        "ldi r31, 0\n"    // z^reg to save. (1c)
        "ld %[tmp], Z\n"      // get the reg (2c)
        "push %[tmp]\n"      // saved it, now we can overwrite with spm command. (2c)
        "push %[cmdReg]\n" // save cmdReg

        // Push the address that will be returned to from the spm instruction.
        "ldi r30,lo8(pm(SpmLeapCmdRet))\n" // (1c)
        "ldi r31,hi8(pm(SpmLeapCmdRet))\n" // (1c)
        "push r30\n"     // (2c)
        "push r31\n"     // return address must be pushed big-endian. (2c)

        //
        "mov r30, %[spmaddr_l]\n" // lo byte of Spm sequence address (1c)
        "mov r31, %[spmaddr_h]\n" // hi byte of Spm sequence address. z^sequence in code. (1c)
        // Convert the gSpmSequenceAddr from a byte address to a word address
        "lsr r31\n"      // (1c)
        "ror r30\n"      // div 2 to get correct Spm program address. (1c)

        "push r30\n"     // (2c)
        "push r31\n"     // Spm sequence program address must be pushed big-endian. (2c)

        "push %A[addr]\n"     // before we overwrite reg used by sequence's spm command
                              // we must first save the spm target address (2c)
        "push %B[addr]\n"     // in case it would get overwritten by the st Z. (2c)

        // store the spm command into the cmdReg
        "mov r30, %[cmdReg]\n"   // get the register used by the sequence's spm command. (1c)
        "ldi r31, 0\n"    // z^reg to save. (1c)
        "st Z, %[spmCmd] \n"      // store the command in the reg. (2c)

        "pop r31\n"      // (2c)
        "pop r30\n"      // restore the spm target address into Z. // (2c)

        // the gSpmSequenceAddr is now on the top of the stack, return here
        // to enter the bootloader
        "ret\n"          // return to bootloader. (4c(16bit PC)) (5c (22bit PC))

        // sts (2c)   // alternatively OUT (1c)
        // spm (1c). 42c in total, timer should be set to 40.

        // Return from the bootloader, pop values from stack and return
        "SpmLeapCmdRet:\n"
        "pop %[cmdReg]\n"             // restore command Reg address
        "pop %[tmp]\n"                // restore command Reg value
        "mov r30, %[cmdReg]\n"
        "ldi r31,0\n"   // z^reg to save.
        "st Z,%[tmp]\n"     // pop the reg
        "pop r31\n"
        "pop r30\n"
        "pop %[tmp]\n"
        "pop r1\n"
        "pop r0\n"
            // output registers
            : "=d" (tmp),                                   // %0
              "=r" (addr)                                   // %1
            // input registers
            : [_TCCR0B] "I" (_SFR_IO_ADDR(TCCR0B)),         // %2
              [cmdReg] "r" (cmdReg),                        // %3
              [spmCmd] "r" (spmCmd),                        // %4
              [optValue] "r" (optValue),                    // %5
              [addr] "0" (addr),                            // %6
              [spmaddr_l] "r" (spmaddr_zl),                 // %7
              [spmaddr_h] "r" (spmaddr_zh),                 // %8
              [spmaddr_z] "r" (spmaddr_rz),                 // %9
              [SPM_CSR] "I" (kSpmCsr),                      // %10
              [tmp] "d" (tmp)                               // %11
    );

}

/**
 * The timer interrupt interrupted bootloader execution
 * just after the spm instruction.
 * if we ret then we'll get back to the bootloader.
 * we need to pop the return address and then ret, which
 * should take us back to the SpmLeapCommand.
 **/
// TODO: need to set this interrupt vector to the one that matches the
// timer we will add later.
ISR(TIMER0_COMPB_vect, ISR_NAKED) { // OCR0B

#if 0
    // Debug code to check if the ISR is being called
    while (1) {
        PORTF ^= (_BV(6));
        _delay_ms(500);
    }
#endif

    asm volatile(
        "ldi r30,0\n"
        "out %[_TCCR0B],r30\n"  // stop timer 0
        "out %[_TCNT0],r30\n"  // reset timer 0.
        "ldi r30,7\n"
        "out %[_TIFR0],r30\n"  // clear interrupts on timer 0.
        "pop r31\n"     // don't care about overwiting Z because SpmLeap doesn't need it.
        "pop r30\n"
        "reti\n"
            :
            : [_TCCR0B] "I" (_SFR_IO_ADDR(TCCR0B)),
            [_TCNT0] "I" (_SFR_IO_ADDR(TCNT0)),
            [_TIFR0] "I" (_SFR_IO_ADDR(TIFR0))
    );
}


// Write the data in src to the address at
void flash_write_page(const uint8_t *src, uint16_t dst, uint8_t length) {
    // Fill the temporary buffer for the page write
    for(uint8_t addr=0; addr < length; addr+=2) {        // 64 words.
        uint16_t data = *((uint16_t*)src);
        spm_leap_cmd(dst+addr, kFlashSpmEn, data);
        src+=2; // advance to next word
    }

    // Erase and write the target page
    spm_leap_cmd(dst, kFlashSpmErase, 0);
    spm_leap_cmd(dst, kFlashSpmWritePage, 0);
}

void bootloader_upgrade(void) {
    uint16_t our_spm_page;

    // TODO: hang and print error message print error message/LEDs
    // Check the bootloader lock bits, if incompatible lock bits are found
    // then don't run the bootloader upgrade procedure
    if (check_bootloader_lock_bits()) {
        while (1) {
            PORTF ^= _BV(6);
            _delay_ms(500);
            PORTF ^= _BV(7);
            _delay_ms(500);
        }
    }

    // Find the SPM instruction and what the surrounding instructions are.
    uint8_t spmType = find_spm();

    if (spmType == SPM_TYPE_NONE) {
        // If we didn't find an SPM instruction, hang here and blink both
        // LEDs.
        while (1) {
            PORTF ^= _BV(6);
            PORTF ^= _BV(7);
            _delay_ms(1000);
        }
    }

    // Need to try and disable any interrupt source that the bootloader my have
    // setup (really the bootloader shouldn't change these from their default
    // values).
#ifdef __AVR_ATmega32U4__

    // Disable all other interrupt sources
	TIMSK0 = 0;	// Disable timer 0 interrupts
	TIFR0 = 0x07;
	TIMSK1 = 0;	// Disable timer 1 interrupts
	TIFR1 = 0x17;
	EIMSK = 0;	// Disable external interrupts
	EIFR = 0x03;
	PCICR = 0;	// all Pin Change interrupts off (Tape uses them).
	PCIFR = 0x01;	// pending pin change interrupts cleared.

    USBCON = 0;
    USBINT = (1<<VBUSTI);

    UDIEN = 0;
    UDINT = 0xff;
#else
#error "Unsupported/tested microcontroller"
#endif

    sei(); // Need interrupts enabled

    if (spmType == SPM_TYPE_STS_SECONDARY || spmType == SPM_TYPE_STS_IDEAL) {
        setup_timer0(SPM_LEAP_CYCLES_USING_STS);   // sts timing.
    } else if (spmType == SPM_TYPE_OUT_SECONARY || spmType == SPM_TYPE_OUT_IDEAL) {
        setup_timer0(SPM_LEAP_CYCLES_USING_OUT); // out timing is one cycle less.
    } else {
    }

    // if (spmType == SPM_TYPE_STS_SECONDARY || spmType == SPM_TYPE_OUT_SECONARY) {
    //     our_spm_page = BOOTLOADER_START;
    // } else {
    //     our_spm_page = BOOTLOADER_END - SPM_PAGE;
    // }
    our_spm_page = IDEAL_BLOCK_ADDR;

    flash_write_page(
        gBootloaderJmpVector,
        our_spm_page,
        sizeof(gBootloaderJmpVector)
    );

    // We write a custom block with an SPM instruction as the last page of flash.
    // It uses an out instruction, so setup the timer appropriately.
    setup_timer0(SPM_LEAP_CYCLES_USING_OUT);
    gSpmSequenceAddr = our_spm_page;

    // We can now overwrite the bootloader with our replacement bootloader.


    {
        // uint16_t boot_pos = 0;
        // while (boot_pos < sizeof(bootloader_data)) {
        //     uint8_t write_size = 0;
        //     uint8_t page_data[SPM_PAGESIZE];
        //     for (
        //         ;
        //         (write_size < SPM_PAGESIZE) && (boot_pos+write_size < sizeof(bootloader_data));
        //         ++write_size
        //     ) {
        //         page_data[write_size] = bootloader_data[boot_pos+write_size];
        //     }

        //     // The data is now loaded into ram, write the page
        //     flash_write_page(
        //         page_data,
        //         BOOTLOADER_START + boot_pos,
        //         write_size
        //     );
        //     boot_pos += write_size;
        // }
        uint16_t boot_pos = 0;
        while (
            boot_pos < bootloader_size &&
            (boot_pos < (BOOTLOADER_END-BOOTLOADER_START)-SPM_PAGESIZE)
        ) {
            uint8_t page_data[SPM_PAGESIZE];
            for (int i = 0; i < SPM_PAGESIZE; ++i) {
                page_data[i] = bootloader_data[boot_pos + i];
            }

            // The data is now loaded into ram, write the page
            flash_write_page(
                page_data,
                BOOTLOADER_START + boot_pos,
                SPM_PAGESIZE
            );
            boot_pos += SPM_PAGESIZE;
        }

        // To write the page at `BOOTLOADER_END-SPM_PAGESIZE` we can use the
        // SPM instruction in the final page (i.e. the currently executing page).
        // So what we want to do is find another SPM instruction that we wrote
        // inside the bootloader elsewhere (obviously if there isn't any this
        // won't work).
        // TODO: check the bootloader_data meets this requirement before
        // attempting to write the bootloader.
        {
            // Find the SPM instruction and what the surrounding instructions are.
            uint8_t spmType = find_spm();

            // Set the timer for the new instruction sequence
            if (spmType == SPM_TYPE_STS_SECONDARY || spmType == SPM_TYPE_STS_IDEAL) {
                setup_timer0(SPM_LEAP_CYCLES_USING_STS);   // sts timing.
            } else {
                setup_timer0(SPM_LEAP_CYCLES_USING_OUT); // out timing is one cycle less.
            }

            // load the final page from flash
            uint8_t page_data[SPM_PAGESIZE];
            for (int i = 0; i < SPM_PAGESIZE; ++i) {
                page_data[i] = bootloader_data[BOOTLOADER_SIZE-SPM_PAGESIZE + i];
            }

            flash_write_page(
                page_data,
                BOOTLOADER_END - SPM_PAGESIZE,
                SPM_PAGESIZE
            );
        }
    }
}

#define CPU_PRESCALE(n) (CLKPR = 0x80, CLKPR = (n))

int main(void) {
    // Clear the pre-scaler in case it was set by fuses
    CPU_PRESCALE(0); // set for 16 MHz clock

    // Disable watch dog if it is active
    wdt_disable();

    // Enable debug LEDs
    DDRF |= _BV(7) | _BV(6);
    // Turn LEDs off as initial state
    PORTF &= ~(_BV(7) | _BV(6));


    // We want to make sure that interrupts are executed from the application
    // section not the bootloader. In case the bootloader set IVSEL to 1, we
    // clear it here.
    {
        // NOTE: JTD, PUD are also cleared here but we don't care
        MCUCR = (1<<IVCE); // Enable change of interrupt vectors
        MCUCR = (0<<IVCE) | (0<<IVSEL); // Clear IVSEL, i.e. interrupts from app section

        if ( (MCUCR & (1<<IVSEL)) != 0 ) {
            // Interrupts must be place at the start of flash. If that is not
            // the case, then we hang here.
            while (1) {
                PORTF ^= (_BV(7));
                _delay_ms(500);
            }
        }
    }

    _delay_ms(100);

    bootloader_upgrade();

    // If we finish, wait forever
    cli();
    while(1);
}
