#include <avr/io.h>

#include <stdint.h>
#include <stdbool.h>

#include <avr/pgmspace.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <util/delay.h>

#define SPIFbit 7
#define SPR (1<<0)

// #define kSpmCsr 0x37
// #define kSpmCsrMem (kSpmCsr+0x20)

#define kSpmCsr (_SFR_IO_ADDR(SPMCSR))
#define kSpmCsrMem (kSpmCsr+0x20)

#define IOAddrInsMask(aPort) (((aPort&0x30)<<5)|(aPort&7))

// #define kBootloaderStart 0x7800
// #define kMicroBootStart 0x7f80
// #define kBootloaderEnd 0x8000

#define kBootloaderStart 0x7000
#define kMicroBootStart 0x7f80
#define kBootloaderEnd 0x8000

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

typedef union {
    struct {
        uint8_t zl;
        uint8_t zh;
        uint8_t rz;
		uint8_t res;
    };
    uint32_t addr_32;
} ADD_T;

extern uint8_t _etext;
// extern void _Upgrader(void);
// extern uint8_t _UpgradeSpmLeap;

uint8_t _UpgradeSpmLeap_TODO(void) {
    return 0;
}

// TODO: check what this value needs to be
// uint8_t _etext_place_holder = 0;
uint8_t _etext_place_holder = 0;

uint16_t gSpmSequenceAddr;

const uint8_t gBootloaderJmpVector[] = {
    0x0c, 0x94, 0xc0, 0x3f, // A vector to the 128b mini Bootloader.
    0x57, 0xbf, 0xe8, 0x95, // An out, spm command.
    0x00, 0x00              // A nop instruction.
};


uint8_t FindSpm(void) {
    uint8_t spmType = SPM_TYPE_NONE;
    uint16_t addr;

    for( addr=kBootloaderStart; addr < kBootloaderEnd; addr+=2) {
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
            if (addr+8 < kMicroBootStart) {
                spmType = SPM_TYPE_STS_IDEAL;
                gSpmSequenceAddr = addr;
                break;
            } else {
                spmType = SPM_TYPE_STS_SECONDARY;
            }
        }

        // OUT A, Rr:
        // 16-bit Opcode
        // 1011 1AAr rrrr AAAA
        // AA.....AAAA ->
        // ..rrrrr.... ->
        //
        // A  = SPMCSR
        // Rr = register (r20 atmel DFU)

// #define kSpmCsr (_SFR_IO_ADDR(SPMCSR))
// #define kSpmCsrMem (kSpmCsr+0x20)
// #define IOAddrInsMask(aPort) (((aPort&0x30)<<5)|(aPort&7))
// #define kOutSpmCsrIns (0xb800 + IOAddrInsMask(kSpmCsr))
// #define kOutSpmCsrRegMask 0x01f0
// #define kSpmIns 0x95e8

        // out SPMCSR, R20; argument 2 decides function (r18)
        // SPM                ;Store program memory
        if (
            (word_0 & ~kOutSpmCsrRegMask) == kOutSpmCsrIns &&
            word_1 == kSpmIns
        ) {
            if (addr+6 < kMicroBootStart) {
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

uint16_t FlashLpmData(uint16_t addr, uint8_t spmCmd) {
    uint16_t val;
    asm volatile(
        "push r0\n" // save registers
        "push r1\n"
        "push r16\n"
        "push r30\n"
        "push r31\n"

        "movw r30,%1\n"  // set the addr to be written.
        "1: in r16,%3\n" //
        "sbrc r16,0\n"   //wait for operation complete.
        "rjmp 1b\n"
        "cli\n"
        "out %3,%2\n"
        "lpm %0,Z\n"     // now we start the load/erase/write.
        "sei\n"

        "pop r31\n" // restore registers
        "pop r30\n"
        "pop r16\n"
        "pop r1\n"
        "pop r0\n" : "=r" (val): "r" (addr), "r" (spmCmd), "I" (kSpmCsr)
    );
    return val;
}

// Check if the lock bits are set. Hang if they are invalid.
uint8_t CheckBootLock(void) {
    uint16_t bootLockBits;
    bootLockBits = FlashLpmData(1, kFlashSpmBlbSet);

    if( (bootLockBits&0x3f) != 0x3f) {
        return -1;
    }

    return 0;
}

void SetupTimer0B(uint8_t cycles) {

#if defined (__AVR_ATmega32U4__)
    TCCR0B = 0;       // stop the timer.
    TCCR0A = 0;       // mode 0, no OCR outputs.
    TCNT0 = 0;        // reset the timer

    // clear all pending timer0 interrupts.
    TIFR0 = (1<<OCF0B) | (1<<OCF0A) | (1<<TOV0);

    OCR0B = cycles;   // 40 clocks from now (40 in test, 31 for real).

    // OCR0B interrupt enabled.
    TIMSK0 = (1<<OCIE0B);
#elif defined (__AVR_ATmega168__)
    // Note: Pretty sure Fignition which bootjacker was made for uses ATmega168

    // clear PCINT2.
    PCICR &= ~(1<<PCIE2);     // disable PCINT2 interrupts.
    PCIFR |= (1<<PCIE2);      // clear any pending PCINT2 interrupts.

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

void SpmLeapCmd(uint16_t addr, uint8_t spmCmd, uint16_t optValue) {
    uint8_t cmdReg, tmp=0;

    const uint8_t spmaddr_zl = (gSpmSequenceAddr >> 0) & 0xff;
    const uint8_t spmaddr_zh = (gSpmSequenceAddr >> 8) & 0xff;
    // const uint8_t spmaddr_rz = (gSpmSequenceAddr >> 16) & 0xff;
    const uint8_t spmaddr_rz = 0;

    // Assume that the instruction before SPM is `OUT SPMCSR, rXX`
    cmdReg = (uint8_t)(
        (pgm_read_word(gSpmSequenceAddr)>>4)&0x1f
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

    // while (1) {
    //     PORTF ^= (_BV(6));
    //     _delay_ms(500);
    // }

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

// NOTE: looks like this function:
// * Loads the 128 bytes from `src` into the temporary buffer
// * Erase the page at `dst`
// * Writes the page at `dst`
void ProgPage(const uint8_t *src, uint16_t dst) {
    uint8_t addr;
    uint16_t data = 0xffff;

    // TODO: Not sure what the purpose of this is, probably just to print
    // whatever src points to
    // ShowProgSrc(src);

    for(addr=0; addr<128; addr+=2) {        // 64 words.
        // data = pgm_read_word(*(uint16_t*)src);
        data = *((uint16_t*)src);
        SpmLeapCmd(dst+addr, kFlashSpmEn, data);
        src+=2; // advance to next word
    }

    // ShowProgProgress((uint8_t*)dst,128); TODO: probably more debugging print code
    SpmLeapCmd(dst, kFlashSpmErase, data);

    // // ShowProgContents(gMigrateSpmErased, (uint8_t*)dst, 128);
    SpmLeapCmd(dst, kFlashSpmWritePage, data);

    // ShowProgContents(gMigrateSpmWritten, (uint8_t*)dst, 128);
}

void BootJacker(void) {

    // // Check the bootloader lock bits, if incompatible lock bits are found
    // // then don't run the bootloader upgrade procedure
    // if (CheckBootLock()) {
    //     // TODO: print error message
    //     while (true) { }
    // }

    uint8_t spmType = FindSpm();

    // if (spmType == SPM_TYPE_NONE) {
    if (spmType != SPM_TYPE_OUT_IDEAL) {
        // If we didn't find an SPM instruction, hang here and blink both
        // LEDs.
        while (1) {
            PORTF ^= _BV(6);
            PORTF ^= _BV(7);
            _delay_ms(1000);
        }
    }


    sei();
#define kTestTimerWait 40
    if (spmType == SPM_TYPE_STS_SECONDARY || spmType == SPM_TYPE_STS_IDEAL) {
        SetupTimer0B(kTestTimerWait);   // sts timing.
    } else {
        SetupTimer0B(kTestTimerWait-1); // out timing is one cycle less.
    }


#define TEST_PAGE_ADDR 0x6F80

    ProgPage(gBootloaderJmpVector, 0x7F80);

    cli();
    while (1);

    if (spmType==SPM_TYPE_STS_IDEAL || SPM_TYPE_OUT_IDEAL) {
        // program the micro boot.
        ProgPage((uint8_t*)&_etext_place_holder, kMicroBootStart);
        // ProgPage((uint8_t*)gBootloaderJmpVector, 0x7E00);

        // cli();
        // while (1);

        OCR0B = kTestTimerWait-1; // it's an out command now.

        //
        gSpmSequenceAddr=(uint16_t)&_UpgradeSpmLeap_TODO;

        // program the jump vector using the microboot spm.
        ProgPage((uint8_t*)gBootloaderJmpVector, kBootloaderStart);
    }

    // } else {
    //     // program the jump vector.
    //     ProgPage((uint8_t*)gBootloaderJmpVector, kBootloaderStart);

    //     OCR0B = kTestTimerWait-1; // it's an out command now.
    //     gSpmSequenceAddr = kBootloaderStart+4;    // it's just after the boot vector.

    //     // program the micro boot using the jump vector spm.
    //     // ProgPage((uint8_t*)&_etext, kMicroBootStart);
    //     ProgPage((uint8_t*)&_etext_place_holder, kMicroBootStart);
    // }

    // NOTE: Looks like this code setups up some values, then calls the function
    // _Upgrader which is used to actually write the payload using the code from
    // kMicroBootStart
    // TODO: probably replace this with our own code
#if 0
    asm volatile(
        "ldi r30, 0\n"
        "ldi r31, 0\n"
        "ldi r16, (1<<3) | (1<<5) | (1<<1) | (1<<2)\n"
        "out %0, r16\n"  // PORTB.0 and B.4 inputs, B.1, B.2, B.3, B.5 outputs."
        "ldi r16, (1<<3) | (1<<5) | (1<<1) | (1<<2)\n"// ;Select Flash, not SRAM."
        "out %1, r16\n"
        "sbi %2, 7\n"    // PORTD.7 output"
        "cbi %3, 7\n"    // outputting 0. Ready to read"
        "jmp _Upgrader\n"
            :
            : "I" (kDDRB), "I" (kPORTB), "I" (kDDRD), "I" (kPORTD)
    );
#endif

}

#define CPU_PRESCALE(n) (CLKPR = 0x80, CLKPR = (n))

int main(void) {
    // set for 16 MHz clock
    CPU_PRESCALE(0);

    wdt_disable();

    DDRF |= _BV(7) | _BV(6);
    PORTF |= (_BV(7) | _BV(6)); // clear LED to begin with.

    {
        // NOTE: JTD, PUD are also cleared here but we don't care in this case
        /* Enable change of interrupt vectors */
        MCUCR = (1<<IVCE);
        /* Move interrupts to application flash section */
        MCUCR = (0<<IVCE) | (0<<IVSEL);

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

    sei();

    BootJacker();

    cli();
    while (1);
}
