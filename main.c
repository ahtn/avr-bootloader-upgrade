#include <avr/io.h>

#include <stdint.h>
#include <stdbool.h>

#include <avr/pgmspace.h>
#include <avr/interrupt.h>

#define SPIFbit 7
#define SPR (1<<0)
#define kSpmCsr 0x37
#define kSpmCsrMem (kSpmCsr+0x20)

#define IOAddrInsMask(aPort) (((aPort&0x30)<<5)|(aPort&7))

#define kBootloaderStart 0x7800
#define kMicroBootStart 0x7f80
#define kBootloaderEnd 0x8000

#define kStsIns 0x9200
#define kStsRegMask 0x01f0
#define kOutSpmCsrIns (0xb800+IOAddrInsMask(kSpmCsr))
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

extern uint8_t _etext;
// extern void _Upgrader(void);
// extern uint8_t _UpgradeSpmLeap;

uint8_t _UpgradeSpmLeap_TODO(void) {
    return 0;
}

// TODO: check what this value needs to be
uint8_t _etext_place_holder = 0;

uint16_t gSpmSequenceAddr;

const uint8_t gBootloaderJmpVector[] PROGMEM =
{ 0x0c, 0x94, 0xc0, 0x3f, // A vector to the 128b mini Bootloader.
  0x57, 0xbf, 0xe8, 0x95, // An out, spm command.
  0x00, 0x00              // A nop instruction.
};


uint8_t FindSpm(void) {
    uint8_t spmType = SPM_TYPE_NONE;
    uint16_t addr;
    for(addr=kBootloaderStart; addr<kBootloaderEnd && ((spmType&1)==1); addr+=2) {
        uint16_t word_0 = pgm_read_word(*(uint16_t*) (addr+0));
        uint16_t word_1 = pgm_read_word(*(uint16_t*) (addr+2));
        uint16_t word_2 = pgm_read_word(*(uint16_t*) (addr+4));
        if (
            (word_0 & ~kStsRegMask) == kStsIns &&
            word_1 == kSpmCsrMem &&
            word_2 == kSpmIns
        ) {
            if (addr+8 < kMicroBootStart) {
                spmType = SPM_TYPE_STS_IDEAL;
            } else {
                spmType = SPM_TYPE_STS_SECONDARY;
            }
        }

        if (
            (word_0 & ~kOutSpmCsrRegMask) == kOutSpmCsrIns &&
            word_1 == kSpmIns
        ) {
            if (addr+6 < kMicroBootStart) {
                spmType = SPM_TYPE_OUT_IDEAL;
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

#define kTCCR0B 0x25
#define kTCNT0 0x26
#define kTIFR0 0x15

void SetupTimer0B(uint8_t cycles) {
 
#if defined (__AVR_ATmega32U4__)
    // TODO: Available timers depend on the chip being used. Need to check the
    // settings for ATmega32u4.

#elif defined (__AVR_ATmega168__)
    // Note: Pretty sure Fignition which bootjacker was made for uses ATmega168

    // clear PCINT2.
    PCICR&=~(1<<PCIE2);     // disable PCINT2 interrupts.
    PCIFR|=(1<<PCIE2);      // clear any pending PCINT2 interrupts.

    TCCR0B=0;       // stop the timer.
    TCCR0A=0;       // mode 0, no OCR outputs.
    TCNT0=0;        // reset the timer
    TIFR0=(1<<OCF0B)|(1<<OCF0A)|(1<<TOV0);  // clear all pending timer0 interrupts.
    OCR0B=cycles;   // 40 clocks from now (40 in test, 31 for real).
    TIMSK0=(1<<OCIE0B);     // OCR0B interrupt enabled.
#endif

}

void SpmLeapCmd(uint16_t addr, uint8_t spmCmd, uint16_t optValue) {
    uint8_t cmdReg, tmp=0;
    cmdReg = (uint8_t)(
        (pgm_read_word(*(uint16_t*)gSpmSequenceAddr)>>4)&0x1f
    );
    PINC|=(1<<4);
    asm volatile(
        "push r0\n"
        "push r1\n"             // needed for opt command.
        "push %9\n"
        "push r30\n"
        "push r31\n"
        "SpmLeapCmdWaitSpm: in %9,%8\n" //
        "sbrc %9,0\n"   //wait for spm operation complete.
        "rjmp SpmLeapCmdWaitSpm\n"
        "ldi %9,1\n"    // timer 0 start at fClk
        "out %2,%9\n"   // set TCCR0B so off we go. This is time 0c.
        "movw r0,%5\n"  // set the value to be written.

        "mov r30,%3\n"  // get the register used by the sequence's spm command.
        "ldi r31,0\n"   // z^reg to save.
        "ld %9,Z\n"     // get the reg
        "push %9\n"     // saved it, now we can overwrite with spm command.

        "ldi r30,lo8(pm(SpmLeapCmdRet))\n"
        "ldi r31,hi8(pm(SpmLeapCmdRet))\n"
        "push r30\n"
        "push r31\n"    // return address must be pushed big-endian.
        "lds r30,%7\n"  // lo byte of Spm sequence address
        "lds r31,%7+1\n" // hi byte of Spm sequence address. z^sequence in code.
        "lsr r31\n"
        "ror r30\n"             // div 2 to get correct Spm program address.
        "push r30\n"
        "push r31\n"    // Spm sequence program address must be pushed big-endian.

        "push %A6\n"    // before we overwrite reg used by sequence's spm command
                                        // we must first save the spm target address
        "push %B6\n"    // in case it would get overwritten by the st Z.

        "mov r30,%3\n"  // get the register used by the sequence's spm command.
        "ldi r31,0\n"   // z^reg to save.
        "st Z,%4\n"     // store the command in the reg.
        "pop r31\n"
        "pop r30\n"     // restore the spm target address into Z.
        "ret\n"                 // return to bootloader.
        // sts (2c)
        // spm (1c). 42c in total, timer should be set to 40.
        "SpmLeapCmdRet:pop %9\n"                // restore command Reg.
        "mov r30,%3\n"
        "ldi r31,0\n"   // z^reg to save.
        "st Z,%9\n"     // pop the reg
        "pop r31\n"
        "pop r30\n"
        "pop %9\n"
        "pop r1\n"
        "pop r0\n"
            : "=d" (tmp), "=r" (addr)
            : "I" (kTCCR0B), "r" (cmdReg), "r" (spmCmd), "r" (optValue),
              "0" (addr), "g" (gSpmSequenceAddr), "I" (kSpmCsr), "d" (tmp)
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
ISR(__vector_15, ISR_NAKED) { // OCR0B
    //PORTC&=~(1<<4);
    PINC|=(1<<4);
    asm volatile(
        "ldi r30,0\n"
        "out %0,r30\n"  // stop timer 0
        "out %1,r30\n"  // reset timer 0.
        "ldi r30,7\n"
        "out %2,r30\n"  // clear interrupts on timer 0.
        "pop r31\n"     // don't care about overwiting Z because SpmLeap doesn't need it.
        "pop r30\n"
        "reti\n"
            :
            : "I" (kTCCR0B), "I" (kTCNT0), "I" (kTIFR0)
    );
}

// NOTE: looks like this function:
// * Loads the 128 bytes from `src` into the temporary buffer
// * Erase the page at `dst`
// * Writes the page at `dst`
void ProgPage(uint8_t *src, uint16_t dst) {
    uint8_t addr;
    uint16_t data;

    // TODO: Not sure what the purpose of this is, probably just to print
    // whatever src points to
    // ShowProgSrc(src);

    for(addr=0; addr<128; addr+=2) {        // 64 words.
        data = pgm_read_word(*(uint16_t*)src);
        SpmLeapCmd(dst+addr, kFlashSpmEn, data);
        src+=2; // advance to next word
    }

    // ShowProgProgress((uint8_t*)dst,128); TODO: probably more debugging print code
    SpmLeapCmd(dst, kFlashSpmErase, data);

    // ShowProgContents(gMigrateSpmErased, (uint8_t*)dst, 128);
    SpmLeapCmd(dst, kFlashSpmWritePage, data);

    // ShowProgContents(gMigrateSpmWritten, (uint8_t*)dst, 128);
}

void BootJacker(void) {
    // Check the bootloader lock bits, if incompatible lock bits are found
    // then don't run the bootloader upgrade procedure
    if (CheckBootLock()) {
        // TODO: print error message
        while (true) { }
    }
    uint8_t spmType = FindSpm();

    // debug code that we can't use
#if 0
// #ifndef __DebugVideoOn
    // We need to disable video on FIGnition.
        TIMSK1=0;       // disable all video interrupts.
        TIFR1=0x17;     // stop all video interrupts.
        TIMSK2=0;       // disable all timer 2 interrupts.
        TIFR2=7;        // clear all timer 2 interrupts.
        EIMSK=0;        // INT1 and INT0 turned off.
        EIFR=3; // clear any pending INT1/INT0 interrupts.
        PCICR=0;        // all Pin Change interrupts off (Tape uses them).
        PCIFR=7;        // pending pin change interrupts cleared.
#endif
    PORTC&=~(1<<4); // clear LED to begin with.

    // TODO: find out what value to user here. Comments in SetupTimer0B suggest
    // that the value 40 and/or 31 were used. Potentially will be different for
    // ATmega32U4 anyway.

// #define kTestTimerWait 40
#define kTestTimerWait 31
    if (spmType == SPM_TYPE_STS_SECONDARY || spmType == SPM_TYPE_STS_IDEAL) {
        SetupTimer0B(kTestTimerWait);   // sts timing.
    } else {
        SetupTimer0B(kTestTimerWait-1); // out timing is one cycle less.
    }

    if (spmType==SPM_TYPE_STS_IDEAL || SPM_TYPE_OUT_IDEAL) {
        // program the micro boot.
        ProgPage((uint8_t*)&_etext_place_holder, kMicroBootStart);
        OCR0B = kTestTimerWait-1; // it's an out command now.

        //
        gSpmSequenceAddr=(uint16_t)&_UpgradeSpmLeap_TODO;

        // program the jump vector using the microboot spm.
        ProgPage((uint8_t*)gBootloaderJmpVector, kBootloaderStart);
    } else {
        // program the jump vector.
        ProgPage((uint8_t*)gBootloaderJmpVector, kBootloaderStart);

        OCR0B = kTestTimerWait-1; // it's an out command now.
        gSpmSequenceAddr = kBootloaderStart+4;    // it's just after the boot vector.

        // program the micro boot using the jump vector spm.
        // ProgPage((uint8_t*)&_etext, kMicroBootStart);
        ProgPage((uint8_t*)&_etext_place_holder, kMicroBootStart);
    }

    cli();
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


int main(void) {
}
