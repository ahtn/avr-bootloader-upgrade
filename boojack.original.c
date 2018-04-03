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

typedef enum {
	kSpmTypeStsIdeal=0,
	kSpmTypeStsSecondary,
	kSpmTypeOutIdeal,
	kSpmTypeOutSecondary,
	kSpmTypeNone=7
} kSpmType;

extern byte _etext;

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

ushort gSpmSequenceAddr;

ushort FlashLpmData(ushort addr, byte spmCmd)
{
	ushort val;
	asm volatile("push r0\n"
				"push r1\n"
				"push r16\n"
				"push r30\n"
				"push r31\n"
				"movw r30,%1\n"	// set the addr to be written.
				"1: in r16,%3\n"	// 
				"sbrc r16,0\n"	//wait for operation complete.
				"rjmp 1b\n"
				"cli\n"
				"out %3,%2\n"
				"lpm %0,Z\n"			// now we start the load/erase/write.
				"sei\n"
				"pop r31\n"
				"pop r30\n"
				"pop r16\n"
				"pop r1\n"
				"pop r0\n" : "=r" (val): "r" (addr), "r" (spmCmd), "I" (kSpmCsr));
	return val;
}

void CheckBootLock(void)
{
	byte k;
	ushort bootLockBits;
	bootLockBits=FlashLpmData(1, kFlashSpmBlbSet);
	if((bootLockBits&0x3f)!=0x3f) {
		DotQuotePgm(gMigrateBootLockMsg);
		DotHex(bootLockBits);
		for(;;)
			;
	}
}

byte FindSpm(void)
{
	byte spmType=kSpmTypeNone;
	ushort ix;
	for(ix=kBootloaderStart;ix<kBootloaderEnd && ((spmType&1)==1);ix+=2) {
		if( (GetPgmWord(*(ushort*)ix)&~kStsRegMask)==kStsIns &&
			GetPgmWord(*(ushort*)(ix+2))==kSpmCsrMem &&
			GetPgmWord(*(ushort*)(ix+4))==kSpmIns) {
			spmType=(ix+8<kMicroBootStart)?kSpmTypeStsIdeal:kSpmTypeStsSecondary;
		}
		if( (GetPgmWord(*(ushort*)ix)&~kOutSpmCsrRegMask)==kOutSpmCsrIns &&
			GetPgmWord(*(ushort*)(ix+2))==kSpmIns) {
			spmType=(ix+6<kMicroBootStart)?kSpmTypeOutIdeal:kSpmTypeOutSecondary;
		}
		if(spmType!=kSpmTypeNone)
			gSpmSequenceAddr=ix;
	}
	return spmType;
}

void SetupTimer0B(byte cycles)
{
	// clear PCINT2.
	PCICR&=~(1<<PCIE2);	// disable PCINT2 interrupts.
	PCIFR|=(1<<PCIE2);	// clear any pending PCINT2 interrupts.
	
	TCCR0B=0;	// stop the timer.
	TCCR0A=0;	// mode 0, no OCR outputs.
	TCNT0=0;	// reset the timer
	TIFR0=(1<<OCF0B)|(1<<OCF0A)|(1<<TOV0);	// clear all pending timer0 interrupts.
	OCR0B=cycles;	// 40 clocks from now (40 in test, 31 for real).
	TIMSK0=(1<<OCIE0B);	// OCR0B interrupt enabled.
}

#define kTCCR0B 0x25
#define kTCNT0 0x26
#define kTIFR0 0x15

void SpmLeapCmd(ushort addr, byte spmCmd, ushort optValue)
{
	byte cmdReg,tmp=0;
	cmdReg=(byte)((GetPgmWord(*(ushort*)gSpmSequenceAddr)>>4)&0x1f);
	PINC|=(1<<4);
	asm volatile(
				"push r0\n"
				"push r1\n"		// needed for opt command.
				"push %9\n"
				"push r30\n"
				"push r31\n"
				"SpmLeapCmdWaitSpm: in %9,%8\n"	// 
				"sbrc %9,0\n"	//wait for spm operation complete.
				"rjmp SpmLeapCmdWaitSpm\n"
				"ldi %9,1\n"	// timer 0 start at fClk
				"out %2,%9\n"	// set TCCR0B so off we go. This is time 0c.
				"movw r0,%5\n"	// set the value to be written.

				"mov r30,%3\n"	// get the register used by the sequence's spm command.
				"ldi r31,0\n"	// z^reg to save.
				"ld %9,Z\n"	// get the reg
				"push %9\n"	// saved it, now we can overwrite with spm command.

				"ldi r30,lo8(pm(SpmLeapCmdRet))\n"
				"ldi r31,hi8(pm(SpmLeapCmdRet))\n"
				"push r30\n"
				"push r31\n"	// return address must be pushed big-endian.
				"lds r30,%7\n"	// lo byte of Spm sequence address
				"lds r31,%7+1\n" // hi byte of Spm sequence address. z^sequence in code.
				"lsr r31\n"
				"ror r30\n"		// div 2 to get correct Spm program address.
				"push r30\n"
				"push r31\n"	// Spm sequence program address must be pushed big-endian.		

				"push %A6\n"	// before we overwrite reg used by sequence's spm command
								// we must first save the spm target address
				"push %B6\n"	// in case it would get overwritten by the st Z.

				"mov r30,%3\n"	// get the register used by the sequence's spm command.
				"ldi r31,0\n"	// z^reg to save.
				"st Z,%4\n"	// store the command in the reg.
				"pop r31\n"
				"pop r30\n"	// restore the spm target address into Z.
				"ret\n"			// return to bootloader.
				// sts (2c)
				// spm (1c). 42c in total, timer should be set to 40.
				"SpmLeapCmdRet:pop %9\n"		// restore command Reg.
				"mov r30,%3\n"
				"ldi r31,0\n"	// z^reg to save.
				"st Z,%9\n"	// pop the reg		
				"pop r31\n"
				"pop r30\n"
				"pop %9\n"
				"pop r1\n"
				"pop r0\n" : "=d" (tmp), "=r" (addr) : "I" (kTCCR0B), "r" (cmdReg), "r" (spmCmd),
								"r" (optValue), "0" (addr), "g" (gSpmSequenceAddr), "I" (kSpmCsr), "d" (tmp) );
}

/**
 * The timer interrupt interrupted bootloader execution
 * just after the spm instruction.
 * if we ret then we'll get back to the bootloader.
 * we need to pop the return address and then ret, which
 * should take us back to the SpmLeapCommand.
 **/
ISR(__vector_15, ISR_NAKED) // OCR0B
{
	//PORTC&=~(1<<4);
	PINC|=(1<<4);
	asm volatile(
		"ldi r30,0\n"
		"out %0,r30\n"	// stop timer 0
		"out %1,r30\n"	// reset timer 0.
		"ldi r30,7\n"
		"out %2,r30\n"	// clear interrupts on timer 0.
		"pop r31\n"	// don't care about overwiting Z because SpmLeap doesn't need it.
		"pop r30\n"
		"reti\n" : : "I" (kTCCR0B), "I" (kTCNT0), "I" (kTIFR0));
}

const byte gBootloaderJmpVector[] PROGMEM =
{ 0x0c, 0x94, 0xc0, 0x3f, // A vector to the 128b mini Bootloader.
  0x57, 0xbf, 0xe8, 0x95, // An out, spm command.
  0x00, 0x00             // A nop instruction.
};

void ProgPage(byte *src, ushort dst)
{
	byte ix;
	ushort data;
	ShowProgSrc(src);
	for(ix=0;ix<128;ix+=2) {	// 64 words.
		data=GetPgmWord(*(ushort*)src);
		SpmLeapCmd(dst+ix,kFlashSpmEn,data);
		src+=2;	// advance to next word
	}
	ShowProgProgress((byte*)dst,128);
	SpmLeapCmd(dst,kFlashSpmErase,data);
	ShowProgContents(gMigrateSpmErased, (byte*)dst, 128);
	SpmLeapCmd(dst,kFlashSpmWritePage,data);
	ShowProgContents(gMigrateSpmWritten, (byte*)dst, 128);
}

extern void _Upgrader(void);
extern byte _UpgradeSpmLeap;

void BootJacker(void)
{
    CheckBootLock();
    byte spmType=FindSpm();
#ifndef __DebugVideoOn
    // We need to disable video on FIGnition.
	TIMSK1=0;	// disable all video interrupts.
	TIFR1=0x17;	// stop all video interrupts.
	TIMSK2=0;	// disable all timer 2 interrupts.
	TIFR2=7;	// clear all timer 2 interrupts.
	EIMSK=0;	// INT1 and INT0 turned off.
	EIFR=3;	// clear any pending INT1/INT0 interrupts.
	PCICR=0;	// all Pin Change interrupts off (Tape uses them).
	PCIFR=7;	// pending pin change interrupts cleared.
#endif
	PORTC&=~(1<<4);	// clear LED to begin with.
	if(spmType==kSpmTypeStsSecondary || spmType==kSpmTypeStsIdeal)
		SetupTimer0B(kTestTimerWait);	// sts timing.
	else
		SetupTimer0B(kTestTimerWait-1);	// out timing is one cycle less.
	if(spmType==kSpmTypeStsIdeal || spmType==kSpmTypeOutIdeal) {
		ProgPage((byte*)&_etext, kMicroBootStart);	// program the micro boot.
		OCR0B=kTestTimerWait-1;	// it's an out command now.
		gSpmSequenceAddr=(ushort)&_UpgradeSpmLeap;
		ProgPage((byte*)gBootloaderJmpVector, kBootloaderStart);	// program the jump vector.
		// using the microboot spm.
	}
	else {
		ProgPage((byte*)gBootloaderJmpVector, kBootloaderStart);	// program the jump vector.
		OCR0B=kTestTimerWait-1;	// it's an out command now.
		gSpmSequenceAddr=kBootloaderStart+4;	// it's just after the boot vector.
		ProgPage((byte*)&_etext, kMicroBootStart);	// program the micro boot using the jump
		// vector spm.
	}
	cli();
	asm volatile("ldi r30,0\n"
			"ldi r31,0\n"
			"ldi r16,(1<<3) | (1<<5) | (1<<1) | (1<<2)\n"
			"out %0,r16\n"	// PORTB.0 and B.4 inputs, B.1, B.2, B.3, B.5 outputs."
			"ldi r16,(1<<3) | (1<<5) | (1<<1) | (1<<2)\n"//	;Select Flash, not SRAM."
			"out %1,r16\n"
			"sbi %2,7\n"	//	PORTD.7 output"
			"cbi %3,7\n"	// outputting 0. Ready to read"
			"jmp _Upgrader\n" : : "I" (kDDRB), "I" (kPORTB), "I" (kDDRD), "I" (kPORTD) );

}

