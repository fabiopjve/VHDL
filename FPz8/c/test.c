// Test application - blink leds on PAOUT
#include <ez8f1622.h>

unsigned char array[]={1,3,7,15,31,63,127,255,127,63,31,15,7,3,1,1,1,127,1,0};

void interrupt isr_timer(){
	static unsigned char temp;
	PAOUT = array[temp++];
	if (!array[temp]) temp=0;
}

void main(){
	PAOUT = 0;
	SET_VECTOR(3,isr_timer);
	T0R=0x7FFF;
	T0CTL1=0xB8;
	IRQ0ENL = 0x80;
	EI();
	while(1);
}
