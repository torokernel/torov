[BITS 64]
global start
extern _mainCRTStartup
start:
  jmp _mainCRTStartup
