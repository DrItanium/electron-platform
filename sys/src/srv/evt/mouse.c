#include <libc.h>
#include "engine.h"
#include <clips.h>
#include <event.h>
#include <draw.h>

static int mouseInitialized = 0;
static void StartupMouse();
static void GetMouseButtons(void* theEnv, DATA_OBJECT_PTR returnValuePtr);
void InitializeMouseInterface(void* theEnv) {
   StartupMouse();
   EnvDefineFunction2(theEnv,
         (char*)"mouse/buttons",
         'm',
         PTIEF GetMouseButtons,
         (char*)"GetMouseButtons",
         (char*)"00a");
}

void StartupMouse() {
   if(!mouseInitialized) {
      einit(Emouse);
      mouseInitialized = 1;
   }
}

void GetMouseButtons(void* theEnv, DATA_OBJECT_PTR returnValuePtr) {
   void* multifield;
   Mouse m;
   int length;

   multifield = EnvCreateMultifield(theEnv, 8);
   /* query the mouse */
   m = emouse();
   /*
    * Mouse combinations
    * m.buttons & 1 => left-click
    * m.buttons & 2 => middle-click 
    * m.buttons & 3 => left + middle
    * m.buttons & 4 => right-click 
    * m.buttons & 5 => left + right
    * m.buttons & 6 => middle + right 
    * m.buttons & 7 => left + middle + right
    *
    */
   if(m.buttons & 1) { 
      length = 1;
      SetMFType(multifield,1, SYMBOL);
      SetMFValue(multifield, 1, EnvAddSymbol(theEnv, (char*)"button1"));
   } else if(m.buttons & 2) {
      length = 1;
      SetMFType(multifield, 1, SYMBOL);
      SetMFValue(multifield, 1, EnvAddSymbol(theEnv, (char*)"button2"));
   } else if(m.buttons & 3) {
      length = 2;
      SetMFType(multifield,1, SYMBOL);
      SetMFValue(multifield, 1, EnvAddSymbol(theEnv, (char*)"button1"));
      SetMFType(multifield, 2, SYMBOL);
      SetMFValue(multifield, 2, EnvAddSymbol(theEnv, (char*)"button2"));
   } else if(m.buttons & 4) {
      length = 1;
      SetMFType(multifield,1, SYMBOL);
      SetMFValue(multifield, 1, EnvAddSymbol(theEnv, (char*)"button3"));
   } else if(m.buttons & 5) {
      length = 2;
      SetMFType(multifield,1, SYMBOL);
      SetMFValue(multifield, 1, EnvAddSymbol(theEnv, (char*)"button1"));
      SetMFType(multifield,2, SYMBOL);
      SetMFValue(multifield, 2, EnvAddSymbol(theEnv, (char*)"button3"));
   } else if(m.buttons & 6) {
      length = 2; 
      SetMFType(multifield, 1, SYMBOL);
      SetMFValue(multifield, 1, EnvAddSymbol(theEnv, (char*)"button2"));
      SetMFType(multifield,2, SYMBOL);
      SetMFValue(multifield, 2, EnvAddSymbol(theEnv, (char*)"button3"));
   } else if(m.buttons & 7) {
      length = 3;
      SetMFType(multifield,1, SYMBOL);
      SetMFValue(multifield, 1, EnvAddSymbol(theEnv, (char*)"button1"));
      SetMFType(multifield, 2, SYMBOL);
      SetMFValue(multifield, 2, EnvAddSymbol(theEnv, (char*)"button2"));
      SetMFType(multifield,3, SYMBOL);
      SetMFValue(multifield, 3, EnvAddSymbol(theEnv, (char*)"button3"));
   } else {
      length = 0;
   }

   if (length > 0) {
      SetpType(returnValuePtr, MULTIFIELD);
      SetpValue(returnValuePtr, multifield);
      SetpDOBegin(returnValuePtr, 1);
      SetpDOEnd(returnValuePtr, length);
   } else {
      SetMultifieldErrorValue(returnValuePtr);
   }

}
