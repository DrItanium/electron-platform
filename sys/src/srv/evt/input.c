#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
#include <clips.h>
#include <srv/evt.h>

#define BUTTON1 (char*)"button1"
#define BUTTON2 (char*)"button2"
#define BUTTON3 (char*)"button3"
/* We init both keyboard and mouse */
static int inputInitialized = 0;
static int GetMouseButtons(void* theEnv);
static void GetMousePosition(void* theEnv, DATA_OBJECT_PTR returnValuePtr);
static uvlong GetMouseTimeStamp(void* theEnv);
static int QueryMouse(void* theEnv);
static int QueryKeyboard(void* theEnv);
static int StartupInput(void* theEnv);
static Mouse m;

void InitializeInputSystem(void* theEnv) {
   /* The input system should be automatically started on initialization */
   EnvDefineFunction2(theEnv,
         (char*)"input/init",
         'b',
         PTIEF StartupInput,
         (char*)"StartupInput",
         "00a");
   EnvDefineFunction2(theEnv,
         (char*)"mouse/query",
         'b',
         PTIEF QueryMouse,
         (char*)"QueryMouse",
         (char*)"00a");
   EnvDefineFunction2(theEnv,
         (char*)"mouse/buttons",
         'i',
         PTIEF GetMouseButtons,
         (char*)"GetMouseButtons",
         (char*)"00a");
   EnvDefineFunction2(theEnv,
         (char*)"mouse/position",
         'm',
         PTIEF GetMousePosition,
         (char*)"GetMousePosition",
         (char*)"00a");
   EnvDefineFunction2(theEnv,
         (char*)"mouse/timestamp",
         'g',
         PTIEF GetMouseTimeStamp,
         (char*)"GetMouseTimeStamp",
         (char*)"00a");
   /* Router system does not support runes but chars! */
   EnvDefineFunction2(theEnv,
         (char*)"kbd/query",
         'i',
         PTIEF QueryKeyboard,
         (char*)"QueryKeyboard",
         (char*)"00a");



}

void eresized(int new) {
   // When eresized is called, we get a fact into the expert system
   if(new) {
      EnvAssertString(GetCurrentEnvironment(), "(event resized new TRUE)");
   } else {
      EnvAssertString(GetCurrentEnvironment(), "(event resized new FALSE)");
   }
}

int StartupInput(void* theEnv) {
   if(!inputInitialized) {
      einit(Emouse|Ekeyboard);
      inputInitialized = 1;
      return TRUE;
   } else {
      return FALSE;
   }
}
int QueryKeyboard(void* theEnv) {
   if(ecankbd()) {
      return ekbd();
   } else {
      return -1;
   }
}
int QueryMouse(void* theEnv) {
   if(ecanmouse()) {
      m = emouse();
      return TRUE;
   } else  {
      return FALSE;
   }

}

int GetMouseButtons(void* theEnv) {
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
   return m.buttons;
}

void GetMousePosition(void* theEnv, DATA_OBJECT_PTR returnValuePtr) {
   void* multifield;

   multifield = EnvCreateMultifield(theEnv, 2);
   SetMFType(multifield, 1, INTEGER);
   SetMFValue(multifield, 1, EnvAddLong(theEnv, m.xy.x));
   SetMFType(multifield, 2, INTEGER);
   SetMFValue(multifield, 2, EnvAddLong(theEnv, m.xy.y));

   SetpType(returnValuePtr, MULTIFIELD);
   SetpValue(returnValuePtr, multifield);
   SetpDOBegin(returnValuePtr, 1);
   SetpDOEnd(returnValuePtr, 2);
}

uvlong GetMouseTimeStamp(void* theEnv) {
   return (uvlong)m.msec;
}
