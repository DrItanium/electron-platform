#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
#include <clips.h>
#include <evt.h>

#define BUTTON1 (char*)"button1"
#define BUTTON2 (char*)"button2"
#define BUTTON3 (char*)"button3"
static int mouseInitialized = 0;
static void StartupMouse();
static void GetMouseButtons(void* theEnv, DATA_OBJECT_PTR returnValuePtr);
static void GetMousePosition(void* theEnv, DATA_OBJECT_PTR returnValuePtr);
static uvlong GetMouseTimeStamp(void* theEnv);
static int QueryMouse(void* theEnv);
static Mouse mouse;

void InitializeMouseInterface(void* theEnv) {
   StartupMouse();
   EnvDefineFunction2(theEnv,
         (char*)"mouse/query",
         'b',
         PTIEF QueryMouse,
         (char*)"QueryMouse",
         (char*)"00a");
   EnvDefineFunction2(theEnv,
         (char*)"mouse/buttons",
         'm',
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
}

void eresized(int new) {
   // When eresized is called, we get a fact into the expert system
   if(new) {
      EnvAssertString(GetCurrentEnvironment(), "(event resized new TRUE)");
   } else {
      EnvAssertString(GetCurrentEnvironment(), "(event resized new FALSE)");
   }
}

void StartupMouse() {
   if(!mouseInitialized) {
      einit(Emouse);
      mouseInitialized = 1;
   }
}
int QueryMouse(void* theEnv) {
   mouse = emouse();
   return TRUE;
}

void GetMouseButtons(void* theEnv, DATA_OBJECT_PTR returnValuePtr) {
   void* multifield;
   int length;

   multifield = EnvCreateMultifield(theEnv, 8);
   /* query the mouse */
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
#define AddButton(index,symb) \
   SetMFType(multifield,index,SYMBOL);\
   SetMFValue(multifield,index,EnvAddSymbol(theEnv, symb))
   if(mouse.buttons & 1) { 
      length = 1;
      AddButton(1,BUTTON1);
   } else if(mouse.buttons & 2) {
      length = 1;
      AddButton(1,BUTTON2);
   } else if(mouse.buttons & 3) {
      length = 2;
      AddButton(1, BUTTON1);
      AddButton(2, BUTTON2);
   } else if(mouse.buttons & 4) {
      length = 1;
      AddButton(1, BUTTON3);
   } else if(mouse.buttons & 5) {
      length = 2;
      AddButton(1, BUTTON1);
      AddButton(2, BUTTON3);
   } else if(mouse.buttons & 6) {
      length = 2; 
      AddButton(1, BUTTON2);
      AddButton(2, BUTTON3);
   } else if(mouse.buttons & 7) {
      length = 3;
      AddButton(1, BUTTON1);
      AddButton(2, BUTTON2);
      AddButton(3, BUTTON3);
   } else {
      length = 0;
   }
#undef AddButton

   if (length > 0) {
      SetpType(returnValuePtr, MULTIFIELD);
      SetpValue(returnValuePtr, multifield);
      SetpDOBegin(returnValuePtr, 1);
      SetpDOEnd(returnValuePtr, length);
   } else {
      SetMultifieldErrorValue(returnValuePtr);
   }
}

void GetMousePosition(void* theEnv, DATA_OBJECT_PTR returnValuePtr) {
   void* multifield;

   multifield = EnvCreateMultifield(theEnv, 2);
   SetMFType(multifield, 1, INTEGER);
   SetMFValue(multifield, 1, EnvAddLong(theEnv, mouse.xy.x));
   SetMFType(multifield, 2, INTEGER);
   SetMFValue(multifield, 2, EnvAddLong(theEnv, mouse.xy.y));

   SetpType(returnValuePtr, MULTIFIELD);
   SetpValue(returnValuePtr, multifield);
}

uvlong GetMouseTimeStamp(void* theEnv) {
   return (uvlong)mouse.msec;
}
