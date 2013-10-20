#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
#include <clips.h>
#include <srv/evt.h>

static void CallEResized(void* theEnv);
static int CallGetWindow(void* theEnv);
void InitializeDrawSystem(void* theEnv) {

   EnvDefineFunction2(theEnv,
         (char*)"eresized",
         'b',
         PTIEF CallEResized,
         (char*)"CallEResized",
         (char*)"11i");

   EnvDefineFunction2(theEnv,
         (char*)"getwindow",
         'i', 
         PTIEF CallGetWindow,
         (char*)"CallGetWindow",
         (char*)"00a");
}

void CallEResized(void* theEnv) {
   eresized((int)EnvRtnLong(theEnv, 1));
}

int CallGetWindow(void* theEnv) {
   return getwindow(display, Refnone);
}

void eresized(int new) {
   // When eresized is called, we get a fact into the expert system
   if(new) {
      EnvAssertString(GetCurrentEnvironment(), "(event resized new TRUE)");
   } else {
      EnvAssertString(GetCurrentEnvironment(), "(event resized new FALSE)");
   }
}
