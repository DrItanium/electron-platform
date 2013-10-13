#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
#include <clips.h>
#include <evt.h>

static int CallInitDraw(void* theEnv);
static void CallEResized(void* theEnv);
static int CallGetWindow(void* theEnv);
static int initdrawCalled = 0;
void InitializeDrawSystem(void* theEnv) {
   EnvDefineFunction2(theEnv,
         (char*)"initdraw",
         'b',
         PTIEF CallInitDraw,
         (char*)"CallInitDraw",
         (char*)"00a");

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

int CallInitDraw(void* theEnv) {
   if(!initdrawCalled) {
      if(initdraw(0,0,"chicanery") < 0) {
         sysfatal("initdraw failed: %r");
      } else {
         initdrawCalled = 1;
         return TRUE;
      }
   } else {
      return FALSE;
   }
}

void CallEResized(void* theEnv) {
   eresized((int)EnvRtnLong(theEnv, 1));
}

int CallGetWindow(void* theEnv) {
   return getwindow(display, Refnone);
}
