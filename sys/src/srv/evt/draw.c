#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
#include <clips.h>
#include <evt.h>

static int CallInitDraw(void* theEnv);
static int initdrawCalled = 0;
void InitializeDrawSystem(void* theEnv) {
   EnvDefineFunction2(theEnv,
         (char*)"initdraw",
         'b',
         PTIEF CallInitDraw,
         (char*)"CallInitDraw",
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
