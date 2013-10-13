#include <srv/evt.h>

void InitializeEventSystem(void* theEnv) {
   InitializeDrawSystem(theEnv);
   InitializeMouseInterface(theEnv);
}
