#include <stdio.h>
#include <electron/setup.h>
#include <electron/clips.h>
#include <efssrv.h>


int main(int argc, char *argv[]) {
   void *theEnv;

   theEnv = CreateEnvironment();
   DefineFSOverrideFunctions(theEnv); 
   RerouteStdin(theEnv,argc,argv);
   CommandLoop(theEnv);

   return(-1);
}
