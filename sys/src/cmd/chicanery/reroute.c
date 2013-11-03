#include <clips.h>
#include <srv/efs.h>
/***************/
/* DEFINITIONS */
/***************/

#define NO_SWITCH              0
#define BATCH_SWITCH           1
#define BATCH_STAR_SWITCH      2
#define LOAD_SWITCH            3

/*************************************************/
/* RerouteStdin: Processes the -f, -f2, and -l   */
/*   options available on machines which support */
/*   argc and arv command line options.          */
/*************************************************/
void RerouteStdin(void *theEnv, int argc, char *argv[]) {
   int i;
   int theSwitch = NO_SWITCH;

   /*======================================*/
   /* If there aren't enough arguments for */
   /* the -f argument, then return.        */
   /*======================================*/

   if (argc < 3) { return; }

   /*=====================================*/
   /* If argv was not passed then return. */
   /*=====================================*/

   if (argv == NULL) return;

   /*=============================================*/
   /* Process each of the command line arguments. */
   /*=============================================*/

   for (i = 1 ; i < argc ; i++) {
      if (strcmp(argv[i],"-f") == 0) theSwitch = BATCH_SWITCH;
#if ! RUN_TIME
      else if (strcmp(argv[i],"-f2") == 0) theSwitch = BATCH_STAR_SWITCH;
      else if (strcmp(argv[i],"-l") == 0) theSwitch = LOAD_SWITCH;
#endif
      else if (theSwitch == NO_SWITCH) {
         PrintErrorID(theEnv,(char*)"SYSDEP",2,FALSE);
         EnvPrintRouter(theEnv,WERROR,(char*)"Invalid option\n");
      }
      if (i > (argc-1)) {
         PrintErrorID(theEnv,(char*)"SYSDEP",1,FALSE);
         EnvPrintRouter(theEnv,WERROR,(char*)"No file found for ");
         switch(theSwitch) {
            case BATCH_SWITCH:
               EnvPrintRouter(theEnv,WERROR,(char*)"-f");
               break;
            case BATCH_STAR_SWITCH:
               EnvPrintRouter(theEnv,WERROR,(char*)"-f2");
               break;
            case LOAD_SWITCH:
               EnvPrintRouter(theEnv,WERROR,(char*)"-l");
         }
         EnvPrintRouter(theEnv,WERROR,(char*)" option\n");
         return;

      }

      switch(theSwitch) {
         case BATCH_SWITCH:
            EFS_OpenBatch(theEnv,argv[++i],TRUE);
            break;

#if (! RUN_TIME) && (! BLOAD_ONLY)
         case BATCH_STAR_SWITCH:
            EFS_EnvBatchStar(theEnv,argv[++i]);
            break;
         case LOAD_SWITCH:
            EFS_EnvLoad(theEnv,argv[++i]);
            break;
         default:
            break;
#endif
      }
   }
}
