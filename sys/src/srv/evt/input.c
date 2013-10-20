#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
#include <cursor.h>
#include <clips.h>
#include <srv/evt.h>


/* We init both keyboard and mouse */
static int inputInitialized = 0;
static int menuExternalAddressID; 
static Mouse m;
/* static functions */
static int GetMouseButtons(void* theEnv);
static void GetMousePosition(void* theEnv, DATA_OBJECT_PTR returnValuePtr);
static uvlong GetMouseTimeStamp(void* theEnv);
static int QueryMouse(void* theEnv);
static int QueryKeyboard(void* theEnv);
static int StartupInput(void* theEnv);

/* Menu related operations */
static void PrintMenuAddress(void*, char*, void*);
static intBool DeallocateMenu(void*, void*);
static void NewMenu(void*, DATA_OBJECT*);

static int ShowMenu(void* theEnv);

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
   EnvDefineFunction2(theEnv,
         (char*)"menu/show",
         'i',
         PTIEF ShowMenu,
         (char*)"ShowMenu",
         (char*)"22aia");

   struct externalAddressType nullTerminatedList = {
      (char*)"menu",
      PrintMenuAddress,
      PrintMenuAddress,
      DeallocateMenu,
      NewMenu,
      NULL
   };

   menuExternalAddressID = InstallExternalAddressType(theEnv, &nullTerminatedList);
   
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

void PrintMenuAddress(void* theEnv, char* logicalName, 
      void* theValue) {
   char buffer[20];
   void* ptr;

   EnvPrintRouter(theEnv, logicalName, 
         (char*)"<Pointer-Menu-");
   ptr = ValueToExternalAddress(theValue);
   if(ptr) {
      gensprintf(buffer, "%p", ptr);
   } else {
      gensprintf(buffer, "%p", theValue);
   }
   EnvPrintRouter(theEnv,logicalName, buffer);
   EnvPrintRouter(theEnv,logicalName, (char*)">");

}

intBool DeallocateMenu(void* theEnv, void* theValue) {
   Menu *m;
   char** contents;
   char* tmp;
   int length, i;

   if(theValue != NULL) {
      m = (Menu*)theValue;
      length = 0;
      contents = m->item;
      //HACK! HACK! Ensure that we know how long it is since we've lost that
      //information
      for(; contents != 0; contents++, length++);
      length++; //include the null element at the end
      for(i = 0; i < length; i++) {
         tmp = (m->item)[i];
         genfree(theEnv,(void*)tmp, strlen(tmp));
      }
      genfree(theEnv, (void*)m->item, sizeof(char*) * length);
      genfree(theEnv, (void*)m, sizeof(Menu));
   }
   return TRUE; 
}

void NewMenu(void* theEnv, DATA_OBJECT* retVal) {
   /* So we need to allocate a new list of elements. To do this we need
    * to know what this list will consist of first */

   int numberOfArguments, i, j, len;
   Menu* m;
   char** elements;
   char* tmp;
   char* persist;
   DATA_OBJECT current;

   numberOfArguments = EnvRtnArgCount(theEnv);
   if(numberOfArguments > 1) {
      m = genalloc(theEnv, sizeof(Menu));
      elements = genalloc(theEnv, sizeof(char*) * (numberOfArguments + 1));
      m->item = elements;
      elements[numberOfArguments] = nil;
      for(i = 0, j = 2; i < (numberOfArguments - 1); i++, j++) {
         /* we need to get the current entry and then copy it to make sure */
         if(EnvArgTypeCheck(theEnv, "new (plan9port menu)", j, SYMBOL_OR_STRING, &current) == FALSE) {
            i--; /* ensure that we don't free the uninitialized part */
            for(; i >= 0; i--) {
               genfree(theEnv, (void*) elements[i], strlen(elements[i]));
            }
            genfree(theEnv, (void *)elements, sizeof(char*) * numberOfArguments);
            genfree(theEnv, (void *)m, sizeof(Menu));
            return;
         }
         tmp = DOToString(current);
         len = strlen(tmp);

         persist = genalloc(theEnv, len + 1);
         gensprintf(persist, "%s", tmp);
         elements[i] = persist;
      }
      SetpType(retVal, EXTERNAL_ADDRESS);
      SetpValue(retVal, EnvAddExternalAddress(theEnv, (void*)m, menuExternalAddressID));
   } else {
      PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
      EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected a list of names.\n");
      SetEvaluationError(theEnv, TRUE);
      return;
   }
}
static int ShowMenu(void* theEnv) {
   int button;
   Menu* menu; 

  //TODO: Finish
  return -1;
}

