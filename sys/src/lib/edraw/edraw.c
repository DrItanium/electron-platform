#include<u.h>
#include<libc.h>
#include<draw.h>
#include<clips.h>
#include<lib/edraw.h>
/* First thing to support is color external types */
static int imageExternalAddressID;
static int rectangleExternalAddressID;

static void BasePrintAddress(void*, char*, void*, char*);

static void PrintImageAddress(void*, char*, void*);
static intBool DeallocateImage(void*, void*);
static void NewImage(void*, DATA_OBJECT*);

static void PrintRectangleAddress(void*, char*, void*);
static intBool DeallocateRectangle(void*, void*);
static void NewRectangle(void*, DATA_OBJECT*);

void InitializeDrawRoutines(void* theEnv) {
   /* register the image type */
   struct externalAddressType image = {
      (char*)"Image",
      PrintImageAddress,
      PrintImageAddress,
      DeallocateImage,
      NewImage,
      NULL
   };
   /* register the rectangle type */
   struct externalAddressType rectangle = {
      (char*)"Rectangle",
      PrintRectangleAddress,
      PrintRectangleAddress,
      DeallocateRectangle,
      NewRectangle,
      NULL
   };

   imageExternalAddressID = InstallExternalAddressType(theEnv, &image);
   rectangleExternalAddressID = InstallExternalAddressType(theEnv, &rectangle);
}
intBool DeallocateRectangle(void* theEnv, void* theValue) {
   Rectangle* r;
   if(theValue != NULL) {
      r = (Rectangle*)theValue;
      genfree(theEnv, (void*)r, sizeof(Rectangle));
   }
   return TRUE;
}

intBool DeallocateImage(void* theEnv, void* theValue) {
   if(theValue != NULL)
      freeimage((Image*)theValue);
   return TRUE;
}
void NewRectangle(void* theEnv, DATA_OBJECT* retVal) {
   Rectangle* r;
   DATA_OBJECT x, y, bx, by;

   if(EnvRtnArgCount(theEnv) == 5) {
      if(EnvArgTypeCheck(theEnv, "new (plan9port Rectangle)", 2, INTEGER, &x) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the second argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Rectangle)", 3, INTEGER, &y) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the third argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Rectangle)", 4, INTEGER, &bx) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the fourth argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Rectangle)", 5, INTEGER, &by) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the fifth argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }

      r = genalloc(theEnv, sizeof(Rectangle));
      r->min.x = (int)DOToLong(x);
      r->min.y = (int)DOToLong(y);
      r->max.x = (int)DOToLong(bx);
      r->max.y = (int)DOToLong(by);

      SetpType(retVal, EXTERNAL_ADDRESS);
      SetpValue(retVal, EnvAddExternalAddress(theEnv, (void*)r, rectangleExternalAddressID));
      return;
   } else {
      PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
      EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected x, y, bx, by.\n");
      SetEvaluationError(theEnv, TRUE);
      return;
   }
}

void NewImage(void* theEnv, DATA_OBJECT* retVal) {
   Image* image;
   Rectangle* r;
   DATA_OBJECT rectangle, repl, color;

   if(EnvRtnArgCount(theEnv)== 4) {
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 2, EXTERNAL_ADDRESS, &rectangle) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected a rectangle as second argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 3, INTEGER, &repl) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected integer value for third argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 4, INTEGER, &color) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected integer value for fourth argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      r = DOToExternalAddress(rectangle);
      image = allocimage(display, *r, screen->chan, (int)DOToLong(repl), (int)DOToLong(color));
      if(image == 0) {
         PrintErrorID(theEnv, (char*)"NEW", 2, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Display server has run out of image memory!\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      } else {
         SetpType(retVal, EXTERNAL_ADDRESS);
         SetpValue(retVal, EnvAddExternalAddress(theEnv, (void*)image, imageExternalAddressID));
         return;
      }
   } else {
      PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
      EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected a rectangle, replication flag, and a color.\n");
      SetEvaluationError(theEnv, TRUE);
      return;
   }
}


void PrintRectangleAddress(void* theEnv, char* logicalName, 
      void* theValue) {
   BasePrintAddress(theEnv, logicalName, theValue, (char*)"<Pointer-Rectangle-");
}
void PrintImageAddress(void* theEnv, char* logicalName, 
      void* theValue) {
   BasePrintAddress(theEnv, logicalName, theValue, (char*)"<Pointer-Image-");
}

void BasePrintAddress(void* theEnv, char* logicalName, void* theValue, 
      char* precursor) {
   char buffer[20];
   void* ptr;

   EnvPrintRouter(theEnv, logicalName, precursor);
   ptr = ValueToExternalAddress(theValue);
   if(ptr) {
      gensprintf(buffer, "%p", ptr);
   } else {
      gensprintf(buffer, "%p", theValue);
   }
   EnvPrintRouter(theEnv, logicalName, buffer);
   EnvPrintRouter(theEnv, logicalName, (char*)">");
}

