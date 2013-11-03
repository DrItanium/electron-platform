#include<u.h>
#include<libc.h>
#include<draw.h>
#include<clips.h>
#include<lib/edraw.h>
/* First thing to support is color external types */
static int imageExternalAddressID;

static void BasePrintAddress(void*, char*, void*, char*);

static void PrintImageAddress(void*, char*, void*);
static intBool DeallocateImage(void*, void*);
static void NewImage(void*, DATA_OBJECT*);

static int Callrgb2cmap(void* theEnv);
static int GetStandardColor(void* theEnv);

void InitializeDrawRoutines(void* theEnv) {
   EnvDefineFunction2(theEnv,
         (char*)"rgb2cmap",
         'i',
         PTIEF Callrgb2cmap,
         (char*)"Callrgb2cmap",
         (char*)"33iiii");
   EnvDefineFunction2(theEnv,
         (char*)"get-standard-color",
         'i',
         PTIEF GetStandardColor,
         (char*)"GetStandardColor",
         (char*)"11ii");
   /* register the image type */
   struct externalAddressType image = {
      (char*)"Image",
      PrintImageAddress,
      PrintImageAddress,
      DeallocateImage,
      NewImage,
      NULL
   };

   imageExternalAddressID = InstallExternalAddressType(theEnv, &image);
}
int GetStandardColor(void* theEnv) {
   switch((int)EnvRtnLong(theEnv, 1)) {
      case 0:
         return DOpaque;
      case 1:
         return DTransparent;
      case 2:
         return DBlack;
      case 3:
         return DWhite;
      case 4:
         return DRed;
      case 5:
         return DGreen;
      case 6:
         return DBlue;
      case 7:
         return DCyan;
      case 8:
         return DMagenta;
      case 9:
         return DYellow;
      case 10:
         return DPaleyellow;
      case 11:
         return DDarkyellow;
      case 12:
         return DDarkgreen;
      case 13:
         return DPalegreen;
      case 14:
         return DMedgreen;
      case 15:
         return DDarkblue;
      case 16:
         return DPalebluegreen;
      case 17:
         return DPaleblue;
      case 18:
         return DBluegreen;
      case 19:
         return DGreygreen;
      case 20:
         return DPalegreygreen;
      case 21:
         return DYellowgreen;
      case 22:
         return DMedblue;
      case 23:
         return DGreyblue;
      case 24:
         return DPalegreyblue;
      case 25:
         return DPurpleblue;
      case 26:
         return DNotacolor;
      default:
         return DNofill;
   }
}
int Callrgb2cmap(void* theEnv) {
   int red, green, blue;
   red = (int)EnvRtnLong(theEnv, 1);
   if(red > 255 || red < 0) {
      PrintErrorID(theEnv, (char*)"CONVERSION", 1, FALSE);
      EnvPrintRouter(theEnv, WERROR, (char*)"Red value is not between 0 and 255\n");
      SetEvaluationError(theEnv, TRUE);
      return -1;
   }
   green = (int)EnvRtnLong(theEnv, 2);
   if(green > 255 || green < 0) {
      PrintErrorID(theEnv, (char*)"CONVERSION", 1, FALSE);
      EnvPrintRouter(theEnv, WERROR, (char*)"Green value is not between 0 and 255\n");
      SetEvaluationError(theEnv, TRUE);
      return -1;
   }
   blue = (int)EnvRtnLong(theEnv, 3);
   if(blue > 255 || blue < 0) {
      PrintErrorID(theEnv, (char*)"CONVERSION", 1, FALSE);
      EnvPrintRouter(theEnv, WERROR, (char*)"Blue value is not between 0 and 255\n");
      SetEvaluationError(theEnv, TRUE);
      return -1;
   }
   return rgb2cmap(red, green, blue);
}
intBool DeallocateImage(void* theEnv, void* theValue) {
   if(theValue != NULL)
      freeimage((Image*)theValue);
   return TRUE;
}

void NewImage(void* theEnv, DATA_OBJECT* retVal) {
   Image* image;
   Rectangle r;
   DATA_OBJECT repl, color, x, y, bx, by;

   if(EnvRtnArgCount(theEnv) == 7) {
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 2, INTEGER, &x) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the second argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 3, INTEGER, &y) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the third argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 4, INTEGER, &bx) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the fourth argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 5, INTEGER, &by) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the fifth argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 6, INTEGER, &repl) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected integer value for sixth argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 7, INTEGER, &color) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected integer value for seventh argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }

      r = Rect((int)DOToLong(x), (int)DOToLong(y), 
            (int)DOToLong(bx), (int)DOToLong(by));
      image = allocimage(display, r, screen->chan, (int)DOToLong(repl), (int)DOToLong(color));

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

