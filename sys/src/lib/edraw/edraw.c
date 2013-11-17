#include<u.h>
#include<libc.h>
#include<draw.h>
#include<clips.h>
#include<lib/edraw.h>

static int imageExternalAddressID;
static int rectangleExternalAddressID;
static int pointExternalAddressID;

static void DrawTextToString(void* theEnv);
static void BasePrintAddress(void*, char*, void*, char*);

static void PrintImageAddress(void*, char*, void*);
static intBool DeallocateImage(void*, void*);
static void NewImage(void*, DATA_OBJECT*);

static void PrintRectangleAddress(void*, char*, void*);
static intBool DeallocateRectangle(void*, void*);
static void NewRectangle(void*, DATA_OBJECT*);

static void PrintPointAddress(void*, char*, void*);
static intBool DeallocatePoint(void*, void*);
static void NewPoint(void*, DATA_OBJECT*);

static int Callrgb2cmap(void* theEnv);
static int GetStandardColor(void* theEnv);

static void CallScreenDraw(void* theEnv);
static int Callflushimage(void* theEnv);

void InitializeDrawRoutines(void* theEnv) {
   EnvDefineFunction2(theEnv,
         (char*)"rgb-to-cmap",
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

   EnvDefineFunction2(theEnv,
         (char*)"screen/draw",
         'v',
         PTIEF CallScreenDraw,
         (char*)"CallScreenDraw",
         (char*)"44aaaaa");

   EnvDefineFunction2(theEnv,
         (char*)"screen/flush",
         'b',
         PTIEF Callflushimage,
         (char*)"Callflushimage", 
         (char*)"11ii");

    EnvDefineFunction2(theEnv,
            (char*)"screen/draw-text",
            'v',
            PTIEF DrawTextToString,
            (char*)"DrawTextToString",
            (char*)"44aaaak");

   /* register the different external types */
   struct externalAddressType image = {
      (char*)"Image",
      PrintImageAddress,
      PrintImageAddress,
      DeallocateImage,
      NewImage,
      NULL
   };

   struct externalAddressType point = {
      (char*)"Point",
      PrintPointAddress,
      PrintPointAddress,
      DeallocatePoint,
      NewPoint,
      NULL
   };

   struct externalAddressType rect = {
      (char*)"Rectangle",
      PrintRectangleAddress,
      PrintRectangleAddress,
      DeallocateRectangle,
      NewRectangle,
      NULL
   };

   imageExternalAddressID = InstallExternalAddressType(theEnv, &image);
   pointExternalAddressID = InstallExternalAddressType(theEnv, &point);
   rectangleExternalAddressID = InstallExternalAddressType(theEnv, &rect);
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

intBool DeallocateRectangle(void* theEnv, void* theValue) {
   if(theValue != NULL)
      genfree(theEnv, theValue, sizeof(Rectangle));
   return TRUE;
}

intBool DeallocatePoint(void* theEnv, void* theValue) {
   if(theValue != NULL)
      genfree(theEnv, theValue, sizeof(Point));
   return TRUE;
}

void NewImage(void* theEnv, DATA_OBJECT* retVal) {
   Image* image;
   Rectangle* r;
   DATA_OBJECT repl, color, rp;
   int count; 
   count = EnvRtnArgCount(theEnv);
   if(count == 1) {
      /* return a nil representation of this */
      SetpType(retVal, EXTERNAL_ADDRESS);
      SetpValue(retVal, EnvAddExternalAddress(theEnv, (void*)0, imageExternalAddressID));
      return;
   } else if(count == 4) {
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 2, EXTERNAL_ADDRESS, &rp) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected a rectangle pointer for second argument.\n");
         SetEvaluationError(theEnv, TRUE);
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 3, INTEGER, &repl) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer value for third argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Image)", 4, INTEGER, &color) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer value for fourth argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      r = DOToExternalAddress(rp);
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

void NewRectangle(void* theEnv, DATA_OBJECT* retVal) {
   int count;
   Rectangle* r;
   DATA_OBJECT x, y, bx, by;

   count = EnvRtnArgCount(theEnv);

   if(count == 5) {
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
      EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected four integers representing x, y, bx, by.\n");
      SetEvaluationError(theEnv, TRUE);
      return;

   }
}
void NewPoint(void* theEnv, DATA_OBJECT* retVal) {
   int count;
   Point* p;
   DATA_OBJECT x, y;

   count = EnvRtnArgCount(theEnv);

   if(count == 3) {
      if(EnvArgTypeCheck(theEnv, "new (plan9port Point)", 2, INTEGER, &x) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the second argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      if(EnvArgTypeCheck(theEnv, "new (plan9port Point)", 3, INTEGER, &y) == FALSE) {
         PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
         EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected an integer as the third argument.\n");
         SetEvaluationError(theEnv, TRUE);
         return;
      }
      p = genalloc(theEnv, sizeof(Point));
      p->x = (int)DOToLong(x);
      p->y = (int)DOToLong(y);
      SetpType(retVal, EXTERNAL_ADDRESS);
      SetpValue(retVal, EnvAddExternalAddress(theEnv, (void*)p, pointExternalAddressID));
      return;
   } else {
      PrintErrorID(theEnv, (char*)"NEW", 1, FALSE);
      EnvPrintRouter(theEnv, WERROR, (char*)"Function new expected two integers representing x, y.\n");
      SetEvaluationError(theEnv, TRUE);
      return;
   }
}

void PrintImageAddress(void* theEnv, char* logicalName, void* theValue) {
   BasePrintAddress(theEnv, logicalName, theValue, (char*)"<Pointer-Image-");
}
void PrintRectangleAddress(void* theEnv, char* logicalName, void* theValue) {
   BasePrintAddress(theEnv, logicalName, theValue, (char*)"<Pointer-Rectangle-");
}
void PrintPointAddress(void* theEnv, char* logicalName, void* theValue) {
   BasePrintAddress(theEnv, logicalName, theValue, (char*)"<Pointer-Point-");
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


void CallScreenDraw(void* theEnv) {
   DATA_OBJECT _r, _src, _mask, _p;
   Rectangle* r;
   Image* src;
   Image* mask;
   Point* p;
   if((EnvArgTypeCheck(theEnv, (char*)"screen/draw", 1, EXTERNAL_ADDRESS, &_r) == FALSE) ||
         (EnvArgTypeCheck(theEnv, (char*)"screen/draw", 2, EXTERNAL_ADDRESS, &_src) == FALSE) ||
         (EnvArgTypeCheck(theEnv, (char*)"screen/draw", 3, EXTERNAL_ADDRESS, &_mask) == FALSE) ||
         (EnvArgTypeCheck(theEnv, (char*)"screen/draw", 4, EXTERNAL_ADDRESS, &_p) == FALSE)) {
      return;
   }
   r = DOToExternalAddress(_r);
   src = DOToExternalAddress(_src);
   mask = DOToExternalAddress(_mask);
   p = DOToExternalAddress(_p);

   if(mask == 0) {
      draw(screen, *r, src, nil, *p);
   } else {
      draw(screen, *r, src, mask, *p);
   }
}

int Callflushimage(void* theEnv) {
   int result;
   result = flushimage(display, (int)EnvRtnLong(theEnv, 1));
   if(result == -1)
      return 0;
   else
      return 1;
}


void DrawTextToString(void* theEnv) {
    DATA_OBJECT _p, _src, _sp, _str;
    Point* p;
    Image* src;
    Point* sp;
    char* str;
   if((EnvArgTypeCheck(theEnv, (char*)"screen/draw-text", 1, EXTERNAL_ADDRESS, &_p) == FALSE) ||
         (EnvArgTypeCheck(theEnv, (char*)"screen/draw-text", 2, EXTERNAL_ADDRESS, &_src) == FALSE) ||
         (EnvArgTypeCheck(theEnv, (char*)"screen/draw-text", 3, EXTERNAL_ADDRESS, &_sp) == FALSE) ||
         (EnvArgTypeCheck(theEnv, (char*)"screen/draw-text", 4, STRING, &_str) == FALSE)) {
      return;
   }
    p = DOToExternalAddress(_p);
    src = DOToExternalAddress(_src);
    sp = DOToExternalAddress(_sp);
    str = DOToString(_str);
    string(screen, *p, src, *sp, display->defaultfont, str);
}
