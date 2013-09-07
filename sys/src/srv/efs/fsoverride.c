/*
 * efssrv
 * Copyright (c) 2013, Joshua Scoggins 
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdlib.h>
#include <electron/clips.h>
#include "config.h"
#include "efssrv.h"

void DefineFSOverrideFunctions(void* theEnv) {
    EnvDefineFunction2(theEnv,
            (char*)"batch",
            'b',
            PTIEF EFS_BatchCommand,
            (char*)"EFS_BatchCommand",
            (char*)"11k");
    EnvDefineFunction2(theEnv,
            (char*)"batch*",
            'b',
            PTIEF EFS_BatchStarCommand,
            (char*)"EFS_BatchStarCommand",
            (char*)"11k");
    EnvDefineFunction2(theEnv,
            (char*)"load",
            'b',
            PTIEF EFS_LoadCommand,
            (char*)"EFS_LoadCommand",
            (char*)"11k");
    EnvDefineFunction2(theEnv,
            (char*)"load*",
            'b',
            PTIEF EFS_LoadStarCommand,
            (char*)"EFS_LoadStarCommand",
            (char*)"11k");
    EnvDefineFunction2(theEnv,
            (char*)"open",       
            'b', 
            PTIEF EFS_OpenFunction,  
            (char*)"EFS_OpenFunction", 
            (char*)"23*k");
    EnvDefineFunction2(theEnv,
            (char*)"remove",   
            'b', 
            PTIEF EFS_RemoveFunction,  
            (char*)"EFS_RemoveFunction", 
            (char*)"11k");
    EnvDefineFunction2(theEnv,
            (char*)"rename",   
            'b',
            PTIEF EFS_RenameFunction, 
            (char*)"EFS_RenameFunction", 
            (char*)"22k");
}
// C access functions
int EFS_OpenBatch(void* theEnv, char* path, int placeAtEnd) {
    int result, size;
    char* tmp;
    char* base;

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base != NULL) {
        size = sizeof(char) * (strlen(path) + strlen(base) + 2);
        tmp = gm1(theEnv, size);
        gensprintf(tmp, "%s/%s", base, path);
        result = OpenBatch(theEnv, tmp, placeAtEnd);
        rm(theEnv,tmp, size);
        return result;
    } else {
        EnvPrintRouter(theEnv, WERROR, (char*)"Variable " FILE_SYSTEM_BASE " was not defined, can't continue!\n");
        SetEvaluationError(theEnv, TRUE);
        SetHaltExecution(theEnv, TRUE);
        return 0;
    }

}
int EFS_Batch(void* theEnv, char* path) {
    int result, size;
    char* tmp;
    char* base;

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base != NULL) {
        size = sizeof(char) * (strlen(path) + strlen(base) + 2);
        tmp = gm1(theEnv, size);
        gensprintf(tmp, "%s/%s", base, path);
        result = Batch(theEnv, tmp);
        rm(theEnv,tmp, size);
        return result;
    } else {
        EnvPrintRouter(theEnv, WERROR, (char*)"Variable " FILE_SYSTEM_BASE " was not defined, can't continue!\n");
        SetEvaluationError(theEnv, TRUE);
        SetHaltExecution(theEnv, TRUE);
        return 0;
    }
}
int EFS_EnvBatchStar(void* theEnv, char* path) {
    int result, size;
    char* tmp;
    char* base;

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base != NULL) {
        size = sizeof(char) * (strlen(path) + strlen(base) + 2);
        tmp = gm1(theEnv, size);
        gensprintf(tmp, "%s/%s", base, path);
        result = EnvBatchStar(theEnv, tmp);
        rm(theEnv,tmp, size);
        return result;
    } else {
        EnvPrintRouter(theEnv, WERROR, (char*)"Variable " FILE_SYSTEM_BASE " was not defined, can't continue!\n");
        SetEvaluationError(theEnv, TRUE);
        SetHaltExecution(theEnv, TRUE);
        return 0;
    }
}

int EFS_EnvLoad(void* theEnv, char* path) {
    int result, size;
    char* tmp; 
    char* base;

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base != NULL) {
        size = sizeof(char) * (strlen(path) + strlen(base) + 2);
        tmp = gm1(theEnv, size);
        gensprintf(tmp, "%s/%s", base, path);
        result = EnvLoad(theEnv, tmp);
        rm(theEnv,tmp, size);
        return result;
    } else {
        EnvPrintRouter(theEnv, WERROR, (char*)"Variable " FILE_SYSTEM_BASE " was not defined, can't continue!\n");
        SetEvaluationError(theEnv, TRUE);
        SetHaltExecution(theEnv, TRUE);
        return 0;
    }
}

//Interface functions - Taken from electron/filecom.c

int EFS_LoadCommand(void *theEnv) {
#if (! BLOAD_ONLY) && (! RUN_TIME)
    char *theFileName;
    int rv;

    if (EnvArgCountCheck(theEnv,(char*)"load",EXACTLY,1) == -1) 
        return(FALSE);
    if ((theFileName = GetFileName(theEnv,(char*)"load",1)) == NULL) 
        return(FALSE);

    SetPrintWhileLoading(theEnv,TRUE);

    if ((rv = EFS_EnvLoad(theEnv,theFileName)) == FALSE) {
        SetPrintWhileLoading(theEnv,FALSE);
        OpenErrorMessage(theEnv,(char*)"load",theFileName);
        return(FALSE);
    }

    SetPrintWhileLoading(theEnv,FALSE);
    if (rv == -1) 
        return(FALSE);
    return(TRUE);
#else
    EnvPrintRouter(theEnv,WDIALOG,(char*)"Load is not available in this environment\n");
    return(FALSE);
#endif
}

int EFS_LoadStarCommand(void *theEnv) {
#if (! BLOAD_ONLY) && (! RUN_TIME)
    char *theFileName;
    int rv;

    if (EnvArgCountCheck(theEnv,(char*)"load*",EXACTLY,1) == -1) 
        return(FALSE);
    if ((theFileName = GetFileName(theEnv,(char*)"load*",1)) == NULL) 
        return(FALSE);

    if ((rv = EFS_EnvLoad(theEnv,theFileName)) == FALSE) {
        OpenErrorMessage(theEnv,(char*)"load*",theFileName);
        return(FALSE);
    }

    if (rv == -1) 
        return(FALSE);
    return(TRUE);
#else
    EnvPrintRouter(theEnv,WDIALOG,(char*)"Load* is not available in this environment\n");
    return(FALSE);
#endif
}


int EFS_BatchCommand(void *theEnv) {
    char *fileName;

    if (EnvArgCountCheck(theEnv,(char*)"batch",EXACTLY,1) == -1) 
        return(FALSE);
    if ((fileName = GetFileName(theEnv,(char*)"batch",1)) == NULL) 
        return(FALSE);

    return(EFS_Batch(theEnv,fileName));
}

int EFS_BatchStarCommand(void *theEnv) {
    char *fileName;

    if (EnvArgCountCheck(theEnv,(char*)"batch*",EXACTLY,1) == -1) 
        return(FALSE);
    if ((fileName = GetFileName(theEnv,(char*)"batch*",1)) == NULL) 
        return(FALSE);

    return(EFS_EnvBatchStar(theEnv,fileName));
}


int EFS_RemoveFunction(void *theEnv) {
    char *theFileName;
    char *base;
    char* tmp;
    int result, size;


    if (EnvArgCountCheck(theEnv,(char*)"remove",EXACTLY,1) == -1) 
        return(FALSE);


    if ((theFileName = GetFileName(theEnv,(char*)"remove",1)) == NULL) 
        return(FALSE);

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base == NULL) {
        return 0;
    } else {
        size = sizeof(char) * (strlen(theFileName) + strlen(base) + 2);
        tmp = gm1(theEnv, size);
        gensprintf(tmp, "%s/%s", base, theFileName);
        result = (genremove(theFileName));
        rm(theEnv,tmp, size);
        return result;
    }

}

int EFS_RenameFunction(void *theEnv) {
    char* oldFileName;
    char* newFileName;
    char* old;
    char* new;
    char* base;
    int sizeOld, sizeNew, result;

    if (EnvArgCountCheck(theEnv,(char*)"rename",EXACTLY,2) == -1) 
        return(FALSE);


    if ((oldFileName = GetFileName(theEnv,(char*)"rename",1)) == NULL) 
        return(FALSE);
    if ((newFileName = GetFileName(theEnv,(char*)"rename",2)) == NULL) 
        return(FALSE);

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base == NULL) {
        return 0;
    } else {
        sizeOld = sizeof(char) * (strlen(base) + strlen(oldFileName) + 2);
        sizeNew = sizeof(char) * (strlen(base) + strlen(newFileName) + 2);
        old = gm1(theEnv, sizeOld);
        new = gm1(theEnv, sizeNew);
        gensprintf(old, "%s/%s", base, oldFileName);
        gensprintf(new, "%s/%s", base, newFileName);
        result = genrename(old,new);
        rm(theEnv,old, sizeOld);
        rm(theEnv,new, sizeNew);
        return result;
    }

}
int EFS_OpenFunction(void *theEnv) {
    int numberOfArguments, size, result;
    char* base;
    char* tmp;
    char* fileName;
    char* logicalName;
    char* accessMode = NULL;
    DATA_OBJECT theArgument;

    if ((numberOfArguments = EnvArgRangeCheck(theEnv,(char*)"open",2,3)) == -1) 
        return(0);

    if ((fileName = GetFileName(theEnv,(char*)"open",1)) == NULL) 
        return(0);


    logicalName = GetLogicalName(theEnv,2,NULL);
    if (logicalName == NULL) {
        SetHaltExecution(theEnv,TRUE);
        SetEvaluationError(theEnv,TRUE);
        IllegalLogicalNameMessage(theEnv,(char*)"open");
        return(0);
    }

    if (FindFile(theEnv,logicalName)) {
        SetHaltExecution(theEnv,TRUE);
        SetEvaluationError(theEnv,TRUE);
        PrintErrorID(theEnv,(char*)"IOFUN",2,FALSE);
        EnvPrintRouter(theEnv,WERROR,(char*)"Logical name ");
        EnvPrintRouter(theEnv,WERROR,logicalName);
        EnvPrintRouter(theEnv,WERROR,(char*)" already in use.\n");
        return(0);
    }

    if (numberOfArguments == 2) { 
        accessMode = (char*)"r"; 
    } else if (numberOfArguments == 3) {
        if (EnvArgTypeCheck(theEnv,(char*)"open",3,STRING,&theArgument) == FALSE) 
            return(0);
        accessMode = DOToString(theArgument);
    }

    if ((strcmp(accessMode,"r") != 0) &&
            (strcmp(accessMode,"w") != 0) &&
            (strcmp(accessMode,"a") != 0) &&
            (strcmp(accessMode,"r+") != 0) &&
            (strcmp(accessMode,"w+") != 0) &&
            (strcmp(accessMode,"a+") != 0) &&
            (strcmp(accessMode,"rb") != 0) &&
            (strcmp(accessMode,"wb") != 0) &&
            (strcmp(accessMode,"ab") != 0) &&
            (strcmp(accessMode,"r+b") != 0) &&
            (strcmp(accessMode,"w+b") != 0) &&
            (strcmp(accessMode,"a+b") != 0)) {
        SetHaltExecution(theEnv,TRUE);
        SetEvaluationError(theEnv,TRUE);
        ExpectedTypeError1(theEnv,(char*)"open",3,(char*)"string with value \"r\", \"w\", \"a\", \"r+\", \"w+\", \"rb\", \"wb\", \"ab\", \"r+b\", or \"w+b\"");
        return(0);
    }


    base = getenv((const char*)FILE_SYSTEM_BASE);
    if(base == NULL) {
        return 0;
    } else {
        size = sizeof(char) * (strlen(fileName) + strlen(base) + 2);
        tmp = gm1(theEnv, size);
        gensprintf(tmp, "%s/%s", base, fileName);
        result = OpenAFile(theEnv,tmp,accessMode,logicalName);
        rm(theEnv,tmp, size);
        return result;
    }
}
