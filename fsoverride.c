
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

#include "efssrv.h"
#include <stdlib.h>

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
}
// C access functions
int EFS_Batch(void* theEnv, char* path) {
    int result;
    char* tmp, base;

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base != NULL) {
        int size = sizeof(char) * (strlen(path) + strlen(base) + 1);
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
    int result;
    char* tmp, base;

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base != NULL) {
        int size = sizeof(char) * (strlen(path) + strlen(base) + 1);
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
    int result;
    char* tmp, base;

    base = getenv((const char*)FILE_SYSTEM_BASE);

    if(base != NULL) {
        int size = sizeof(char) * (strlen(path) + strlen(base) + 2);
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


