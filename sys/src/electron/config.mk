# Config.mk for electron
# /*******************************************/
# /* RUN_TIME:  Specifies whether a run-time */
# /*   module is being created.              */
# /*******************************************/

RUN_TIME = 0

# /*************************************************/
# /* DEFRULE_CONSTRUCT: Determines whether defrule */
# /*   construct is included.                      */
# /*************************************************/

DEFRULE_CONSTRUCT = 1

# /************************************************/
# /* DEFMODULE_CONSTRUCT:  Determines whether the */
# /*   defmodule construct is included.           */
# /************************************************/

DEFMODULE_CONSTRUCT=1

# /****************************************************/
# /* DEFTEMPLATE_CONSTRUCT:  Determines whether facts */
# /*   and the deftemplate construct are included.    */
# /****************************************************/

DEFTEMPLATE_CONSTRUCT=1


# /************************************************************/
# /* FACT_SET_QUERIES: Determines if fact-set query functions */
# /*  such as any-factp and do-for-all-facts are included.    */
# /************************************************************/

FACT_SET_QUERIES=1

# /****************************************************/
# /* DEFFACTS_CONSTRUCT:  Determines whether deffacts */
# /*   construct is included.                         */
# /****************************************************/

DEFFACTS_CONSTRUCT=1

# /************************************************/
# /* DEFGLOBAL_CONSTRUCT:  Determines whether the */
# /*   defglobal construct is included.           */
# /************************************************/

DEFGLOBAL_CONSTRUCT=1

# /**********************************************/
# /* DEFFUNCTION_CONSTRUCT:  Determines whether */
# /*   deffunction construct is included.       */
# /**********************************************/

DEFFUNCTION_CONSTRUCT=1

# /*********************************************/
# /* DEFGENERIC_CONSTRUCT:  Determines whether */
# /*   generic functions  are included.        */
# /*********************************************/

DEFGENERIC_CONSTRUCT=1

# /*****************************************************************/
# /* OBJECT_SYSTEM:  Determines whether object system is included. */
# /*   The MULTIFIELD_FUNCTIONS flag should also be on if you want */
# /*   to be able to manipulate multi-field slots.                 */
# /*****************************************************************/

OBJECT_SYSTEM=1

# /*****************************************************************/
# /* DEFINSTANCES_CONSTRUCT: Determines whether the definstances   */
# /*   construct is enabled.                                       */
# /*****************************************************************/

DEFINSTANCES_CONSTRUCT=1

# /********************************************************************/
# /* INSTANCE_SET_QUERIES: Determines if instance-set query functions */
# /*  such as any-instancep and do-for-all-instances are included.    */
# /********************************************************************/

INSTANCE_SET_QUERIES=1

# /*******************************************************************/
# /* BLOAD/BSAVE_INSTANCES: Determines if the save/restore-instances */
# /*  functions can be enhanced to perform more quickly by using     */
# /*  binary files                                                   */
# /*******************************************************************/

BLOAD_INSTANCES=1
BSAVE_INSTANCES=1

# /****************************************************************/
# /* EXTENDED MATH PACKAGE FLAG: If this is on, then the extended */
# /* math package functions will be available for use, (normal    */
# /* default). If this flag is off, then the extended math        */
# /* functions will not be available, and the 30K or so of space  */
# /* they require will be free. Usually a concern only on PC type */
# /* machines.                                                    */
# /****************************************************************/

EXTENDED_MATH_FUNCTIONS=1

# /****************************************************************/
# /* TEXT PROCESSING : Turn on this flag for support of the       */
# /* hierarchical lookup system.                                  */
# /****************************************************************/

TEXTPRO_FUNCTIONS=1

# /****************************************************************/
# /* HELP: To implement the help facility, set the flag below and */
# /* specify the path and name of the help data file your system. */
# /****************************************************************/

HELP_FUNCTIONS=1

HELP_DEFAULT="clips.hlp"

# /*************************************************************************/
# /* BLOAD_ONLY:      Enables bload command and disables the load command. */
# /* BLOAD:           Enables bload command.                               */
# /* BLOAD_AND_BSAVE: Enables bload, and bsave commands.                   */
# /*************************************************************************/

BLOAD_ONLY=0
BLOAD=0
BLOAD_AND_BSAVE=1

# /********************************************************************/
# /* CONSTRUCT COMPILER: If this flag is turned on, you can generate  */
# /*   C code representing the constructs in the current environment. */
# /*   With the RUN_TIME flag set, this code can be compiled and      */
# /*   linked to create a stand-alone run-time executable.            */
# /********************************************************************/

CONSTRUCT_COMPILER=1
API_HEADER="clips.h"

# /************************************************/
# /* IO_FUNCTIONS: Includes printout, read, open, */
# /*   close, format, and readline functions.     */
# /************************************************/

IO_FUNCTIONS=1

# /************************************************/
# /* STRING_FUNCTIONS: Includes string functions: */
# /*   str-length, str-compare, upcase, lowcase,  */
# /*   sub-string, str-index, and eval.           */
# /************************************************/

STRING_FUNCTIONS=1

# /*********************************************/
# /* MULTIFIELD_FUNCTIONS: Includes multifield */
# /*   functions:  mv-subseq, mv-delete,       */
# /*   mv-append, str-explode, str-implode.    */
# /*********************************************/

MULTIFIELD_FUNCTIONS=1

# /****************************************************/
# /* DEBUGGING_FUNCTIONS: Includes functions such as  */
# /*   rules, facts, matches, ppdefrule, etc.         */
# /****************************************************/

DEBUGGING_FUNCTIONS=1

# /***************************************************/
# /* PROFILING_FUNCTIONS: Enables code for profiling */
# /*   constructs and user functions.                */
# /***************************************************/

PROFILING_FUNCTIONS=1

# /************************************************************************/
# /* BLOCK_MEMORY: Causes memory to be allocated in large blocks.         */
# /*   INITBUFFERSIZE and BLOCKSIZE should both be set to less than the   */
# /*   maximum size of a signed integer.                                  */
# /************************************************************************/

BLOCK_MEMORY=0

# /*******************************************************************/
# /* WINDOW_INTERFACE : Set this flag if you are recompiling any of  */
# /*   the machine specific GUI interfaces. Currently, when enabled, */
# /*   this flag disables the more processing used by the help       */
# /*   system. This flag also prevents any input or output being     */
# /*   directly sent to stdin or stdout.                             */
# /*******************************************************************/

WINDOW_INTERFACE=0

# /*************************************************************/
# /* ALLOW_ENVIRONMENT_GLOBALS: If enabled, tracks the current */
# /*   environment and allows environments to be referred to   */
# /*   by index. If disabled, CLIPS makes no use of global     */
# /*   variables.                                              */
# /*************************************************************/

ALLOW_ENVIRONMENT_GLOBALS=1


# /********************************************/
# /* DEVELOPER: Enables code for debugging a  */
# /*   development version of the executable. */
# /********************************************/

DEVELOPER=1

DEFS= -D_POSIX_C_SOURCE=200112L \
		-DRUN_TIME=${RUN_TIME} \
		-DDEFRULE_CONSTRUCT=${DEFRULE_CONSTRUCT} \
		-DDEFMODULE_CONSTRUCT=${DEFMODULE_CONSTRUCT} \
		-DDEFTEMPLATE_CONSTRUCT=${DEFTEMPLATE_CONSTRUCT} \
		-DFACT_SET_QUERIES=${FACT_SET_QUERIES} \
		-DDEFFACTS_CONSTRUCT=${DEFFACTS_CONSTRUCT} \
		-DDEFGLOBAL_CONSTRUCT=${DEFGLOBAL_CONSTRUCT} \
		-DDEFFUNCTION_CONSTRUCT=${DEFFUNCTION_CONSTRUCT} \
		-DDEFGENERIC_CONSTRUCT=${DEFGENERIC_CONSTRUCT} \
		-DOBJECT_SYSTEM=${OBJECT_SYSTEM} \
		-DDEFINSTANCES_CONSTRUCT=${DEFINSTANCES_CONSTRUCT} \
		-DINSTANCE_SET_QUERIES=${INSTANCE_SET_QUERIES} \
		-DBLOAD_INSTANCES=${BLOAD_INSTANCES} \
		-DBSAVE_INSTANCES=${BSAVE_INSTANCES} \
		-DEXTENDED_MATH_FUNCTIONS=${EXTENDED_MATH_FUNCTIONS} \
		-DTEXTPRO_FUNCTIONS=${TEXTPRO_FUNCTIONS} \
		-DHELP_FUNCTIONS=${HELP_FUNCTIONS} \
		-DHELP_DEFAULT=\"${HELP_DEFAULT}\" \
		-DBLOAD_ONLY=${BLOAD_ONLY} \
		-DBLOAD=${BLOAD} \
		-DBLOAD_AND_BSAVE=${BLOAD_AND_BSAVE} \
		-DCONSTRUCT_COMPILER=${CONSTRUCT_COMPILER} \
		-DAPI_HEADER=\"${API_HEADER}\" \
		-DIO_FUNCTIONS=${IO_FUNCTIONS} \
		-DSTRING_FUNCTIONS=${STRING_FUNCTIONS} \
		-DMULTIFIELD_FUNCTIONS=${MULTIFIELD_FUNCTIONS} \
		-DDEBUGGING_FUNCTIONS=${DEBUGGING_FUNCTIONS} \
		-DPROFILING_FUNCTIONS=${PROFILING_FUNCTIONS} \
		-DBLOCK_MEMORY=${BLOCK_MEMORY} \
		-DWINDOW_INTERFACE=${WINDOW_INTERFACE}\
		-DALLOW_ENVIRONMENT_GLOBALS=${ALLOW_ENVIRONMENT_GLOBALS}\
		-DDEVELOPER=${DEVELOPER}

PREFIX = ${ELECTRON_PLATFORM_ROOT}/sys

INCS = -I. -I${PREFIX}/include -I/usr/include
LIBS = -L/usr/lib -lc -lm -lrt

CFLAGS = -Os ${INCS} ${DEFS}
LDFLAGS = ${LIBS}

CC ?= cc
LD ?= cc
