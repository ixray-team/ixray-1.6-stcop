////////////////////////////////////////////////////////////////////////////
//	Module 		: misc.h
//	Created 	: 20.03.2002
//  Modified 	: 09.10.2002
//	Author		: Dmitriy Iassenev
//	Description : Miscellanious routines
////////////////////////////////////////////////////////////////////////////

#pragma once

////////////////////////////////////////////////////////////////////////////
// exportes variables
////////////////////////////////////////////////////////////////////////////

extern char caLogDataFormat[260]		;
extern char caTextDataFormat[260]		;
extern char caBinaryDataFormat[260]		;
extern char caConfigDataFormat[260]		;
extern char caPatternDataFormat[260]	;
extern char caCoreDataFormat[260]		;
extern char caParametersDataFormat[260]	;
extern char caEFDataFormat[260]			;

extern FILE  *fOutput;

////////////////////////////////////////////////////////////////////////////
// exportes functions
////////////////////////////////////////////////////////////////////////////

extern void __cdecl vfDualPrintF(const char *caFormat, ... );
extern bool bfCheckForSwitch(int argc, char **argv, const char *caSwitch);
extern char *cafGetStringNextToSwitch(int argc, char **argv, const char *caSwitch);
extern void vfQuickSortTestExamples(uint **uipArray, uint *uiaSortOrder, int iMaxDepth, int iStartDepth, uint iStartI, int n);
extern void vfQuickSortDifferencies(double *daDiffs, uint *uipSortArray, int i, int j);
extern char *cafReadString(FILE *fIniFile, char *caName, char *caResult);
extern int  ifReadInteger(FILE *fIniFile, char *caName);
extern __int64 i64fReadI64(FILE *fIniFile, char *caName);
extern double dfReadDouble(FILE *fIniFile, char *caName);
extern bool bfLoadIniFile(char *caIniFileName);
extern bool bfReadUInt(char *caIniFileName, char *caName, uint &uiResult);
extern void vfPrintFunctionHeader(const char *caString);
extern void vfPrintFunctionFooter(const char *caString);