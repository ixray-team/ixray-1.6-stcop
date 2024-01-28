////////////////////////////////////////////////////////////////////////////
//	Module 		: misc.cpp
//	Created 	: 20.03.2002
//  Modified 	: 28.12.2002
//	Author		: Dmitriy Iassenev
//	Description : Miscellanious routines
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "misc.h"
#include "fitter.h"
 
typedef struct tagSQuickStructure {
	uint	si;
	uint	sj;
	uint	sd;
} SQuickStructure;

#define QUICK_SORT_STACK_SIZE			1024
#define QUICK_SORT_THRESHOLD			17
#define COMPARE_STRINGS(a,b,c)			(a < b ? bfCmp(a,b,c) : !bfCmp(b,a,c))
#define QPUSH(i,j,d)					tpStackPointer->si = i, tpStackPointer->sj = j, (tpStackPointer++)->sd = d
#define QPOP(i,j,d)						i = (--tpStackPointer)->si, j = tpStackPointer->sj, d = tpStackPointer->sd
#define QSWAP(i,j) {\
	uint uiTemp = *i;\
	*i = *j;\
	*j = uiTemp;\
}

FILE *fOutput;

char caLogDataFormat[260]			= LOG_DATA;
char caTextDataFormat[260]			= TEXT_DATA;
char caBinaryDataFormat[260]		= BINARY_DATA;
char caConfigDataFormat[260]		= CONFIG_DATA;
char caPatternDataFormat[260]		= PATTERN_DATA;
char caCoreDataFormat[260]			= CORE_DATA;
char caParametersDataFormat[260]	= PARAMETERS_DATA;
char caEFDataFormat[260]			= EF_DATA;

void __cdecl vfDualPrintF(const char *caFormat, ... )
{
	va_list marker;

	va_start(marker,caFormat);
   
	vfprintf(stdout,caFormat,marker);
   
	if (fOutput)
		vfprintf(fOutput,caFormat,marker);
   
	va_end( marker );              
}

bool bfCheckForSwitch(int argc, char **argv, const char *caSwitch)
{
	for (int i=1; i<argc; i++)
		if (((argv[i][0] == '-') || (argv[i][0] == '\\') || (argv[i][0] == '/')) && (tolower(argv[i][1]) == tolower(caSwitch[0]))) {
			for (int j=1; j<(int)strlen(caSwitch); j++) {
				if (!strchr(argv[i],caSwitch[j]))
					return(false);
			}
			return(true);
		}
	return(false);
}

char *cafGetStringNextToSwitch(int argc, char **argv, const char *caSwitch)
{
	for (int i=1; i<argc; i++)
		if (((argv[i][0] == '-') || (argv[i][0] == '\\') || (argv[i][0] == '/')) && (tolower(argv[i][1]) == tolower(caSwitch[0])))
			if (i >= argc - 1)
				return(0);
			else {
				i++;
				if ((argv[i][0] == '-') || (argv[i][0] == '\\') || (argv[i][0] == '/'))
					return(0);
				else
					return(argv[i]);
			}
	return(0);
}

void vfSimpleInsertionSortTestExamples(uint **uipArray, uint *uipSortArray, int i, int j, uint uiBlockSize, const uint uiStartDepth)
{
	for (int k = i + 1; k<=j; k++) {
		bool bOk = true;
		for (int m = k - 1; m>=i; m--)
			if (ucfCompareTestExamples(uipArray[uipSortArray[m]],uipArray[uipSortArray[k]],uiBlockSize,uiStartDepth)) {
				uint uiTemp = uipSortArray[k];
				for (int p = k - 1; p>m; p--)
					uipSortArray[p + 1] = uipSortArray[p];
				uipSortArray[m + 1] = uiTemp;
				bOk = false;
				break;
			}
		if (bOk) {
			uint uiTemp = uipSortArray[k];
			for (int p = k - 1; p>m; p--)
				uipSortArray[p + 1] = uipSortArray[p];
			uipSortArray[m + 1] = uiTemp;
		}
	}
}

__forceinline void vfSwapVectors(uint *uiaArray, uint *i, uint *j, int n, int iStartI)
{
	while (n-- > 0) {
		QSWAP(i,j);
		i++;
		j++;
	}
}

void vfQuickSortTestExamples(uint **uipArray, uint *uiaSortOrder, int iMaxDepth, int iStartDepth, uint iStartI, int n)
{
	SQuickStructure tpStack[QUICK_SORT_STACK_SIZE], *tpStackPointer = tpStack;
	int iDepth;
	QPUSH(iStartI,n,iStartDepth);
	while (tpStackPointer > tpStack) {
		QPOP(iStartI,n,iDepth);
		for (;;) {
			if (iDepth >= iMaxDepth)
				break;
			if ((n < QUICK_SORT_THRESHOLD) || (tpStackPointer - tpStack>= QUICK_SORT_STACK_SIZE - 2)) {
				vfSimpleInsertionSortTestExamples(uipArray, uiaSortOrder, iStartI,iStartI + n - 1,iMaxDepth,iDepth);
				break;
			}
			int r, v;
			uint *a, *b, *c, *d;
			uint *uiaArray = uiaSortOrder + iStartI;
			v = uipArray[*uiaArray][iDepth];
			a = b = uiaArray + 1;
			c = d = uiaArray + n - 1;
			for (;;) {
				while ((b <= c) && ((r = uipArray[*b][iDepth] - v) <= 0)) {
					if (r == 0) {
						QSWAP(a,b);
						a++;
					}
					b++;
				}
				while ((b <= c) && ((r = uipArray[*c][iDepth] - v) >= 0)) {
					if (r == 0) {
						QSWAP(c,d);
						d--;
					}
					c--;
				}
				if (b > c)
					break;
				QSWAP(b,c);
				b++;
				c--;
			}
			if (d < a) {
				iDepth++;
				continue;
			}
			r = min(a - uiaArray,b - a);
			vfSwapVectors(uiaArray,uiaArray,b - r,r,iStartI);
			r = min(d - c,n - (d - uiaArray) - 1);
			vfSwapVectors(uiaArray,b,uiaArray + n - r,r,iStartI);
			if ((r = b - a) > 1)
				QPUSH(iStartI,r,iDepth);
			if ((v = a + n - d - 1) > 1)
				QPUSH(iStartI + r, v, iDepth + 1);
			if ((r = d - c) > 1) {
				iStartI += n - r;
				n = r;
			}
			else
				break;
		}
	}
}

void vfInsertionSortDifferencies(double *daDiffs, uint *uipSortArray, int i, int j)
{
	for (int k = i + 1; k<=j; k++) {
		bool bOk = true;
		for (int m = k - 1; m>=i; m--)
			if (fabs(daDiffs[uipSortArray[m]]) > fabs(daDiffs[uipSortArray[k]])) {
				uint uiTemp = uipSortArray[k];
				for (int p = k - 1; p>m; p--)
					uipSortArray[p + 1] = uipSortArray[p];
				uipSortArray[m + 1] = uiTemp;
				bOk = false;
				break;
			}
		if (bOk) {
			uint uiTemp = uipSortArray[k];
			for (int p = k - 1; p>m; p--)
				uipSortArray[p + 1] = uipSortArray[p];
			uipSortArray[m + 1] = uiTemp;
		}
	}
}

void vfQuickSortDifferencies(double *daDiffs, uint *uipSortArray, int i, int j)
{
	SQuickStructure tpStack[QUICK_SORT_STACK_SIZE], *tpStackPointer = tpStack;
	int d = 0;
	QPUSH(i,j,0);
	while (tpStackPointer > tpStack) {
		QPOP(i,j,d);
		if ((j - i < QUICK_SORT_THRESHOLD) || (tpStackPointer - tpStack>= QUICK_SORT_STACK_SIZE - 1)) {
			vfInsertionSortDifferencies(daDiffs,uipSortArray,i,j);
			continue;
		}
		int iStartI = i, iStartJ = j;
		do {
			bool bDecJ = true;
			uint *uipPointer1 = uipSortArray + i, *uipPointer2 = uipSortArray + j;
			while (uipPointer1 < uipPointer2) {
				if (fabs(daDiffs[*uipPointer1]) < fabs(daDiffs[*uipPointer2])) {
					uint uiTmp = *uipPointer1;
					*uipPointer1 = *uipPointer2;
					*uipPointer2 = uiTmp;
					bDecJ = !bDecJ;
				}
				if (bDecJ) *uipPointer2--;
				else *uipPointer1++;
			}
			i = uipPointer1 - uipSortArray;
			if (iStartI < i - 1)
				QPUSH(iStartI,i-1,d+1);
			j = iStartJ;
			iStartI =  ++i;
		}
		while (iStartJ > i);
	}
}

bool bfReadString(FILE *fIniFile, char *caConstName, char *caResult)
{
	char caString[256];
	char caReadName[256];
	char caEqualSign[256];
	char caName[256];
	memcpy(caName,caConstName,strlen(caConstName) + 1);
	_strupr(caName);
	fseek(fIniFile,0,SEEK_SET);
	while (!feof(fIniFile)) {
		fgets(caString,255,fIniFile);
		if (!strlen(caString) || (caString[0] == ';'))
			continue;
		sscanf(caString,"%s%s%s",caReadName,caEqualSign,caResult);
		if (!strcmp(caName,_strupr(caReadName)) && (strlen(caEqualSign) == 1) && (caEqualSign[0] == '='))
			return(true);
	}
	return(false);
}

bool bfReadDouble(FILE *fIniFile, char *caName, double &dResult)
{
	char caResult[256];
	if (bfReadString(fIniFile,caName,caResult))
		if (strlen(caResult)) {
			dResult = atof(caResult);
			return(true);
		}

	return(false);
}

bool bfReadInteger(FILE *fIniFile, char *caName, int &iResult)
{
	char caResult[256];
	if (bfReadString(fIniFile,caName,caResult))
		if (strlen(caResult)) {
			iResult = atoi(caResult);
			return(true);
		}
	return(false);
}

bool bfReadI64(FILE *fIniFile, char *caName, __int64 &i64Result)
{
	char caResult[256];
	if (bfReadString(fIniFile,caName,caResult))
		if (strlen(caResult)) {
			i64Result = _atoi64(caResult);
			return(true);
		}
	return(false);
}

bool bfReadUInt(FILE *fIniFile, char *caName, uint &uiResult)
{
	char caResult[256];
	if (bfReadString(fIniFile,caName,caResult))
		if (strlen(caResult)) {
			__int64 i64Dummy = _atoi64(caResult);
			uiResult = (uint)i64Dummy;
			return(true);
		}
	return(false);
}

bool bfReadUInt(char *caIniFileName, char *caName, uint &uiResult)
{
	FILE *fIniFile = fopen(caIniFileName,"rb");
	if (!fIniFile) {
		vfDualPrintF("\nCannot open ini file %s\n",INI_FILE);
		return(false);
	}
	char caResult[256];
	if (bfReadString(fIniFile,caName,caResult))
		if (strlen(caResult)) {
			__int64 i64Dummy = _atoi64(caResult);
			uiResult = (uint)i64Dummy;
			return(true);
		}
	return(false);
}

bool bfLoadIniFile(char *caIniFileName)
{
	FILE *fIniFile = fopen(caIniFileName,"rb");
	if (!fIniFile) {
		vfDualPrintF("\nCannot open ini file %s\n",INI_FILE);
		return(false);
	}
	char caString[256];
	
	if (bfReadString(fIniFile,LOG_DATA_MASK_NAME,&caString[0]))
		memcpy(caLogDataFormat,caString,strlen(caString) + 1);
	if (bfReadString(fIniFile,TEXT_DATA_MASK_NAME,caString))
		memcpy(caTextDataFormat,caString,strlen(caString) + 1);
	if (bfReadString(fIniFile,BINARY_DATA_MASK_NAME,caString))
		memcpy(caBinaryDataFormat,caString,strlen(caString) + 1);
	if (bfReadString(fIniFile,CONFIG_DATA_MASK_NAME,caString))
		memcpy(caConfigDataFormat,caString,strlen(caString) + 1);
	if (bfReadString(fIniFile,PATTERN_DATA_MASK_NAME,caString))
		memcpy(caPatternDataFormat,caString,strlen(caString) + 1);
	if (bfReadString(fIniFile,CORE_DATA_MASK_NAME,caString))
		memcpy(caCoreDataFormat,caString,strlen(caString) + 1);
	if (bfReadString(fIniFile,PARAMETERS_DATA_MASK_NAME,caString))
		memcpy(caParametersDataFormat,caString,strlen(caString) + 1);
	if (bfReadString(fIniFile,EF_DATA_MASK_NAME,caString))
		memcpy(caEFDataFormat,caString,strlen(caString) + 1);

	bfReadDouble(fIniFile,EPSILON_NAME 						,dEpsilon);
	bfReadDouble(fIniFile,ALPHA_NAME   						,dAlphaCoefficient);
	bfReadDouble(fIniFile,BETA_NAME							,dBetaCoefficient);
	bfReadUInt	(fIniFile,MAX_ITERATION_COUNT_NAME			,uiMaxIterationCount);
	bfReadUInt	(fIniFile,RANDOM_FACTOR_NAME 				,uiRandomFactor);
	bfReadUInt	(fIniFile,RANDOM_PROBABILITY_NAME			,uiRandomProbability);
	bfReadUInt	(fIniFile,RANDOM_UPDATE_NAME				,uiRandomUpdate);
	bfReadUInt	(fIniFile,RANDOM_START_SEED_NAME			,uiRandomStartSeed);
	bfReadUInt	(fIniFile,MATCH_THRESHOLD_NAME				,uiMatchThreshold);
	bfReadUInt	(fIniFile,MAX_CARDINALITY_NAME				,uiMaxCardinalityCount);
	bfReadDouble(fIniFile,PATTERN_EXISTANCE_COEFFICIENT_NAME,dPatternsExistanceCoefficient);
		
	return(true);
}

void vfPrintFunctionHeader(const char *caString)
{
	char s[80];
	memset(s,0,sizeof(s));
	memset(s,'/',sizeof(s)-1);
	vfDualPrintF("\n%s\n/ %s\n%s\n\n",s,caString,s);
}

void vfPrintFunctionFooter(const char *caString)
{
	char s[80];
	memset(s,0,sizeof(s));
	memset(s,'/',sizeof(s)-1);
	vfDualPrintF("%s successfully completed\n%s\n",caString,s);
}
