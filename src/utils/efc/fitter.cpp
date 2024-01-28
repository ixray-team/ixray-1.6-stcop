////////////////////////////////////////////////////////////////////////////
//	Module 		: fitter.cpp
//	Created 	: 25.03.2002
//  Modified 	: 28.12.2002
//	Author		: Dmitriy Iassenev
//	Description : Pattern Configuration Generation and Weight Fitting Algorithms
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "fitter.h"
#include "misc.h"

// common parameters
const uint g31							= 0x49249249ul;			// = 0100_1001_0010_0100_1001_0010_0100_1001
const uint g32							= 0x381c0e07ul;			// = 0011_1000_0001_1100_0000_1110_0000_0111

#define MEGABYTE						((double)1048576.0)
#define MIN_LINE_LENGTH					256
#define DOUBLE_DATA_FORMAT				0
#define FLOAT_DATA_FORMAT				1
#define EFC_VERSION						1
#define COUNT_BITS(uiTemp) {\
	uiTemp = (uiTemp & g31) + ((uiTemp >> 1) & g31) + ((uiTemp >> 2) & g31);\
	uiTemp = ((uiTemp + (uiTemp >> 3)) & g32) + ((uiTemp >> 6) & g32);\
	uiMatchCount += (uiTemp + (uiTemp >> 9) + (uiTemp >> 18) + (uiTemp >> 27)) & 0x3f;\
}

double dEpsilon							= EPSILON;
double dAlphaCoefficient				= ALPHA;
double dBetaCoefficient					= BETA;
uint   uiMaxIterationCount				= MAX_ITERATION;
uint   uiRandomFactor					= RANDOM_FACTOR;
uint   uiRandomProbability				= RANDOM_PROBABILITY;
uint   uiRandomUpdate					= RANDOM_UPDATE;
uint   uiRandomStartSeed				= RANDOM_START_SEED;
uint   uiMatchThreshold					= MATCH_THRESHOLD;
uint   uiMaxCardinalityCount			= MAX_CARDINALITY;
double dPatternsExistanceCoefficient	= PATTERN_EXISTANCE_COEFFICIENT;

uint		**uiaTestParameters;
uint		*uiaPatternConfigUsageCount;
uint		**uiaPatternConfigUsage;
double		*daParameters;
double		*daDelta;
double		*daGradient;
double		*daTestResults;
double		*daEvalResults;
char		*caRandom;
uint		**uiaAtomicFeatureMasks;
uint		*uiaAtomicFeatureRange;
uint		*uiaAtomicIndexes;
uint		*uiaPatternIndexes;
SPattern	*tpPatterns;
uint		*uiaVariableTypes;

uint		uiRandSeed = 0;
uint		uiVariableCount;
uint		uiPatternCount;
uint		uiParameterCount;
uint		uiTestCount;
uint		uiFunctionType;
double		dMinResultValue;
double		dMaxResultValue;
SEFHeader	tEFHeader;

__forceinline uint uifRandom(uint uiRange)
{
    uint uiResult;
    __asm {
            MOV     EAX,uiRange
            IMUL    EDX,uiRandSeed,08088405H
            INC     EDX
            MOV     uiRandSeed,EDX
            MUL     EDX
            MOV     uiResult,EDX
    }
    return(uiResult);
}

__forceinline uint uifGetPatternIndex(uint *uipTest, int iPatternIndex)
{
	SPattern &tPattern = tpPatterns[iPatternIndex];
	for (uint i=1, uiIndex = uipTest[tPattern.uiaVariableIndexes[0]]; i<(int)tPattern.uiCardinality; i++)
		uiIndex = uiIndex*uiaAtomicFeatureRange[tPattern.uiaVariableIndexes[i]] + uipTest[tPattern.uiaVariableIndexes[i]];
	return(uiIndex + uiaPatternIndexes[iPatternIndex]);
}

double dfEvaluation(uint *uiaPatternIndexes)
{
	double dResult = 0.0;
	for (uint i=0; i<uiPatternCount; i++)
		dResult += daParameters[uifGetPatternIndex(uiaPatternIndexes,i)];
	return(dResult);
}

void vfReadEFHeader(FILE *fFile, SEFHeader &tEFHeader, char *caFileName)
{
	fread(&tEFHeader,1,sizeof(SEFHeader),fFile);
	if (tEFHeader.uiBuilderVersion != EFC_VERSION) {
		vfDualPrintF("\nERROR\nFile \"%s\" : not supported version of the efc\nRebuild file and try again\n",caFileName);
		throw "";
	}
}
	
void vfWriteEFHeader(FILE *fFile, SEFHeader &tEFHeader)
{
	fwrite(&tEFHeader,1,sizeof(SEFHeader),fFile);
}
	
void vfLoadTestData(char *caFileName, bool bLoadTestExamples = true)
{
	vfDualPrintF("Loading binary data...");

	uint uiMemoryAllocated = 0;

	FILE *fTestParameters = fopen(caFileName,"rb");
	CHECK_FILE_IF_OPENED(fTestParameters,caFileName);
	
	vfReadEFHeader(fTestParameters,tEFHeader,caFileName);
	
	fread(&uiVariableCount,1,sizeof(uiVariableCount),fTestParameters);
	uiaAtomicFeatureRange = (uint *)calloc(uiVariableCount,sizeof(uint));
	uiaAtomicIndexes = (uint *)calloc(uiVariableCount,sizeof(uint));

	for (uint i=0; i<uiVariableCount; i++) {
		fread(uiaAtomicFeatureRange + i,1,sizeof(uint),fTestParameters);
		if (i)
			uiaAtomicIndexes[i] = uiaAtomicIndexes[i-1] + uiaAtomicFeatureRange[i-1];
	}

	uiaVariableTypes = (uint *)malloc(uiVariableCount*sizeof(uint));
	fread(uiaVariableTypes,uiVariableCount,sizeof(uint),fTestParameters);
	free(uiaVariableTypes);

	fread(&uiFunctionType,1,sizeof(uint),fTestParameters);
	
	if (tEFHeader.uiDataFormat == DOUBLE_DATA_FORMAT) {
		fread(&dMinResultValue,1,sizeof(double),fTestParameters);
		fread(&dMaxResultValue,1,sizeof(double),fTestParameters);
	}
	else {
		float fDummy;
		fread(&fDummy,1,sizeof(float),fTestParameters);
		dMinResultValue = (double)(fDummy);
		fread(&fDummy,1,sizeof(float),fTestParameters);
		dMaxResultValue = (double)(fDummy);
	}

	fread(&uiPatternCount,1,sizeof(uiPatternCount),fTestParameters);
	tpPatterns = (SPattern *)malloc(uiPatternCount*sizeof(SPattern));
	uiaPatternIndexes = (uint *)calloc(uiPatternCount,sizeof(uint));
	uiParameterCount = 0;
	for ( i=0; i<uiPatternCount; i++) {
		if (i)
			uiaPatternIndexes[i] = uiParameterCount;
		fread(&(tpPatterns[i].uiCardinality),1,sizeof(tpPatterns[i].uiCardinality),fTestParameters);
		tpPatterns[i].uiaVariableIndexes = (uint *)malloc(tpPatterns[i].uiCardinality*sizeof(uint));
		fread(tpPatterns[i].uiaVariableIndexes,tpPatterns[i].uiCardinality,sizeof(uint),fTestParameters);
		uint uiComplexity = 1;
		for (int j=0; j<(int)tpPatterns[i].uiCardinality; j++)
			uiComplexity *= uiaAtomicFeatureRange[tpPatterns[i].uiaVariableIndexes[j]];
		uiParameterCount += uiComplexity;
	}
	
	daParameters = (double *)calloc(uiParameterCount,sizeof(double));
	if (bLoadTestExamples) {
		fread(&uiTestCount,1,sizeof(uiTestCount),fTestParameters);

		daDelta = (double *)calloc(uiParameterCount,sizeof(double));
		daGradient = (double *)calloc(uiParameterCount,sizeof(double));
		uiaPatternConfigUsage = (uint **)calloc(uiParameterCount,sizeof(uint *));
		uiaPatternConfigUsageCount = (uint *)calloc(uiParameterCount,sizeof(uint));
		uiaTestParameters = (uint **)malloc(uiTestCount*sizeof(uint *));
		daTestResults = (double *)malloc(uiTestCount*sizeof(double));
		daEvalResults = (double *)malloc(uiTestCount*sizeof(double));
		caRandom = (char *)malloc(uiParameterCount*sizeof(char));
		
		uiMemoryAllocated += uiParameterCount*sizeof(double);
		uiMemoryAllocated += uiParameterCount*sizeof(double);
		uiMemoryAllocated += uiParameterCount*sizeof(double);
		uiMemoryAllocated += uiParameterCount*sizeof(uint *);
		uiMemoryAllocated += uiParameterCount*sizeof(uint);
		uiMemoryAllocated += uiTestCount*sizeof(uint *);
		uiMemoryAllocated += uiTestCount*sizeof(double);
		uiMemoryAllocated += uiTestCount*sizeof(double);
		uiMemoryAllocated += uiParameterCount*sizeof(char);

		for ( i=0; i<(int)uiTestCount; i++) {
			uiaTestParameters[i] = (uint *)malloc(uiVariableCount*sizeof(uint));
			fread(uiaTestParameters[i],uiVariableCount,sizeof(uint),fTestParameters);
			fread(daTestResults + i,1,sizeof(double),fTestParameters);
			for (int j=0; j<(int)uiPatternCount; j++)
				uiaPatternConfigUsageCount[uifGetPatternIndex(uiaTestParameters[i],j)]++;
		}

		fclose(fTestParameters);

		for ( i=0; i<uiParameterCount; i++) {
			uiaPatternConfigUsage[i] = (uint *)calloc(uiaPatternConfigUsageCount[i],sizeof(uint));
			uiMemoryAllocated += uiaPatternConfigUsageCount[i]*sizeof(uint);
		}

		memset(uiaPatternConfigUsageCount,0,uiParameterCount*sizeof(uint));
		for ( i=0; i<uiTestCount; i++)
			for (uint j=0; j<uiPatternCount; j++) {
				uint uiDummy = uifGetPatternIndex(uiaTestParameters[i],j);
				uiaPatternConfigUsage[uiDummy][uiaPatternConfigUsageCount[uiDummy]++] = i;
			}
		vfDualPrintF("completed (%6.2fMb memory allocated)\n",(double)uiMemoryAllocated/(double)MEGABYTE);
	}
	else
		vfDualPrintF("completed\n");
}

void vfFreeTestData(bool bUnloadTesExamples = true)
{
	vfDualPrintF("Freeing binary data...");

	free(uiaAtomicFeatureRange);
	free(uiaAtomicIndexes);
	free(uiaPatternIndexes);
	free(tpPatterns);
	
	if (bUnloadTesExamples) {
		free(daParameters);
		free(daDelta);
		free(daGradient);
		free(uiaPatternConfigUsageCount);
		free(daTestResults);
		free(daEvalResults);
		free(caRandom);

		for (uint i=0; i<uiParameterCount; i++)
			free(uiaPatternConfigUsage[i]);
		free(uiaPatternConfigUsage);

		for ( i=0; i<uiTestCount; i++)
			free(uiaTestParameters[i]);
		free(uiaTestParameters);
	}
	vfDualPrintF("completed\n");
}

double *dafMultiplicateVectorByConstant(double *daParameters, double dConstant, double *daResult)
{
	for (uint i=0; i<uiParameterCount; i++)
		daResult[i] = daParameters[i] * dConstant;
	return(daResult);
}

double *dafAddVectors(double *daVector0, double *daVector1, double *daResult)
{
	for (uint i=0; i<uiParameterCount; i++)
		daResult[i] = daVector0[i] + daVector1[i];
	return(daResult);
}

double dfComputeEvalResults()
{
	double dResult = 0.0, dTemp;
	for (uint i=0; i<uiTestCount; i++) {
		daEvalResults[i] = dfEvaluation(uiaTestParameters[i]);
		dTemp = daTestResults[i] - daEvalResults[i];
		dResult += SQR(dTemp);
	}
	return(dResult);
}

double *dafGradient(double *daParameters, double *daResult, bool bUseRandom)
{
	memset(daResult,0,uiParameterCount*sizeof(double));
	
	for (uint i=0; i<uiParameterCount; i++)
		if ((!bUseRandom) || (!caRandom[i]))
			for (uint j=0; j<uiaPatternConfigUsageCount[i]; j++)
				daResult[i] -= (daTestResults[uiaPatternConfigUsage[i][j]]-daEvalResults[uiaPatternConfigUsage[i][j]]);

	for ( i=0; i<uiParameterCount; i++)
		if ((!bUseRandom) || (!caRandom[i]))
			if (uiaPatternConfigUsageCount[i])
				daResult[i] *= (uiaPatternConfigUsageCount[i]<3 ? 0.01 : (uiaPatternConfigUsageCount[i]<20 ? 0.1 : 2.0/uiaPatternConfigUsageCount[i]))/uiPatternCount;

	return(daResult);
}

void vfGenerateRandom()
{
	memset(caRandom,0,uiParameterCount*sizeof(char));
	for (uint i=0; i<uiParameterCount; i++)
		if (uifRandom(uiRandomFactor) < uiRandomProbability)
			caRandom[i] = 1;
}

void vfOptimizeParameters(char *caFileName, char *caResultFileName, bool bRandom, bool bUseParameters)
{
	vfPrintFunctionHeader("Fitting pattern configuration weights");
	vfLoadTestData(caFileName);

	if (bRandom)
		uiRandSeed = uiRandomStartSeed;
	
	memset(daDelta,0,uiParameterCount*sizeof(double));
	if (bUseParameters) {
		FILE *fData = fopen(caResultFileName,"rb");
		if (fData) {
			fseek(fData,0,SEEK_END);
			if (ftell(fData) == (int)(uiParameterCount*sizeof(double))) {
				fseek(fData,SEEK_SET,0);
				fread(daParameters,uiParameterCount,sizeof(double),fData);
			}
			fclose(fData);
		}
	}
		
	double dFunctional = dfComputeEvalResults(), dPreviousFunctional;
	__int64	ui64 = __int64(1) << 32;
	uint i = 0, uiUpdate = (uint)(__int64(uiTestCount)*uiParameterCount > ui64 ? 1 : ui64/(__int64(uiTestCount)*uiParameterCount));
	vfDualPrintF("%6d : %17.8f (%17.8f)\n",i % uiParameterCount,dFunctional,dFunctional/uiTestCount);
	do {
		
		dPreviousFunctional = dFunctional;
		
		if (bRandom && (!(i % uiRandomUpdate)))
			vfGenerateRandom();
		
		dafGradient(daParameters,daGradient,bRandom);
		dafMultiplicateVectorByConstant(daGradient,-dAlphaCoefficient,daGradient);
		dafMultiplicateVectorByConstant(daDelta,dBetaCoefficient,daDelta);
		dafAddVectors(daGradient,daDelta,daDelta);
		dafAddVectors(daParameters,daDelta,daParameters);

		dFunctional = dfComputeEvalResults();
		i++;
		
		if (!(i % uiUpdate))
			vfDualPrintF("%6d : %17.8f (%17.8f)\n",i,dFunctional,dFunctional/uiTestCount);
		
		if (!(i % uiUpdate)) {
			FILE *fOutput = fopen(caResultFileName,"wb");
			CHECK_FILE_IF_OPENED(fOutput,caResultFileName);
			fwrite(daParameters,sizeof(double),uiParameterCount,fOutput);
			fclose(fOutput);
		}
	}
	while ((fabs((dPreviousFunctional - dFunctional)/uiTestCount) > dEpsilon) && (i <= uiMaxIterationCount));

	vfDualPrintF("%6d : %17.8f (%17.8f)\n",i,dFunctional,dFunctional/uiTestCount);
		
	FILE *fOutput = fopen(caResultFileName,"wb");
	CHECK_FILE_IF_OPENED(fOutput,caResultFileName);
	fwrite(daParameters,sizeof(double),uiParameterCount,fOutput);
	fclose(fOutput);

	vfFreeTestData();
	vfPrintFunctionFooter("Fitting pattern configuration weights");
}

void vfShowTestData(char *caTestDataFileName, char *caPatternDataFileName, bool bShowSimpleStats, bool bShowSortedStats, bool bShowPatternStats)
{
	vfPrintFunctionHeader("Listing statistics");
	// initializing test data
	vfLoadTestData(caTestDataFileName);
	
	FILE *fData = fopen(caPatternDataFileName,"rb");
	CHECK_FILE_IF_OPENED(fData,caPatternDataFileName);
	fread(daParameters,uiParameterCount,sizeof(double),fData);
	fclose(fData);

	// printing results
	if (bShowPatternStats) {
		for (int i=0; i<(int)uiPatternCount - 1; i++) {
			uint uiCount = uiaPatternIndexes[i + 1];
			vfDualPrintF("Pattern #%d:\n",i + 1);
			for (int j=uiaPatternIndexes[i]; j<(int)uiCount; j++)
				vfDualPrintF("Value %d: %6.2f\n",j - uiaPatternIndexes[i] + 1,daParameters[j]);
		}
		uint uiCount = uiParameterCount;
		vfDualPrintF("Pattern #%d:\n",i + 1);
		for (int j=uiaPatternIndexes[i]; j<(int)uiCount; j++)
			vfDualPrintF("Value %d: %6.2f\n",j - uiaPatternIndexes[i] + 1,daParameters[j]);
	}

	if (bShowSimpleStats) {
		vfDualPrintF("\nTest examples evaluation:\n");
		for (int i=0; i<(int)uiTestCount; i++) {
			vfDualPrintF("%5d : ",i + 1);
			for (int j=0; j<(int)uiVariableCount; j++)
				vfDualPrintF("%6d",uiaTestParameters[i][j] + 1);
			double dEval = dfEvaluation(uiaTestParameters[i]);
			vfDualPrintF("%8.2f  -> %6.2f (%6.2f)\n",daTestResults[i],dEval,dEval - daTestResults[i]);
		}
	}
	
	if (bShowSortedStats) {
		double *daDiffs = (double *)malloc(uiTestCount*sizeof(double));
		for (int i=0; i<(int)uiTestCount; i++)
			daDiffs[i] = dfEvaluation(uiaTestParameters[i]) - daTestResults[i];
		
		// sorting differences
		uint *uipSortIndexes = (uint *)malloc(uiTestCount*sizeof(uint));
		for ( i=0; i<(int)uiTestCount; i++)
			uipSortIndexes[i] = i;

		vfQuickSortDifferencies(daDiffs, uipSortIndexes, 0, uiTestCount - 1);
		
		vfDualPrintF("\nTest examples being sorted by error:\n");
		for ( i=0; i<(int)uiTestCount; i++) {
			vfDualPrintF("%5d : ",uipSortIndexes[i] + 1);
			for (int j=0; j<(int)uiVariableCount; j++)
				vfDualPrintF("%6d",uiaTestParameters[uipSortIndexes[i]][j]  + 1);
			double dEval = dfEvaluation(uiaTestParameters[uipSortIndexes[i]]);
			vfDualPrintF("%8.2f  -> %6.2f (%6.2f)\n",daTestResults[uipSortIndexes[i]],dEval,daDiffs[uipSortIndexes[i]]);
		}
		free(daDiffs);
		free(uipSortIndexes);
	}

	vfFreeTestData();
	vfPrintFunctionFooter("Listing statistics");
}

uchar ucfCompareTestExamples(uint *uipFirst, uint *uipSecond, uint uiLength, const uint uiStartDepth)
{
	for (int i=uiStartDepth; i<(int)uiLength; i++)
		if (uipFirst[i] < uipSecond[i])
			return(1);
		else
			if (uipFirst[i] > uipSecond[i])
				return(0);
	return(2);
}

void vfConvertTestData(char *caRawDataFileName, char *caTestDataFileName, char *caPatternDataFileName, bool bShowDuplicates)
{
	vfPrintFunctionHeader("Converting text to binary data");
	vfDualPrintF("Reading text...");
	FILE *fData = fopen(caRawDataFileName,"rt");
	CHECK_FILE_IF_OPENED(fData,caRawDataFileName);
	
	// determinining variable count and for each variable its range
	char *caTextLine = (char *)malloc(MIN_LINE_LENGTH*sizeof(char));
	for (int i=2; fgets(caTextLine,16*1024,fData) == NULL; i++)
		caTextLine = (char *)realloc(caTextLine,MIN_LINE_LENGTH*i*sizeof(char));
	for (i = strlen(caTextLine) - 1; i>=0; i--)
		if ((caTextLine[i] != ' ') && (caTextLine[i] != 9) && (caTextLine[i] != 10))
			break;
	free(caTextLine);
	uint uiPosition = i;
	fseek(fData,0,SEEK_SET);
	uint uiVariableCount = 0;
	uint *uipRanges = 0;
	uint uiParameters = 0;
	while (uiPosition > (uint)ftell(fData)) {
		uipRanges = (uint *)realloc(uipRanges,++uiVariableCount*sizeof(uint));
		fscanf(fData,"%d",uipRanges + uiVariableCount - 1);
		uiParameters += uipRanges[uiVariableCount - 1];
	}
	
	uiaVariableTypes = (uint *)malloc(uiVariableCount*sizeof(uint));
	for ( i=0; i<(int)uiVariableCount; i++) {
		char caString[256];
		fscanf(fData,"%s",caString);
		if (!bfReadUInt(INI_FILE,caString,uiaVariableTypes[i])) {
			vfDualPrintF("Cannot find variable type \"%s\"",caString);
			throw "";
		}
	}

	{
		char caString[256];
		fscanf(fData,"%s",caString);
		if (!bfReadUInt(INI_FILE,caString,uiFunctionType)) {
			vfDualPrintF("Cannot find variable type \"%s\"",caString);
			throw "";
		}
	}

	// computing offsets
	uint *uipIndexes = (uint *)malloc(uiVariableCount*sizeof(uint));
	uipIndexes[0] = 0;
	for ( i=1; i<(int)uiVariableCount; i++)
		uipIndexes[i] = uipIndexes[i - 1] + uipRanges[i - 1];

	// reading test examples
	uint	**uippTestData = 0;
	double	*dpTestResults = 0;
	uint	uiTestCount;
	for ( uiTestCount = 0; !feof(fData); ) {
		uippTestData = (uint **)realloc(uippTestData,(++uiTestCount)*sizeof(uint *));
		uippTestData[uiTestCount - 1] = (uint *)malloc(uiVariableCount*sizeof(uint));
		for (int j=0; j<(int)uiVariableCount; j++) {
			uint uiTemp;
			fscanf(fData,"%d",&uiTemp);
			if (feof(fData) && (j < (int)uiVariableCount)) {
				free(uippTestData[uiTestCount - 1]);
				uiTestCount--;
				uippTestData = (uint **)realloc(uippTestData,uiTestCount*sizeof(uint *));
				break;
			}
			uippTestData[uiTestCount - 1][j] = uiTemp;
		}
		if (!feof(fData)) {
			dpTestResults = (double *)realloc(dpTestResults,uiTestCount*sizeof(double));
			float fDummy;
			fscanf(fData,"%f",&fDummy);
			dpTestResults[uiTestCount - 1] = (double)fDummy;
		}
	}
	
	// closing raw data file
	fclose(fData);
	vfDualPrintF("completed\n");

	// sorting test examples to determine duplicates
	if (!bShowDuplicates)
		vfDualPrintF("Searching for duplicates...");
	else
		vfDualPrintF("Started searching for duplicates\n");
	
	uint *uipSortOrder = (uint *)malloc(uiTestCount*sizeof(uint));
	bool *bpUsage = (bool *)malloc(uiTestCount*sizeof(bool));
	for ( i=0; i<(int)uiTestCount; i++) {
		uipSortOrder[i] = i;
		bpUsage[i] = true;
	}

	//uint uiTimeStart = clock();
	vfQuickSortTestExamples(uippTestData,uipSortOrder,uiVariableCount,0,0,uiTestCount);
	//vfDualPrintF("%6.3f\n",(float)(clock() - uiTimeStart)/1000.f);

	uint uiDuplicateCount = 0;
	uint uiDuplicates = 0;
	for ( i=0; i<(int)uiTestCount - 1; i++) {
		for (int j = i + 1; (j<(int)uiTestCount) && (ucfCompareTestExamples(uippTestData[uipSortOrder[i]],uippTestData[uipSortOrder[j]],uiVariableCount,0) == 2); j++) ;
		if (j > i + 1) {
			uiDuplicates++;
			if (bShowDuplicates) {
				vfDualPrintF("Duplication %5d\n",uiDuplicates);
				vfDualPrintF("%5d : ",uipSortOrder[i]);
				for (int k=0; k<(int)uiVariableCount - 1; k++)
					vfDualPrintF("%6d",uippTestData[uipSortOrder[i]][k]);
				vfDualPrintF(" : %8.2f\n",dpTestResults[uipSortOrder[i]]);
				for ( k = i + 1; k<j; k++) {
					vfDualPrintF("%5d : ",uipSortOrder[k]);
					for (int m=0; m<(int)uiVariableCount - 1; m++)
						vfDualPrintF("%6d",uippTestData[uipSortOrder[k]][m]);
					vfDualPrintF(" : %8.2f\n",dpTestResults[uipSortOrder[k]]);
				}
			}
			double dTemp = dpTestResults[uipSortOrder[i]];
			for (int k = i + 1; k<j; k++) {
				bpUsage[uipSortOrder[k]] = false;
				dTemp += dpTestResults[uipSortOrder[k]];
			}
			dpTestResults[uipSortOrder[i]] = dTemp / (double)(j - i);
			uiDuplicateCount += j - i - 1;
			i = j - 1;
		}
	}
	if (!bShowDuplicates)
		vfDualPrintF("completed (found %d duplicates)\n",uiDuplicateCount);
	else
		vfDualPrintF("Searching for duplicates completed (found %d duplicates)\n",uiDuplicateCount);

	dMinResultValue = 100000000.0;
	dMaxResultValue = -100000000.0;
	for ( i=0; i<(int)uiTestCount - 1; i++) {
		if (dpTestResults[i] < dMinResultValue)
			dMinResultValue = dpTestResults[i];
		if (dpTestResults[i] > dMaxResultValue)
			dMaxResultValue = dpTestResults[i];
	}

	vfDualPrintF("Saving converted data...");
	
	fData = fopen(caTestDataFileName,"wb");
	CHECK_FILE_IF_OPENED(fData,caTestDataFileName);
	
	tEFHeader.uiBuilderVersion = EFC_VERSION;
	tEFHeader.uiDataFormat = DOUBLE_DATA_FORMAT;
	vfWriteEFHeader(fData,tEFHeader);
	
	fwrite(&uiVariableCount,1,sizeof(uint),fData);
	for ( i=0; i<(int)uiVariableCount; i++)
		fwrite(uipRanges + i,1,sizeof(uint),fData);
	
	for ( i=0; i<(int)uiVariableCount; i++)
		fwrite(uiaVariableTypes + i,1,sizeof(uint),fData);

	fwrite(&uiFunctionType,1,sizeof(uint),fData);
	
	fwrite(&dMinResultValue,1,sizeof(double),fData);
	fwrite(&dMaxResultValue,1,sizeof(double),fData);

	fwrite(&uiVariableCount,1,sizeof(uint),fData);
	for ( i=0; i<(int)uiVariableCount; i++) {
		uint uiDummy = 1;
		fwrite(&uiDummy,1,sizeof(uint),fData);
		fwrite(&i,1,sizeof(int),fData);
	}
	uiDuplicateCount = uiTestCount - uiDuplicateCount;
	fwrite(&uiDuplicateCount,1,sizeof(uint),fData);
	
	for ( i=0; i<(int)uiTestCount; i++) {
		if (!bpUsage[i])
			continue;

		for (int j=0; j<(int)uiVariableCount; j++) {
			uint uiTemp = uippTestData[i][j] - 1;
			fwrite(&uiTemp,1,sizeof(uint),fData);
		}
		fwrite(dpTestResults + i,1,sizeof(double),fData);
	}
	fclose(fData);
	
	fData = fopen(caPatternDataFileName,"wb");
	CHECK_FILE_IF_OPENED(fData,caPatternDataFileName);
	fwrite(&uiVariableCount,1,sizeof(uint),fData);
	for ( i=0; i<(int)uiVariableCount; i++) {
		uint uiDummy = 1;
		fwrite(&uiDummy,1,sizeof(uint),fData);
		fwrite(&i,1,sizeof(uint),fData);
	}
	fclose(fData);
	
	vfDualPrintF("completed\n");

	vfDualPrintF("Freeing additional resources...");
	free(uiaVariableTypes);
	free(uipSortOrder);
	free(bpUsage);
	free(uipRanges);
	free(uipIndexes);
	for ( i=0; i<(int)uiTestCount; i++)
		free(uippTestData[i]);
	free(uippTestData);
	free(dpTestResults);
	vfDualPrintF("completed\n");

	vfPrintFunctionFooter("Converting text to binary data");
}

void vfLoadRawData(char *caFileName)
{
	vfDualPrintF("Loading binary data...");

	FILE *fTestParameters = fopen(caFileName,"rb");
	CHECK_FILE_IF_OPENED(fTestParameters,caFileName);
	
	vfReadEFHeader(fTestParameters,tEFHeader,caFileName);
	
	fread(&uiVariableCount,1,sizeof(uiVariableCount),fTestParameters);
	uiaAtomicFeatureRange = (uint *)calloc(uiVariableCount,sizeof(uint));
	uiaAtomicIndexes = (uint *)calloc(uiVariableCount,sizeof(uint));

	for (uint i=0; i<uiVariableCount; i++) {
		fread(uiaAtomicFeatureRange + i,1,sizeof(uint),fTestParameters);
		if (i)
			uiaAtomicIndexes[i] = uiaAtomicIndexes[i-1] + uiaAtomicFeatureRange[i-1];
	}
	
	uiaVariableTypes = (uint *)malloc(uiVariableCount*sizeof(uint));
	fread(uiaVariableTypes,uiVariableCount,sizeof(uint),fTestParameters);
	free(uiaVariableTypes);

	fread(&uiFunctionType,1,sizeof(uint),fTestParameters);
	
	fread(&dMinResultValue,1,sizeof(double),fTestParameters);
	fread(&dMaxResultValue,1,sizeof(double),fTestParameters);

	fread(&uiPatternCount,1,sizeof(uiPatternCount),fTestParameters);
	tpPatterns = (SPattern *)malloc(uiPatternCount*sizeof(SPattern));
	uiaPatternIndexes = (uint *)calloc(uiPatternCount,sizeof(uint));
	uiParameterCount = 0;
	for ( i=0; i<uiPatternCount; i++) {
		if (i)
			uiaPatternIndexes[i] = uiParameterCount;
		fread(&(tpPatterns[i].uiCardinality),1,sizeof(tpPatterns[i].uiCardinality),fTestParameters);
		tpPatterns[i].uiaVariableIndexes = (uint *)malloc(tpPatterns[i].uiCardinality*sizeof(uint));
		fread(tpPatterns[i].uiaVariableIndexes,tpPatterns[i].uiCardinality,sizeof(uint),fTestParameters);
		uint uiComplexity = 1;
		for (int j=0; j<(int)tpPatterns[i].uiCardinality; j++)
			uiComplexity *= uiaAtomicFeatureRange[tpPatterns[i].uiaVariableIndexes[j]];
		uiParameterCount += uiComplexity;
	}
	
	fread(&uiTestCount,1,sizeof(uiTestCount),fTestParameters);
	
	uiaTestParameters = (uint **)malloc(uiTestCount*sizeof(uint *));

	for ( i=0; i<uiTestCount; i++) {
		uiaTestParameters[i] = (uint *)malloc(uiVariableCount*sizeof(uint));
		fread(uiaTestParameters[i],uiVariableCount,sizeof(uint),fTestParameters);
		double dTemp;
		fread(&dTemp,1,sizeof(dTemp),fTestParameters);
		for (uint j=0; j<uiVariableCount; j++)
			uiaTestParameters[i][j] += uiaAtomicIndexes[j];
	}

	fclose(fTestParameters);
	
	vfDualPrintF("completed\n");
}

void vfFreeRawData()
{
	vfDualPrintF("Freeing binary data...");
	
	free(uiaAtomicFeatureRange);
	free(uiaAtomicIndexes);

	for (uint i=0; i<uiTestCount; i++)
		free(uiaTestParameters[i]);
	free(uiaTestParameters);
	vfDualPrintF("completed\n");
}

void vfSaveCurrentIterationConfigurations(FILE *fFile, uint **uiaPreviousIterationConfigurationSet, uint uiPreviousIterationConfigurationCount, uchar ucCardinality)
{
	vfDualPrintF("Saving configurations being generated on the current iteration...");
	fwrite(&uiPreviousIterationConfigurationCount,1,sizeof(uint),fFile);
	for (uint i=0; i<uiPreviousIterationConfigurationCount; i++)
		fwrite(uiaPreviousIterationConfigurationSet[i],ucCardinality,sizeof(uint),fFile);
	fflush(fFile);
	vfDualPrintF("completed\n");
}

bool bfMatchCount(uchar ucCardinality, uint *uiaPreviousConfiguration, uint uiAtomicFeature)
{
	for (uint i=0, uiMatchCount = 0, uiCount = ((uiTestCount - 1) >> 5) + 1; (i < uiCount) && (uiMatchCount < uiMatchThreshold); i++) {
		uint uiTemp = uiaAtomicFeatureMasks[uiAtomicFeature][i];
		if (uiTemp) {
			for (uint j=0; (j<ucCardinality) && (uiTemp); j++)
				uiTemp &= uiaAtomicFeatureMasks[uiaPreviousConfiguration[j]][i];
			
			if (uiTemp)
				COUNT_BITS(uiTemp)
		}
	}
	return(uiMatchCount >= uiMatchThreshold);
}

void vfGenerateComplexConfigurations(char *caTestFileName, char *caConfigDataFileName)
{
	vfPrintFunctionHeader("Generating complex configurations");
	// loading data
	vfLoadRawData(caTestFileName);
	
	// initializing
	uint *uiaAtomicFeatureSet;
	uint **uiaPreviousIterationConfigurationSet;
	uint **uiaCurrentIterationConfigurationSet;
	
	uint uiPreviousIterationConfigurationCount = 0;
	uint uiCurrentIterationConfigurationCount = 0;

	// allocating additional memory
	vfDualPrintF("Creating mask arrays...");
	uiaAtomicFeatureMasks = (uint **)calloc(uiParameterCount,sizeof(uint *));
	uiaAtomicFeatureSet = (uint *)calloc(uiParameterCount,sizeof(uint));
	uiaPreviousIterationConfigurationSet = 0;
	uiaCurrentIterationConfigurationSet = 0;

	for (uint i=0; i<uiParameterCount; i++)
		uiaAtomicFeatureMasks[i] = (uint *)calloc((((uiTestCount - 1) >> 5) + 1),sizeof(uint));

	// initializing atomic feature bit mask arrays
	for ( i=0; i<uiTestCount; i++)
		for (uint j=0; j<uiVariableCount; j++)
			uiaAtomicFeatureMasks[uiaTestParameters[i][j]][i >> 5] |= (uint)1 << (i & 31);

	vfDualPrintF("completed\n");
	
	// freeing resources no more needed
	vfFreeRawData();

	// generating active atomic feature set
	vfDualPrintF("Generating atomic feature set...");
	uint uiAtomicFeatureCount = 0;
	for ( i=0; i<uiParameterCount; i++) {
		uint uiMatchCount = 0;
		for (uint j=0, uiCount = ((uiTestCount - 1) >> 5) + 1; (j < uiCount) && (uiMatchCount < uiMatchThreshold); j++) {
			uint uiTemp = uiaAtomicFeatureMasks[i][j];
			if (uiTemp)
				COUNT_BITS(uiTemp)
		}
		if (uiMatchCount >= uiMatchThreshold) {
			uiaAtomicFeatureSet[uiAtomicFeatureCount++] = i;
			uiaPreviousIterationConfigurationSet = (uint **)realloc(uiaPreviousIterationConfigurationSet,uiAtomicFeatureCount*sizeof(uint *));
			uiaPreviousIterationConfigurationSet[uiAtomicFeatureCount - 1] = (uint *)malloc(1*sizeof(uint));
			uiaPreviousIterationConfigurationSet[uiAtomicFeatureCount - 1][0] = i;
		}
	}
	uiPreviousIterationConfigurationCount = uiAtomicFeatureCount;
	vfDualPrintF("completed (%d)\n",uiPreviousIterationConfigurationCount);
	
	FILE *fOutput = fopen(caConfigDataFileName,"wb");
	CHECK_FILE_IF_OPENED(fOutput,caConfigDataFileName);
	vfSaveCurrentIterationConfigurations(fOutput,uiaPreviousIterationConfigurationSet,uiPreviousIterationConfigurationCount,1);
	
	// generating complex configurations
	vfDualPrintF("Started generating configurations\n");
	uint uiMaxCardinality = min(uiVariableCount,uiMaxCardinalityCount);
	for (uchar ucCardinality = 1; (uiPreviousIterationConfigurationCount) && (ucCardinality < uiMaxCardinality); ucCardinality++) {
		vfDualPrintF("Cardinality : %2d ...\n",ucCardinality + 1);
		uint uiCurrentIterationConfigurationCount = 0;
		uiaCurrentIterationConfigurationSet = (uint **)realloc(uiaCurrentIterationConfigurationSet,0*sizeof(uint *));
		for ( i=0; i<uiPreviousIterationConfigurationCount; i++) {
			if (i % 10 == 0)
				vfDualPrintF("%5d : %5d\n",i,uiCurrentIterationConfigurationCount);
			
			for (uint j=0, uiMaxAtomicFeatureIndex = 0; j<ucCardinality; j++)
				if (uiaPreviousIterationConfigurationSet[i][j] > uiMaxAtomicFeatureIndex)
					uiMaxAtomicFeatureIndex = uiaPreviousIterationConfigurationSet[i][j];

			for ( j=uiMaxAtomicFeatureIndex + 1; j<uiAtomicFeatureCount; j++)
				if (bfMatchCount(ucCardinality,uiaPreviousIterationConfigurationSet[i],uiaAtomicFeatureSet[j])) {
					uiCurrentIterationConfigurationCount++;
					uiaCurrentIterationConfigurationSet = (uint **)realloc(uiaCurrentIterationConfigurationSet,uiCurrentIterationConfigurationCount*sizeof(uint *));
					uiaCurrentIterationConfigurationSet[uiCurrentIterationConfigurationCount - 1] = (uint *)malloc((ucCardinality + 1)*sizeof(uint));
					memcpy(uiaCurrentIterationConfigurationSet[uiCurrentIterationConfigurationCount - 1],uiaPreviousIterationConfigurationSet[i],ucCardinality*sizeof(uint));
					uiaCurrentIterationConfigurationSet[uiCurrentIterationConfigurationCount - 1][ucCardinality] = uiaAtomicFeatureSet[j];
				}
			
		}
		vfDualPrintF("completed -> %8d\n",uiCurrentIterationConfigurationCount);
		// freeing previous iteration results
		vfDualPrintF("Updating global sets...");
		for ( i=0; i<uiPreviousIterationConfigurationCount; i++)
			free(uiaPreviousIterationConfigurationSet[i]);
		uiaPreviousIterationConfigurationSet = (uint **)realloc(uiaPreviousIterationConfigurationSet,uiCurrentIterationConfigurationCount*sizeof(uint *));
		
		// copying current iteration results
		uiPreviousIterationConfigurationCount = uiCurrentIterationConfigurationCount;
		memcpy(uiaPreviousIterationConfigurationSet,uiaCurrentIterationConfigurationSet,uiCurrentIterationConfigurationCount*sizeof(uint *));
		vfDualPrintF("completed\n");

		vfSaveCurrentIterationConfigurations(fOutput,uiaPreviousIterationConfigurationSet,uiPreviousIterationConfigurationCount,ucCardinality + 1);
	}
	fclose(fOutput);
	vfDualPrintF("Finished generating configurations\n");

	// freeing additional resources
	vfDualPrintF("Freeing additional resources...");
	
	free(uiaAtomicFeatureSet);

	for ( i=0; i<uiParameterCount; i++)
		free(uiaAtomicFeatureMasks[i]);
	free(uiaAtomicFeatureMasks);
	
	free(uiaCurrentIterationConfigurationSet);
	
	for ( i=0; i<uiPreviousIterationConfigurationCount; i++)
		free(uiaPreviousIterationConfigurationSet[i]);
	free(uiaPreviousIterationConfigurationSet);

	vfDualPrintF("completed\n");
	vfPrintFunctionFooter("Generating complex configurations");
}

uint uifGetIndexByValue(uint uiValue)
{
	for (uint i=0; i<uiVariableCount; i++)
		if (uiValue < uiaAtomicIndexes[i])
			return(i - 1);
	return(uiVariableCount - 1);
}

void vfRemoveSubConfigurations(SConfiguration **tppPatterns, uint *uipPatternCounts, int iCardinality, uint uiIndex)
{
	SConfiguration &tConfiguration = tppPatterns[iCardinality][uiIndex];
	for (int i=iCardinality - 1; i>=0; i--)
		for (int j=0; j<(int)uipPatternCounts[i]; j++) {
			bool bOk;
			for (int k=0; k<i + 1; k++) {
				bOk = false;
				for (int m=0; (tConfiguration.uiaAtomicFeatures[m]<=tppPatterns[i][j].uiaAtomicFeatures[k]) && (m<=iCardinality); m++)
					if (tConfiguration.uiaAtomicFeatures[m] == tppPatterns[i][j].uiaAtomicFeatures[k]) {
						bOk = true;
						break;
					}
				if (!bOk)
					break;
			}
			if (bOk) {
				tppPatterns[i][j].bUseIt = false;
			}
		}
}

void vfGeneratePatterns(char *caTestDataFileName, char *caCCFileName, char *caPatternFileName, bool bShowConfigurations)
{
	vfPrintFunctionHeader("Generating patterns");
	vfLoadTestData(caTestDataFileName);
	
	uint **uiaConfigurations = 0;
	uint *uiaConfigCardinalityCounts = 0;
	uint uiConfigCount = 0;
	uint uiCardinalityCount = 0;
	int  i;
	
	FILE *fConfigs = fopen(caCCFileName,"rb");
	CHECK_FILE_IF_OPENED(fConfigs,caCCFileName);
	
	for ( ; !feof(fConfigs); uiCardinalityCount++) {
		uiaConfigCardinalityCounts = (uint *)realloc(uiaConfigCardinalityCounts,(uiCardinalityCount + 1)*sizeof(uint));
		if (!fread(uiaConfigCardinalityCounts + uiCardinalityCount,1,sizeof(uint),fConfigs))
			break;
		uiConfigCount += uiaConfigCardinalityCounts[uiCardinalityCount];
		uiaConfigurations = (uint **)realloc(uiaConfigurations,(uiCardinalityCount + 1)*sizeof(uint *));
		uiaConfigurations[uiCardinalityCount] = (uint *)malloc(uiaConfigCardinalityCounts[uiCardinalityCount]*(uiCardinalityCount + 1)*sizeof(uint));
		fread(uiaConfigurations[uiCardinalityCount],uiaConfigCardinalityCounts[uiCardinalityCount],(uiCardinalityCount + 1)*sizeof(uint),fConfigs);
	}
	fclose(fConfigs);

	if (bShowConfigurations) {
		vfDualPrintF("List of configurations being generated:\n");
		for ( i=0; i<(int)uiCardinalityCount; i++)
			for (uint j=0; j<(uiaConfigCardinalityCounts[i])*(i + 1); j++) {
				uint uiIndex = uifGetIndexByValue(uiaConfigurations[i][j]);
				vfDualPrintF("%6d[%d]", uiIndex + 1, uiaConfigurations[i][j] + 1 - uiaAtomicIndexes[uiIndex]);
				if (((j + 1) % (i + 1)) == 0)
					vfDualPrintF("\n");
			}
		vfDualPrintF("End of list of configurations being generated\n");
	}
		
	SConfiguration **tppPatterns = (SConfiguration **)calloc(uiCardinalityCount,sizeof(SConfiguration *));
	uint *uipPatternCounts = (uint *)calloc(uiCardinalityCount,sizeof(uint));
	uint uiTotalPatternCount = 0;
	vfDualPrintF("Started computing patterns\n");
	for ( i=(int)uiCardinalityCount - 1; i>=0; i--) {
		uint *uipCurrentPattern = (uint *)malloc((i + 1)*sizeof(uint));
		for (uint j=0; j<(uiaConfigCardinalityCounts[i]); j++) {
			for (uint k=0; k<(uint)i + 1; k++)
				uipCurrentPattern[k] = uifGetIndexByValue(uiaConfigurations[i][j*(i + 1) + k]);
			
			bool bOk = false;
			for ( k=0; k<uipPatternCounts[i]; k++) {
				bOk = true;
				for (int m=0; m<i + 1; m++)
					if (tppPatterns[i][k].uiaAtomicFeatures[m] != uipCurrentPattern[m]) {
						bOk = false;
						break;
					}
				if (bOk) {
					(tppPatterns[i][k].uiCount)++;
					break;
				}
			}
			if (!bOk) {
				tppPatterns[i] = (SConfiguration *)realloc(tppPatterns[i],(++(uipPatternCounts[i]))*sizeof(SConfiguration));
				tppPatterns[i][uipPatternCounts[i] - 1].uiCount = 1;
				tppPatterns[i][uipPatternCounts[i] - 1].bUseIt = true;
				tppPatterns[i][uipPatternCounts[i] - 1].uiaAtomicFeatures = (uint *)malloc((i + 1)*sizeof(uint));
				memcpy(tppPatterns[i][uipPatternCounts[i] - 1].uiaAtomicFeatures,uipCurrentPattern,(i + 1)*sizeof(uint));
			}
		}
		free(uipCurrentPattern);
	}
	
	for ( i=uiCardinalityCount - 1; i>=0; i--)
		for (int j=0; j<(int)uipPatternCounts[i]; j++) {
			if (!tppPatterns[i][j].bUseIt)
				continue;
			uint uiComplexity = 1;
			for (int m=0; m<i + 1; m++)
				uiComplexity *= uiaAtomicFeatureRange[tppPatterns[i][j].uiaAtomicFeatures[m]];
			if ((double)tppPatterns[i][j].uiCount/(double)uiComplexity < PATTERN_EXISTANCE_COEFFICIENT)
				tppPatterns[i][j].bUseIt = false;
			else
				vfRemoveSubConfigurations(tppPatterns,uipPatternCounts,i,j);
		}
	vfDualPrintF("Finished computing patterns\n");

	vfDualPrintF("List of patterns being generated:\n");
	uiPatternCount = 0;
	for ( i=(int)uiCardinalityCount - 1; i>=0; i--)
		for (int j=0; j<(int)uipPatternCounts[i]; j++) {
			if (!tppPatterns[i][j].bUseIt)
				continue;
			uiPatternCount++;
			for (int k=0; k<=i; k++)
				vfDualPrintF("[%d]", tppPatterns[i][j].uiaAtomicFeatures[k] + 1);
			vfDualPrintF("\n");
		}
	vfDualPrintF("End of list of patterns being generated (%d)\n",uiPatternCount);
		
	vfDualPrintF("Saving patterns being generated...");
	FILE *fPatterns = fopen(caPatternFileName,"wb");
	CHECK_FILE_IF_OPENED(fPatterns,caPatternFileName);
	fwrite(&uiPatternCount,1,sizeof(uint),fPatterns);
	for ( i=(int)uiCardinalityCount - 1; i>=0; i--)
		for (int j=0; j<(int)uipPatternCounts[i]; j++) {
			if (!tppPatterns[i][j].bUseIt)
				continue;
			uint uiDummy = (uint)i + 1;
			fwrite(&uiDummy,1,sizeof(uint),fPatterns);
			for (int k=0; k<=i; k++)
				fwrite(&(tppPatterns[i][j].uiaAtomicFeatures[k]),1,sizeof(uint),fPatterns);
		}
	fclose(fPatterns);
	vfDualPrintF("completed\n");
	
	vfDualPrintF("Freeing additional resources...");
	
	for ( i=0; i<(int)uiCardinalityCount; i++) {
		for (int j=0; j<(int)uipPatternCounts[i]; j++)
			free(tppPatterns[i][j].uiaAtomicFeatures);
		free(tppPatterns[i]);
	}
	free(tppPatterns);
	free(uipPatternCounts);
	
	for ( i=0; i<(int)uiCardinalityCount; i++)
		free(uiaConfigurations[i]);
	free(uiaConfigurations);
	
	free(uiaConfigCardinalityCounts);
	
	vfFreeTestData();

	vfPrintFunctionFooter("Generating patterns");
}

void vfGeneratePatternBasis(char *caBinaryDataFileName, char *caPatternDataFileName, char *caCoreDataFileName)
{
	vfPrintFunctionHeader("Generating pattern basis");
	FILE *fBinary = fopen(caBinaryDataFileName,"rb");
	CHECK_FILE_IF_OPENED(fBinary,caBinaryDataFileName);
	FILE *fPatterns = fopen(caPatternDataFileName,"rb");
	CHECK_FILE_IF_OPENED(fPatterns,caPatternDataFileName);
	FILE *fCore = fopen(caCoreDataFileName,"wb");
	CHECK_FILE_IF_OPENED(fCore,caCoreDataFileName);

	uint	uiDummy;
	double	dDummy;

	vfReadEFHeader(fBinary,tEFHeader,caBinaryDataFileName);
	vfWriteEFHeader(fCore,tEFHeader);

	fread (&uiVariableCount,1,sizeof(uiVariableCount),fBinary);
	fwrite(&uiVariableCount,1,sizeof(uiVariableCount),fCore);
	for (uint i=0; i<uiVariableCount; i++) {
		fread (&uiDummy,1,sizeof(uint),fBinary);
		fwrite(&uiDummy,1,sizeof(uint),fCore);
	}
	
	for ( i=0; i<uiVariableCount; i++) {
		fread (&uiDummy,1,sizeof(uint),fBinary);
		fwrite(&uiDummy,1,sizeof(uint),fCore);
	}

	fread(&uiFunctionType,1,sizeof(uint),fBinary);
	fwrite(&uiFunctionType,1,sizeof(uint),fCore);
	
	fread(&dMinResultValue,1,sizeof(double),fBinary);
	fwrite(&dMinResultValue,1,sizeof(double),fCore);
	
	fread(&dMaxResultValue,1,sizeof(double),fBinary);
	fwrite(&dMaxResultValue,1,sizeof(double),fCore);

	fread (&uiPatternCount,1,sizeof(uiPatternCount),fPatterns);
	fwrite(&uiPatternCount,1,sizeof(uiPatternCount),fCore);
	for ( i=0; i<uiPatternCount; i++) {
		fread (&uiDummy,1,sizeof(uint),fPatterns);
		if (feof(fPatterns))
			break;
		fwrite(&uiDummy,1,sizeof(uint),fCore);
		for (uint j=0, uiDummy2 = uiDummy; j<uiDummy2; j++) {
			fread (&uiDummy,1,sizeof(uint),fPatterns);
			fwrite(&uiDummy,1,sizeof(uint),fCore);
		}
	}
	fclose(fPatterns);

	fread (&uiPatternCount,1,sizeof(uiPatternCount),fBinary);
	for ( i=0; i<uiPatternCount; i++) {
		fread (&uiDummy,1,sizeof(uint),fBinary);
		if (feof(fPatterns))
			break;
		for (uint j=0, uiDummy2 = uiDummy; j<uiDummy2; j++)
			fread (&uiDummy,1,sizeof(uint),fBinary);
	}
	
	fread (&uiTestCount,1,sizeof(uiTestCount),fBinary);
	fwrite(&uiTestCount,1,sizeof(uiTestCount),fCore);

	for ( i=0; i<uiTestCount; i++) {
		for (int j=0; j<(int)uiVariableCount; j++) {
			fread (&uiDummy,1,sizeof(uint),fBinary);
			fwrite(&uiDummy,1,sizeof(uint),fCore);
		}
		fread (&dDummy,1,sizeof(double),fBinary);
		fwrite(&dDummy,1,sizeof(double),fCore);
	}

	fclose(fBinary);
	fclose(fCore);
	vfPrintFunctionFooter("Generating pattern basis");
}

void vfBuildEvaluationFunction(char *caCoreDataFileName, char *caParametersDataFileName, char *caEFDataFileName, bool bConvertToFloat)
{
	vfPrintFunctionHeader("Building evaluation function");
	FILE *fCore = fopen(caCoreDataFileName,"rb");
	CHECK_FILE_IF_OPENED(fCore,caCoreDataFileName);
	FILE *fParams = fopen(caParametersDataFileName,"rb");
	CHECK_FILE_IF_OPENED(fParams,caParametersDataFileName);
	FILE *fEF = fopen(caEFDataFileName,"wb");
	CHECK_FILE_IF_OPENED(fEF,caEFDataFileName);

	uint	uiDummy;
	double	dDummy;

	vfReadEFHeader(fCore,tEFHeader,caCoreDataFileName);

	if (bConvertToFloat)
		tEFHeader.uiDataFormat = FLOAT_DATA_FORMAT;
	else
		tEFHeader.uiDataFormat = DOUBLE_DATA_FORMAT;
	vfWriteEFHeader(fEF,tEFHeader);

	fread (&uiVariableCount,1,sizeof(uiVariableCount),fCore);
	fwrite(&uiVariableCount,1,sizeof(uiVariableCount),fEF);
	for (uint i=0; i<uiVariableCount; i++) {
		fread (&uiDummy,1,sizeof(uint),fCore);
		fwrite(&uiDummy,1,sizeof(uint),fEF);
	}
	
	for ( i=0; i<uiVariableCount; i++) {
		fread (&uiDummy,1,sizeof(uint),fCore);
		fwrite(&uiDummy,1,sizeof(uint),fEF);
	}

	fread(&uiFunctionType,1,sizeof(uint),fCore);
	fwrite(&uiFunctionType,1,sizeof(uint),fEF);
	
	if (!bConvertToFloat) {
		fread(&dMinResultValue,1,sizeof(double),fCore);
		fwrite(&dMinResultValue,1,sizeof(double),fEF);

		fread(&dMaxResultValue,1,sizeof(double),fCore);
		fwrite(&dMaxResultValue,1,sizeof(double),fEF);
	}
	else {
		float fDummy;
		fread(&dMinResultValue,1,sizeof(double),fCore);
		fDummy = (float)dMinResultValue;
		fwrite(&fDummy,1,sizeof(float),fEF);

		fread(&dMaxResultValue,1,sizeof(double),fCore);
		fDummy = (float)dMaxResultValue;
		fwrite(&fDummy,1,sizeof(float),fEF);
	}

	
	fread (&uiPatternCount,1,sizeof(uiPatternCount),fCore);
	fwrite(&uiPatternCount,1,sizeof(uiPatternCount),fEF);
	for ( i=0; i<uiPatternCount; i++) {
		fread (&uiDummy,1,sizeof(uint),fCore);
		if (feof(fCore))
			break;
		fwrite(&uiDummy,1,sizeof(uint),fEF);
		for (uint j=0, uiDummy2 = uiDummy; j<uiDummy2; j++) {
			fread (&uiDummy,1,sizeof(uint),fCore);
			fwrite(&uiDummy,1,sizeof(uint),fEF);
		}
	}
	fclose(fCore);
	
	fseek(fParams,0,SEEK_END);
	uint uiParameterCount = ftell(fParams)/sizeof(double);
	fseek(fParams,0,SEEK_SET);
	for ( i=0; i<uiParameterCount; i++) {
		fread (&dDummy,1,sizeof(double),fParams);
		if (!bConvertToFloat)
			fwrite(&dDummy,1,sizeof(double),fEF);
		else {
			float fDummy = (float)dDummy;
			fwrite(&fDummy,1,sizeof(float),fEF);
		}
	}

	fclose(fParams);
	fclose(fEF);
	
	vfPrintFunctionFooter("Building evaluation function");
}

void vfValidateTestData(char *caEFDataFileName)
{
	vfPrintFunctionHeader("Validating evaluation function");
	vfLoadTestData(caEFDataFileName,false);
	
	FILE *fData = fopen(caEFDataFileName,"rb");
	CHECK_FILE_IF_OPENED(fData,caEFDataFileName);
	if (tEFHeader.uiDataFormat == DOUBLE_DATA_FORMAT) {
		fseek(fData,-(int)(uiParameterCount*sizeof(double)),SEEK_END);
		fread(daParameters,uiParameterCount,sizeof(double),fData);
	}
	else {
		fseek(fData,-(int)(uiParameterCount*sizeof(float)),SEEK_END);
		float fDummy;
		for (uint k=0; k<uiParameterCount; k++) {
			fread(&fDummy,1,sizeof(float),fData);
			daParameters[k] = (double)fDummy;
		}
	}
	fclose(fData);

	vfDualPrintF("Validating evaluation function (type \"quit\" to exit):\n");
	printf(">");
	fflush(fOutput);
	uiaTestParameters = (uint **)malloc(1*sizeof(uint *));
	uiaTestParameters[0] = (uint *)malloc(uiVariableCount*sizeof(uint));
	bool bStop = false;
	while (true) {
		for (int i=0; i<(int)uiVariableCount; ) {
			uint uiTemp;
			char caString[260];
			scanf("%s",&caString);
			if (!strcmp(strupr(caString),"QUIT")) {
				bStop = true;
				break;
			}
			uint uiDummy = strlen(caString);
			caString[uiDummy] = ' ';
			caString[uiDummy + 1] = 0;
			for (int j=0, k=-1; (j<(int)strlen(caString)) && (i<(int)uiVariableCount); j++)
				if (!isdigit(caString[j])) {
					caString[j] = ' ';
					if (j - k > 1) {
						sscanf(caString + k + 1,"%d",&uiTemp);
						uiaTestParameters[0][i] = uiTemp - 1;
						i++;
						if (i >= (int)uiVariableCount)
							break;
					}
					k = j;
				}
		}
		if (bStop)
			break;
		for ( i=0; i<(int)uiVariableCount; i++)
			if (i)
				fprintf(fOutput,"%6d",uiaTestParameters[0][i] + 1);
			else
				fprintf(fOutput,"%d",uiaTestParameters[0][i] + 1);
		vfDualPrintF("%7.2f\n",dfEvaluation(uiaTestParameters[0]));
		printf(">");
		fflush(fOutput);
	}
	
	vfFreeTestData(false);
	free(uiaTestParameters[0]);
	free(uiaTestParameters);
	vfPrintFunctionFooter("Validating evaluation function");
}