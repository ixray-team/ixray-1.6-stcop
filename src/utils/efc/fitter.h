////////////////////////////////////////////////////////////////////////////
//	Module 		: fitter.h
//	Created 	: 25.03.2002
//  Modified 	: 09.10.2002
//	Author		: Dmitriy Iassenev
//	Description : Pattern Configuration Generation and Weight Fitting Algorithms
////////////////////////////////////////////////////////////////////////////

#pragma once

////////////////////////////////////////////////////////////////////////////
// type declarations
////////////////////////////////////////////////////////////////////////////

typedef struct tagSConfiguration {
	uint	uiCount;
	bool	bUseIt;
	uint	*uiaAtomicFeatures;
} SConfiguration;

typedef struct tagSPattern {
	uint	uiCardinality;
	uint	*uiaVariableIndexes;
} SPattern;

typedef struct tagSEFHeader {
	uint	uiBuilderVersion;
	uint	uiDataFormat;
} SEFHeader;

////////////////////////////////////////////////////////////////////////////
// exportes variables
////////////////////////////////////////////////////////////////////////////

// weight fitting parameters
#define EPSILON							0.00001
#define ALPHA							1.0
#define BETA							0.01
#define MAX_ITERATION					10000
// random weight fitting parameters
#define RANDOM_FACTOR					4
#define RANDOM_PROBABILITY				1
#define RANDOM_UPDATE					1
#define RANDOM_START_SEED				1

// configuration generator parameters
#define MATCH_THRESHOLD					1
#define MAX_CARDINALITY					30
#define PATTERN_EXISTANCE_COEFFICIENT	((double)(1.0))

extern double dEpsilon;
extern double dAlphaCoefficient;
extern double dBetaCoefficient;
extern uint	  uiMaxIterationCount;
extern uint	  uiRandomFactor;
extern uint	  uiRandomProbability;
extern uint	  uiRandomUpdate;
extern uint	  uiRandomStartSeed;
extern uint	  uiMatchThreshold;
extern uint	  uiMaxCardinalityCount;
extern double dPatternsExistanceCoefficient;

////////////////////////////////////////////////////////////////////////////
// exportes functions
////////////////////////////////////////////////////////////////////////////

extern uint uifRandom(uint uiRange);
extern uchar ucfCompareTestExamples(uint *uipFirst, uint *uipSecond, uint uiLength, const uint uiStartDepth);
extern void vfConvertTestData(char *caRawDataFileName, char *caTestDataFileName, char *caPatternDataFileName, bool bShowDuplicates = true);
extern void vfGenerateComplexConfigurations(char *caTestFileName, char *caConfigDataFileName);
extern void vfGeneratePatterns(char *caTestDataFileName, char *caCCFileName, char *caPatternFileName, bool bShowConfigurations = true);
extern void vfGeneratePatternBasis(char *caBinaryDataFileName, char *caPatternDataFileName, char *caCoreDataFileName);
extern void vfOptimizeParameters(char *caFileName, char *caResultFileName, bool bRandom = true, bool bUseParameters = true);
extern void vfShowTestData(char *caTestDataFileName, char *caPatternDataFileName, bool bShowSimpleStats = true, bool bShowSortedStats = true, bool bShowPatternStats = true);
extern void vfBuildEvaluationFunction(char *caCoreDataFileName, char *caParametersDataFileName, char *caEFDataFileName, bool bConvertToFloat);
extern void vfValidateTestData(char *caEFDataFileName);