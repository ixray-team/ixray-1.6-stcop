////////////////////////////////////////////////////////////////////////////
//	Module 		: efc.cpp
//	Created 	: 20.03.2002
//  Modified 	: 09.10.2002
//	Author		: Dmitriy Iassenev
//	Description : Evaluation Function Constructor
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "fitter.h"
#include "misc.h"	   

char caLogData[260];
char caTextData[260];
char caBinaryData[260];
char caConfigData[260];
char caPatternData[260];
char caCoreData[260];
char caParametersData[260];
char caEFData[260];

void vfShowHelpScreen()
{
	vfDualPrintF(" Available switches :\n");
	vfDualPrintF(" -p name  obligatory switch with project name\n");
	vfDualPrintF(" -pa      perform operations for all projects\n\n");
	vfDualPrintF(" -c[d]    convert text to binary data and generate initial patterns\n");
	vfDualPrintF("    d     show duplicates being found in text data\n\n");
	vfDualPrintF(" -g{c,p[s],b}\n");
	vfDualPrintF("   -gc    generate configurations from atomic features based on test data\n");
	vfDualPrintF("   -gp    generate patterns from configurations being generated\n");
	vfDualPrintF("   -gps   show all configurations during generation\n");
	vfDualPrintF("   -gb    generate pattern basis from patterns being generated\n\n");
	vfDualPrintF(" -f[p,u]  fit weights of pattern configurations\n");
	vfDualPrintF("    p     force to use probabilistic weight fitting algorithm\n");
	vfDualPrintF("    u     force to use previous parameters if exist\n\n");
	vfDualPrintF(" -l       list stats on test data\n");
	vfDualPrintF(" -s       list sorted stats on test data\n");
	vfDualPrintF(" -w       list pattern configuration weights\n\n");
	vfDualPrintF(" -b[f]    build evaluation function\n");
	vfDualPrintF("    f     save parameters in float (default is double)\n\n");
	vfDualPrintF(" -v       validate evaluation function\n");
	vfDualPrintF(" -a       append log file\n\n");
	vfDualPrintF(" -h,\n -?,\n -i       this screen\n");
}

void vfPrintHeader(bool bShowHeader)
{
	char s[80];
	memset(s,0,sizeof(s));
	memset(s,'*',sizeof(s)-1);
	if (bShowHeader) {
		vfDualPrintF("%s\n",s);
		vfDualPrintF("*                    Evaluation function constructor v0.564                   *\n");
		vfDualPrintF("*                       for Windows 95,98,ME,NT,2000,XP                       *\n");
		vfDualPrintF("*                             by Dmitriy Iassenev                             *\n");
		vfDualPrintF("*                      Copyright(C) GSC Game World 2002                       *\n");
		vfDualPrintF("*                      Compiled on %s %s                       *\n",__DATE__,__TIME__);
		vfDualPrintF("%s\n",s);
	}
	else {
		fprintf(fOutput,"%s\n",s);
		fprintf(fOutput,"*                    Evaluation function constructor v0.564                   *\n");
		fprintf(fOutput,"*                       for Windows 95,98,ME,NT,2000,XP                       *\n");
		fprintf(fOutput,"*                             by Dmitriy Iassenev                             *\n");
		fprintf(fOutput,"*                      Copyright(C) GSC Game World 2002                       *\n");
		fprintf(fOutput,"*                      Compiled on %s %s                       *\n",__DATE__,__TIME__);
		fprintf(fOutput,"%s\n",s);
	}
}

void vfPerformOperations( int argc, char *argv[], char *caProjectFolder, bool bShowHeader = true)
{
	sprintf(caLogData		,caLogDataFormat	   ,caProjectFolder);
	sprintf(caTextData		,caTextDataFormat	   ,caProjectFolder);
	sprintf(caBinaryData	,caBinaryDataFormat	   ,caProjectFolder);
	sprintf(caConfigData	,caConfigDataFormat	   ,caProjectFolder);
	sprintf(caPatternData	,caPatternDataFormat   ,caProjectFolder);
	sprintf(caCoreData		,caCoreDataFormat	   ,caProjectFolder);
	sprintf(caParametersData,caParametersDataFormat,caProjectFolder);
	sprintf(caEFData		,caEFDataFormat		   ,caProjectFolder);
	
	if (bfCheckForSwitch(argc, argv,"a"))
		fOutput = fopen(caLogData,"at");
	else
		fOutput = fopen(caLogData,"wt");
	
	try {
		CHECK_FILE_IF_OPENED(fOutput,caLogData)
		
		vfPrintHeader(bShowHeader);
			
		if (bfCheckForSwitch(argc, argv,"c"))
			vfConvertTestData(caTextData,caBinaryData,caPatternData,bfCheckForSwitch(argc, argv,"cd"));
		if (bfCheckForSwitch(argc, argv,"gc"))
			vfGenerateComplexConfigurations(caBinaryData,caConfigData);
		if (bfCheckForSwitch(argc, argv,"gp"))
			vfGeneratePatterns(caBinaryData,caConfigData,caPatternData,bfCheckForSwitch(argc, argv,"gps"));
		if (bfCheckForSwitch(argc, argv,"gb"))
			vfGeneratePatternBasis(caBinaryData,caPatternData,caCoreData);
		if (bfCheckForSwitch(argc, argv,"f"))
			vfOptimizeParameters(caCoreData,caParametersData,bfCheckForSwitch(argc, argv,"fp"),bfCheckForSwitch(argc, argv,"fu"));
		if (bfCheckForSwitch(argc, argv,"l") || bfCheckForSwitch(argc, argv,"s") || bfCheckForSwitch(argc, argv,"w"))
			vfShowTestData(caCoreData,caParametersData,bfCheckForSwitch(argc, argv,"l"),bfCheckForSwitch(argc, argv,"s"),bfCheckForSwitch(argc, argv,"w"));
		if (bfCheckForSwitch(argc, argv,"b"))
			vfBuildEvaluationFunction(caCoreData,caParametersData,caEFData,bfCheckForSwitch(argc, argv,"bf"));
		if (bShowHeader && bfCheckForSwitch(argc, argv,"v"))
			vfValidateTestData(caEFData);
		if ((bfCheckForSwitch(argc, argv,"h") || bfCheckForSwitch(argc, argv,"?") || bfCheckForSwitch(argc, argv,"i")) ||
			 !bfCheckForSwitch(argc, argv,"p") ||
			(!bfCheckForSwitch(argc, argv,"c") && 
 			 !bfCheckForSwitch(argc, argv,"gp") && 
 			 !bfCheckForSwitch(argc, argv,"gc") && 
 			 !bfCheckForSwitch(argc, argv,"gb") && 
			 !bfCheckForSwitch(argc, argv,"f") && 
			 !bfCheckForSwitch(argc, argv,"l") && 
			 !bfCheckForSwitch(argc, argv,"s") && 
			 !bfCheckForSwitch(argc, argv,"w") && 
			 !bfCheckForSwitch(argc, argv,"b") && 
			 !bfCheckForSwitch(argc, argv,"v") && 
			 !bfCheckForSwitch(argc, argv,"h") && 
			 !bfCheckForSwitch(argc, argv,"i") &&
			 !bfCheckForSwitch(argc, argv,"?")))
			vfShowHelpScreen();
		fclose(fOutput);
	}
	catch(char *fFileName) {
		if (strlen(fFileName))
			vfDualPrintF("\nCannot open file %s\n",fFileName);
	}
}

void __cdecl main( int argc, char *argv[], char *envp[])
{
	bfLoadIniFile(INI_FILE);
	if (bfCheckForSwitch(argc, argv,"pa")) {
		struct _finddata_t tProject;
		bool bFirst = true;
		uint uiHandle;
		uint uiProjectProcessedCount = 0;
		vfPrintHeader(true);
		while (true) {
			if (bFirst) {
				bFirst = false;
				uiHandle = _findfirst("data\\*.*",&tProject);
				if (uiHandle == 0xffffffff) {
					vfDualPrintF("Projects not found\n");
					break;
				}
			}
			else
				if (_findnext(uiHandle,&tProject) != 0) {
					_findclose(uiHandle);
					if (!uiProjectProcessedCount)
						vfDualPrintF("Projects not found\n");
					else
						vfDualPrintF("\nTotal processed %d projects\n\n",uiProjectProcessedCount);
					break;
				}
				if ((tProject.attrib & _A_SUBDIR) && (strcmp(tProject.name,".")) && (strcmp(tProject.name,".."))) {
					uiProjectProcessedCount++;
					char *caString = (char *)malloc((strlen(tProject.name) + 1)*sizeof(char));
					memcpy(caString,tProject.name,(strlen(tProject.name) + 1)*sizeof(char));
					vfDualPrintF("\nPROJECT BEING PROCESSED : %s\n",caString);
					free(caString);
					vfPerformOperations(argc,argv,tProject.name,false);
				}
		}
	}
	else {
		char *caProjectFolder = cafGetStringNextToSwitch(argc, argv,"p");
		if (!caProjectFolder) {
			vfPrintHeader(true);
			if (argc > 1)
				vfDualPrintF(" Project folder is not specified!\n\n");
			vfShowHelpScreen();
			return;
		}
		vfPerformOperations(argc,argv,caProjectFolder);
	}
}