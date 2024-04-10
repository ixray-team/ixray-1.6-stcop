#include "StdAfx.h"
#include "xrCompress.h"
#include "xrDecompress.h"

#ifndef MOD_COMPRESS
extern int ProcessDifference();
#endif

void Unpacker(char* argv[])
{
	Core._initialize("xrCompress", 0, true, argv[2]);

	xrDecompressor D(argv[3]);
	D.Decompress();

	Core._destroy();
}

int main(int argc, char* argv[])
{
#ifdef IXR_WINDOWS
	const char* params = GetCommandLineA();
#else
	xr_string TempBuffer = "";
	for (size_t Iter = 0; Iter < argc; Iter++)
	{
		TempBuffer += argv[Iter];
		TempBuffer += " ";
	}

	const char* params = TempBuffer.data();
#endif

	Debug._initialize	(false);

	if (strstr(params, "-unpack"))
	{
		Unpacker(argv);
		return 0;
	}
	else
	{
		Core._initialize("xrCompress", 0, false);
	}
	printf				("\n\n");

	xrCompressor		C;

	C.SetStoreFiles(0!=strstr(params,"-store"));

#ifndef MOD_COMPRESS
	if(strstr(params,"-diff"))
	{
		ProcessDifference	();
	}else
#endif
	{
#ifndef MOD_COMPRESS
		if (argc<2)	
		{
			printf("ERROR: u must pass folder name as parameter.\n");
			printf("-diff /? option to get information about creating difference.\n");
			printf("-fast	- fast compression.\n");
			printf("-store	- store files. No compression.\n");
			printf("-ltx <file_name.ltx> - pathes to compress.\n");
			printf("\n");
			printf("LTX format:\n");
			printf("	[config]\n");
			printf("	;<path>     = <recurse>\n");
			printf("	.\\         = false\n");
			printf("	textures    = true\n");
			
			Core._destroy();
			return 3;
		}
#endif

		string_path		folder;		
		xr_strconcat(folder,argv[1],"\\");
		_strlwr_s		(folder,sizeof(folder));
		printf			("\nCompressing files (%s)...\n\n",folder);

		FS._initialize	(CLocatorAPI::flTargetFolderOnly, folder);
		FS.append_path	("$working_folder$","",0,false);

		C.SetFastMode	(0!=strstr(params,"-fast"));
		C.SetTargetName	(argv[1]);

		LPCSTR p = strstr(params,"-ltx");

		if(0!=p)
		{
			string64				ltx_name;
			sscanf					(strstr(params,"-ltx ")+5,"%[^ ] ", ltx_name);

			CInifile ini			(ltx_name);
			printf					("Processing LTX...\n");
			C.ProcessLTX			(ini);
		}else{
			string64				header_name;
			sscanf					(strstr(params,"-header ")+8,"%[^ ] ", header_name);
			C.SetPackHeaderName		(header_name);
			C.ProcessTargetFolder	();
		}
	}

	Core._destroy		();
	return 0;
}
