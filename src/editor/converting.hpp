#ifndef CONVERTING_H_INCLUDED
#define CONVERTING_H_INCLUDED

inline LPSTR to_string(System::String^ string)
{
	size_t convertedChars		= 0;
	size_t sizeInBytes			= ((string->Length + 1) * 2);
	pin_ptr<const wchar_t> wch	= PtrToStringChars(string);
	LPSTR result				= (LPSTR)_alloca((u32)sizeInBytes);
	errno_t err					=
		wcstombs_s(
			&convertedChars, 
			result,
			sizeInBytes,
			wch,
			sizeInBytes
		);

	if (err)
		VERIFY					(!"[tostring][failed] : wcstombs_s failed");

	return result;
}

inline System::String^ to_string(LPCSTR string)
{
	return gcnew System::String(string);
}

inline float to_single(System::String^ string)
{
	size_t convertedChars		= 0;
	size_t sizeInBytes			= ((string->Length + 1) * 2);
	pin_ptr<const wchar_t>	wch = PtrToStringChars(string);
	LPSTR result				= (LPSTR)_alloca((u32)sizeInBytes);
	errno_t err					=
		wcstombs_s(
			&convertedChars, 
			result,
			sizeInBytes,
			wch,
			sizeInBytes
		);
		
	if (err)
		VERIFY					(!"[tostring][failed] : wcstombs_s failed");

	return (float)atof(result);
}

inline float to_single(System::Object^ string)
{
	return to_single(safe_cast<System::String^>(string));
}

#endif // CONVERTING_H_INCLUDED