#include "stdafx.h"


#include <time.h>

char* timestamp(string64& dest)
{
    time_t     now = time(nullptr);
    struct tm  tstruct;

#ifdef IXR_WINDOWS
    localtime_s(&tstruct, &now);// thread-safe for windows
#else
    localtime_r(&now, &tstruct);// thread-safe for posix systems
#endif

    strftime(dest, sizeof(dest), "%m-%d-%y_%H-%M-%S", &tstruct);

    return dest;
}

bool NaturalCompare(const char* a, const char* b)
{
	if (!a || !b)
	{
		return false;
	}

	while (*a && *b)
	{
		if (*a == *b)
		{
			a++;
			b++;
			continue;
		}

		if (std::isdigit((unsigned char)*a) && std::isdigit((unsigned char)*b))
		{
			long num_a = 0, num_b = 0;

			const char* ptr_a = a;
			while (std::isdigit((unsigned char)*ptr_a))
			{
				num_a = num_a * 10 + (*ptr_a - '0');
				ptr_a++;
			}

			const char* ptr_b = b;
			while (std::isdigit((unsigned char)*ptr_b))
			{
				num_b = num_b * 10 + (*ptr_b - '0');
				ptr_b++;
			}

			if (num_a != num_b)
			{
				return num_a < num_b;
			}

			a = ptr_a;
			b = ptr_b;
		}
		else
		{
			return *a < *b;
		}
	}

	return *a == '\0' && *b != '\0';
}