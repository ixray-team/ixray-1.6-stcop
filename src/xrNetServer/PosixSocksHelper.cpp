#include <cstdio>
#include <cstring>
#include <netdb.h>
#include <arpa/inet.h>
#include <unistd.h>

class WinsocksHelper
{
public:
    static unsigned long GetIpAddress(const char* hostName)
    {
        if (!hostName || hostName[0] == '\0')
            return 0;

        struct addrinfo hints{};
        hints.ai_family = AF_INET; // IPv4 только
        hints.ai_socktype = SOCK_STREAM;

        struct addrinfo* res = nullptr;
        int err = getaddrinfo(hostName, nullptr, &hints, &res);
        if (err != 0 || res == nullptr)
        {
            fprintf(stderr, "[WinsocksHelper] getaddrinfo failed: %s\n", gai_strerror(err));
            return 0;
        }

        unsigned long ip = 0;
        if (res->ai_family == AF_INET)
        {
            struct sockaddr_in* addr = (struct sockaddr_in*)res->ai_addr;
            ip = ntohl(addr->sin_addr.s_addr); // в хост-байтовый порядок
        }

        freeaddrinfo(res);
        return ip;
    }
};
