
#pragma once
#include <winsock2.h>
#include <ws2tcpip.h>

#define CZ_OK "0"
#define CZ_InvalidFormat "1"
#define CZ_UnknownCommand "2"
#define CZ_NoResponsibleType "3"
#define CZ_ServerError "10"

#pragma comment(lib, "Ws2_32.lib")
class ChezzeClient {
private:
    bool Send(const xr_string& data) {
        if (!connected || fatal) return false;
        return send(sock, data.c_str(), static_cast<int>(data.size()), 0) != SOCKET_ERROR;
    }
public:
        static ChezzeClient& Instance() {
            static ChezzeClient instance;
            return instance;
        }

        bool Connect(const xr_string& ip, int port) {
            if (fatal) return false;
            if (connected) return true;

            sockaddr_in serverAddr{};
            serverAddr.sin_family = AF_INET;
            serverAddr.sin_port = htons(port);
            inet_pton(AF_INET, ip.c_str(), &serverAddr.sin_addr);

            sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
            if (sock == INVALID_SOCKET) {
                //Msg("ERROR socket()\n");
                return false;
            }

            if (connect(sock, (sockaddr*)&serverAddr, sizeof(serverAddr)) == SOCKET_ERROR) {
                //Msg("ERROR connect()\n");
                closesocket(sock);
                sock = INVALID_SOCKET;
                return false;
            }

            connected = true;
            return true;
        }

       
        void SplashInfo(const xr_string& persent, const xr_string& desc) {
            /*
            * Example
                {
                "res_type": "spl_game_lnch",
                "spl_pers": 0,
                "spl_desc": "Test"
                }
            */
            xr_string snd = std::format(
                "{{\"res_type\":\"spl_game_lnch\",\"spl_pers\":\"{}\",\"spl_desc\":\"{}\"}}",
                persent.c_str(),
                desc.c_str()
            ).c_str();
            Send(snd);
            auto rec = Receive();
            if (rec != CZ_OK)
            {
                //if error do something
            }
        }

        void Execute(const xr_string& res_type)
        {
            xr_string snd = std::format(
                "{{\"res_type\":\"{}\"}}", res_type.c_str()).c_str();
            Send(snd);
            auto rec = Receive();
            if (rec != CZ_OK)
            {
                //if error do something
            }
        }

        xr_string Receive(int bufferSize = 1024) {
            if (!connected) return "";

            char* buffer = new char[bufferSize];
            memset(buffer, 0, bufferSize);

            int bytesReceived = recv(sock, buffer, bufferSize, 0);
            xr_string result;
            if (bytesReceived > 0) {
                result.assign(buffer, bytesReceived);
            }

            delete[] buffer;
            return result;
        }

        void Disconnect() {
            if (connected) {
                closesocket(sock);
                sock = INVALID_SOCKET;
                connected = false;
            }
        }

        ~ChezzeClient() {
            Disconnect();
            WSACleanup();
        }

    private:
        SOCKET sock = INVALID_SOCKET;
        bool connected = false;
        bool fatal = false;
        ChezzeClient() {
            WSADATA wsaData;
            if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
                //Msg("Error WSAStartup\n");
                fatal = true;
            }
        }

        ChezzeClient(const ChezzeClient&) = delete;
        ChezzeClient& operator=(const ChezzeClient&) = delete;
    };
