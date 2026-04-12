#pragma once
#include <string>
#include <vector>
#include "LogicMetainfo.h"

// Простейший загрузчик .ltx конфигов логики в структуру редактора
class LogicLoader
{
public:
    // Рекурсивно считывает все .ltx файлы из папки и возвращает вектор состояний (FState)
    static xr_vector<FState> LoadAsStates(const xr_string& folder);
    // Load single .ltx/.ini file and return parsed states
    static xr_vector<FState> LoadFromFile(const xr_string& filename);
};
