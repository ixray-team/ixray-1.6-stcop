# Выделенный сервер
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

## Dedicated Server
> Dedicated Server - это выделенный сервер для работы режимов игры, отличных от Single Player

![image](https://github.com/user-attachments/assets/4719f23f-6d64-4096-ab2e-f47d81bd26a5)

## Основные изменения 
* Вынесен из xrEngine
* Переведён на `xrRender_DS0`, который не использует GPU, т.к. она не требуется для работы сервера
* Добавлен режим FreeMP
* Переведён с `dplay8` на `SteamNetworking`

## FreeMP
> FreeMP - режим игры с поддержкой A-Life системы

### Реализовано
* Мутанты 
* Починка снаряжения
* Личные ящики
* Торговля
* Voice чат
