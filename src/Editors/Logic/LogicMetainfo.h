#pragma once
#include "../../xrCore/stdafx.h"
#include <variant>

#include "imgui.h"
#include "imgui_node_editor.h"

namespace ed = ax::NodeEditor;

struct FColor
{
    uint8_t R = 255;
    uint8_t G = 255;
    uint8_t B = 255;
    uint8_t A = 255;
};

enum class EConditionType
{
    OnTimer,
    OnCombat,
    OnInfo,
    OnDeath,
    OnHit,
};

enum class EStateType
{
    Walker,      // Патрулирование/ходьба
    Combat,      // Боевая логика
    Camper,      // Оборона позиции
    Trader,      // Торговля
    Anim,        // Анимации
    Sound,       // Звуки
    Panic,       // Паника
    Reactor,     // Реакция на события
    Guard,       // Охрана объекта
    Follow,      // Следование за игроком
    Idle,        // Ожидание
    Trigger,     // Триггер для зон
    Action,       // Выполнение действия
    Custom
};

// Направление движения
enum class EMoveType
{
    Walk,
    Run,
    Crawl,
    Stand
};

// Боевой стиль
enum class ECombatStyle
{
    Aggressive,  // Агрессивный
    Sniper,      // Снайпер (держит дистанцию)
    Coward,      // Трусливый (убегает)
    Balanced     // Сбалансированный
};

// Логическая операция для комбинирования условий
enum class ELogicOperation
{
    None,
    And,
    Or
};

// Условие перехода (Condlist)
struct FCondition
{
    EConditionType Type;
    float Value;                        // Для таймеров, дистанций, здоровья
    xr_string InfoName;               // Для инфопоршнов
    xr_string AnimationName;          // Для анимаций
    xr_string ZoneName;               // Для зон-триггеров
    
    // Операторы для комбинирования условий (AND/OR)
    ELogicOperation CombineWith = ELogicOperation::None;
    xr_shared_ptr<FCondition> LeftCondition;
    xr_shared_ptr<FCondition> RightCondition;
    
    // Проверка условия (вызывается движком)
    bool Evaluate(class FGameState* GameState) const;
};

// Действия при переходе
struct FTransitionActions
{
    xr_vector<xr_string> OnStartInfo;      // Инфопоршни при входе
    xr_vector<xr_string> OnEndInfo;        // Инфопоршни при выходе
    xr_vector<xr_string> OnStartFunction;  // Функции при входе
    xr_vector<xr_string> OnEndFunction;    // Функции при выходе
    
    // Специальные действия
    xr_string SpawnItemOnStart;              // Заспавнить предмет
    xr_string PlaySoundOnStart;              // Проиграть звук
    float DelayBeforeTransition = 0.0f;        // Задержка перед переходом
};

struct FParsedCondition
{
    enum EOperator
    {
        AddInfo,      // +infoportion
        RemoveInfo,   // -infoportion
        FuncTrue,     // =func()
        FuncFalse,    // !func()
        Probability   // ~50
    };

    EOperator Op;
    xr_string FuncName;
    xr_vector<xr_string> Params;
    xr_string InfoName;
    int ProbabilityValue;
    bool bExpectedTrue;
};

struct FParsedEffect
{
    enum EEffectType
    {
        GiveInfo,        // +infoportion
        RemoveInfo,      // -infoportion
        CallFunction,    // =func()
        CustomCommand    // все остальное
    };

    EEffectType Type;
    xr_string InfoName;
    xr_string FuncName;
    xr_vector<xr_string> Params;
    xr_string RawCommand;  // Для нестандартных команд
};

// Переход между состояниями
struct FTransition
{
    xr_string TargetState;
    xr_string RawCondition;
    xr_string RawEffects;
    xr_vector<FParsedCondition> ParsedConditions;
    xr_vector<FParsedEffect> Effects;

    FCondition Condition;                      // Условие перехода
    FTransitionActions Actions;                // Действия при переходе
    int Priority = 0;                          // Приоритет (чем выше, тем важнее)
    bool bOneTime = false;                     // Одноразовый переход?
    
    // Для отладки в редакторе
    xr_string DebugName;
};

// Базовые параметры (общие для всех состояний)
struct FBaseParams
{
    xr_string CustomScript;                  // Кастомный скрипт
    std::unordered_map<xr_string, xr_string> CustomVariables;
};

// Параметры для Walker
struct FWalkerParams : FBaseParams
{
    xr_string PathWalk;                      // Имя пути (way_*.ltx)
    xr_string PathLook;                      // Имя пути взгляда
    EMoveType MoveType = EMoveType::Walk;
    bool bCombatIgnore = false;                // Игнорировать бой
    bool bKeepSafeAlife = true;                // Сохранять в ALife
    float WalkSpeed = 1.0f;                    // Множитель скорости
    xr_string AnimationOverride;             // Переопределение анимации
};

// Параметры для Combat
struct FCombatParams : FBaseParams
{
    ECombatStyle Style = ECombatStyle::Balanced;
    bool bUseCover = true;                     // Использовать укрытия
    float FireRate = 1.0f;                     // Множитель скорострельности
    float GrenadeChance = 0.3f;                // Шанс кинуть гранату (0.0 - 1.0)
    float AccuracyModifier = 1.0f;             // Точность стрельбы
    float AggressionRadius = 50.0f;            // Радиус агрессии в метрах
};

// Параметры для Camper (оборона позиции)
struct FCamperParams : FBaseParams
{
    xr_string PositionName;                  // Имя позиции в .ltx
    float RotationAngle = 0.0f;                // Угол обзора
    float LookRadius = 30.0f;                  // Радиус осмотра
    bool bStayInCover = true;                  // Оставаться в укрытии
};

// Параметры для Trader (торговля)
struct FTraderParams : FBaseParams
{
    xr_string TradeConfig;                   // Путь к файлу торговли
    xr_string TraderSections;                // Секции товаров
    bool bBuyItems = true;                     // Покупать ли предметы
    bool bSellItems = true;                    // Продавать ли предметы
};

// Параметры для Anim (анимации)
struct FAnimParams : FBaseParams
{
    xr_string AnimationName;                 // Название анимации
    bool bUseSingleHand = false;               // Анимация одной рукой?
    xr_string SoundName;                     // Зацикленный звук
    xr_string MentalState;                   // free, danger, panic
    bool bLoopAnimation = false;                // Зациклить анимацию
    float BlendInTime = 0.3f;                  // Время плавного входа
    float BlendOutTime = 0.3f;                 // Время плавного выхода
};

// Параметры для Sound (звуки)
struct FSoundParams : FBaseParams
{
    xr_vector<xr_string> SoundList;        // Список звуков
    float Volume = 1.0f;                       // Громкость (0.0 - 1.0)
    float MinDelay = 5.0f;                     // Минимальная задержка между звуками
    float MaxDelay = 15.0f;                    // Максимальная задержка
    bool bRandomOrder = true;                  // Случайный порядок
    bool bLoopSound = false;                   // Зациклить звук
};

// Параметры для Panic (паника)
struct FPanicParams : FBaseParams
{
    float IgnoreDistance = 50.0f;              // Дистанция игнора опасности
    float RunSpeed = 1.5f;                     // Скорость бега (множитель)
    int PanicTimeoutMs = 10000;                // Сколько мс паниковать
    bool bRunAway = true;                      // Убегать от опасности
};

// Параметры для Reactor (реакция на события)
struct FReactorParams : FBaseParams
{
    xr_vector<xr_string> ReactionEvents;   // События для реакции
    xr_string DefaultState;                  // Состояние по умолчанию
    float ReactionDelay = 0.0f;                // Задержка реакции
};

// Параметры для Guard (охрана)
struct FGuardParams : FBaseParams
{
    xr_string GuardedObjectName;             // Имя охраняемого объекта
    float GuardRadius = 10.0f;                 // Радиус охраны
    bool bAlertOnAttack = true;                // Поднимать тревогу при атаке
    bool bFollowIfStolen = false;              // Преследовать при краже
};

// Параметры для Follow (следование)
struct FFollowParams : FBaseParams
{
    xr_string TargetName = "actor";          // Имя цели (actor или имя NPC)
    float FollowDistance = 2.0f;               // Дистанция следования
    float MaxDistance = 10.0f;                 // Максимальная дистанция
    bool bWaitIfFar = true;                    // Ждать если далеко
    bool bCombatIfAttacked = true;             // Вступать в бой при атаке цели
};

// Параметры для Idle (ожидание)
struct FIdleParams : FBaseParams
{
    float MinIdleTime = 5.0f;                  // Минимальное время ожидания
    float MaxIdleTime = 15.0f;                 // Максимальное время ожидания
    xr_vector<xr_string> IdleAnimations;   // Анимации безделья
};

// Параметры для Trigger (триггер зоны)
struct FTriggerParams : FBaseParams
{
    bool bOneTimeUse = false;                  // Одноразовый триггер
    bool bRepeatable = true;                   // Можно срабатывать多次
    float CooldownSeconds = 0.0f;              // Перезарядка в секундах
};

// Параметры для Action (выполнение действия)
struct FActionParams : FBaseParams
{
    xr_string ActionType;                    // Тип действия: spawn, destroy, teleport и т.д.
    xr_vector<xr_string> ActionTargets;    // Цели действия
    std::unordered_map<xr_string, xr_string> ActionParameters; // Параметры действия
};

struct FEventInfo
{
    xr_string EventKey;
    xr_string EventType;

    FTransition Transition;
    int EventIndex = 0;
};

struct FEventTransition
{
    int EventIndex = 0;
    xr_string EventKey;       // "on_info_myflag", "active", "wounded", etc.
    xr_string EventType;      // "on_timer", "on_info", "active", etc.
    FTransition Transition;
    xr_string SourceState;    // Из какого состояния это событие
};

// Состояние (схема поведения)
struct FState
{
    xr_string StateName;
    EStateType StateType = EStateType::Custom;

    // Параметры в зависимости от типа
    std::variant<
        FWalkerParams,
        FCombatParams,
        FCamperParams,
        FTraderParams,
        FAnimParams,
        FSoundParams,
        FPanicParams,
        FReactorParams,
        FGuardParams,
        FFollowParams,
        FIdleParams,
        FTriggerParams,
        FActionParams,
        FBaseParams
    > Params;

    ed::PinId InputPinId;
    ed::PinId OutputPinId;

    xr_vector<FTransition> Transitions;      // Переходы в другие состояния
    xr_vector<FEventTransition> Events;

    FColor EditorColor;                         // Цвет ноды (нужно определить FColor)
};

// Глобальные настройки логики персонажа
struct FGlobalLogicSettings
{
    xr_string Relation;                       // enemy, neutral, friend
    xr_string TradeConfig;                    // Путь к файлу торговли
    xr_string LevelSpot;                      // Отметка на карте
    bool bRespawnable = false;                  // Может ли респавниться
    int RespawnTimeSeconds = 300;               // Время до респавна
};

// Обработчики событий (hit, death, combat)
struct FEventHandlers
{
    xr_string OnHitState;                     // Состояние при попадании
    xr_string OnDeathState;                   // Состояние при смерти
    xr_string OnCombatState;                  // Состояние при начале боя
    xr_string OnTalkState;                    // Состояние при разговоре
};

// Корневая структура логики NPC или объекта
struct FLogicData
{
    xr_string LogicUid;                       // Уникальный идентификатор
    xr_string InitialState;                   // Активная схема при старте
    FGlobalLogicSettings GlobalSettings;
    xr_vector<FState> States;
    FEventHandlers EventHandlers;
    
    // Метаданные для редактора
    xr_string Description;
    xr_string Author;
    xr_string Version;
    float EditorZoom = 1.0f;                    // Масштаб в редакторе
};