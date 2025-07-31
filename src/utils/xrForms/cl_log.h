#pragma	once

extern class i_lc_log 
{
public:
	virtual void clMsg(LPCSTR msg) = 0;
	virtual void clLog(LPCSTR msg) = 0;
	virtual void Status(LPCSTR msg) = 0;
	virtual	void Progress(const float F) = 0;
	virtual	void Phase(LPCSTR phase_name) = 0;
} *lc_log;

enum IterationStatus
{
	Skip = 0,
	InProgress,
	Pending,
	Complited,
};

struct IterationPhase 
{
	xr_string PhaseName = "";
	u32 elapsed_time = 0;
	u32 remain_time = 0;
	IterationStatus status = InProgress;
	float PhasePersent = 0;
	size_t used_memory = 0;
	xr_string AdditionalData;
};

struct IterationData 
{
	xr_string iterationName;
	
	u32 elapsed_time = 0; // Общее время работы итерации

	int warnings = 0;
	IterationStatus status = Pending;
	xr_vector<IterationPhase> phases;
	float Persent = 0;
};

static xrCriticalSection	csLog
#ifdef PROFILE_CRITICAL_SECTIONS
(MUTEX_PROFILE_ID(csLog))
#endif // PROFILE_CRITICAL_SECTIONS
;

void clMsg(const char* format, ...);
void Status(const char* format, ...);
void StatusNoMsg(const char* format, ...);
void Progress(const float F);
void Phase(const char* phase_name);

void AditionalData(const char* text, ...);


void logThread	    (void *dummy);
void logCallback	(LPCSTR c);

float GetProgress();
xr_vector<IterationData>& GetIterationData();
IterationData* GetActiveIteration();
void SetActiveIteration(IterationData* i);
xr_vector<xr_string>& GetLogVector();
u32& GetPhaseStartTime();
xr_string make_time(u32 sec);
