#ifndef UI_ShaderMainH
#define UI_ShaderMainH


enum {
	COMMAND_EXTFIRST_EXT = COMMAND_MAIN_LAST-1,
	COMMAND_UPDATE_LIST,
};
//------------------------------------------------------------------------------
class CShaderMain: public TUI{
	typedef TUI inherited;
    
    virtual void 	RealUpdateScene			();
    virtual void 	RealQuit				();
public:
    				CShaderMain 			();
    virtual 		~CShaderMain			();

    virtual LPSTR	GetCaption				();

    virtual void 	ResetStatus				();
    virtual void 	SetStatus				(const char* s, bool bOutLog);
    virtual void	ProgressDraw			();

    virtual const char*	EditorName				(){return "shader";}
    virtual const char*	EditorDesc				(){return "Shader Editor";}

    virtual bool 	ApplyShortCut			(DWORD Key, TShiftState Shift);
    virtual bool 	ApplyGlobalShortCut		(DWORD Key, TShiftState Shift);

    // commands
	virtual	void		RegisterCommands	(); 
protected:
    virtual void    OnDrawUI();
};    

#endif //UI_MainCommandH



