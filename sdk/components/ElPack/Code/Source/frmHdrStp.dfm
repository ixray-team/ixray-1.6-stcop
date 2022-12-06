object frmHeaderSetup: TfrmHeaderSetup
  Left = 367
  Top = 290
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Customize Columns'
  ClientHeight = 204
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSections: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 204
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblAvailable: TElLabel
      Left = 8
      Top = 9
      Width = 89
      Height = 13
      Caption = 'A&vailable Columns:'
      FocusControl = lbxAvailable
    end
    object lblVisible: TElLabel
      Left = 250
      Top = 9
      Width = 80
      Height = 13
      Caption = '&Current Columns:'
      FocusControl = lbxVisible
    end
    object lbxAvailable: TElListBox
      Left = 6
      Top = 24
      Width = 151
      Height = 172
      AllowGrayed = False
      ItemHeight = 13
      ItemIndex = -1
      TopIndex = 0
      ActiveBorderType = fbtFlat
      BorderSides = [ebsLeft, ebsRight, ebsTop, ebsBottom]
      Flat = True
      HorizontalScroll = False
      LineBorderActiveColor = clBlack
      LineBorderInactiveColor = clBlack
      SelectedFont.Charset = DEFAULT_CHARSET
      SelectedFont.Color = clHighlightText
      SelectedFont.Height = -11
      SelectedFont.Name = 'MS Sans Serif'
      SelectedFont.Style = []
      OnClick = FormShow
      OnDblClick = btnAddClick
      OnDragDrop = lbxAvailableDragDrop
      OnDragOver = lbxAvailableDragOver
      OnEnter = lbxAvailableEnter
      OnExit = FormShow
      OnMouseDown = lbxAvailableMouseDown
      TabOrder = 0
    end
    object lbxVisible: TElListBox
      Left = 250
      Top = 24
      Width = 151
      Height = 172
      AllowGrayed = False
      ItemHeight = 13
      ItemIndex = -1
      TopIndex = 0
      ActiveBorderType = fbtFlat
      BorderSides = [ebsLeft, ebsRight, ebsTop, ebsBottom]
      Flat = True
      HorizontalScroll = False
      LineBorderActiveColor = clBlack
      LineBorderInactiveColor = clBlack
      SelectedFont.Charset = DEFAULT_CHARSET
      SelectedFont.Color = clHighlightText
      SelectedFont.Height = -11
      SelectedFont.Name = 'MS Sans Serif'
      SelectedFont.Style = []
      OnClick = FormShow
      OnDblClick = btnDeleteClick
      OnDragDrop = lbxVisibleDragDrop
      OnDragOver = lbxVisibleDragOver
      OnEnter = lbxVisibleEnter
      OnExit = FormShow
      OnMouseDown = lbxVisibleMouseDown
      TabOrder = 1
    end
    object btnAdd: TElPopupButton
      Left = 166
      Top = 82
      Width = 75
      Height = 23
      ImageIndex = 0
      DrawDefaultFrame = False
      NumGlyphs = 1
      ShowFocus = False
      Caption = '&Add ->'
      TabStop = False
      TabOrder = 2
      Color = clBtnFace
      ParentColor = False
      OnClick = btnAddClick
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object btnDelete: TElPopupButton
      Left = 166
      Top = 111
      Width = 75
      Height = 23
      ImageIndex = 0
      DrawDefaultFrame = False
      NumGlyphs = 1
      ShowFocus = False
      Caption = '<- &Remove'
      Enabled = False
      TabStop = False
      TabOrder = 3
      Color = clBtnFace
      ParentColor = False
      OnClick = btnDeleteClick
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object btnUp: TElPopupButton
      Left = 411
      Top = 114
      Width = 75
      Height = 23
      ImageIndex = 0
      DrawDefaultFrame = False
      NumGlyphs = 1
      ShowFocus = False
      Caption = 'Move &Up'
      Enabled = False
      TabStop = False
      TabOrder = 4
      Color = clBtnFace
      ParentColor = False
      OnClick = btnUpClick
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object btnDown: TElPopupButton
      Left = 411
      Top = 143
      Width = 75
      Height = 23
      ImageIndex = 0
      DrawDefaultFrame = False
      NumGlyphs = 1
      ShowFocus = False
      Caption = 'Move &Down'
      Enabled = False
      TabStop = False
      TabOrder = 5
      Color = clBtnFace
      ParentColor = False
      OnClick = btnDownClick
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object btnOk: TElPopupButton
      Left = 411
      Top = 9
      Width = 75
      Height = 23
      ImageIndex = 0
      DrawDefaultFrame = False
      Default = True
      ModalResult = 1
      NumGlyphs = 1
      ShowFocus = False
      Caption = 'OK'
      TabOrder = 6
      Color = clBtnFace
      ParentColor = False
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
    object btnCancel: TElPopupButton
      Left = 411
      Top = 38
      Width = 75
      Height = 23
      ImageIndex = 0
      DrawDefaultFrame = False
      Cancel = True
      ModalResult = 2
      NumGlyphs = 1
      ShowFocus = False
      Caption = 'Cancel'
      TabOrder = 7
      Color = clBtnFace
      ParentColor = False
      DockOrientation = doNoOrient
      DoubleBuffered = False
    end
  end
end