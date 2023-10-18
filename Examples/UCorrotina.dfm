object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 509
  ClientWidth = 869
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    869
    509)
  TextHeight = 15
  object LBL: TLabel
    Left = 368
    Top = 150
    Width = 121
    Height = 106
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -80
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 2
    Width = 116
    Height = 31
    Caption = 'Contador()'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -23
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 504
    Top = 2
    Width = 240
    Height = 31
    Caption = 'Contador_Regressivo()'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -23
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 8
    Top = 34
    Width = 351
    Height = 466
    Alignment = taCenter
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitHeight = 457
  end
  object Memo2: TMemo
    Left = 504
    Top = 34
    Width = 351
    Height = 466
    Alignment = taCenter
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitHeight = 457
  end
  object BtnCoRoutine: TButton
    Left = 368
    Top = 34
    Width = 121
    Height = 52
    Caption = 'Coroutine'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = BtnCoRoutineClick
  end
  object Button1: TButton
    Left = 368
    Top = 443
    Width = 121
    Height = 52
    Anchors = [akLeft, akRight]
    Caption = 'Clear'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
    ExplicitTop = 435
    ExplicitWidth = 115
  end
  object BtnAsyncAwait: TButton
    Left = 368
    Top = 92
    Width = 121
    Height = 52
    Caption = 'Coroutine Async/Await'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    WordWrap = True
    OnClick = BtnAsyncAwaitClick
  end
  object Button2: TButton
    Left = 372
    Top = 288
    Width = 121
    Height = 52
    Anchors = [akLeft, akRight]
    Caption = 'Paused'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = Button2Click
    ExplicitTop = 282
    ExplicitWidth = 115
  end
  object Button3: TButton
    Left = 372
    Top = 356
    Width = 121
    Height = 52
    Anchors = [akLeft, akRight]
    Caption = 'Send'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = Button3Click
    ExplicitTop = 349
    ExplicitWidth = 115
  end
end
