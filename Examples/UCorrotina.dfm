object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 499
  ClientWidth = 863
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    863
    499)
  TextHeight = 15
  object LBL: TLabel
    Left = 365
    Top = 188
    Width = 133
    Height = 99
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -80
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
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
    Height = 456
    Alignment = taCenter
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitHeight = 448
  end
  object Memo2: TMemo
    Left = 504
    Top = 34
    Width = 351
    Height = 456
    Alignment = taCenter
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitHeight = 448
  end
  object BtnCoRoutine: TButton
    Left = 365
    Top = 34
    Width = 133
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
    Left = 365
    Top = 450
    Width = 133
    Height = 42
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
    ExplicitTop = 442
    ExplicitWidth = 131
  end
  object BtnAsyncAwait: TButton
    Left = 365
    Top = 293
    Width = 133
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
    Left = 365
    Top = 92
    Width = 133
    Height = 42
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
    ExplicitTop = 90
    ExplicitWidth = 131
  end
  object Button3: TButton
    Left = 365
    Top = 141
    Width = 133
    Height = 42
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
    ExplicitTop = 138
    ExplicitWidth = 131
  end
  object Button4: TButton
    Left = 365
    Top = 351
    Width = 133
    Height = 42
    Anchors = [akLeft, akRight]
    Caption = 'Paused (Yield)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    OnClick = Button4Click
    ExplicitTop = 345
    ExplicitWidth = 131
  end
  object Button5: TButton
    Left = 365
    Top = 400
    Width = 133
    Height = 42
    Anchors = [akLeft, akRight]
    Caption = 'Send (Value)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    OnClick = Button5Click
    ExplicitTop = 393
    ExplicitWidth = 131
  end
end
