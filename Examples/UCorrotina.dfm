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
  DesignSize = (
    869
    509)
  TextHeight = 15
  object LBL: TLabel
    Left = 412
    Top = 184
    Width = 36
    Height = 37
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 8
    Top = 20
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
    Top = 20
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
  object Button4: TButton
    Left = 368
    Top = 20
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
    OnClick = Button4Click
  end
  object Button1: TButton
    Left = 368
    Top = 434
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
    ExplicitTop = 426
    ExplicitWidth = 115
  end
  object Button2: TButton
    Left = 368
    Top = 78
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
    OnClick = Button2Click
  end
end
