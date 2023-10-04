object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 442
  ClientWidth = 869
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Memo1: TMemo
    Left = 8
    Top = 20
    Width = 351
    Height = 399
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 504
    Top = 20
    Width = 351
    Height = 399
    TabOrder = 1
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
    Top = 364
    Width = 121
    Height = 52
    Caption = 'Clear'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
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
