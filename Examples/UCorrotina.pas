unit UCorrotina;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Rtti,
  eclbr.coroutine,
  eclbr.threading;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button4: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FScheduler: IScheduler;
    function Contador(Value: TValue): TValue;
    function Contador_Regressivo(Value: TValue): TValue;
    function Contador_Async(Value: TValue): TValue;
    function Contador_Regressivo_Async(Value: TValue): TValue;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm2.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
  Memo2.Clear;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  FScheduler := TScheduler.New
                          .Add(Contador_Async, 1, procedure
                              begin
                                Memo1.Lines.Add(FScheduler.Value.ToString);
                              end)
                          .Add(Contador_Regressivo_Async, 15, procedure
                              begin
                                Memo2.Lines.Add(FScheduler.Value.ToString);
                              end)
                          .Run;

end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  FScheduler := TScheduler.New
                          .Add(Contador, 1, procedure
                              begin
                                Memo1.Lines.Add(FScheduler.Value.ToString);
                              end)
                          .Add(Contador_Regressivo, 15, procedure
                              begin
                                Memo2.Lines.Add(FScheduler.Value.ToString);
                              end)
                          .Run;
end;

function TForm2.Contador(Value: TValue): TValue;
var
  LValueSend: TValue;
begin
  LValueSend := FScheduler.Yield;
  if Value.AsInteger < 10 then
    Result := Value.AsInteger + 1
  else
    Result := TValue.Empty;
  Sleep(100)
end;

function TForm2.Contador_Async(Value: TValue): TValue;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     if Value.AsInteger < 10 then
                       Result := Value.AsInteger + 1
                     else
                       Result := TValue.Empty;
                     FScheduler.Yield;
                     Sleep(100)
                   end).Await;
  if LFuture.IsOk then
    Result := LFuture.Ok<TValue>
  else
    Result := LFuture.Err;
end;

function TForm2.Contador_Regressivo(Value: TValue): TValue;
var
  LValueSend: TValue;
begin
  LValueSend := FScheduler.Yield;
  if (Value.AsInteger > 0) and (Value.AsInteger <= 15) then
    Result := Value.AsInteger - 1
  else
    Result := TValue.Empty;
  Sleep(100)
end;

function TForm2.Contador_Regressivo_Async(Value: TValue): TValue;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     if (Value.AsInteger > 0) and (Value.AsInteger <= 15) then
                       Result := Value.AsInteger - 1
                     else
                       Result := TValue.Empty;
                     FScheduler.Yield;
                     Sleep(100)
                   end).Await;
  if LFuture.IsOk then
    Result := LFuture.Ok<TValue>
  else
    Result := LFuture.Err;
end;

end.

