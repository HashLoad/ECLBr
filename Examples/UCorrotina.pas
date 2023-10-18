unit UCorrotina;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Rtti,
  eclbr.std,
  eclbr.coroutine,
  eclbr.threading;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    BtnCoRoutine: TButton;
    Button1: TButton;
    BtnAsyncAwait: TButton;
    LBL: TLabel;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure BtnCoRoutineClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnAsyncAwaitClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
  FScheduler.Suspend;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  FScheduler.Send(0);
end;

procedure TForm2.BtnCoRoutineClick(Sender: TObject);
begin
  FScheduler := TScheduler.New;
  FScheduler.Add(Contador, 0, procedure
                              begin
                                LBL.Caption := '<=';
                                Memo1.Lines.Add(FScheduler.Value.ToString);
                              end)
            .Add(Contador_Regressivo, 15, procedure
                              begin
                                LBL.Caption := '=>';
                                Memo2.Lines.Add(FScheduler.Value.ToString);
                              end)
            .Run(procedure(E: Exception)
                 begin
                   raise E;
                 end);
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
  Sleep(500);
// Simulação de error
//  raise Exception.Create('Error Message');
end;

function TForm2.Contador_Regressivo(Value: TValue): TValue;
var
  LValueSend: TValue;
begin
  LValueSend := FScheduler.Yield;
  if (Value.AsInteger > 1) and (Value.AsInteger <= 15) then
    Result := Value.AsInteger - 1
  else
    Result := TValue.Empty;
  Sleep(500);
end;

procedure TForm2.BtnAsyncAwaitClick(Sender: TObject);
begin
  FScheduler := TScheduler.New;
  FScheduler.Add(Contador_Async, 0, procedure
                              begin
                                LBL.Caption := '<=';
                                Memo1.Lines.Add(FScheduler.Value.ToString);
                              end)
            .Add(Contador_Regressivo_Async, 15, procedure
                              begin
                                LBL.Caption := '=>';
                                Memo2.Lines.Add(FScheduler.Value.ToString);
                              end)
            .Run(procedure(E: Exception)
                 begin
                   raise E;
                 end);
end;

function TForm2.Contador_Async(Value: TValue): TValue;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     FScheduler.Yield;
                     if Value.AsInteger < 10 then
                       Result := Value.AsInteger + 1
                     else
                       Result := TValue.Empty;
                     Sleep(500);
                   end).Await;
  if LFuture.IsOk then
    Result := LFuture.Ok<TValue>
  else
    Result := LFuture.Err;
// Simulação de error
//  raise Exception.Create('Error Message');
end;

function TForm2.Contador_Regressivo_Async(Value: TValue): TValue;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     FScheduler.Yield;
                     Result := Value;
                     if (Value.AsInteger > 1) and (Value.AsInteger <= 15) then
                       Result := Value.AsInteger - 1
                     else
                       Result := TValue.Empty;
                     Sleep(500);
                   end).Await;
  if LFuture.IsOk then
    Result := LFuture.Ok<TValue>
  else
    Result := LFuture.Err;
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FScheduler.Stop;
end;

end.

