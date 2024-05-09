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
    Button4: TButton;
    Button5: TButton;
    Memo3: TMemo;
    Label3: TLabel;
    procedure BtnCoRoutineClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnAsyncAwaitClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    FScheduler: IScheduler;
    FValueYeild: TValue;
    FCoroutine_0: String;
    FCoroutine_1: String;
    function Contador(const SendValue, Value: TValue): TFuture;
    function Contador_Regressivo(const SendValue, Value: TValue): TFuture;
    function Contador_Async(const SendValue, Value: TValue): TFuture;
    function Contador_Regressivo_Async(const SendValue, Value: TValue): TFuture;
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
  if not Assigned(FScheduler) then
    Exit;
  FScheduler.Suspend(FCoroutine_0);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  if not Assigned(FScheduler) then
    Exit;
  FScheduler.Send(FCoroutine_0, 0)
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  if not Assigned(FScheduler) then
    Exit;
  FValueYeild := FScheduler.Yield(FCoroutine_1);
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  if not Assigned(FScheduler) then
    Exit;
  FScheduler.Send(FCoroutine_1, FValueYeild);
end;

procedure TForm2.BtnCoRoutineClick(Sender: TObject);
begin
  FCoroutine_0 := 'CONTADOR';
  FCoroutine_1 := 'CONTADOR_REGRESSIVO';

  FScheduler := TScheduler.New;
  FScheduler.Add(FCoroutine_0, Contador, 0, procedure
                              begin
                                LBL.Caption := '<=';
                                Memo1.Lines.Add(FScheduler.Value.ToString);
                              end, 2000)
            .Add(FCoroutine_1, Contador_Regressivo, 15, procedure
                              begin
                                LBL.Caption := '=>';
                                Memo2.Lines.Add(FScheduler.Value.ToString);
                              end)
            .Run(procedure(AError: String)
                 begin
                   Memo3.Lines.Add(AError);
                 end);
end;

function TForm2.Contador(const SendValue, Value: TValue): TFuture;
begin
  try
    if Value.AsInteger < 10 then
    begin
      Result.SetOk(Value.AsInteger + 1);
//    simulação de error
//    raise Exception.Create('Error Message');
    end
    else
      Result.SetOk(TCompletion);
  except
    on E: Exception do
    begin
      Result.SetErr(E.Message);
      raise;
    end;
  end;
end;

function TForm2.Contador_Regressivo(const SendValue, Value: TValue): TFuture;
begin
  try
    if (Value.AsInteger > 1) and (Value.AsInteger <= 15) then
    begin
      Result.SetOk(Value.AsInteger - 1);
//    simulação de error
//    raise Exception.Create('Error Message');
    end
    else
      Result.SetOk(TCompletion);
  except
    on E: Exception do
    begin
      Result.SetErr(E.Message);
      raise;
    end;
  end;
end;

procedure TForm2.BtnAsyncAwaitClick(Sender: TObject);
begin
  FCoroutine_0 := 'CONTADOR_ASYNC';
  FCoroutine_1 := 'CONTADOR_REGRESSIVO_ASYNC';

  FScheduler := TScheduler.New;
  FScheduler.Add(FCoroutine_0, Contador_Async, 0, procedure
                              begin
                                LBL.Caption := '<=';
                                Memo1.Lines.Add(FScheduler.Value.ToString);
                              end)
            .Add(FCoroutine_1, Contador_Regressivo_Async, 15, procedure
                              begin
                                LBL.Caption := '=>';
                                Memo2.Lines.Add(FScheduler.Value.ToString);
                              end, 2000)
            .Run(procedure(AError: String)
                 begin
                   Memo3.Lines.Add(AError);
                 end);
end;

function TForm2.Contador_Async(const SendValue, Value: TValue): TFuture;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     if Value.AsInteger < 10 then
                       Result := Value.AsInteger + 1
                     else
                       Result := TValue.Empty;
//                     simulação de error
//                     raise Exception.Create('Error Message');
                   end).Await;
  Result := LFuture;
end;

function TForm2.Contador_Regressivo_Async(const SendValue, Value: TValue): TFuture;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     Result := Value;
                     if (Value.AsInteger > 1) and (Value.AsInteger <= 15) then
                       Result := Value.AsInteger - 1
                     else
                       Result := TValue.Empty;
//                     simulação de error
//                     raise Exception.Create('Error Message');
                   end).Await;
  Result := LFuture;
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FScheduler) then
    FScheduler.Stop;
end;

end.

