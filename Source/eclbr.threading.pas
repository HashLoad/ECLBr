unit eclbr.threading;

interface

uses
  Rtti,
  SysUtils,
  Classes,
  Threading;

type
  TFuture = packed record
  private
    FValue: TValue;
    FErr: string;
    FIsOK: boolean;
    FIsErr: boolean;
    procedure SetOk(const AValue: TValue);
    procedure SetErr(const AErr: string);
  public
    function IsOk: Boolean;
    function IsErr: Boolean;
    function Ok<T>: T;
    function Err: string;
  end;

  PAsync = ^TAsync;
  TAsync = record
  private
    FTask: ITask;
    FProc: TProc;
    FFunc: TFunc<TValue>;
  private
    function _AwaitProc(const AContinue: TProc; const ATimeout: Cardinal): TFuture; overload;
    function _AwaitFunc(const AContinue: TProc; const ATimeout: Cardinal): TFuture; overload;
    function _AwaitProc(const ATimeout: Cardinal): TFuture; overload;
    function _AwaitFunc(const ATimeout: Cardinal): TFuture; overload;
    function _ExecProc: TFuture;
    constructor Create(const AProc: TProc); overload;
    constructor Create(const AFunc: TFunc<TValue>); overload;
  public
    function Await(const AContinue: TProc; const ATimeout: Cardinal = INFINITE): TFuture; overload;
    function Await(const ATimeout: Cardinal = INFINITE): TFuture; overload;
    function Run: TFuture;
    function Status: TTaskStatus;
    function GetId: Integer;
    procedure Cancel;
    procedure CheckCanceled;
  end;

function Async(const AProc: TProc): TAsync; overload;
function Async(const AFunc: TFunc<TValue>): TAsync; overload;

implementation

function Async(const AProc: TProc): TAsync;
var
  LAsync: TAsync;
begin
  LAsync := TAsync.Create(AProc);
  Result := LAsync;
end;

function Async(const AFunc: TFunc<TValue>): TAsync;
begin
  Result := TAsync.Create(AFunc);
end;

function TAsync.Await(const AContinue: TProc; const ATimeout: Cardinal): TFuture;
begin
  if Assigned(FProc) then
    Result := _AwaitProc(AContinue, ATimeout)
  else
  if Assigned(FFunc) then
    Result := _AwaitFunc(AContinue, ATimeout)
end;

constructor TAsync.Create(const AProc: TProc);
begin
  FProc := AProc;
  FFunc := nil;
end;

function TAsync.Await(const ATimeout: Cardinal): TFuture;
begin
  if Assigned(FProc) then
    Result := _AwaitProc(ATimeout)
  else
  if Assigned(FFunc) then
    Result := _AwaitFunc(ATimeout)
end;

procedure TAsync.Cancel;
begin
  FTask.Cancel;
end;

procedure TAsync.CheckCanceled;
begin
  FTask.CheckCanceled;
end;

constructor TAsync.Create(const AFunc: TFunc<TValue>);
begin
  FProc := nil;
  FFunc := AFunc;
end;

function TAsync.Run: TFuture;
begin
  if Assigned(FProc) then
    Result := _ExecProc
  else
  if Assigned(FFunc) then
    Result.SetErr('O método "Exec" não deve ser chamado como função. Use o método "Await" para aguardar a conclusão da tarefa e acessar o resultado, ou chame-o como procedure.');
end;

function TAsync.GetId: Integer;
begin
  Result := FTask.GetId;
end;

function TAsync.Status: TTaskStatus;
begin
  Result := FTask.Status;
end;

function TAsync._AwaitProc(const AContinue: TProc;
  const ATimeout: Cardinal): TFuture;
var
  LSelf: PAsync;
begin
  LSelf := @Self;
  try
    FTask := TTask.Run(procedure
                       begin
                         LSelf^.FProc();
                       end);
    FTask.Wait(ATimeout);

    if not Assigned(AContinue) then
      exit;
    TThread.Synchronize(TThread.CurrentThread,
                        procedure
                        begin
                          AContinue();
                        end);

    Result.SetOk(true);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

function TAsync._ExecProc: TFuture;
var
  LProc: TProc;
begin
  LProc := FProc;
  try
    FTask := TTask.Run(procedure
                       begin
                         LProc();
                       end);
    Result.SetOk(true);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

function TAsync._AwaitFunc(const AContinue: TProc;
  const ATimeout: Cardinal): TFuture;
var
  LValue: TValue;
  LSelf: PAsync;
begin
  LSelf := @Self;
  try
    FTask := TTask.Run(procedure
                       begin
                         LValue := LSelf^.FFunc();
                       end);
    FTask.Wait(ATimeout);

    if not Assigned(AContinue) then
      exit;
    TThread.Synchronize(TThread.CurrentThread,
                        procedure
                        begin
                          AContinue();
                        end);

    Result.SetOk(LValue);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

{ TFuture }

function TFuture.Err: string;
begin
  Result := FErr;
end;

function TFuture.IsErr: Boolean;
begin
  Result := FIsErr;
end;

function TFuture.IsOk: Boolean;
begin
  Result := FIsOK;
end;

function TFuture.Ok<T>: T;
begin
  Result := FValue.AsType<T>;
end;

procedure TFuture.SetErr(const AErr: string);
begin
  FErr := AErr;
  FIsErr := true;
  FIsOK := false;
end;

procedure TFuture.SetOk(const AValue: TValue);
begin
  FValue := AValue;
  FIsOK := true;
  FIsErr := false;
end;

function TAsync._AwaitFunc(const ATimeout: Cardinal): TFuture;
var
  LValue: TValue;
  LSelf: PAsync;
begin
  LSelf := @Self;
  try
    FTask := TTask.Run(procedure
                       begin
                         LValue := LSelf^.FFunc();
                       end);
    FTask.Wait(ATimeout);

    Result.SetOk(LValue);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

function TAsync._AwaitProc(const ATimeout: Cardinal): TFuture;
var
  LSelf: PAsync;
begin
  LSelf := @Self;
  try
    FTask := TTask.Run(procedure
                       begin
                         LSelf^.FProc();
                       end);
    FTask.Wait(ATimeout);

    Result.SetOk(true);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

end.
