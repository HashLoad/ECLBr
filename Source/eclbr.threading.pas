{
               ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(ECLBr Library)
  @created(23 Abr 2023)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @Discord(https://discord.gg/S5yvvGu7)
}

{$T+}

unit eclbr.threading;

interface

uses
  Rtti,
  SysUtils,
  Classes,
  Threading;

type
  TValue = Rtti.TValue;

  TFuture = record
  private
    FValue: TValue;
    FErr: string;
    FIsOK: Boolean;
    FIsErr: Boolean;
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
    function Await(const AContinue: TProc; const ATimeout: Cardinal = INFINITE): TFuture; overload; inline;
    function Await(const ATimeout: Cardinal = INFINITE): TFuture; overload; inline;
    function Run: TFuture; inline;
    function Status: TTaskStatus; inline;
    function GetId: Integer; inline;
    procedure Cancel; inline;
    procedure CheckCanceled; inline;
  end;

function Async(const AProc: TProc): TAsync; overload; inline;
function Async(const AFunc: TFunc<TValue>): TAsync; overload; inline;

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
    Result.SetErr('The "Exec" method should not be invoked as a function. Utilize the "Await" method to wait for task completion and access the result, or invoke it as a procedure.');
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
