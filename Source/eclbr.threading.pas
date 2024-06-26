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
  @Discord(https://discord.gg/T2zJC8zX)
}

{$T+}

unit eclbr.threading;

interface

uses
  Rtti,
  SysUtils,
  Classes,
  Threading,
  eclbr.std;

type
  TValue = Rtti.TValue;
  TFuture = eclbr.std.TFuture;

  EAsyncAwait = Exception;

  PAsync = ^TAsync;
  TAsync = record
  strict private
    FTask: ITask;
    FProc: TProc;
    FFunc: TFunc<TValue>;
    FError: TFunc<Exception, TFuture>;
  strict private
    function _AwaitProc(const AContinue: TProc; const ATimeout: Cardinal): TFuture; overload;
    function _AwaitFunc(const AContinue: TProc; const ATimeout: Cardinal): TFuture; overload;
    function _AwaitProc(const ATimeout: Cardinal): TFuture; overload;
    function _AwaitFunc(const ATimeout: Cardinal): TFuture; overload;
    function _ExecProc: TFuture;
  private
    constructor Create(const AProc: TProc); overload;
    constructor Create(const AFunc: TFunc<TValue>); overload;
  public
    function Await(const AContinue: TProc; const ATimeout: Cardinal = INFINITE): TFuture; overload; inline;
    function Await(const ATimeout: Cardinal = INFINITE): TFuture; overload; inline;
    function Run: TFuture; overload;
    function Run(const AError: TFunc<Exception, TFuture>): TFuture; overload; inline;
    function NoAwait: TFuture; overload;
    function NoAwait(const AError: TFunc<Exception, TFuture>): TFuture; overload; inline;
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
    Result.SetErr('The "Run" method should not be invoked as a function. Utilize the "Await" method to wait for task completion and access the result, or invoke it as a procedure.');
end;

function TAsync.GetId: Integer;
begin
  Result := FTask.GetId;
end;

function TAsync.NoAwait(const AError: TFunc<Exception, TFuture>): TFuture;
begin
  Result :=  Run(AError);
end;

function TAsync.NoAwait: TFuture;
begin
  Result := Run;
end;

function TAsync.Run(const AError: TFunc<Exception, TFuture>): TFuture;
begin
  FError := AError;
  Result := Self.Run;
end;

function TAsync.Status: TTaskStatus;
begin
  Result := FTask.Status;
end;

function TAsync._AwaitProc(const AContinue: TProc;
  const ATimeout: Cardinal): TFuture;
var
  LSelf: PAsync;
  LMessage: String;
begin
  LSelf := @Self;
  try
    FTask := TTask.Run(procedure
                       begin
                         try
                           LSelf^.FProc();
                         except
                           on E: Exception do
                             LMessage := E.Message;
                         end;
                       end);
    FTask.Wait(ATimeout);
    if LMessage <> '' then
      raise EAsyncAwait.Create(LMessage);

    if not Assigned(AContinue) then
      Exit;
    TThread.Queue(TThread.CurrentThread,
                  procedure
                  begin
                    try
                      AContinue();
                    except
                      on E: Exception do
                        LMessage := E.Message;
                    end;
                  end);
    if LMessage <> '' then
      raise EAsyncAwait.Create(LMessage);

    Result.SetOk(True);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

function TAsync._ExecProc: TFuture;
var
  LProc: TProc;
  LError: TFunc<Exception, TFuture>;
begin
  LProc := FProc;
  LError := FError;
  try
    FTask := TTask.Run(procedure
                       var
                         LMessage: String;
                       begin
                         try
                           LProc();
                         except
                           on E: Exception do
                           begin
                             LMessage := E.Message;
                             if Assigned(LError) then
                             begin
                               TThread.Queue(TThread.CurrentThread,
                                 procedure
                                 begin
                                   LError(Exception.Create(LMessage));
                                 end);
                             end;
                           end;
                         end;
                       end);
    Result.SetOk(True);
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
  LMessage: String;
begin
  LSelf := @Self;
  try
    FTask := TTask.Run(procedure
                       begin
                         try
                           LValue := LSelf^.FFunc();
                         except
                           on E: Exception do
                             LMessage := E.Message;
                         end;
                       end);
    FTask.Wait(ATimeout);
    if (LMessage <> '') then
      raise EAsyncAwait.Create(LMessage);

    if not Assigned(AContinue) then
      Exit;
    TThread.Queue(TThread.CurrentThread,
                  procedure
                  begin
                    try
                      AContinue();
                    except
                      on E: Exception do
                        LMessage := E.Message;
                    end;
                  end);
    if (LMessage <> '') then
      raise EAsyncAwait.Create(LMessage);

    Result.SetOk(LValue);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

function TAsync._AwaitFunc(const ATimeout: Cardinal): TFuture;
var
  LValue: TValue;
  LSelf: PAsync;
  LMessage: String;
begin
  LSelf := @Self;
  try
    FTask := TTask.Run(procedure
                       begin
                         try
                           LValue := LSelf^.FFunc();
                         except
                           on E: Exception do
                             LMessage := E.Message;
                         end;
                       end);
    FTask.Wait(ATimeout);
    if (LMessage <> '') then
      raise EAsyncAwait.Create(LMessage);

    Result.SetOk(LValue);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

function TAsync._AwaitProc(const ATimeout: Cardinal): TFuture;
var
  LSelf: PAsync;
  LMessage: String;
begin
  LSelf := @Self;
  try
    FTask := TTask.Run(procedure
                       begin
                         try
                           LSelf^.FProc();
                         except
                           on E: Exception do
                             LMessage := E.Message;
                         end;
                       end);
    FTask.Wait(ATimeout);
    if (LMessage <> '') then
      raise EAsyncAwait.Create(LMessage);

    Result.SetOk(True);
  except
    on E: Exception do
      Result.SetErr(E.Message);
  end;
end;

end.
