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

unit eclbr.coroutine;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  SyncObjs,
  Threading,
  Generics.Collections,
  eclbr.std;

type
  TFuture = eclbr.std.TFuture;
  IScheduler = interface;
  TFuncCoroutine = reference to function(const ASendValue: TValue; const AValue: TValue): TValue;

  {$SCOPEDENUMS ON}
  TCoroutineState = (csActive, csPaused, csFinished);
  {$SCOPEDENUMS OFF}

  TException = record
    IsException: Boolean;
    Message: String;
  end;

  TSend = record
    IsSend: Boolean;
    Name: String;
    Value: TValue;
  end;

  TPause = record
    IsPaused: Boolean;
    Name: String;
    Value: TValue;
  end;

  TParamNotify = record
  strict private
    FName: String;
    FValue: TValue;
    FSendValue: TValue;
  public
    constructor Create(const AName: String; const AValue: TValue;
      const ASendValue: TValue);
  end;

  TCoroutine = class sealed strict
  private
    FName: String;
    FState: TCoroutineState;
    FFunc: TFuncCoroutine;
    FProc: TProc;
    FValue: TValue;
    FSendValue: TValue;
    FSendCount: UInt32;
    FObserverList: TList<TCoroutine>;
    FParamNotify: TParamNotify;
    FLock: TCriticalSection;
  public
    {$MESSAGE WARN 'This class should not be used directly.'}
    constructor Create; overload;
    constructor Create(const AName: String; const AFunc: TFuncCoroutine;
      const AValue: TValue; const ACountSend: UInt32; const AProc: TProc = nil); overload;
    destructor Destroy; override;
    procedure Attach(const AObserver: TCoroutine);
    procedure Detach(const AObserver: TCoroutine);
    procedure ObserverNotify;
    procedure Notify(const AParams: TParamNotify);
    function Assign: TCoroutine;
    property Name: String read FName write FName;
    property Func: TFuncCoroutine read FFunc;
    property Proc: TProc read FProc;
    property Value: TValue read FValue write FValue;
    property State: TCoroutineState read FState write FState;
    property SendValue: TValue read FSendValue write FSendValue;
    property SendCount: UInt32 read FSendCount write FSendCount;
  end;

  IScheduler = interface
    ['{BC104A19-9657-4093-A494-8D3CFD4CAF09}']
    function _GetCoroutine(AValue: String): TCoroutine;
    procedure Send(const AName: String); overload;
    procedure Send(const AName: String; const AValue: TValue); overload;
    procedure Suspend(const AName: String);
    procedure Stop(const ATimeout: Cardinal = 1000);
    procedure Next;
    function Add(const AName: String; const ARoutine: TFuncCoroutine; const AValue: TValue;
      const AProc: TProc = nil): IScheduler; overload;
    function Value: TValue;
    function Yield(const AName: String): TValue;
    function Count: UInt32;
    function SendValue: TValue;
    function SendCount: UInt32;
    function Run(const AError: TProc<String>): IScheduler; overload;
    property Coroutine[Name: String]: TCoroutine read _GetCoroutine;
  end;

  TScheduler = class(TInterfacedObject, IScheduler)
  strict private
    type
      TGather<T> = class sealed(TList<T>)
      protected
        procedure Enqueue(const AValue: T);
        function Dequeue: T;
        function Peek: T;
      end;
    const C_COROUTINE_NOT_FOUND = 'No coroutine found with the specified name.';
  strict private
    FSleepTime: UInt16;
    FCurrentRoutine: TCoroutine;
    FCoroutines: TGather<TCoroutine>;
    FTask: ITask;
    FErrorCallback: TProc<String>;
    FStoped: Boolean;
    FSend: TSend;
    FPause: TPause;
    FException: TException;
    FLock: TCriticalSection;
    function _GetCoroutine(AValue: String): TCoroutine;
  protected
    function Run: IScheduler; overload;
    constructor Create(const ASleepTime: UInt16); overload;
  public
    class function New(const ASleepTime: UInt16 = 500): IScheduler;
    destructor Destroy; override;
    procedure Send(const AName: String); overload;
    procedure Send(const AName: String; const AValue: TValue); overload;
    procedure Suspend(const AName: String);
    procedure Stop(const ATimeout: Cardinal = 1000);
    procedure Next;
    function Add(const AName: String; const ARoutine: TFuncCoroutine; const AValue: TValue;
      const AProc: TProc = nil): IScheduler; overload;
    function Value: TValue;
    function Yield(const AName: String): TValue;
    function Count: UInt32;
    function SendCount: UInt32;
    function SendValue: TValue;
    function Run(const AError: TProc<String>): IScheduler; overload;
    property Coroutine[Name: String]: TCoroutine read _GetCoroutine;
  end;

implementation

{ TScheduler }

function TScheduler.Count: UInt32;
begin
  Result := FCoroutines.Count;
end;

procedure TScheduler.Send(const AName: String);
begin
  FSend.IsSend := True;
  FSend.Name := AName;
  FSend.Value := Default(TValue);
end;

function TScheduler.SendCount: UInt32;
begin
  Result := 0;
  if FCoroutines.Count = 0 then
    exit;
  Result := FCoroutines.Peek.SendCount;
end;

function TScheduler.SendValue: TValue;
begin
  Result := FCurrentRoutine.SendValue;
end;

constructor TScheduler.Create(const ASleepTime: UInt16);
begin
  FStoped := False;
  FSleepTime := ASleepTime;
  FException := Default(TException);
  FCoroutines := TGather<TCoroutine>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TScheduler.Destroy;
var
  LItem: TCoroutine;
begin
  for LItem in FCoroutines do
    LItem.Free;
  FCoroutines.Free;
  FLock.Free;
  inherited;
end;

function TScheduler._GetCoroutine(AValue: String): TCoroutine;
var
  LItem: TCoroutine;
begin
  Result := Default(TCoroutine);
  for LItem in FCoroutines do
  begin
    if LItem.Name = AValue then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

function TScheduler.Yield(const AName: String): TValue;
begin
  if FCoroutines.Count = 0 then
    raise Exception.Create(C_COROUTINE_NOT_FOUND);

  Suspend(AName);
  while FPause.IsPaused do
  Result := FPause.Value;
  FPause.Value := Default(TValue);
end;

procedure TScheduler.Send(const AName: String; const AValue: TValue);
begin
  FCurrentRoutine.SendValue := TValue.Empty;
  FSend.IsSend := True;
  FSend.Name := AName;
  FSend.Value := AVAlue;
end;

procedure TScheduler.Stop(const ATimeout: Cardinal);
begin
  FStoped := True;
  Sleep(ATimeout);
end;

procedure TScheduler.Suspend(const AName: String);
begin
  FPause.IsPaused := True;
  FPause.Name := AName;
end;

function TScheduler.Value: TValue;
begin
  Result := FCurrentRoutine.Value;
end;

function TScheduler.Add(const AName: String; const ARoutine: TFuncCoroutine;
  const AValue: TValue; const AProc: TProc = nil): IScheduler;
begin
  Result := Self;
  FCoroutines.Enqueue(TCoroutine.Create(AName, ARoutine, AValue, 1, AProc));
  FCurrentRoutine := FCoroutines.Peek;
end;

class function TScheduler.New(const ASleepTime: UInt16): IScheduler;
begin
  Result := TScheduler.Create(ASleepTime);
end;

procedure TScheduler.Next;
var
  LResultValue: TValue;
begin
  if FCoroutines.Count = 0 then
    Exit;
  try
    FCurrentRoutine := FCoroutines.Dequeue;
    if (FPause.IsPaused) and (FCurrentRoutine.Name = FPause.Name) then
    begin
      FPause.Name := EmptyStr;
      FPause.Value := FCurrentRoutine.Value;
      FPause.IsPaused := False;
      FCurrentRoutine.State := TCoroutineState.csPaused;
    end;

    if FCurrentRoutine.State in [TCoroutineState.csActive] then
    begin
      if Assigned(FCurrentRoutine.Proc) then
      begin
        TThread.Queue(TThread.CurrentThread, procedure
                                             begin
                                               FCurrentRoutine.Proc();
                                             end);
      end;
      Sleep(FSleepTime);
      LResultValue := FCurrentRoutine.Func(FCurrentRoutine.SendValue, FCurrentRoutine.Value);
      if not LResultValue.IsEmpty then
      begin
        FCurrentRoutine.Value := LResultValue;
        FCurrentRoutine.ObserverNotify;
        FCoroutines.Enqueue(FCurrentRoutine);
      end
      else
      if (LResultValue.IsEmpty) or (FCoroutines.Count = 0) then
      begin
        FCurrentRoutine.Free;
        FCurrentRoutine := nil;
        Exit;
      end;
    end
    else
    if (FCurrentRoutine.State in [TCoroutineState.csPaused]) and
       (FCurrentRoutine.Func <> nil) then
    begin
      if (FSend.IsSend) and (FCurrentRoutine.Name = FSend.Name) then
      begin
        FCurrentRoutine.State := TCoroutineState.csActive;
        FCurrentRoutine.SendCount := FCurrentRoutine.SendCount + 1;
        if not FSend.Value.IsEmpty then
          FCurrentRoutine.SendValue := FSend.Value;
        FSend.IsSend := False;
        FSend.Name := EmptyStr;
        FSend.Value := Default(TValue);
      end;
      FCoroutines.Enqueue(FCurrentRoutine);
    end;
  except
    on E: Exception do
    begin
      FCurrentRoutine.Free;
      FCurrentRoutine := nil;
      FException.IsException := True;
      FException.Message := E.Message;
    end;
  end;
end;

function TScheduler.Run(const AError: TProc<String>): IScheduler;
begin
  FErrorCallback := AError;
  Result := Self.Run;
end;

function TScheduler.Run: IScheduler;
begin
  FTask := TTask.Run(procedure
                     var
                       LMessage: String;
                     begin
                       while (not FStoped) and (FCoroutines.Count > 0) do
                       begin
                         FLock.Acquire;
                         try
                           Next;
                           if FException.IsException then
                           begin
                             LMessage := FException.Message;
                             if Assigned(FErrorCallback) then
                             begin
                               TThread.Queue(TThread.CurrentThread,
                                 procedure
                                 begin
                                   FErrorCallback(LMessage);
                                 end);
                             end;
                             FException.IsException := False;
                             FException.Message := '';
                           end;
                         finally
                           FLock.Release;
                         end;
                       end;
                     end);
  Result := Self;
end;

{ TRoutine }

function TCoroutine.Assign: TCoroutine;
begin
  Result := Self;
end;

procedure TCoroutine.Attach(const AObserver: TCoroutine);
begin
  FLock.Acquire;
  try
    FObserverList.Add(AObserver);
  finally
    FLock.Release;
  end;
end;

constructor TCoroutine.Create;
begin
  raise Exception.Create('This class should not be used directly.');
end;

constructor TCoroutine.Create(const AName: String; const AFunc: TFuncCoroutine;
  const AValue: TValue; const ACountSend: UInt32; const AProc: TProc = nil);
begin
  FName := AName;
  FFunc := AFunc;
  FProc := AProc;
  FValue := AValue;
  FSendValue := Default(TValue);
  FSendCount := ACountSend;
  FState := TCoroutineState.csActive;
  FObserverList := TList<TCoroutine>.Create;
  FParamNotify := Default(TParamNotify);
  FLock := TCriticalSection.Create;
end;

destructor TCoroutine.Destroy;
begin
  FObserverList.Free;
  FLock.Free;
  inherited;
end;

procedure TCoroutine.Detach(const AObserver: TCoroutine);
begin
  FLock.Acquire;
  try
    FObserverList.Remove(AObserver);
  finally
    FLock.Release;
  end;
end;

procedure TCoroutine.Notify(const AParams: TParamNotify);
begin
  FParamNotify := AParams;
end;

procedure TCoroutine.ObserverNotify;
var
  LItem: TCoroutine;
begin
  FLock.Acquire;
  try
    for LItem in FObserverList do
      LItem.Notify(TParamNotify.Create(FName, FValue, FSendValue));
  finally
    FLock.Release;
  end;
end;

{ TScheduler.TGather<T> }

function TScheduler.TGather<T>.Dequeue: T;
begin
  if Self.Count > 0 then
  begin
    Result := Self[0];
    Self.Delete(0);
  end
  else
    Result := Default(T);
end;

procedure TScheduler.TGather<T>.Enqueue(const AValue: T);
begin
  Self.Add(AValue);
end;

function TScheduler.TGather<T>.Peek: T;
begin
  if Self.Count > 0 then
    Result := Self[0]
  else
    Result := Default(T);
end;

{ TParamNotify }

constructor TParamNotify.Create(const AName: String; const AValue,
  ASendValue: TValue);
begin
  FName := AName;
  FValue := AValue;
  FSendValue := ASendValue;
end;

end.
