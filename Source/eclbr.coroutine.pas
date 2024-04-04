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
  Threading,
  Generics.Collections,
  eclbr.std;

type
  TFuture = eclbr.std.TFuture;

  IScheduler = interface
    ['{BC104A19-9657-4093-A494-8D3CFD4CAF09}']
    procedure Next;
    procedure Send(const AName: String); overload;
    procedure Send(const AName: String; const Value: TValue); overload;
    procedure Suspend(const AName: String);
    procedure Stop(const ATimeout: Cardinal = 1000);
    function Add(const AName: String; const ARoutine: TFunc<TValue, TValue>; const Value: TValue;
      const Proc: TProc = nil): IScheduler; overload;
    function Value: TValue;
    function Yield(const AName: String): TValue;
    function Count: Integer;
    function SendCount: Integer;
    function Run: IScheduler; overload;
    function Run(const AError: TProc<Exception>): IScheduler; overload;
  end;

  TRoutineState = (rsActive, rsPaused, rsFinished);

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

  TRoutine = record
  strict private
    FName: String;
    FState: TRoutineState;
    FFunc: TFunc<TValue, TValue>;
    FProc: TProc;
    FValue: TValue;
    FSendCount: Integer;
  public
    constructor Create(const AName: String; const AFunc: TFunc<TValue, TValue>;
      const AValue: TValue; const ACountSend: Integer; const AProc: TProc = nil); overload;
    function Assign: TRoutine;
    property Name: String read FName write FName;
    property Func: TFunc<TValue, TValue> read FFunc;
    property Proc: TProc read FProc;
    property Value: TValue read FValue write FValue;
    property State: TRoutineState read FState write FState;
    property SendCount: Integer read FSendCount write FSendCount;
  end;

  TScheduler = class(TInterfacedObject, IScheduler)
  strict private
    type
      TListHelper<T> = class sealed(TList<T>)
      protected
        procedure Enqueue(const AValue: T);
        function Dequeue: T;
        function Peek: T;
      end;
    const C_COROUTINE_NOT_FOUND = 'No coroutine found with the specified name.';
  strict private
    FSleepTime: UInt16;
    FCurrentRoutine: TRoutine;
    FRoutines: TListHelper<TRoutine>;
    FTask: ITask;
    FError: TProc<Exception>;
    FStoped: Boolean;
    FSend: TSend;
    FPause: TPause;
  protected
    constructor Create(const ASleepTime: UInt16 = 500); overload;
  public
    class function New(const ASleepTime: UInt16 = 500): IScheduler;
    destructor Destroy; override;
    procedure Next;
    procedure Send(const AName: String); overload;
    procedure Send(const AName: String; const AValue: TValue); overload;
    procedure Suspend(const AName: String);
    procedure Stop(const ATimeout: Cardinal = 1000);
    function Add(const AName: String; const ARoutine: TFunc<TValue, TValue>; const AValue: TValue;
      const AProc: TProc = nil): IScheduler; overload;
    function Value: TValue;
    function Yield(const AName: String): TValue;
    function Count: Integer;
    function SendCount: Integer;
    function Run: IScheduler; overload;
    function Run(const AError: TProc<Exception>): IScheduler; overload;
  end;

implementation

{ TScheduler }

function TScheduler.Count: Integer;
begin
  Result := FRoutines.Count;
end;

procedure TScheduler.Send(const AName: String);
begin
  FSend.IsSend := True;
  FSend.Name := AName;
  FSend.Value := Default(TValue);
end;

function TScheduler.SendCount: Integer;
begin
  Result := 0;
  if FRoutines.Count = 0 then
    exit;
  Result := FRoutines.Peek.SendCount;
end;

constructor TScheduler.Create(const ASleepTime: UInt16);
begin
  FStoped := False;
  FSleepTime := ASleepTime;
  FRoutines := TListHelper<TRoutine>.Create;
end;

destructor TScheduler.Destroy;
begin
  FRoutines.Free;
  inherited;
end;

function TScheduler.Yield(const AName: String): TValue;
begin
  if FRoutines.Count = 0 then
    raise Exception.Create(C_COROUTINE_NOT_FOUND);
  Suspend(AName);
  repeat until not FPause.IsPaused;
  Result := FPause.Value;
  FPause.Value := Default(TValue);
end;

procedure TScheduler.Send(const AName: String; const AValue: TValue);
begin
  FCurrentRoutine.Value := TValue.Empty;
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

function TScheduler.Add(const AName: String; const ARoutine: TFunc<TValue, TValue>;
  const AValue: TValue; const AProc: TProc = nil): IScheduler;
begin
  Result := Self;
  FRoutines.Enqueue(TRoutine.Create(AName, ARoutine, AValue, 1, AProc));
  FCurrentRoutine := FRoutines.Peek;
end;

class function TScheduler.New(const ASleepTime: UInt16): IScheduler;
begin
  Result := TScheduler.Create(ASleepTime);
end;

procedure TScheduler.Next;
var
  LResultValue: TValue;
begin
  if FRoutines.Count = 0 then
    Exit;

  FCurrentRoutine := FRoutines.Dequeue;
  if (FPause.IsPaused) and (FCurrentRoutine.Name = FPause.Name) then
  begin
    FPause.Name := EmptyStr;
    FPause.Value := FCurrentRoutine.Value;
    FPause.IsPaused := False;
    FCurrentRoutine.State := TRoutineState.rsPaused;
  end;

  if FCurrentRoutine.State in [TRoutineState.rsActive] then
  begin
    if Assigned(FCurrentRoutine.Proc) then
    begin
      TThread.Queue(TThread.CurrentThread, procedure
                                           begin
                                             FCurrentRoutine.Proc();
                                           end);
    end;
    LResultValue := FCurrentRoutine.Func(FCurrentRoutine.Value);
    Sleep(FSleepTime);
    if not LResultValue.IsEmpty then
    begin
      FCurrentRoutine.Value := LResultValue;
      FRoutines.Enqueue(FCurrentRoutine);
    end;
    if (LResultValue.IsEmpty) or (FRoutines.Count = 0) then
      Exit;
  end
  else
  if (FCurrentRoutine.State in [TRoutineState.rsPaused]) and
     (FCurrentRoutine.Func <> nil) then
  begin
    if (FSend.IsSend) and (FCurrentRoutine.Name = FSend.Name) then
    begin
      FCurrentRoutine.State := TRoutineState.rsActive;
      FCurrentRoutine.SendCount := FCurrentRoutine.SendCount + 1;
      if not FSend.Value.IsEmpty then
        FCurrentRoutine.Value := FSend.Value;
      FSend.Value := Default(TValue);
      FSend.Name := EmptyStr;
      FSend.IsSend := False;
    end;
    FRoutines.Enqueue(FCurrentRoutine);
  end;
end;

function TScheduler.Run(const AError: TProc<Exception>): IScheduler;
begin
  FError := AError;
  Result := Self.Run;
end;

function TScheduler.Run: IScheduler;
begin
  FTask := TTask.Run(procedure
                     var
                       LMessage: string;
                     begin
                       try
                         while (not FStoped) and (FRoutines.Count > 0) do
                           Next;
                       except
                         on E: Exception do
                         begin
                           LMessage := E.Message;
                           if Assigned(FError) then
                           begin
                             TThread.Queue(TThread.CurrentThread,
                               procedure
                               begin
                                 FError(Exception.Create(LMessage));
                               end);
                           end;
                         end;
                       end;
                     end);
  Result := Self;
end;

{ TRoutine }

function TRoutine.Assign: TRoutine;
begin
  Result := Self;
end;

constructor TRoutine.Create(const AName: String; const AFunc: TFunc<TValue, TValue>;
  const AValue: TValue; const ACountSend: Integer; const AProc: TProc = nil);
begin
  FName := AName;
  FFunc := AFunc;
  FProc := AProc;
  FValue := AValue;
  FSendCount := ACountSend;
  FState := TRoutineState.rsActive;
end;

{ TListHelper<T> }

function TScheduler.TListHelper<T>.Dequeue: T;
begin
  if Self.Count > 0 then
  begin
    Result := Self[0];
    Self.Delete(0);
  end
  else
    Result := Default(T);
end;

procedure TScheduler.TListHelper<T>.Enqueue(const AValue: T);
begin
  Self.Add(AValue);
end;

function TScheduler.TListHelper<T>.Peek: T;
begin
  if Self.Count > 0 then
    Result := Self[0]
  else
    Result := Default(T);
end;

end.
