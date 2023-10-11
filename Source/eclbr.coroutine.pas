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
  Generics.Collections;

type
  IScheduler = interface
    ['{BC104A19-9657-4093-A494-8D3CFD4CAF09}']
    procedure Next;
    procedure Send(const Value: TValue);
    procedure Suspend;
    function Add(const ARoutine: TFunc<TValue, TValue>; const Value: TValue;
      const Proc: TProc = nil): IScheduler; overload;
    function Value: TValue;
    function Yield: TValue;
    function Count: Integer;
    function CountSend: Integer;
    function Run: IScheduler;
  end;

  TRoutineState = (rsActive, rsPaused, rsFinished);

  // deprecated 'This class should not be used. Internal use.'
  TListHelper<T> = class sealed(TList<T>)
  protected
    procedure Enqueue(const AValue: T);
    function Dequeue: T;
    function Peek: T;
  end;

  TRoutine = record
  private
    FState: TRoutineState;
    FFunc: TFunc<TValue, TValue>;
    FProc: TProc;
    FValue: TValue;
    FValueSend: TValue;
    FCountSend: Integer;
  public
    constructor Create(const AFunc: TFunc<TValue, TValue>;
      const AValue: TValue; const ACountSend: Integer; const AProc: TProc = nil); overload;
    function Assign: TRoutine;
    property Func: TFunc<TValue, TValue> read FFunc;
    property Proc: TProc read FProc;
    property Value: TValue read FValue write FValue;
    property State: TRoutineState read FState write FState;
    property ValueSend: TValue read FValueSend write FValueSend;
    property CountSend: Integer read FCountSend write FCountSend;
  end;

  TScheduler = class(TInterfacedObject, IScheduler)
  private
    FCurrentRoutine: TRoutine;
    FRoutines: TListHelper<TRoutine>;
    FTask: ITask;
  protected
    constructor Create; overload;
    constructor Create(const ARoutine: TFunc<TValue, TValue>); overload;
    constructor Create(const ARoutine: TFunc<TValue, TValue>; const AValue: TValue); overload;
    constructor Create(const ARoutine: TFunc<TValue, TValue>; const AValue: TValue;
      const AProc: TProc); overload;
  public
    class function New: IScheduler;
    destructor Destroy; override;
    procedure Next;
    procedure Send(const AValue: TValue);
    procedure Suspend;
    function Add(const ARoutine: TFunc<TValue, TValue>; const AValue: TValue;
      const AProc: TProc = nil): IScheduler; overload;
    function Value: TValue;
    function Yield: TValue;
    function Count: Integer;
    function CountSend: Integer;
    function Run: IScheduler;
  end;

implementation

{ TScheduler }

constructor TScheduler.Create(const ARoutine: TFunc<TValue, TValue>;
  const AValue: TValue);
begin
  Create(ARoutine, AValue, nil);
end;

constructor TScheduler.Create(const ARoutine: TFunc<TValue, TValue>);
begin
  Create(ARoutine, TValue.Empty, nil);
end;

function TScheduler.Count: Integer;
begin
  Result := FRoutines.Count;
end;

function TScheduler.CountSend: Integer;
begin
  Result := 0;
  if FRoutines.Count = 0 then
    exit;
  Result := FRoutines.Peek.CountSend;
end;

constructor TScheduler.Create(const ARoutine: TFunc<TValue, TValue>;
  const AValue: TValue; const AProc: TProc);
begin
  FRoutines := TListHelper<TRoutine>.Create;
  if Assigned(ARoutine) then
    FRoutines.Enqueue(TRoutine.Create(ARoutine, AValue, 1, AProc));
end;

constructor TScheduler.Create;
begin
  Create(nil, TValue.Empty, nil);
end;

destructor TScheduler.Destroy;
begin
  FRoutines.Free;
  inherited;
end;

function TScheduler.Yield: TValue;
begin
  if FRoutines.Count = 0 then
    exit;

  Result := FCurrentRoutine.ValueSend;
  FCurrentRoutine.ValueSend := TValue.Empty;
end;

procedure TScheduler.Send(const AValue: TValue);
begin
  if FRoutines.Count = 0 then
    Exit;

  FCurrentRoutine.ValueSend := Value;
  FCurrentRoutine.CountSend := FCurrentRoutine.CountSend + 1;
  FCurrentRoutine.State := TRoutineState.rsActive;
end;

procedure TScheduler.Suspend;
begin
  if FRoutines.Count = 0 then
    Exit;

  FCurrentRoutine.State := TRoutineState.rsPaused;
end;

function TScheduler.Value: TValue;
begin
  Result := FCurrentRoutine.Value;
end;

function TScheduler.Add(const ARoutine: TFunc<TValue, TValue>;
  const AValue: TValue; const AProc: TProc = nil): IScheduler;
begin
  Result := Self;
  FRoutines.Enqueue(TRoutine.Create(ARoutine, AValue, 1, AProc));
  FCurrentRoutine := FRoutines.Peek;
end;

class function TScheduler.New: IScheduler;
begin
  Result := TScheduler.Create;
end;

procedure TScheduler.Next;
var
  LResultValue: TValue;
  LRoutine: TRoutine;
begin
  if FRoutines.Count = 0 then
    exit;

  if FRoutines.Peek.State in [TRoutineState.rsPaused] then
    LRoutine := FRoutines.Dequeue;

  FCurrentRoutine := FRoutines.Dequeue;
  LResultValue := FCurrentRoutine.Func(FCurrentRoutine.Value);
  if not LResultValue.IsEmpty then
  begin
    FCurrentRoutine.Value := LResultValue;
    FRoutines.Enqueue(FCurrentRoutine);
  end;
  if (LResultValue.IsEmpty) or (FRoutines.Count = 0) then
    exit;

  if Assigned(FCurrentRoutine.Proc) then
  begin
    TThread.Synchronize(nil, procedure
                             begin
                               FCurrentRoutine.Proc();
                             end);
  end;
  if (LRoutine.State in [TRoutineState.rsPaused]) and (LRoutine.Func <> nil) then
    FRoutines.Enqueue(LRoutine);
end;

function TScheduler.Run: IScheduler;
begin
  Result := Self;
  FTask := TTask.Run(procedure
                     begin
                       while FRoutines.Count > 0 do
                         Next;
                     end);
end;

{ TRoutine }

function TRoutine.Assign: TRoutine;
begin
  Result := Self;
end;

constructor TRoutine.Create(const AFunc: TFunc<TValue, TValue>;
  const AValue: TValue; const ACountSend: Integer; const AProc: TProc = nil);
begin
  FFunc := AFunc;
  FProc := AProc;
  FValue := AValue;
  FCountSend := ACountSend;
  FState := TRoutineState.rsActive;
end;

{ TListHelper<T> }

function TListHelper<T>.Dequeue: T;
begin
  if Self.Count > 0 then
  begin
    Result := Self[0];
    Self.Delete(0);
  end
  else
    Result := Default(T);
end;

procedure TListHelper<T>.Enqueue(const AValue: T);
begin
  Self.Add(AValue);
end;

function TListHelper<T>.Peek: T;
begin
  if Self.Count > 0 then
    Result := Self[0]
  else
    Result := Default(T);
end;

end.
