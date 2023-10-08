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
    function Add(const Routine: TFunc<TValue, TValue>; const Value: TValue;
      const Proc: TProc = nil): IScheduler; overload;
    function Yield: TValue;
    function Value: TValue;
    function Run: IScheduler;
  end;

  TRoutineState = (rsActive, rsPaused, rsFinished);

  TSchedulerPair = record
  private
    FState: TRoutineState;
    FFunc: TFunc<TValue, TValue>;
    FProc: TProc;
    FValue: TValue;
    FValueSend: TValue;
  public
    constructor Create(const AFunc: TFunc<TValue, TValue>;
      const AValue: TValue; const AProc: TProc = nil);
    procedure Pause;
    procedure Resume;
    property Func: TFunc<TValue, TValue> read FFunc;
    property Proc: TProc read FProc;
    property State: TRoutineState read FState;
    property Value: TValue read FValue write FValue;
    property ValueSend: TValue read FValueSend write FValueSend;
  end;

  TScheduler = class(TInterfacedObject, IScheduler)
  private
    FCurrentValue: TSchedulerPair;
    FRoutines: TQueue<TSchedulerPair>;
    FTask: ITask;
  public
    class function New: IScheduler;
    constructor Create; overload;
    constructor Create(const Routine: TFunc<TValue, TValue>); overload;
    constructor Create(const Routine: TFunc<TValue, TValue>; const Value: TValue); overload;
    constructor Create(const Routine: TFunc<TValue, TValue>; const Value: TValue;
      const Proc: TProc); overload;
    destructor Destroy; override;
    procedure Next;
    procedure Send(const Value: TValue);
    function Add(const Routine: TFunc<TValue, TValue>; const Value: TValue;
      const Proc: TProc = nil): IScheduler; overload;
    function Yield: TValue;
    function Value: TValue;
    function Run: IScheduler;
  end;

implementation

{ TCoro }

constructor TScheduler.Create(const Routine: TFunc<TValue, TValue>;
  const Value: TValue);
begin
  Create(Routine, Value, nil);
end;

constructor TScheduler.Create(const Routine: TFunc<TValue, TValue>);
begin
  Create(Routine, TValue.Empty, nil);
end;

constructor TScheduler.Create(const Routine: TFunc<TValue, TValue>;
  const Value: TValue; const Proc: TProc);
begin
  FRoutines := TQueue<TSchedulerPair>.Create;
  if Assigned(Routine) then
    FRoutines.Enqueue(TSchedulerPair.Create(Routine, Value, Proc));
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
var
  LPair: TSchedulerPair;
begin
  if FRoutines.Count = 0 then
    exit;
  LPair := FRoutines.Peek;
  Result := LPair.FValueSend;
  LPair.FValueSend := TValue.Empty;
end;

procedure TScheduler.Send(const Value: TValue);
var
  LPair: TSchedulerPair;
begin
  if FRoutines.Count = 0 then
    exit;
  LPair := FRoutines.Dequeue;
  if LPair.Value.IsEmpty then
    LPair.Value := Value
  else
    LPair.ValueSend := Value;
  FRoutines.Enqueue(LPair);
end;

function TScheduler.Value: TValue;
begin
  Result := FCurrentValue.Value;
end;

function TScheduler.Add(const Routine: TFunc<TValue, TValue>;
  const Value: TValue; const Proc: TProc = nil): IScheduler;
begin
  Result := Self;
  FRoutines.Enqueue(TSchedulerPair.Create(Routine, Value, Proc));
end;

class function TScheduler.New: IScheduler;
begin
  Result := TScheduler.Create;
end;

procedure TScheduler.Next;
var
  LResultValue: TValue;
begin
  if FRoutines.Count = 0 then
    exit;
  FCurrentValue := FRoutines.Dequeue;
  LResultValue := FCurrentValue.Func(FCurrentValue.Value);
  if not LResultValue.IsEmpty then
    FRoutines.Enqueue(TSchedulerPair.Create(FCurrentValue.Func,
                                            LResultValue,
                                            FCurrentValue.Proc));
  if (LResultValue.IsEmpty) and (FRoutines.Count = 0) then
    exit;
  if Assigned(FCurrentValue.Proc) then
  begin
    TThread.Synchronize(nil, procedure
                             begin
                               FCurrentValue.Proc();
                             end);
  end;
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

{ TCoroPair }

constructor TSchedulerPair.Create(const AFunc: TFunc<TValue, TValue>;
  const AValue: TValue; const AProc: TProc = nil);
begin
  FFunc := AFunc;
  FProc := AProc;
  FValue := AValue;
end;

procedure TSchedulerPair.Pause;
begin
  FState := TRoutineState.rsPaused;
end;

procedure TSchedulerPair.Resume;
begin
  FState := TRoutineState.rsActive;
end;

end.
