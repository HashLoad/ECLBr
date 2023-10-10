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
    function Add(const Routine: TFunc<TValue, TValue>; const Value: TValue;
      const Proc: TProc = nil): IScheduler; overload;
    function Value: TValue;
    function Yield: TValue;
    function Count: Integer;
    function CountSend: Integer;
    function Run: IScheduler;
  end;

  TRoutine = record
  private
    FFunc: TFunc<TValue, TValue>;
    FProc: TProc;
    FValue: TValue;
    FValueSend: TValue;
    FCountSend: Integer;
  public
    constructor Create(const AFunc: TFunc<TValue, TValue>;
      const AValue: TValue; const ACount: Integer; const AProc: TProc = nil);
    property Func: TFunc<TValue, TValue> read FFunc;
    property Proc: TProc read FProc;
    property Value: TValue read FValue write FValue;
    property Yield: TValue read FValueSend write FValueSend;
    property CountSend: Integer read FCountSend write FCountSend;
  end;

  TScheduler = class(TInterfacedObject, IScheduler)
  private
    FCurrentRoutine: TRoutine;
    FRoutines: TQueue<TRoutine>;
    FTask: ITask;
  protected
    constructor Create; overload;
    constructor Create(const Routine: TFunc<TValue, TValue>); overload;
    constructor Create(const Routine: TFunc<TValue, TValue>; const Value: TValue); overload;
    constructor Create(const Routine: TFunc<TValue, TValue>; const Value: TValue;
      const Proc: TProc); overload;
  public
    class function New: IScheduler;
    destructor Destroy; override;
    procedure Next;
    procedure Send(const Value: TValue);
    function Add(const Routine: TFunc<TValue, TValue>; const Value: TValue;
      const Proc: TProc = nil): IScheduler; overload;
    function Value: TValue;
    function Yield: TValue;
    function Count: Integer;
    function CountSend: Integer;
    function Run: IScheduler;
  end;

implementation

{ TScheduler }

constructor TScheduler.Create(const Routine: TFunc<TValue, TValue>;
  const Value: TValue);
begin
  Create(Routine, Value, nil);
end;

constructor TScheduler.Create(const Routine: TFunc<TValue, TValue>);
begin
  Create(Routine, TValue.Empty, nil);
end;

function TScheduler.Count: Integer;
begin
  Result := FRoutines.Count;
end;

function TScheduler.CountSend: Integer;
var
  LRoutine: TRoutine;
begin
  if FRoutines.Count = 0 then
    exit;
  LRoutine := FRoutines.Peek;
  Result := LRoutine.CountSend;
end;

constructor TScheduler.Create(const Routine: TFunc<TValue, TValue>;
  const Value: TValue; const Proc: TProc);
begin
  FRoutines := TQueue<TRoutine>.Create;
  if Assigned(Routine) then
    FRoutines.Enqueue(TRoutine.Create(Routine, Value, 1, Proc));
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
  LRoutine: TRoutine;
begin
  if FRoutines.Count = 0 then
    exit;
  LRoutine := FRoutines.Peek;
  Result := LRoutine.Yield;
  LRoutine.Yield := TValue.Empty;
end;

procedure TScheduler.Send(const Value: TValue);
var
  LRoutine: TRoutine;
begin
  if FRoutines.Count = 0 then
    exit;
  LRoutine := FRoutines.Peek;
  LRoutine.Yield := Value;
  LRoutine.CountSend := LRoutine.CountSend + 1;
end;

function TScheduler.Value: TValue;
begin
  Result := FCurrentRoutine.Value;
end;

function TScheduler.Add(const Routine: TFunc<TValue, TValue>;
  const Value: TValue; const Proc: TProc = nil): IScheduler;
begin
  Result := Self;
  FRoutines.Enqueue(TRoutine.Create(Routine, Value, 1, Proc));
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
  FCurrentRoutine := FRoutines.Dequeue;
  LResultValue := FCurrentRoutine.Func(FCurrentRoutine.Value);
  if not LResultValue.IsEmpty then
    FRoutines.Enqueue(TRoutine.Create(FCurrentRoutine.Func,
                                      LResultValue,
                                      FCurrentRoutine.CountSend,
                                      FCurrentRoutine.Proc));
  if (LResultValue.IsEmpty) and (FRoutines.Count = 0) then
    exit;
  if Assigned(FCurrentRoutine.Proc) then
  begin
    TThread.Synchronize(nil, procedure
                             begin
                               FCurrentRoutine.Proc();
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

{ TRoutine }

constructor TRoutine.Create(const AFunc: TFunc<TValue, TValue>;
  const AValue: TValue; const ACount: Integer; const AProc: TProc = nil);
begin
  FFunc := AFunc;
  FProc := AProc;
  FValue := AValue;
  FCountSend := ACount;
end;

end.
