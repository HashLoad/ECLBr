unit eclbr.safetry;

interface

uses
  SysUtils;

type
  // Result type to encapsulate success or error state
  TSafeResult<T> = record
  private
    FIsOk: Boolean;
    FValue: T;
    FException: String;
  public
    procedure Ok(const AValue: T);
    procedure Err(const AException: String);
    function IsOk: Boolean;
    function IsErr: Boolean;
    function GetValue: T;
    function GetExceptionMessage: String;
    class function CreateOk(const AValue: T): TSafeResult<T>; static;
    class function CreateErr(const AException: String): TSafeResult<T>; static;
  end;

  // Type to encapsulate try-except-finally logic with fluent interface
  TSafeTry = record
  private
    FTryProc: TProc;
    FExcept: TProc<Exception>;
    FFinally: TProc;
    function _EndExecute: TSafeResult<Boolean>;
    constructor Create(const AProc: TProc); overload;
  public
    class function &Try(const AProc: TProc = nil): TSafeTry; overload; static;
    function &Except(const AProc: TProc<Exception>): TSafeTry;
    function &Finally(const AProc: TProc): TSafeTry;
    function &End: TSafeResult<Boolean>;
  end;

implementation

{ TSafeResult<T> }

procedure TSafeResult<T>.Ok(const AValue: T);
begin
  FIsOk := True;
  FValue := AValue;
  FException := '';
end;

class function TSafeResult<T>.CreateErr(const AException: String): TSafeResult<T>;
begin
  Result.Err(AException);
end;

class function TSafeResult<T>.CreateOk(const AValue: T): TSafeResult<T>;
begin
  Result.Ok(AValue);
end;

procedure TSafeResult<T>.Err(const AException: String);
begin
  FIsOk := False;
  FValue := Default(T);
  FException := AException;
end;

function TSafeResult<T>.IsOk: Boolean;
begin
  Result := FIsOk;
end;

function TSafeResult<T>.IsErr: Boolean;
begin
  Result := not FIsOk;
end;

function TSafeResult<T>.GetValue: T;
begin
  if not FIsOk then
    raise Exception.Create('Cannot get value when result is an error.');
  Result := FValue;
end;

function TSafeResult<T>.GetExceptionMessage: String;
begin
  Result := FException;
end;

{ TSafeTry<T> }

function TSafeTry._EndExecute: TSafeResult<Boolean>;
begin
  try
    try
      if Assigned(FTryProc) then
        FTryProc();
      Result := TSafeResult<Boolean>.CreateOk(True);
    except
      on E: Exception do
      begin
        if Assigned(FExcept) then
          FExcept(E);
        Result := TSafeResult<Boolean>.CreateErr(E.Message);
      end;
    end;
  finally
    if Assigned(FFinally) then
      FFinally();
  end;
end;

function TSafeTry.&Except(const AProc: TProc<Exception>): TSafeTry;
begin
  FExcept := AProc;
  Result := Self;
end;

function TSafeTry.&Finally(const AProc: TProc): TSafeTry;
begin
  FFinally := AProc;
  Result := Self;
end;

class function TSafeTry.&Try(const AProc: TProc): TSafeTry;
begin
  Result := TSafeTry.Create(AProc);
end;

constructor TSafeTry.Create(const AProc: TProc);
begin
  FTryProc := AProc;
  FExcept := nil;
  FFinally := nil;
end;

function TSafeTry.&End: TSafeResult<Boolean>;
begin
  Result := _EndExecute();
end;

end.
