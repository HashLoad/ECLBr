{
               ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(ECLBr Library)
  @created(23 Abr 2023)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @Discord(https://discord.gg/T2zJC8zX)
}

unit eclbr.objects;

{$I ..\eclbr.inc}

interface

uses
  Rtti,
  SysUtils,
  Classes,
  Variants,
  Generics.Collections;

type
  TArrayValue = array of TValue;

  IObject = interface
    ['{E3B4DFC3-25AD-46F5-947C-1509E802C047}']
    function Factory(const AClass: TClass): TObject; overload;
    function Factory(const AClass: TClass; const AArgs: TArrayValue;
      const AMethodName: String): TObject; overload;
  end;

  TObjectEx = class sealed(TInterfacedObject, IObject)
  strict private
    FContext: TRttiContext;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IObject;
    function Factory(const AClass: TClass): TObject; overload;
    function Factory(const AClass: TClass; const AArgs: TArrayValue;
      const AMethodName: String): TObject; overload;
  end;

  {$MESSAGE WARN 'This interface has been deprecated. Use AutoRef<T> instead.'}
  IAutoRef<T: class, constructor> = interface
    ['{F1196B06-4C61-4512-B06D-1691199A073C}']
    function Get: T; deprecated 'Use AsRef instead';
    function AsRef: T;
    procedure Release;
  end;

  {$MESSAGE WARN 'This class has been deprecated. Use AutoRef<T> instead.'}
  TAutoRef<T: class, constructor> = class sealed(TInterfacedObject, IAutoRef<T>)
  strict private
    FObject: T;
  protected
    constructor Create(const ACallbackNew: TFunc<T>); overload;
    constructor Create(const ACallbackNew: TFunc<TArrayValue, T>;
      const AArgs: TArrayValue); overload;
    constructor Create; overload;
    constructor Create(const AObject: T); overload;
  public
    destructor Destroy; override;
    class function New(const ACallbackNew: TFunc<TArrayValue, T>;
      const AArgs: TArrayValue): IAutoRef<T>; overload; deprecated 'Use AutoRef<T> instead';
    class function New(const ACallbackNew: TFunc<T>): IAutoRef<T>; overload;
    class function New: IAutoRef<T>; overload;
    class function New(const AObject: T): IAutoRef<T>; overload;
    class function LazyLoad: IAutoRef<T>;
    function Get: T; deprecated 'Use AsRef instead';
    function AsRef: T;
    procedure Release;
  end;

  ISmartPtr<T: class, constructor> = interface
    ['{17C30A2D-74C9-48B2-917D-6C5DC99D2B78}']
    function IsNull: Boolean;
  end;

  AutoRef<T: class, constructor> = record
  strict private
    FValue: T;
    FSmartPtr: ISmartPtr<T>;
    function _GetAsRef: T;
  strict private
    type
      TSmartPtr = class (TInterfacedObject, ISmartPtr<T>)
      private
        FObject: TObject;
      public
        constructor Create(const AObjectRef: TObject);
        destructor Destroy; override;
        function IsNull: Boolean;
      end;
  public
    constructor Create(const AObjectRef: T);
    class operator Implicit(const AObjectRef: T): AutoRef<T>;
    class operator Implicit(const AAutoRef: AutoRef<T>): T;
    function IsNull: Boolean;
    procedure Free;
    {$MESSAGE WARN 'This property [Value] has been deprecated, use AsRef instead.'}
    property Value: T read _GetAsRef;
    property AsRef: T read _GetAsRef;
  end;

implementation

{ TAutoRef<T> }

constructor TAutoRef<T>.Create(const ACallbackNew: TFunc<TArrayValue, T>;
  const AArgs: TArrayValue);
begin
  FObject := ACallbackNew(AArgs);
end;

constructor TAutoRef<T>.Create(const ACallbackNew: TFunc<T>);
begin
  FObject := ACallbackNew();
end;

constructor TAutoRef<T>.Create;
begin
  FObject := T.Create;
end;

function TAutoRef<T>.AsRef: T;
begin
  // LazyLoad
  if FObject = nil then
    FObject := T.Create;

  Result := FObject;
end;

constructor TAutoRef<T>.Create(const AObject: T);
begin
  FObject := AObject;
end;

destructor TAutoRef<T>.Destroy;
begin
  Self.Release;
  inherited;
end;

class function TAutoRef<T>.New(const ACallbackNew: TFunc<T>): IAutoRef<T>;
begin
  Result := TAutoRef<T>.Create(ACallbackNew);
end;

class function TAutoRef<T>.New(const ACallbackNew: TFunc<TArrayValue, T>;
  const AArgs: TArrayValue): IAutoRef<T>;
begin
  Result := TAutoRef<T>.Create(ACallbackNew, AArgs);
end;

function TAutoRef<T>.Get: T;
begin
  // LazyLoad
  if FObject = nil then
    FObject := T.Create;

  Result := FObject;
end;

class function TAutoRef<T>.LazyLoad: IAutoRef<T>;
begin
  Result := TAutoRef<T>.Create(nil);
end;

class function TAutoRef<T>.New(const AObject: T): IAutoRef<T>;
begin
  Result := TAutoRef<T>.Create(AObject);
end;

procedure TAutoRef<T>.Release;
begin
  if FObject = nil then
    Exit;

  FObject.Free;
  FObject := nil;
end;

class function TAutoRef<T>.New: IAutoRef<T>;
begin
  Result := TAutoRef<T>.Create;
end;

{ TObjectEx<T> }

constructor TObjectEx.Create;
begin
  FContext := TRttiContext.Create;
end;

destructor TObjectEx.Destroy;
begin
  FContext.Free;
end;

function TObjectEx.Factory(const AClass: TClass): TObject;
begin
  Result := Factory(AClass, [], 'Create');
end;

function TObjectEx.Factory(const AClass: TClass;
  const AArgs: TArrayValue; const AMethodName: String): TObject;
var
  LConstructor: TRttiMethod;
  LInstance: TValue;
  LType: TRttiType;
begin
  try
    LType := FContext.GetType(AClass);
    LConstructor := LType.GetMethod(AMethodName);
    if not LConstructor.IsConstructor then
      raise Exception.CreateFmt('Constructor method %s not found in class %s', [AMethodName, AClass.ClassName]);
    LInstance := LConstructor.Invoke(LType.AsInstance.MetaClassType, AArgs);
    Result := LInstance.AsObject;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error class [%s] : %s', [AClass.ClassName, E.Message]);
  end;
end;

class function TObjectEx.New: IObject;
begin
  Result := Self.Create;
end;

{ TSmartPtr }

constructor AutoRef<T>.TSmartPtr.Create(const AObjectRef: TObject);
begin
  FObject := AObjectRef;
end;

destructor AutoRef<T>.TSmartPtr.Destroy;
begin
  FObject.Free;
  inherited;
end;

function AutoRef<T>.TSmartPtr.IsNull: Boolean;
begin
  Result := FObject = nil;
end;

{ AutoRef<T> }

constructor AutoRef<T>.Create(const AObjectRef: T);
begin
  FValue := AObjectRef;
  FSmartPtr := TSmartPtr.Create(AObjectRef);
end;

procedure AutoRef<T>.Free;
begin
  // Free Fake
end;

function AutoRef<T>._GetAsRef: T;
begin
  if FSmartPtr = nil then
    Self := AutoRef<T>.Create(T.Create);

  Result := Self.FValue;
end;

class operator AutoRef<T>.Implicit(const AAutoRef: AutoRef<T>): T;
begin
  Result := AAutoRef.AsRef;
end;

function AutoRef<T>.IsNull: Boolean;
begin
  Result := True;
  if FSmartPtr = nil then
    Exit;

  Result := FSmartPtr.IsNull;
end;

class operator AutoRef<T>.Implicit(const AObjectRef: T): AutoRef<T>;
begin
  Result := AutoRef<T>.Create(AObjectRef);
end;

end.



