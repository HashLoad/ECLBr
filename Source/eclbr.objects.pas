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
  IObject = interface
    ['{E3B4DFC3-25AD-46F5-947C-1509E802C047}']
    function Factory(const AClass: TClass; const AMetadata: TArray<TValue> = [];
      const AMethodName: string = 'Create'): TObject;
  end;

  TObjectEx = class sealed(TInterfacedObject, IObject)
  strict private
    FContext: TRttiContext;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IObject;
    function Factory(const AClass: TClass;
      const AArgs: TArray<TValue> = []; const AMethodName: string = 'Create'): TObject;
  end;

  // [Deprecated('use AutoRef<T>')]
  IAutoRef<T: class, constructor> = interface
    ['{F1196B06-4C61-4512-B06D-1691199A073C}']
    function Get: T;
    procedure Release;
  end;

  // [Deprecated('use AutoRef<T>')]
  TAutoRef<T: class, constructor> = class sealed(TInterfacedObject, IAutoRef<T>)
  strict private
    FObject: T;
  protected
    constructor Create(const ACallbackNew: TFunc<T>); overload;
    constructor Create(const ACallbackNew: TFunc<TArray<TValue>, T>;
      const AArgs: TArray<TValue>); overload;
    constructor Create; overload;
    constructor Create(const AObject: T); overload;
  public
    destructor Destroy; override;
    class function New(const ACallbackNew: TFunc<TArray<TValue>, T>;
      const AArgs: TArray<TValue>): IAutoRef<T>; overload;
    class function New(const ACallbackNew: TFunc<T>): IAutoRef<T>; overload;
    class function New: IAutoRef<T>; overload;
    class function New(const AObject: T): IAutoRef<T>; overload;
    class function LazyLoad: IAutoRef<T>;
    function Get: T;
    procedure Release;
  end;

  ISmartPtr<T: class, constructor> = interface
    ['{17C30A2D-74C9-48B2-917D-6C5DC99D2B78}']
    function IsNull: boolean;
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
        function IsNull: boolean;
      end;
  public
    constructor Create(const AObjectRef: T);
    class operator Implicit(const AObjectRef: T): AutoRef<T>;
    class operator Implicit(const AAutoRef: AutoRef<T>): T;
    function IsNull: Boolean;
    procedure Free;
    property AsRef: T read _GetAsRef;
  end;

implementation

{ TAutoRef<T> }

constructor TAutoRef<T>.Create(const ACallbackNew: TFunc<TArray<TValue>, T>;
  const AArgs: TArray<TValue>);
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

class function TAutoRef<T>.New(const ACallbackNew: TFunc<TArray<TValue>, T>;
  const AArgs: TArray<TValue>): IAutoRef<T>;
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

function TObjectEx.Factory(const AClass: TClass;
  const AArgs: TArray<TValue>; const AMethodName: string): TObject;
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

function AutoRef<T>.TSmartPtr.IsNull: boolean;
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
