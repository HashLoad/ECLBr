{
             ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2016, Isaque Pinheiro
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
  @Telegram(https://t.me/ormbr)
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
  IECL = interface
    ['{E3B4DFC3-25AD-46F5-947C-1509E802C047}']
    function Factory(const AClass: TClass; const AMetadata: TArray<TValue> = [];
      const AMethodName: string = 'Create'): TObject;
  end;

  TObjectEx = class sealed(TInterfacedObject, IECL)
  private
    FContext: TRttiContext;
  protected
    constructor Create;
    destructor Destroy;
  public
    class function New: IECL;
    function Factory(const AClass: TClass;
      const AArgs: TArray<TValue> = []; const AMethodName: string = 'Create'): TObject;
  end;

  IAutoRef<T: class, constructor> = interface
    ['{F1196B06-4C61-4512-B06D-1691199A073C}']
    function Get: T;
  end;

  TAutoRef<T: class, constructor> = class sealed(TInterfacedObject, IAutoRef<T>)
  private
    FObjectInternal: T;
  protected
    constructor Create(const ACallbackNew: TFunc<T>); overload;
    constructor Create(const ACallbackNew: TFunc<TArray<TValue>, T>;
      const AArgs: TArray<TValue>); overload;
    constructor Create; overload;
    constructor Create(const AObject: T); overload;
    destructor Destroy; override;
  public
    class function New(const ACallbackNew: TFunc<TArray<TValue>, T>;
      const AArgs: TArray<TValue>): IAutoRef<T>; overload;
    class function New(const ACallbackNew: TFunc<T>): IAutoRef<T>; overload;
    class function New: IAutoRef<T>; overload;
    class function New(const AObject: T): IAutoRef<T>; overload;
    function Get: T;
  end;

implementation

{ TObject<T> }

constructor TAutoRef<T>.Create(const ACallbackNew: TFunc<TArray<TValue>, T>;
  const AArgs: TArray<TValue>);
begin
  FObjectInternal := ACallbackNew(AArgs);
end;

constructor TAutoRef<T>.Create(const ACallbackNew: TFunc<T>);
begin
  FObjectInternal := ACallbackNew();
end;

constructor TAutoRef<T>.Create;
var
  LNewT: IECL;
begin
  LNewT := TObjectEx.New;
  FObjectInternal := LNewT.Factory(T, []) as T;
end;

constructor TAutoRef<T>.Create(const AObject: T);
begin
  FObjectInternal := AObject;
end;

destructor TAutoRef<T>.Destroy;
begin
  if Assigned(FObjectInternal) then
    FObjectInternal.Free;
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
  Result := FObjectInternal;
end;

class function TAutoRef<T>.New(const AObject: T): IAutoRef<T>;
begin
  Result := TAutoRef<T>.Create(AObject);
end;

class function TAutoRef<T>.New: IAutoRef<T>;
begin
  Result := TAutoRef<T>.Create;
end;

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

class function TObjectEx.New: IECL;
begin
  Result := Self.Create;
end;

end.
