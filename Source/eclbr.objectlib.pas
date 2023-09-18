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

unit eclbr.objectlib;

{$I ..\eclbr.inc}

interface

uses
  Rtti,
  SysUtils;

type
  IECL = interface
    ['{E3B4DFC3-25AD-46F5-947C-1509E802C047}']
    function Factory(const AClass: TClass; const AMetadata: TArray<TValue> = [];
      const AMethodName: string = 'Create'): TObject;
  end;

  TObjectLib = class sealed(TInterfacedObject, IECL)
  public
    function Factory(const AClass: TClass;
      const AArgs: TArray<TValue> = []; const AMethodName: string = 'Create'): TObject;
    class function New: IECL;
  end;

  IOption<T> = interface
    ['{F1196B06-4C61-4512-B06D-1691199A073C}']
    function Get: T;
  end;

  TOption<T: class> = class sealed(TInterfacedObject, IOption<T>)
  private
    FObjectInternal: T;
  protected
    constructor Create(const ACallbackNew: TFunc<T>); overload;
    constructor Create(const ACallbackNew: TFunc<TArray<TValue>, T>;
      const AArgs: TArray<TValue>); overload;
    destructor Destroy; override;
  public
    class function New(const ACallbackNew: TFunc<TArray<TValue>, T>;
      const AArgs: TArray<TValue>): IOption<T>; overload;
    class function New(const ACallbackNew: TFunc<T>): IOption<T>; overload;
    function Get: T;
  end;

implementation

{ TObject<T> }

constructor TOption<T>.Create(const ACallbackNew: TFunc<TArray<TValue>, T>;
  const AArgs: TArray<TValue>);
begin
  FObjectInternal := ACallbackNew(AArgs);
end;

constructor TOption<T>.Create(const ACallbackNew: TFunc<T>);
begin
  FObjectInternal := ACallbackNew();
end;

destructor TOption<T>.Destroy;
begin
  if Assigned(FObjectInternal) then
    FObjectInternal.Free;
end;

class function TOption<T>.New(const ACallbackNew: TFunc<T>): IOption<T>;
begin
  Result := TOption<T>.Create(ACallbackNew);
end;

class function TOption<T>.New(const ACallbackNew: TFunc<TArray<TValue>, T>;
  const AArgs: TArray<TValue>): IOption<T>;
begin
  Result := TOption<T>.Create(ACallbackNew, AArgs);
end;

function TOption<T>.Get: T;
begin
  Result := FObjectInternal;
end;

{ TObjectLib }

function TObjectLib.Factory(const AClass: TClass;
  const AArgs: TArray<TValue>; const AMethodName: string): TObject;
var
  LContext: TRttiContext;
  LConstructor: TRttiMethod;
  LInstance: TValue;
  LType: TRttiType;
begin
  try
    LContext := TRttiContext.Create;
    try
      LType := LContext.GetType(AClass);
      LConstructor := LType.GetMethod(AMethodName);
      if not LConstructor.IsConstructor then
        raise Exception.CreateFmt('Constructor method %s not found in class %s', [AMethodName, AClass.ClassName]);
      LInstance := LConstructor.Invoke(LType.AsInstance.MetaClassType, AArgs);
      Result := LInstance.AsObject;
    finally
      LContext.Free;
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error class [%s] : %s', [AClass.ClassName, E.Message]);
  end;
end;

class function TObjectLib.New: IECL;
begin
  Result := Self.Create;
end;

end.
