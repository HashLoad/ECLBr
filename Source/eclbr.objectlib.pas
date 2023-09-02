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
  SysUtils,
  eclbr.interfaces;

type
  TObjectLib = class sealed(TInterfacedObject, IECL)
  protected
    function _FactoryInternal(const AClass: TClass; const AArgs: TArray<TValue>;
      const AMethodName: string): TObject;
  public
    function Factory(const AClass: TClass;
      const AArgs: TArray<TValue> = []; const AMethodName: string = 'Create'): TObject;
    class function New: IECL;
  end;

implementation

function TObjectLib.Factory(const AClass: TClass;
  const AArgs: TArray<TValue>; const AMethodName: string): TObject;
begin
  Result := _FactoryInternal(AClass, AArgs, AMethodName);
end;

class function TObjectLib.New: IECL;
begin
  Result := Self.Create;
end;

function TObjectLib._FactoryInternal(const AClass: TClass;
  const AArgs: TArray<TValue>; const AMethodName: string): TObject;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LInstance: TValue;
  LConstructor: TRttiMethod;
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

end.
