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

unit eclbr.arrow.fun;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  Generics.Collections,
  eclbr.core;

type
  EArrowException = Exception;

  TArrow = record
  private
    class var FValue: TValue;
  public
    class destructor Destroy;
    class function Fn<W>(const AValue: W): TFunc<W>; overload; inline; static;
    class function Fn<W>(var AVar: W; const AValue: W): TProc<TValue>; overload; inline; static;
    class function Fn(var AVarRefs: TArray<Pointer>; const AValues: Tuple): TProc<TValue>; overload; static;
    class function Value<W>: W; static;
  end;

implementation

{ TArrowFn }

class function TArrow.Fn<W>(const AValue: W): TFunc<W>;
begin
  Result := function: W
            begin
              Result := AValue;
            end;
end;

class function TArrow.Fn<W>(var AVar: W; const AValue: W): TProc<TValue>;
var
  LVar: ^W;
  LValue: W;
begin
  LVar := @AVar;
  LValue := AValue;
  Result := procedure(AValue: TValue)
            begin
              try
                FValue := nil;
                FValue := Avalue;
                LVar^ := LValue;
              except
                on E: Exception do
                  raise EArrowException.Create('Error in TArrow.Fn: ' + E.Message);
              end;
            end;
end;

class function TArrow.Value<W>: W;
begin
  try
    Result := FValue.AsType<W>;
  except
    on E: Exception do
      raise EArrowException.Create('Error in TArrow.Value: ' + E.Message);
  end;
end;

class destructor TArrow.Destroy;
begin
  FValue := nil;
end;

class function TArrow.Fn(var AVarRefs: TArray<Pointer>; const AValues: Tuple): TProc<TValue>;
var
  LVarRefs: TArray<Pointer>;
begin
  if Length(AVarRefs) <> Length(AValues) then
    raise Exception.Create('Different-sized arrays.');

  LVarRefs := AVarRefs;
  Result := procedure(AValue: TValue)
            var
              LFor: Integer;
              LTypeInfo: PTypeInfo;
            begin
              try
                FValue := nil;
                FValue := Avalue;
                for LFor := 0 to High(LVarRefs) do
                begin
                  case AValues[LFor].Kind of
                    tkInt64:
                      PInt64(LVarRefs[LFor])^ := AValues[LFor].AsInt64;
                    tkInteger, tkSet:
                      PInteger(LVarRefs[LFor])^ := AValues[LFor].AsInteger;
                    tkFloat:
                      PDouble(LVarRefs[LFor])^ := AValues[LFor].AsExtended;
                    tkUString, tkLString, tkWString, tkString, tkChar, tkWChar:
                      PUnicodeString(LVarRefs[LFor])^ := AValues[LFor].AsString;
                    tkClass:
                      PObject(LVarRefs[LFor])^ := AValues[LFor].AsObject;
                    tkEnumeration:
                      PBoolean(LVarRefs[LFor])^ := AValues[LFor].AsBoolean;
                    tkRecord, tkVariant:
                      PVariant(LVarRefs[LFor])^ := AValues[LFor].AsVariant;
                    tkArray, tkDynArray:
                    begin
                      LTypeInfo := AValues[LFor].TypeInfo;
                      case GetTypeData(LTypeInfo).elType2^.Kind of
                        tkInt64:
                          TArray<Int64>(LVarRefs[LFor]) := AValues[LFor].AsType<TArray<Int64>>;
                        tkInteger, tkSet:
                          TArray<Integer>(LVarRefs[LFor]) := AValues[LFor].AsType<TArray<Integer>>;
                        tkFloat:
                          TArray<Extended>(LVarRefs[LFor]) := AValues[LFor].AsType<TArray<Extended>>;
                        tkUString, tkLString, tkWString, tkString, tkChar, tkWChar:
                          TArray<string>(LVarRefs[LFor]) := AValues[LFor].AsType<TArray<string>>;
                        tkClass:
                          TArray<TObject>(LVarRefs[LFor]) := AValues[LFor].AsType<TArray<TObject>>;
                        tkEnumeration:
                          TArray<boolean>(LVarRefs[LFor]) := AValues[LFor].AsType<TArray<boolean>>;
                        tkRecord, tkVariant:
                          TArray<variant>(LVarRefs[LFor]) := AValues[LFor].AsType<TArray<variant>>;
                        else
                          raise Exception.Create('The array contains elements of an unsupported type.');
                      end;
                    end;
                  end;
                end;
              except
                on E: Exception do
                  raise EArrowException.Create('Error in TArrow.Fn (array): ' + E.Message);
              end;
            end;
end;

end.








