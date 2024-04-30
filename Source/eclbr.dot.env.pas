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

unit eclbr.dot.env;

interface

uses
  Rtti,
  Types,
  SysUtils,
  Classes,
  Windows,
  Generics.Collections;

type
  TDotEnv = class
  private
    FFileName: String;
    FVariables: TDictionary<String, TValue>;
    procedure _LoadFromFile(const AFileName: String);
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    // File .ENV
    procedure Open;
    procedure Save;
    procedure Add(const AName: String; const AValue: TValue);
    procedure Delete(const AName: String);
    function Value<T>(const AName: String): T; overload;
    // Environment Variable
    function EnvCreate(const AName: String; const AValue: String): String;
    function EnvLoad(const AName: String): String;
    function EnvUpdate(const AName: String; const AValue: String): String;
    function EnvDelete(const AName: String): String;
  end;

implementation

destructor TDotEnv.Destroy;
begin
  if Assigned(FVariables) then
    FVariables.Free;
  inherited;
end;

procedure TDotEnv._LoadFromFile(const AFileName: String);
var
  LLines: TStringList;
  LLine: String;
  LPosSeparator: Integer;
begin
  FVariables.Clear;
  if FileExists(AFileName) then
  begin
    LLines := TStringList.Create;
    try
      LLines.LoadFromFile(AFileName);
      for LLine in LLines do
      begin
        LPosSeparator := Pos('=', LLine);
        if LPosSeparator > 0 then
        begin
          FVariables.Add(
            Trim(Copy(LLine, 1, LPosSeparator - 1)),
            TValue.From<String>(Trim(Copy(LLine, LPosSeparator + 1, Length(LLine))))
          );
        end;
      end;
    finally
      LLines.Free;
    end;
  end;
end;

procedure TDotEnv.Open;
begin
  if not Assigned(FVariables) then
    FVariables := TDictionary<String, TValue>.Create;

  _LoadFromFile(FFileName);
end;

function TDotEnv.Value<T>(const AName: String): T;
var
  LResult: TValue;
begin
  if FVariables.TryGetValue(AName, LResult) then
    Result := LResult.AsType<T>
  else
    raise Exception.CreateFmt('Variable %s not found', [AName]);
end;

function TDotEnv.EnvCreate(const AName: String; const AValue: String): String;
begin
  if not SetEnvironmentVariable(PChar(AName), PChar(AValue)) then
    RaiseLastOSError;
end;

function TDotEnv.EnvDelete(const AName: String): String;
begin
  if not SetEnvironmentVariable(PChar(AName), nil) then
    RaiseLastOSError;
end;

function TDotEnv.EnvLoad(const AName: String): String;
begin
  Result := GetEnvironmentVariable(AName);
end;

function TDotEnv.EnvUpdate(const AName: String; const AValue: String): String;
begin
  if not SetEnvironmentVariable(PChar(AName), PChar(AValue)) then
    RaiseLastOSError;
end;

procedure TDotEnv.Add(const AName: String; const AValue: TValue);
begin
  FVariables.AddOrSetValue(AName, AValue);
end;

constructor TDotEnv.Create(const AFileName: String);
begin
  FFileName := AFileName;
end;

procedure TDotEnv.Delete(const AName: String);
begin
  FVariables.Remove(AName);
end;

procedure TDotEnv.Save;
var
  LLines: TStringList;
  LPair: TPair<String, TValue>;
begin
  LLines := TStringList.Create;
  try
    for LPair in FVariables do
      LLines.Add(LPair.Key + '=' + LPair.Value.ToString);
    LLines.SaveToFile(FFileName);
  finally
    LLines.Free;
  end;
end;

end.
