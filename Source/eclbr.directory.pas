{
             ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2022, Isaque Pinheiro
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

unit eclbr.directory;

interface

uses
  SysUtils,
  IOUtils;

type
  TDirectoryEx = record helper for TDirectory
  private
    class var FDirectory: string;
  public
    class function New(const ADirectory: string): TDirectory; static;
    class function Filter(const AFilterFunc: TFunc<string, boolean>): TArray<string>; static;
    class function Map<T>(const AMapFunc: TFunc<string, T>): TArray<T>; static;
  end;

implementation

{ TDirectoryEx }

class function TDirectoryEx.Filter(
  const AFilterFunc: TFunc<string, boolean>): TArray<string>;
var
  LFiles: TArray<string>;
  LFilteredFiles: TArray<string>;
  LFileName: string;
begin
  LFiles := TDirectory.GetFiles(FDirectory);
  SetLength(LFilteredFiles, 0);
  for LFileName in LFiles do
  begin
    if AFilterFunc(LFileName) then
    begin
      SetLength(LFilteredFiles, Length(LFilteredFiles) + 1);
      LFilteredFiles[High(LFilteredFiles)] := LFileName;
    end;
  end;
  Result := LFilteredFiles;
end;

class function TDirectoryEx.Map<T>(const AMapFunc: TFunc<string, T>): TArray<T>;
var
  LFiles: TArray<string>;
  LMappedValues: TArray<T>;
  LFileName: string;
begin
  LFiles := GetFiles(FDirectory);
  SetLength(LMappedValues, Length(LFiles));
  for LFileName in LFiles do
  begin
    LMappedValues[High(LMappedValues)] := AMapFunc(LFileName);
  end;
  Result := LMappedValues;
end;

class function TDirectoryEx.New(const ADirectory: string): TDirectory;
begin
  FDirectory := ADirectory;
end;

end.
