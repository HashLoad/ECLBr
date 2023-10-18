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

unit eclbr.directory;

interface

uses
  SysUtils,
  IOUtils,
  eclbr.vector;

type
  TFilterPredicate = IOUtils.TDirectory.TFilterPredicate;
  TSearchOption = IOUtils.TSearchOption;
  TFileMode = IOUtils.TFileMode;
  TFileAccess = IOUtils.TFileAccess;
  TFileShare = IOUtils.TFileShare;
  TPath = IOUtils.TPath;
  TPathPrefixType = IOUtils.TPathPrefixType;

  TDirEx = record
  strict private
    class var FDirectory: string;
  public
    class function New(const ADirectory: string): TDirEx; static; inline;

    class function GetDirectories(const APath: string): TVector<string>; overload; inline; static;
    class function GetDirectories(const APath: string; const APredicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetDirectories(const APath, ASearchPattern: string): TVector<string>; overload; inline; static;
    class function GetDirectories(const APath, ASearchPattern: string; const APredicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetDirectories(const APath, ASearchPattern: string; const ASearchOption: TSearchOption): TVector<string>; overload; static;
    class function GetDirectories(const APath, ASearchPattern: string; const ASearchOption: TSearchOption; const APredicate: TFilterPredicate): TVector<string>; overload; static;
    class function GetDirectories(const APath: string; const ASearchOption: TSearchOption; const APredicate: TFilterPredicate): TVector<string>; overload; static;

    class function GetFileSystemEntries(const APath: string): TVector<string>; overload; inline; static;
    class function GetFileSystemEntries(const APath: string; const APredicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetFileSystemEntries(const APath, ASearchPattern: string): TVector<string>; overload; static;
    class function GetFileSystemEntries(const APath, ASearchPattern: string; const APredicate: TFilterPredicate): TVector<string>; overload; static;
    class function GetFileSystemEntries(const APath: string; const ASearchOption: TSearchOption; const APredicate: TFilterPredicate): TVector<string>; overload; static;

    class function GetFiles(const APath: string): TVector<string>; overload; inline; static;
    class function GetFiles(const APath: string; const APredicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetFiles(const APath, ASearchPattern: string): TVector<string>; overload; inline; static;
    class function GetFiles(const APath, ASearchPattern: string; const APredicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption): TVector<string>; overload; static;
    class function GetFiles(const APath, ASearchPattern: string; const ASearchOption: TSearchOption; const APredicate: TFilterPredicate): TVector<string>; overload; static;
    class function GetFiles(const APath: string; const ASearchOption: TSearchOption; const APredicate: TFilterPredicate): TVector<string>; overload; static;
  end;

implementation

{ TDirectoryEx }

class function TDirEx.GetDirectories(const APath,
  ASearchPattern: string): TVector<string>;
begin
  Result := TDirectory.GetDirectories(APath, ASearchPattern);
end;

class function TDirEx.GetDirectories(const APath: string;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetDirectories(APath, APredicate);
end;

class function TDirEx.GetDirectories(const APath: string): TVector<string>;
begin
  Result := TDirectory.GetDirectories(APath);
end;

class function TDirEx.GetDirectories(const APath, ASearchPattern: string;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetDirectories(APath, ASearchPattern, APredicate);
end;

class function TDirEx.GetDirectories(const APath: string;
  const ASearchOption: TSearchOption;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetDirectories(APath, ASearchOption, APredicate);
end;

class function TDirEx.GetFileSystemEntries(
  const APath: string): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(APath);
end;

class function TDirEx.GetFileSystemEntries(const APath: string;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(APath, APredicate);
end;

class function TDirEx.GetFileSystemEntries(const APath,
  ASearchPattern: string): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(APath, ASearchPattern);
end;

class function TDirEx.GetFileSystemEntries(const APath,
  ASearchPattern: string; const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(APath, ASearchPattern, APredicate);
end;

class function TDirEx.GetFiles(const APath,
  ASearchPattern: string): TVector<string>;
begin
  Result := TDirectory.GetFiles(APath, ASearchPattern);
end;

class function TDirEx.GetFiles(const APath: string;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFiles(APath, APredicate);
end;

class function TDirEx.GetFiles(const APath: string): TVector<string>;
begin
  Result := TDirectory.GetFiles(APath);
end;

class function TDirEx.GetFiles(const APath, ASearchPattern: string;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFiles(APath, ASearchPattern, APredicate);
end;

class function TDirEx.GetFiles(const APath: string;
  const ASearchOption: TSearchOption;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFiles(APath, ASearchOption, APredicate);
end;

class function TDirEx.GetFiles(const APath, ASearchPattern: string;
  const ASearchOption: TSearchOption;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFiles(APath, ASearchPattern, ASearchOption, APredicate);
end;

class function TDirEx.GetFiles(const APath, ASearchPattern: string;
  const ASearchOption: TSearchOption): TVector<string>;
begin
  Result := TDirectory.GetFiles(APath, ASearchPattern, ASearchOption);
end;

class function TDirEx.GetFileSystemEntries(const APath: string;
  const ASearchOption: TSearchOption;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(APath, ASearchOption, APredicate);
end;

class function TDirEx.GetDirectories(const APath, ASearchPattern: string;
  const ASearchOption: TSearchOption;
  const APredicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetDirectories(APath, ASearchPattern, ASearchOption, APredicate);
end;

class function TDirEx.GetDirectories(const APath, ASearchPattern: string;
  const ASearchOption: TSearchOption): TVector<string>;
begin
  Result := TDirectory.GetDirectories(APath, ASearchPattern, ASearchOption);
end;

class function TDirEx.New(const ADirectory: string): TDirEx;
begin
  FDirectory := ADirectory;
end;

end.
