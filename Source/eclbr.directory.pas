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
  private
    class var FDirectory: string;
  public
    class function New(const ADirectory: string): TDirEx; static; inline;

    class function GetDirectories(const Path: string): TVector<string>; overload; inline; static;
    class function GetDirectories(const Path: string; const Predicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetDirectories(const Path, SearchPattern: string): TVector<string>; overload; inline; static;
    class function GetDirectories(const Path, SearchPattern: string; const Predicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetDirectories(const Path, SearchPattern: string; const SearchOption: TSearchOption): TVector<string>; overload; static;
    class function GetDirectories(const Path, SearchPattern: string; const SearchOption: TSearchOption; const Predicate: TFilterPredicate): TVector<string>; overload; static;
    class function GetDirectories(const Path: string; const SearchOption: TSearchOption; const Predicate: TFilterPredicate): TVector<string>; overload; static;

    class function GetFileSystemEntries(const Path: string): TVector<string>; overload; inline; static;
    class function GetFileSystemEntries(const Path: string; const Predicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetFileSystemEntries(const Path, SearchPattern: string): TVector<string>; overload; static;
    class function GetFileSystemEntries(const Path, SearchPattern: string; const Predicate: TFilterPredicate): TVector<string>; overload; static;
    class function GetFileSystemEntries(const Path: string; const SearchOption: TSearchOption; const Predicate: TFilterPredicate): TVector<string>; overload; static;

    class function GetFiles(const Path: string): TVector<string>; overload; inline; static;
    class function GetFiles(const Path: string; const Predicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetFiles(const Path, SearchPattern: string): TVector<string>; overload; inline; static;
    class function GetFiles(const Path, SearchPattern: string; const Predicate: TFilterPredicate): TVector<string>; overload; inline; static;
    class function GetFiles(const Path, SearchPattern: string; const SearchOption: TSearchOption): TVector<string>; overload; static;
    class function GetFiles(const Path, SearchPattern: string; const SearchOption: TSearchOption; const Predicate: TFilterPredicate): TVector<string>; overload; static;
    class function GetFiles(const Path: string; const SearchOption: TSearchOption; const Predicate: TFilterPredicate): TVector<string>; overload; static;
  end;

implementation

{ TDirectoryEx }

class function TDirEx.GetDirectories(const Path,
  SearchPattern: string): TVector<string>;
begin
  Result := TDirectory.GetDirectories(Path, SearchPattern);
end;

class function TDirEx.GetDirectories(const Path: string;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetDirectories(Path, Predicate);
end;

class function TDirEx.GetDirectories(const Path: string): TVector<string>;
begin
  Result := TDirectory.GetDirectories(Path);
end;

class function TDirEx.GetDirectories(const Path, SearchPattern: string;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetDirectories(Path, SearchPattern, Predicate);
end;

class function TDirEx.GetDirectories(const Path: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetDirectories(Path, SearchOption, Predicate);
end;

class function TDirEx.GetFileSystemEntries(
  const Path: string): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(Path);
end;

class function TDirEx.GetFileSystemEntries(const Path: string;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(Path, Predicate);
end;

class function TDirEx.GetFileSystemEntries(const Path,
  SearchPattern: string): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(Path, SearchPattern);
end;

class function TDirEx.GetFileSystemEntries(const Path,
  SearchPattern: string; const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(Path, SearchPattern, Predicate);
end;

class function TDirEx.GetFiles(const Path,
  SearchPattern: string): TVector<string>;
begin
  Result := TDirectory.GetFiles(Path, SearchPattern);
end;

class function TDirEx.GetFiles(const Path: string;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFiles(Path, Predicate);
end;

class function TDirEx.GetFiles(const Path: string): TVector<string>;
begin
  Result := TDirectory.GetFiles(Path);
end;

class function TDirEx.GetFiles(const Path, SearchPattern: string;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFiles(Path, SearchPattern, Predicate);
end;

class function TDirEx.GetFiles(const Path: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFiles(Path, SearchOption, Predicate);
end;

class function TDirEx.GetFiles(const Path, SearchPattern: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFiles(Path, SearchPattern, SearchOption, Predicate);
end;

class function TDirEx.GetFiles(const Path, SearchPattern: string;
  const SearchOption: TSearchOption): TVector<string>;
begin
  Result := TDirectory.GetFiles(Path, SearchPattern, SearchOption);
end;

class function TDirEx.GetFileSystemEntries(const Path: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetFileSystemEntries(Path, SearchOption, Predicate);
end;

class function TDirEx.GetDirectories(const Path, SearchPattern: string;
  const SearchOption: TSearchOption;
  const Predicate: TFilterPredicate): TVector<string>;
begin
  Result := TDirectory.GetDirectories(Path, SearchPattern, SearchOption, Predicate);
end;

class function TDirEx.GetDirectories(const Path, SearchPattern: string;
  const SearchOption: TSearchOption): TVector<string>;
begin
  Result := TDirectory.GetDirectories(Path, SearchPattern, SearchOption);
end;

class function TDirEx.New(const ADirectory: string): TDirEx;
begin
  FDirectory := ADirectory;
end;

end.
