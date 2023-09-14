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
