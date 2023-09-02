unit eclbr.result.pair.container;

interface

uses
  SysUtils,
  eclbr.result.pair;

type
  ICrp<S, F> = interface
    ['{C0218DB7-DB2D-486C-84D7-0C72397B9019}']
    function Grp: TResultPair<S, F>;
  end;

  TCrp<S, F> = class(TInterfacedObject, ICrp<S, F>)
  private
    PResultPair: ^TResultPair<S, F>;
    FResultPair: TResultPair<S, F>;
  public
    class function New: ICrp<S, F>;
    constructor Create;
    destructor Destroy; override;
    function Grp: TResultPair<S, F>;
  end;

implementation

constructor TCrp<S, F>.Create;
begin
  PResultPair := @FResultPair;
  PResultPair^.Success(Default(S));
end;

destructor TCrp<S, F>.Destroy;
begin
  PResultPair^.Dispose;
  PResultPair := nil;
  inherited;
end;

function TCrp<S, F>.Grp: TResultPair<S, F>;
begin
  Result := PResultPair^;
end;

class function TCrp<S, F>.New;
begin
  Result := TCrp<S, F>.Create;
end;

end.
