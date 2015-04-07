unit UDBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil;

type

  { TMDDBConnection }

  TMDDBConnection = class(TDataModule)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
  end;

var
  MDDBConnection: TMDDBConnection;

implementation

{$R *.lfm}

end.

