{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fplilpackage; 

interface

uses
  FPLIL, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('FPLIL', @FPLIL.Register);
end; 

initialization
  RegisterPackage('fplilpackage', @Register); 
end.
