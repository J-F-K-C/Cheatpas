program cheatpas;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, IniFiles, Process;

type
  TConfig = record
    CheatDir: string;
    Editor: string;
    Color: Boolean;
  end;

function LoadConfig: TConfig;
var
  Ini: TIniFile;
  ConfigDir, IniPath: string;
begin
  // standardparameter
  Result.CheatDir := GetEnvironmentVariable('HOME') + '/cheats';
  Result.Editor := 'nano';
  Result.Color := True;

  // conf file directory
  ConfigDir := GetEnvironmentVariable('HOME') + '/.config/cheatpas';
  IniPath := ConfigDir + '/config.conf';

  // create dir at first start
  if not DirectoryExists(ConfigDir) then
    ForceDirectories(ConfigDir);

  // create configfile at first start
  if not FileExists(IniPath) then
  begin
    Ini := TIniFile.Create(IniPath);
    try
      Ini.WriteString('cheatpas', 'cheat_dir', Result.CheatDir);
      Ini.WriteString('cheatpas', 'editor', Result.Editor);
      Ini.WriteBool('cheatpas', 'color', Result.Color);
    finally
      Ini.Free;
    end;
  end;

  Ini := TIniFile.Create(IniPath);
  try
    Result.CheatDir := Ini.ReadString('cheatpas', 'cheat_dir', Result.CheatDir);
    Result.Editor   := Ini.ReadString('cheatpas', 'editor', Result.Editor);
    Result.Color    := Ini.ReadBool('cheatpas', 'color', Result.Color);
  finally
    Ini.Free;
  end;

  // editor fallback: $EDITOR
  if GetEnvironmentVariable('EDITOR') <> '' then
    Result.Editor := GetEnvironmentVariable('EDITOR');
end;

procedure PrintColor(const S: string; Enabled: Boolean; ColorCode: string);
begin
  if Enabled then
    WriteLn(ColorCode + S + #27'[0m')
  else
    WriteLn(S);
end;

procedure ListCheats(const Dir: string; Color: Boolean);
var
  SR: TSearchRec;
begin
  if not DirectoryExists(Dir) then
  begin
    WriteLn('Directory not found: ', Dir);
    Exit;
  end;

  if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.txt', faAnyFile, SR) = 0 then
  begin
    repeat
      PrintColor(ChangeFileExt(SR.Name, ''), Color, #27'[33m'); // yellow
    until FindNext(SR) <> 0;
    FindClose(SR);
  end
  else
    WriteLn('No Cheatsheets found.');
end;

procedure ShowCheat(const Dir, Name: string; Color: Boolean);
var
  Full: string;
  SL: TStringList;
begin
  Full := IncludeTrailingPathDelimiter(Dir) + Name + '.txt';

  if not FileExists(Full) then
  begin
    WriteLn('Cheatsheet not found: ', Name);
    Exit;
  end;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(Full);
    if Color then
      PrintColor('=== ' + Name + ' ===', True, #27'[32m') //green
    else
      WriteLn('=== ', Name, ' ===');

    WriteLn(SL.Text);
  finally
    SL.Free;
  end;
end;

procedure NewCheat(const Dir, Editor, Name: string);
var
  Full: string;
  P: TProcess;
begin
  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  Full := IncludeTrailingPathDelimiter(Dir) + Name + '.txt';

  if not FileExists(Full) then
  begin
    with TStringList.Create do
    try
      Add('# ' + Name);
      Add('');
      SaveToFile(Full);
    finally
      Free;
    end;
  end;

  P := TProcess.Create(nil);
  try
    P.Executable := Editor;
    P.Parameters.Add(Full);
    P.Options := P.Options + [poWaitOnExit];
    P.Execute;
  finally
    P.Free;
  end;

  WriteLn('Processed: ', Full);
end;

procedure SearchCheats(const Dir, Query: string; Color: Boolean);
var
  SR: TSearchRec;
  SL: TStringList;
  Full: string;
  i: integer;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*.txt', faAnyFile, SR) = 0 then
  begin
    repeat
      Full := IncludeTrailingPathDelimiter(Dir) + SR.Name;
      SL := TStringList.Create;
      try
        SL.LoadFromFile(Full);
        for i := 0 to SL.Count - 1 do
        begin
          if Pos(LowerCase(Query), LowerCase(SL[i])) > 0 then
          begin
            PrintColor(ChangeFileExt(SR.Name, '') + ':', Color, #27'[34m'); // blue
            WriteLn('  ', SL[i]);
            WriteLn;
            Break;
          end;
        end;
      finally
        SL.Free;
      end;
    until FindNext(SR) <> 0;

    FindClose(SR);
  end
  else
    WriteLn('No Cheatsheets found.');
end;

var
  CFG: TConfig;

begin
  CFG := LoadConfig;

  if ParamCount = 0 then
    ListCheats(CFG.CheatDir, CFG.Color)
  else if (ParamStr(1) = 'list') then
    ListCheats(CFG.CheatDir, CFG.Color)
  else if (ParamStr(1) = 'show') and (ParamCount >= 2) then
    ShowCheat(CFG.CheatDir, ParamStr(2), CFG.Color)
  else if (ParamStr(1) = 'new') and (ParamCount >= 2) then
    NewCheat(CFG.CheatDir, CFG.Editor, ParamStr(2))
  else if (ParamStr(1) = 'search') and (ParamCount >= 2) then
    SearchCheats(CFG.CheatDir, ParamStr(2), CFG.Color)
  else
  begin
    WriteLn('Usage:');
    WriteLn('  cheatpas list');
    WriteLn('  cheatpas show <name>');
    WriteLn('  cheatpas new <name>');
    WriteLn('  cheatpas search <text>');
  end;
end.

