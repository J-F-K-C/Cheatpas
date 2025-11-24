program cheatpas;

uses
  SysUtils, IniFiles, StrUtils, Unix;

var
  cheatDir, editor: string;

procedure LoadConfig();
var
  config: TIniFile;
begin
  if FileExists('/etc/cheatpas/config.ini') then
  begin
    config := TIniFile.Create('/etc/cheatpas/config.ini');
    try
    cheatDir := config.ReadString('cheatpas', 'cheat_dir', ExpandFileName('~/cheats'));
    editor := config.ReadString('cheatpas', 'editor', 'nano');
    finally
    config.Free;
  end;
end
else
begin
  cheatDir := ExpandFileName('~/cheats');
  editor := 'nano';
end;

if not DirectoryExists(cheatDir) then
  if not CreateDir(cheatDir) then
    WriteLn('Warning: Could not create cheat_dir: ', cheatDir);
end;

procedure ListSheets();
var
  SR: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(cheatDir) + '*.txt', faAnyFile, SR) = 0 then
  begin
    repeat
      WriteLn(SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end
  else
    WriteLn('No cheatsheets found in ', cheatDir);
end;

procedure ShowSheet(sheetName: string);
var
  path: string;
  f: TextFile;
  line: string;
begin
  path := IncludeTrailingPathDelimiter(cheatDir) + sheetName + '.txt';
  if not FileExists(path) then
  begin
    WriteLn('Error: Cheatsheet "', sheetName, '" does not exist.');
    Exit;
  end;

  AssignFile(f, path);
  Reset(f);
  while not Eof(f) do
  begin
    ReadLn(f, line);
    WriteLn(line);
  end;
  CloseFile(f);
end;

procedure NewSheet(sheetName: string);
var
  path: string;
  f: TextFile;
begin
  path := IncludeTrailingPathDelimiter(cheatDir) + sheetName + '.txt';
  if FileExists(path) then
  begin
    WriteLn('Error: Cheatsheet "', sheetName, '" already exists.');
    Exit;
  end;

  AssignFile(f, path);
  Rewrite(f);
  CloseFile(f);

  {$IFDEF UNIX}
  fpSystem(editor + ' ' + path);
  {$ELSE}
  SysUtils.ExecuteProcess(editor, [path]);
  {$ENDIF}
end;

procedure EditSheet(sheetName: string);
var
  path: string;
  ret: longint;
begin
  path := IncludeTrailingPathDelimiter(cheatDir) + sheetName + '.txt';
  if not FileExists(path) then
  begin
    WriteLn('Error: Cheatsheet "', sheetName, '" does not exist.');
    Exit;
  end;

  {$IFDEF UNIX}
  ret := fpSystem(editor + ' ' + path);
  {$ELSE}
  ret := SysUtils.ExecuteProcess(editor, [path]);
  {$ENDIF}

  if ret <> 0 then
    WriteLn('Warning: Editor exited with code ', ret);
end;

procedure DeleteSheet(sheetName: string);
var
  path: string;
  answer: string;
begin
  path := IncludeTrailingPathDelimiter(cheatDir) + sheetName + '.txt';
  if not FileExists(path) then
  begin
    WriteLn('Error: Cheatsheet "', sheetName, '" does not exist.');
    Exit;
  end;

  Write('Really delete "', sheetName, '"? (y/n): ');
  ReadLn(answer);
  if (LowerCase(answer) = 'y') or (LowerCase(answer) = 'yes') then
  begin
    if DeleteFile(path) then
      WriteLn('Cheatsheet "', sheetName, '" deleted successfully.')
    else
      WriteLn('Error: Could not delete "', sheetName, '".');
  end
  else
    WriteLn('Delete cancelled.');
end;

procedure SearchSheets(searchTerm: string);
var
  SR: TSearchRec;
  path: string;
  f: TextFile;
  line: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(cheatDir) + '*.txt', faAnyFile, SR) <> 0 then
  begin
    WriteLn('No cheatsheets found.');
    Exit;
  end;

  repeat
    path := IncludeTrailingPathDelimiter(cheatDir) + SR.Name;
    AssignFile(f, path);
    Reset(f);
    while not Eof(f) do
    begin
      ReadLn(f, line);
      if Pos(searchTerm, line) > 0 then
        WriteLn(SR.Name, ': ', line);
    end;
    CloseFile(f);
  until FindNext(SR) <> 0;
  FindClose(SR);
end;

procedure ShowHelp();
begin
  WriteLn('cheatpas - A Pascal cheatsheet CLI tool');
  WriteLn('Usage: cheatpas <command> [sheet_name]');
  WriteLn('');
  WriteLn('Commands:');
  WriteLn('  list                List all cheatsheets');
  WriteLn('  show <name>         Show a cheatsheet');
  WriteLn('  new <name>          Create a new cheatsheet');
  WriteLn('  edit <name>         Edit an existing cheatsheet');
  WriteLn('  delete <name>       Delete a cheatsheet');
  WriteLn('  search <text>       Search inside all cheatsheets');
end;

begin
  LoadConfig();

  if ParamCount < 1 then
  begin
    ShowHelp();
    Halt(1);
  end;

  case ParamStr(1) of
    'list': ListSheets();
    'show':
      if ParamCount < 2 then WriteLn('Usage: cheatpas show <sheet_name>')
      else ShowSheet(ParamStr(2));
    'new':
      if ParamCount < 2 then WriteLn('Usage: cheatpas new <sheet_name>')
      else NewSheet(ParamStr(2));
    'edit':
      if ParamCount < 2 then WriteLn('Usage: cheatpas edit <sheet_name>')
      else EditSheet(ParamStr(2));
    'delete':
      if ParamCount < 2 then WriteLn('Usage: cheatpas delete <sheet_name>')
      else DeleteSheet(ParamStr(2));
    'search':
      if ParamCount < 2 then WriteLn('Usage: cheatpas search <text>')
      else SearchSheets(ParamStr(2));
  else
    WriteLn('Unknown command: ', ParamStr(1));
  ShowHelp();
  end;
end.
