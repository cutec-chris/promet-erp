unit uminuteframe;
{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 01.06.2013
*******************************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, DbCtrls, StdCtrls,
  Buttons, ComCtrls, Spin, EditBtn, uExtControls, DBZVDateTimePicker;

type
  TfMinuteFrame = class(TFrame)
    Bevel3: TBevel;
    eName: TDBMemo;
    Label3: TLabel;
    Label4: TLabel;
    lname: TLabel;
    mInfo: TDBMemo;
    Panel4: TPanel;
    Panel6: TPanel;
    pComponents: TPanel;
    pcPages: TExtMenuPageControl;
    sbMenue: TSpeedButton;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    tsInfo: TTabSheet;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

