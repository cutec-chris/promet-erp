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
Created 01.06.2006
*******************************************************************************}
unit uprometframesinplace;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ActnList, ExtCtrls,
  uPrometFrames, uExtControls;
type

  { TPrometInplaceFrame }

  TPrometInplaceFrame = class(TExtControlFrame)
  public
    procedure SetRights(Editable : Boolean);virtual;abstract;
    procedure ArrangeToolBar(Control: TPanel; ActionList: TActionList; aName: string
      );
  published
    property OnEnter;
    property OnExit;
  end;
implementation
{$R *.lfm}

{ TPrometInplaceFrame }

procedure TPrometInplaceFrame.ArrangeToolBar(Control: TPanel;ActionList : TActionList;aName: string);
begin
  Control.Align:=alRight;
end;

end.
