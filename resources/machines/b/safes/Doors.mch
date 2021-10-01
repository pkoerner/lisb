MACHINE           Doors
/* This B Machine is taken from the book: The B-Method: An Introduction
   (Editor Palgrave, ISBN 0-33-79284-X, Year 2002) by Steve Schneider.
   This machine is distributed with kind permission by Steve Schneider.
*/
SETS              DOOR; POSITION = {open, closed}

VARIABLES         position

INVARIANT         position : DOOR --> POSITION

INITIALISATION    position := DOOR * {closed}

OPERATIONS

  opening(dd) = 
  PRE dd : DOOR THEN position(dd) := open END;

  closedoor(dd) = 
  PRE dd : DOOR THEN position(dd) := closed END

END
