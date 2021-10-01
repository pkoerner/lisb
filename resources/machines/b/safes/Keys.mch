MACHINE           Keys
/* This B Machine is taken from the book: The B-Method: An Introduction
   (Editor Palgrave, ISBN 0-33-79284-X, Year 2002) by Steve Schneider.
   This machine is distributed with kind permission by Steve Schneider.
*/
SETS              KEY

VARIABLES         keys

INVARIANT         keys <: KEY

INITIALISATION    keys := {}

OPERATIONS

  insertkey(kk) =
  PRE kk : KEY THEN keys := keys \/ {kk} END;

  removekey(kk) =
  PRE kk : KEY THEN keys := keys - {kk} END

END
