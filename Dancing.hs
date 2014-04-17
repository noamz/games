{- 
 A variation on Conway's game of "Square Dancing"
-}

module Dancing where

{-
 The game is best played with four people, using two strands of rope, in three rounds.  At the beginning of the game, the players arrange themselves in a square, and will then move around the square according to the rules described below.  In these instructions I will refer to the players by their *current* position on the square, which may be respectively NW, NE, SW, or SE.  In an actual game, obvious accomodations can be made for playing with fewer than four people (though preferably at least two), in which case one person may simultaneously occupy two or more positions.

 Round 1 ("Preparation"). The players begin by using some procedure to agree on a rational number q. For example, they might take the average of the birthdays (DD/MM or MM/DD) of each player, or they might each write down a rational and then take the second smallest one, etc. Then the players arrange themselves into the "0 tangle", with the pair NW-NE standing opposite the pair SW-SE, each pair holding a strand of rope between them.

 Round 2 ("Dance"). The players use a series of "crossing" moves (or "C-move" for short) in order to go from the initial position to the tangle representing the rational number q. During a C-move, either the pair of players SW-SE exchanges positions, or the pair NE-SE exchanges positions. In either case, in order for the players to exchange positions while holding onto their ropes, one will have to crouch/limbo under the other player's rope (making for a total of four possible C-moves).
-}

data CMove = SWSE | SESW | NESE | SENE
  deriving Show

dance :: Rational -> [CMove]
dance q = reverse (map neg $ down q)
  where
    down :: Rational -> [CMove]
    down 0 = []
    down q | q >= 1 = SENE:down (q-1)
    down q | q <= -1 = NESE:down (q+1)
    down q | q > -1 && q < 0 = SWSE:down (q/(1+q))
    down q | q < 1 && q > 0 = SESW:down (q/(1-q))

    neg :: CMove -> CMove
    neg SWSE = SESW
    neg SESW = SWSE
    neg SENE = NESE
    neg NESE = SENE

{-
 Round 3 ("Undance"). The players use a series of "PSL(2,Z)" moves (or "P-move" for short) in order to go from the tangle representing q back down to the initial position 0.  A P-move is either an "add", which is the same thing as a C-move NE-SE with SE crossing under NE, or else a "turn", which consists of all four players simultaneously moving one position clockwise around the square.
-}
    
data PMove = Add | Turn
  deriving Show
undance :: Rational -> [PMove]
undance 0 = []
undance q | q < 0 = Add:undance (q+1)
undance q | q > 0 = Turn:undance (-1/q)

{-
 References:
 * http://ncatlab.org/nlab/show/continued+fraction
 * John Conway, "An enumeration of knots and links and some of their algebraic properties"
 * Kauffman & Lambropoulou, "On the classification of rational tangles" (see in particular Section 6)
 * Dan Piponi, "Untangling with continued fractions" (http://blog.sigfpe.com/2008/08/untangling-with-continued-fractions.html)
-}
