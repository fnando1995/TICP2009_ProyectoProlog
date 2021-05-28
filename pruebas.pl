suma(0,0).
suma(N,R):- A is N-1,suma(A,Ra),R is N+Ra.