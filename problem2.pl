%first line
connected(new_elmarg,elmarg).
connected(elmarg,ezbet_elnakhl).
connected(ezbet_elnakhl,ain_shams).
connected(ain_shams,elmatareyya).
connected(elmatareyya,helmeyet_elzaitoun).
connected(helmeyet_elzaitoun,hadayeq_elzaitoun).
connected(hadayeq_elzaitoun,saray_elqobba).
connected(saray_elqobba,hammamat_elqobba).
connected(hammamat_elqobba,kobri_elqobba).
connected(kobri_elqobba,manshiet_elsadr).
connected(manshiet_elsadr,eldemerdash).
connected(eldemerdash,ghamra).
connected(ghamra,alshohadaa).
connected(alshohadaa,urabi).
connected(urabi,nasser).
connected(nasser,sadat).
connected(sadat,saad_zaghloul).
connected(saad_zaghloul, alsayyeda_zeinab).
connected(alsayyeda_zeinab,elmalek_elsaleh).
connected(elmalek_elsaleh,margirgis).
connected(margirgis,elzahraa).
connected(elzahraa,dar_elsalam).
connected(dar_elsalam,hadayeq_elmaadi).
connected(hadayeq_elmaadi,maadi).
connected(maadi,thakanat_elmaadi).
connected(thakanat_elmaadi,tora_elbalad).
connected(tora_elbalad,kozzika).
connected(kozzika,tora_elasmant).
connected(tora_elasmant,elmaasara).
connected(elmaasara,hadayeq_helwan).
connected(hadayeq_helwan,wadi_hof).
connected(wadi_hof,helwan_university).
connected(helwan_university,ain_helwan).
connected(ain_helwan,helwan).
%second line
connected(shobra_elkheima,koliet_elzeraa).
connected(koliet_elzeraa,mezallat).
connected(mezallat,khalafawy).
connected(khalafawy,sainte_teresa).
connected(sainte_teresa,road_elfarag).
connected(road_elfarag,massara).
connected(massara,alshohadaa).
connected(alshohadaa,ataba).
connected(ataba,naguib).
connected(naguib,sadat).
connected(sadat,opera).
connected(opera,dokki).
connected(dokki,bohooth).
connected(bohooth,cairo_university).
connected(cairo_university,faisal).
connected(faisal,giza).
connected(giza,omm_elmisryeen).
connected(omm_elmisryeen,sakiat_mekki).
connected(sakiat_mekki,elmounib).


%task 1
path(S,E,N,G2):-
path2(S,E,N,[],G,0,_),
reverse(G,G2),!.

path2(E,E,N,G,G,X,_):-
 (X=N),

!.

path2(S,E,N,V,G,X,_):-
connected(S,W),
\+ (member([S,W],V)),
Xnew is X+1,

path2(W,E,N,[[S,W]|V],G,Xnew,_).






%task 2

nstations(S,L):-
      stationsone(S,[],L1),
      stationstwo(S,[],L2),
	L is L1+ L2,
      %add(L1,L2,L),
!.
stationsone(S,Visited,L):-
         connected(S,Z),
         not(member([S,Z],Visited)),
         stationsone(S,[[S,Z]|Visited],L).
stationsone(_,Visited,L):-
      length(Visited,L),!.

stationstwo(S,Visited2,L):-
         connected(Z,S),
         not(member([Z,S],Visited2)),
         stationstwo(S,[[Z,S]|Visited2],L).
stationstwo(_,Visited2,L):-
      length(Visited2,L),!.
add(L1,L2,L):-
L is L1+L2.




%set_prolog_flag(answer_write_options,
  %                 [ quoted(true),
   %                  portray(true),
    %                 spacing(next_argument)
     %              ]).





%task3
cost(X,Y,F):-
 calc(X,Y,N),
((N=<7)->(F=3);(N>7,N<16)->(F=5);(F=7)).

calc(X,Y,N):-
moving(X,Y,1,N),!.

calc(X,Y,N):-
moving(Y,X,1,N),!.

moving(X,Y,Num,N):-
connected(Y,X),
N is Num ,!.


moving(X,Y,Num,N):-
connected(Y,Z),
NewN is Num+1,
moving(X,Z,NewN,N),!.





%task 4
checkPath(X):-

getfirst(X,A),


reverse(X,G),

getsec(G,B),

path(A,B,_,G2),
X=G2,


!.




%checkPath([[A,B],[C,D]]):-
%B=@=C,
%connected(A,B),
%connected(C,D),!.




getfirst([[A,_]|_],S):-

S = A ,!.
getsec([[_,A]|_],S):-

S = A ,!.
same([], []).

same([H1|R1], [H2|R2]):-
    H1 = H2,
    same(R1, R2).






