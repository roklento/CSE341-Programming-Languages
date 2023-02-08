%knowledge
schedule(canakkale,erzincan,6).
schedule(erzincan,antalya,3).
schedule(antalya,izmir,2).
schedule(antalya,diyarbakir,4).
schedule(izmir,ankara,6).
schedule(izmir,istanbul,2).
schedule(istanbul,rize,4).
schedule(istanbul,ankara,1).
schedule(rize,ankara,5).
schedule(van,ankara,4).
schedule(diyarbakir,ankara,8).
schedule(van,gaziantep,3).

schedule(gaziantep,kirikkale,7).
schedule(kirikkale,ankara,1).
schedule(kirikkale,samsun,5).

schedule(erzincan,canakkale,6).
schedule(antalya,erzincan,3).
schedule(izmir,antalya,2).
schedule(diyarbakir,antalya,4).
schedule(ankara,izmir,6).
schedule(istanbul,izmir,2).
schedule(rize,istanbul,4).
schedule(ankara,istanbul,1).
schedule(ankara,rize,5).
schedule(ankara,van,4).
schedule(ankara,diyarbakir,8).
schedule(gaziantep,van,3).

schedule(kirikkale,gaziantep,7). 
schedule(ankara,kirikkale,1).
schedule(samsun,kirikkale,5).
%rules


connection(Start,End,C):-
	schedule(Start,End,C).

connection(Start,End,C):-
	schedule(Start,Temp,C1),
	connection(Temp,End,C2),
	C is C1+C2.