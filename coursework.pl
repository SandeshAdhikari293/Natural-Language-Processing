%Author: Sandesh Adhikari
%Student ID: 2035469
%
%
%Sentence structure. Noun Phrase followed by a Verb Phrase.
s(s(NP, VP)) --> np(NP, Plurality, Number, Animacy, subject), vp(VP, Plurality, Number, _, Animacy).

%Noun Phrases.
%
%Determiner followed by a noun.
np(np(Det, nbar(Noun)), Plurality, _, Animacy, _) --> det(Det, Plurality), n(Noun, Plurality, Animacy).
%Determiner followed by an adjective phrase.
np(np(Det, Adj), Plurality, _, Animacy, _) --> det(Det, Plurality), jp(Adj, Plurality, Animacy).
%A noun phrase can also just be a pronoun.
np(np(Pro), Plurality, Number, _, Subject) --> pro(Pro, Plurality, Number, Subject).
%
%Indentical Noun Phrases but with a Prepositional Phrase
np(np(Det, nbar(Noun), PP), Plurality, _, Animacy, _) --> det(Det, Plurality), n(Noun, Plurality, Animacy), pp(PP).
np(np(Det, Adj, PP), Plurality, _, Animacy,_) --> det(Det ,Plurality), jp(Adj, Plurality, Animacy) , pp(PP).
np(np(Pro, PP), Plurality, Number, _, Subject) --> pro(Pro, Plurality, Number, Subject), pp(PP).

%Verb Phrases
%
%Transitive Verb followed by a Noun Phrase
vp(vp(Verb, NP), Plurality, Number, _, Animacy) --> tv(Verb, Plurality, Number, Animacy), np(NP, _, Number, _, object).
%Intransitive Verb
vp(vp(Verb), Plurality, Number, _, Animacy) --> iv(Verb, Plurality, Number, Animacy).
%Intransitive Verb followed by Prepositional Phrase
vp(vp(Verb, PP), Plurality, Number, PP, Animacy) --> iv(Verb, Plurality, Number, Animacy), pp(PP).

%Prepositional Phrase. Preposition followed by a Noun Phrase.
pp(pp(Prep, NP)) --> prep(Prep), np(NP, _, _, _,_).
%Pronoun.
pro(pro(Word), Plurality, Number, Subject) --> [Word], {lex(Word, pro, Plurality, Number, Subject)}.
%Preposition.
prep(prep(Word)) --> [Word], {lex(Word, prep)}.
%Determiner.
det(det(Word), T) --> [Word], {lex(Word, det, T)}.
%Noun.
n(n(Word), T, Animacy) --> [Word], {lex(Word, n, T, Animacy)}.

%Adjective.
adj(Word) --> [Word], {lex(Word, adj)}.
%Adjective Phrase
%
%Adjective followed by a Noun.
jp(jp(adj(Word), Noun), Plurality, Animacy) --> adj(Word), n(Noun, Plurality, Animacy).
%Recursive function allowing stacked/chained adjectives.
jp(nbar(jp(adj(Word), A)), Plurality, Animacy) --> adj(Word), jp(A, Plurality, Animacy).

%Verbs
%
%Transitive
tv(v(Word), Plurality, N, Animacy) --> [Word], {lex(Word, tv, Plurality, N, Animacy)}.
%Intransitive.
iv(v(Word), Plurality, N, Animacy) --> [Word], {lex(Word, iv, Plurality, N, Animacy)}.

%Lexicon
%
%
%Pronouns.
lex(i,pro,singular,1,subject).
lex(you,pro,singular,2,subject).
lex(he,pro,singular,3,subject).
lex(she,pro,singular,3,subject).
lex(it,pro,singular,3,subject).
lex(we,pro,plural,1,subject).
lex(you,pro,plural,2,subject).
lex(they,pro,plural,3,subject).
lex(me,pro,singular,1,object).
lex(you,pro,singular,2,object).
lex(him,pro,singular,3,object).
lex(her,pro,singular,3,object).
lex(it,pro,singular,3,object).
lex(us,pro,plural,1,object).
lex(you,pro,plural,2,object).
lex(them,pro,plural,3,object).
%
%Transitive Verbs.
lex(know,tv,singular,1, animate).
lex(know,tv,singular,2, animate).
lex(knows,tv,singular,3, animate).
lex(know,tv,plural,_, animate).
lex(see,tv,singular,1, animate).
lex(see,tv,singular,2, animate).
lex(sees,tv,singular,3, animate).
lex(see,tv,plural,_, animate).
lex(hire,tv,singular,1, animate).
lex(hire,tv,singular,2, animate).
lex(hires,tv,singular,3, animate).
lex(hire,tv,plural,_, animate).
%
%Intransitive Verbs.
lex(fall,iv,singular,1, _).
lex(fall,iv,singular,2, _).
lex(falls,iv,singular,3, _).
lex(fall,iv,plural,_,_).
lex(sleep,iv,singular,1, animate).
lex(sleep,iv,singular,2, animate).
lex(sleeps,iv,singular,3, animate).
lex(sleep,iv,plural,_, animate).
%
%Determiners.
lex(the,det,_).
lex(a,det,singular).
lex(two,det,plural).
%
%Nouns.
lex(man,n,singular, animate).
lex(woman,n,singular, animate).
lex(apple,n,singular, inanimate).
lex(chair,n,singular, inanimate).
lex(room,n,singular, inanimate).
lex(men,n,plural, animate).
lex(women,n,plural, animate).
lex(apples,n,plural, inanimate).
lex(chairs,n,plural, inanimate).
lex(rooms,n,plural, inanimate).
%
%Prepositions.
lex(on,prep).
lex(in,prep).
lex(under,prep).
%
%Adjectives.
lex(old,adj).
lex(young,adj).
lex(red,adj).
lex(short,adj).
lex(tall,adj).




%----------| TESTS |----------%
%
% 1)
% Input: ?- s(Tree, [the,woman,sees,the,apples], []).
% Output: Tree = s(np(det(the), nbar(n(woman))), vp(v(sees), np(det(the), nbar(n(apples)))))
%
% 2)
% Input: ?- s(Tree, [a,woman,knows,him], []).
% Output:Tree = s(np(det(a), nbar(n(woman))), vp(v(knows), np(pro(him))))
%
% *3)
% Input: ?- s(Tree, [two,woman,hires,a,man], []).
% Output: false.
%
% 4)
% Input: ?- s(Tree, [two,women,hire,a,man], []).
% Output:Tree = s(np(det(two), nbar(n(women))), vp(v(hire), np(det(a), nbar(n(man)))))
%
% 5)
% Input: ?- s(Tree, [she,knows,her], [])
% Output: Tree = s(np(pro(she)), vp(v(knows), np(pro(her))))
%
% *6)
% Input: ?- s(Tree, [she,know,the,man], []).
% Output: false.
%
% *7)
% Input: ?- s(Tree, [us,see,the,apple], []).
% Output: false.
%
% 8)
% Input: ?- s(Tree, [we,see,the,apple], []).
% Output: Tree = s(np(pro(we)), vp(v(see), np(det(the), nbar(n(apple)))))
%
% 9)
% Input: ?- s(Tree, [i,know,a,short,man], []).
% Output: Tree = s(np(pro(i)), vp(v(know), np(det(a), jp(adj(short), n(man)))))
%
% *10)
% Input: ?- s(Tree, [he,hires,they], []).
% Output: false.
%
% 11)
% Input: ?- s(Tree, [two,apples,fall], [])
% Output: Tree = s(np(det(two), nbar(n(apples))), vp(v(fall)))
%
% 12)
% Input: ?- s(Tree, [the,apple,falls], []).
% Output: Tree = s(np(det(the), nbar(n(apple))), vp(v(falls)))
%
% 13)
% Input: ?- s(Tree, [the,apples,fall], []).
% Output: Tree = s(np(det(the), nbar(n(apples))), vp(v(fall)))
%
% 14)
% Input: ?- s(Tree, [i,sleep], []).
% Output: Tree = s(np(pro(i)), vp(v(sleep)))
%
% 15)
% Input: ?- s(Tree, [you,sleep], []).
% Output: Tree = s(np(pro(you)), vp(v(sleep)))
%
% 16)
% Input: ?- s(Tree, [she,sleeps], []).
% Output: Tree = s(np(pro(she)), vp(v(sleeps)))
%
% *17)
% Input: ?- s(Tree, [he,sleep], []).
% Output: false.
%
% *18)
% Input: ?- s(Tree, [them,sleep], []).
% Output: false.
%
% *19)
% Input: ?- s(Tree,[a,men,sleep],[]).
% Output: false.
%
% *20)
% Input: ?- s(Tree,[the,tall,woman,sees,the,red],[])
% Output: false.
%
% 21)
% Input: ?- s(Tree,[the,young,tall,man,knows,the,old,short,woman],[]).
% Output: Tree = s(np(det(the), nbar(jp(adj(young), jp(adj(tall), n(man))))), vp(v(knows), np(det(the), nbar(jp(adj(old), jp(adj(short), n(woman)))))))
%
% *22)
% Input: ?- s(Tree, [a,man,tall,knows,the,short,woman], []).
% Output: false.
%
% 23)
% Input: ?- s(Tree, [a,man,on,a,chair,sees,a,woman,in,a,room], []).
% Output: Tree = s(np(det(a), nbar(n(man)), pp(prep(on), np(det(a), nbar(n(chair))))), vp(v(sees), np(det(a), nbar(n(woman)), pp(prep(in), np(det(a), nbar(n(room)))))))
%
% *24)
% Input: ?- s(Tree, [a,man,on,a,chair,sees,a,woman,a,room,in], []).
% Output: false.
%
% 25)
% Input: ?- s(Tree,
% [the,tall,young,woman,in,a,room,on,the,chair,in,a,room,in,the,room,sees,the,red,apples,under,the,chair],[]).
% Output: Tree = s(np(det(the), nbar(jp(adj(tall), jp(adj(young), n(woman)))), pp(prep(in), np(det(a), nbar(n(room)), pp(prep(on), np(det(the), nbar(n(chair)), pp(prep(in), np(det(a), nbar(n(...)), pp(prep(...), np(..., ...))))))))), vp(v(sees), np(det(the), jp(adj(red), n(apples)), pp(prep(under), np(det(the), nbar(n(chair)))))))
%
% 26)
% Input: ?- s(Tree, [the,woman,sees,the,apples], []).
% Output: Tree = s(np(det(the), nbar(n(woman))), vp(v(sees), np(det(the), nbar(n(apples)))))
%
% 27)
% Input: ?- s(Tree, [a,woman,knows,him], []).
% Output: Tree = s(np(det(a), nbar(n(woman))), vp(v(knows), np(pro(him))))
%
% 28)
% Input: ?- s(Tree, [the,man,sleeps], [])
% Output: Tree = s(np(det(the), nbar(n(man))), vp(v(sleeps)))
%
% *29)
% Input: ?- s(Tree, [the,room,sleeps], []).
% Output: false.
%
% *30)
% Input: ?- s(Tree, [the,apple,sees,the,chair], []).
% Output: false.
%
% *31)
% Input: ?- s(Tree, [the,room,knows,the,man], []).
% Output: false.
%
% 32)
% Input: ?- s(Tree, [the,apple,falls], []).
% Output: Tree = s(np(det(the), nbar(n(apple))), vp(v(falls)))
%
% 33)
% Input: ?- s(Tree, [the,man,falls], []).
% Output: Tree = s(np(det(the), nbar(n(man))), vp(v(falls)))
%
