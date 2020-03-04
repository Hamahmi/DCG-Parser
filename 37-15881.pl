/* - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~
Author      :   Abdelrahman Gharib El-Hamahmi
ID          :   37-15881
Tutorial    :   T-16

------------------------------------------------
Disclaimer  :   Base code taken from (Wikipedia)
[https://en.wikipedia.org/wiki/Definite_clause_grammar#Parsing_with_DCGs]
```
sentence(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase(np(D,N)) --> det(D), noun(N).
verb_phrase(vp(V,NP)) --> verb(V), noun_phrase(NP).
det(d(the)) --> [the].
det(d(a)) --> [a].
noun(n(bat)) --> [bat].
noun(n(cat)) --> [cat].
verb(v(eats)) --> [eats].
```
The rest of the code is completely written by the author
Most of the calssifications in the corpus were made according to
[https://en.wikipedia.org/wiki/English_grammar]
------------------------------------------------

Parser for a definite clause grammar (DCG) of English-Light.
- ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - */


%%%%%%%%%%%%%%%%%%%%%%% Main Query %%%%%%%%%%%%%%%%%%%%%%%
s(S) --> sentences(S).


%%%%%%%%%%%%%%%%%%%%%%% Sentences %%%%%%%%%%%%%%%%%%%%%%%
sentences(S)            --> sentence(S).
sentences(ss(S, C, SS)) --> sentence(S), conj(C), sentences(SS).

sentence(s(NP,VP))      --> noun_phrases(NP), verb_phrases(VP).


%%%%%%%%%%%%%%%%%%%%%%% Noun Phrases %%%%%%%%%%%%%%%%%%%%%%%
noun_phrases(NP)                    --> noun_phrase(NP).
noun_phrases(nps(NP, C, NPS))       --> noun_phrase(NP), conj(C), noun_phrases(NPS).

noun_phrase(np(N))                  --> noun(N).
noun_phrase(np(PR))                 --> pronoun(PR).
noun_phrase(np(D, N))               --> det(D), noun(N).
noun_phrase(np(AS, N))              --> adjs(AS), noun(N).
noun_phrase(np(D, AS, N))           --> det(D), adjs(AS), noun(N).
noun_phrase(np(N, RP, S))           --> noun(N), relpronoun(RP), sentence(S).
noun_phrase(np(PR, RP, S))          --> pronoun(PR), relpronoun(RP), sentence(S).
noun_phrase(np(N, RPV, VP))         --> noun(N), relpronounv(RPV), verb_phrases(VP).
noun_phrase(np(PR, RPV, VP))        --> pronoun(PR), relpronounv(RPV), verb_phrases(VP).
noun_phrase(np(D, N, RP, S))        --> det(D), noun(N), relpronoun(RP), sentence(S).
noun_phrase(np(D, N, RPV, VP))      --> det(D), noun(N), relpronounv(RPV), verb_phrases(VP).
noun_phrase(np(AS, N, RP, S))       --> adjs(AS), noun(N), relpronoun(RP), sentence(S).
noun_phrase(np(AS, N, RPV, VP))     --> adjs(AS), noun(N), relpronounv(RPV), verb_phrases(VP).
noun_phrase(np(D, AS, N, RP, S))    --> det(D), adjs(AS), noun(N), relpronoun(RP), sentence(S).
noun_phrase(np(D, AS, N, RPV, VP))  --> det(D), adjs(AS), noun(N), relpronounv(RPV), verb_phrases(VP).


%%%%%%%%%%%%%%%%%%%%%%% Verb Phrases %%%%%%%%%%%%%%%%%%%%%%%
verb_phrases(VP)                        --> verb_phrase(VP).
verb_phrases(vps(VP, C, VPS))           --> verb_phrase(VP), conj(C), verb_phrases(VPS).

verb_phrase(vp(V))                      --> verb(V).
verb_phrase(vp(V, PS))                  --> verb(V), preposes(PS).
verb_phrase(vp(V, OS))                  --> verb(V), objects(OS).
verb_phrase(vp(ADS, V))                 --> advs(ADS), verb(V).
verb_phrase(vp(V, OS, PS))              --> verb(V), objects(OS), preposes(PS).
verb_phrase(vp(ADS, V, PS))             --> advs(ADS), verb(V), preposes(PS).
verb_phrase(vp(ADS, V, OS))             --> advs(ADS), verb(V), objects(OS).
verb_phrase(vp(V, OSF, OSS))            --> verb(V), objects(OSF), objects(OSS).
verb_phrase(vp(ADS, V, OS, PS))         --> advs(ADS), verb(V), objects(OS), preposes(PS).
verb_phrase(vp(V, OSF, OSS, PS))        --> verb(V), objects(OSF), objects(OSS), preposes(PS).
verb_phrase(vp(ADS, V, OSF, OSS))       --> advs(ADS), verb(V), objects(OSF), objects(OSS).
verb_phrase(vp(ADS, V, OSF, OSS, PS))   --> advs(ADS), verb(V), objects(OSF), objects(OSS), preposes(PS).


%%%%%%%%%%%%%%%%%%%%%%% Objects %%%%%%%%%%%%%%%%%%%%%%%
objects(O)                      --> object(O).
objects(objs(O, C, OS))         --> object(O), conj(C), objects(OS).

object(obj(N))                  --> noun(N).
object(obj(D,N))                --> det(D), noun(N).
object(obj(ADS, N))             --> adjs(ADS), noun(N).
object(obj(N, RP, S))           --> noun(N), relpronoun(RP), sentence(S).
object(obj(D, ADS, N))          --> det(D), adjs(ADS), noun(N).
object(obj(N, RPV, VP))         --> noun(N), relpronounv(RPV), verb_phrases(VP).
object(obj(D, N, RP, S))        --> det(D), noun(N), relpronoun(RP), sentence(S).
object(obj(D, N, RPV, VP))      --> det(D), noun(N), relpronounv(RPV), verb_phrases(VP).
object(obj(ADS, N, RP, S))      --> adjs(ADS), noun(N), relpronoun(RP), sentence(S).
object(obj(ADS, N, RPV, VP))    --> adjs(ADS), noun(N), relpronounv(RPV), verb_phrases(VP).
object(obj(D, ADS, N, RP, S))   --> det(D), adjs(ADS), noun(N), relpronoun(RP), sentence(S).
object(obj(D, ADS, N, RPV, VP)) --> det(D), adjs(ADS), noun(N), relpronounv(RPV), verb_phrases(VP).


%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%
adjs(A)                     --> adj(A).
adjs(ajs(A, AS))            --> adj(A), adjs(AS).

advs(A)                     --> adv(A).
advs(avs(A, AS))            --> adv(A), advs(AS).

preposes(P)                 --> prep_phrase(P).
preposes(preposes(P, PS))   --> prep_phrase(P), preposes(PS).

prep_phrase(prepp(P, O))    --> prepos(P), object(O).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Corpus %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%% Nouns %%%%%%%%%%%%%%%%%%%%%%%
% at least twenty nouns ✓
noun(n(bat))            --> [bat].
noun(n(cat))            --> [cat].
noun(n(boy))            --> [boy].
noun(n(man))            --> [man].
noun(n(men))            --> [men].
noun(n(box))            --> [box].
noun(n(shed))           --> [shed].
noun(n(tree))           --> [tree].
noun(n(room))           --> [room].
noun(n(girl))           --> [girl].
noun(n(bats))           --> [bats].
noun(n(cats))           --> [cats].
noun(n(boys))           --> [boys].
noun(n(sheds))          --> [sheds].
noun(n(trees))          --> [trees].
noun(n(girls))          --> [girls].
noun(n(woman))          --> [woman].
noun(n(women))          --> [women].
noun(n(boxes))          --> [boxes].
noun(n(rooms))          --> [rooms].
noun(n(school))         --> [school].
noun(n(hamahmi))        --> [hamahmi].
noun(n(schools))        --> [schools].
noun(n(student))        --> [student].
noun(n(building))       --> [building].
noun(n(students))       --> [students].
noun(n(envelope))       --> [envelope].
noun(n(lecturer))       --> [lecturer].
noun(n(envelopes))      --> [envelopes].
noun(n(buildings))      --> [buildings].
noun(n(professor))      --> [professor].
noun(n(lecturers))      --> [lecturers].
noun(n(scientist))      --> [scientist].
noun(n(professors))     --> [professors].
noun(n(scientists))     --> [scientists].
noun(n(researcher))     --> [researcher].
noun(n(researchers))    --> [researchers].
noun(n(abdelrahman))    --> [abdelrahman].


%%%%%%%%%%%%%%%%%%%%%%% Verbs %%%%%%%%%%%%%%%%%%%%%%%
% at least twenty verbs (with the past tense inflection) ✓
verb(v(ate))            --> [ate].
verb(v(put))            --> [put].
verb(v(gave))           --> [gave].
verb(v(read))           --> [read].
verb(v(sold))           --> [sold].
verb(v(lost))           --> [lost].
verb(v(cried))          --> [cried].
verb(v(drank))          --> [drank].
verb(v(loved))          --> [loved].
verb(v(liked))          --> [liked].
verb(v(slept))          --> [slept].
verb(v(killed))         --> [killed].
verb(v(worked))         --> [worked].
verb(v(pushed))         --> [pushed].
verb(v(wanted))         --> [wanted].
verb(v(missed))         --> [missed].
verb(v(stored))         --> [stored].
verb(v(climbed))        --> [climbed].
verb(v(watched))        --> [watched].
verb(v(admired))        --> [admired].
verb(v(finished))       --> [finished].
verb(v(appreciated))    --> [appreciated].


%%%%%%%%%%%%%%%%%%%%%%% Adjectives %%%%%%%%%%%%%%%%%%%%%%%
% at least twenty adjectives ✓
adj(aj(old))        --> [old].
adj(aj(big))        --> [big].
adj(aj(sad))        --> [sad].
adj(aj(poor))       --> [poor].
adj(aj(weak))       --> [weak].
adj(aj(full))       --> [full].
adj(aj(rich))       --> [rich].
adj(aj(huge))       --> [huge].
adj(aj(smart))      --> [smart].
adj(aj(happy))      --> [happy].
adj(aj(black))      --> [black].
adj(aj(large))      --> [large].
adj(aj(empty))      --> [empty].
adj(aj(small))      --> [small].
adj(aj(young))      --> [young].
adj(aj(white))      --> [white].
adj(aj(strong))     --> [strong].
adj(aj(bright))     --> [bright].
adj(aj(talented))   --> [talented].
adj(aj(brilliant))  --> [brilliant].


%%%%%%%%%%%%%%%%%%%%%%% Adverbs %%%%%%%%%%%%%%%%%%%%%%%
% at least ten adverbs ✓
adv(av(sadly))      --> [sadly].
adv(av(badly))      --> [badly].
adv(av(gently))     --> [gently].
adv(av(calmly))     --> [calmly].
adv(av(quietly))    --> [quietly].
adv(av(quickly))    --> [quickly].
adv(av(happily))    --> [happily].
adv(av(secretly))   --> [secretly].
adv(av(silently))   --> [silently].
adv(av(painfully))  --> [painfully].


%%%%%%%%%%%%%%%%%%%%%%% Prepositions %%%%%%%%%%%%%%%%%%%%%%%
% at least ten prepositions ✓
prepos(prep(of))        --> [of].
prepos(prep(in))        --> [in].
prepos(prep(on))        --> [on].
prepos(prep(to))        --> [to].
prepos(prep(by))        --> [by].
prepos(prep(for))       --> [for].
prepos(prep(over))      --> [over].
prepos(prep(with))      --> [with].
prepos(prep(from))      --> [from].
prepos(prep(among))     --> [among].
prepos(prep(after))     --> [after].
prepos(prep(under))     --> [under].
prepos(prep(during))    --> [during].
prepos(prep(before))    --> [before].
prepos(prep(behind))    --> [behind].
prepos(prep(through))   --> [through].
prepos(prep(despite))   --> [despite].
prepos(prep(between))   --> [between].
prepos(prep(opposite))  --> [opposite].


%%%%%%%%%%%%%%%%%%%%%%% Determiners %%%%%%%%%%%%%%%%%%%%%%%
% at least ten determiners ✓
det(d(a))       --> [a].
det(d(an))      --> [an].
det(d(my))      --> [my].
det(d(the))     --> [the].
det(d(all))     --> [all].
det(d(this))    --> [this].
det(d(some))    --> [some].
det(d(many))    --> [many].
det(d(that))    --> [that].
det(d(every))   --> [every].
det(d(which))   --> [which].
det(d(whose))   --> [whose].
det(d(various)) --> [various].


%%%%%%%%%%%%%%%%%%%%%%% Pronouns %%%%%%%%%%%%%%%%%%%%%%%
pronoun(pr(i))          --> [i].
pronoun(pr(he))         --> [he].
pronoun(pr(it))         --> [it].
pronoun(pr(we))         --> [we].
pronoun(pr(you))        --> [you].
pronoun(pr(she))        --> [she].
pronoun(pr(they))       --> [they].


%%%%%%%%%%%%%%%%%%%%%%% Relative Pronouns %%%%%%%%%%%%%%%%%%%%%%%
relpronounv(rpn(who))   --> [who].
relpronoun(rpn(whom))   --> [whom].


%%%%%%%%%%%%%%%%%%%%%%% Conjunctions %%%%%%%%%%%%%%%%%%%%%%%
conj(cnj(or))       --> [or].
conj(cnj(so))       --> [so].
conj(cnj(yet))      --> [yet].
conj(cnj(but))      --> [but].
conj(cnj(nor))      --> [nor].
conj(cnj(and))      --> [and].
conj(cnj(while))    --> [while].


%%%%%%%%%%%%%%%%%%%%%%% Fancy %%%%%%%%%%%%%%%%%%%%%%%
clear   :- cls.
cls     :- write('\e[H\e[2J').


%%%%%%%%%%%%%%%%%%%%%%% Test Cases %%%%%%%%%%%%%%%%%%%%%%%
/*
% | ?- s(PT, [the,bat,ate,a,cat], []).
% PT = s(np(d(the), n(bat)), vp(v(ate), obj(d(a), n(cat))))
%                       s
%                     /   \
%                    /     \
%                   /       \
%                 np         vp
%               /    \      /   \
%              d      n    v     \
%              |      |    |      \
%             the    bat  ate     obj
%                               /    \
%                              d      n
%                              |      |
%                              a     cat
*/
/*
a) The/ young/ boy/ who/ worked/ for/ the/ old/ man/ pushed/ and/ stored/ a/ big/ box/ in/ the/ large/ empty/ room/ after/ school/. ✓
| ?- s(PT, [the,young,boy,who,worked,for,the,old,man,pushed,and,stored,a,big,box,in,the,large,empty,room,after,school], []).
PT = s(np(d(the), aj(young), n(boy), rpn(who), vp(v(worked), prepp(prep(for), obj(d(the), aj(old), n(man))))), vps(vp(v(pushed)), cnj(and), vp(v(stored), obj(d(a), aj(big), n(box)), preposes(prepp(prep(in), obj(d(the), ajs(aj(large), aj(empty)), n(room))), prepp(prep(after), obj(n(school)))))))

                                                    s
                                                  /   \
                                                /       \
                                              /           \
                                            /               \
                                        np                  vps
                                        |                    |
----------------------------------------                   / | \
|       |       |       |       |                         /  |  \
d       aj      n      rpn     vp                       vp  cnj  vp
|       |       |       |       |                       /    |     \
the   young    boy     who    /   \                    v    and     ----------------
                             /     \                   |            |       |       |
                            v       prepp            pushed         v      obj      preposes
                            |         /   \                        /      / | \         |
                        worked      prep   obj                  stored   /  |  \        ----------------
                                    |       |                           d   aj  n       |               |
                                   for    / | \                         |   |   |      prepp           prepp
                                         /  |  \                        a  big  box   /     \            ------------
                                        d   aj  n                                    /       \           |          |
                                        |   |   |                                  prep      obj        prep       obj
                                      the  old  man                                 |      /  |  \       |          |
                                                                                    in    d  ajs  n     after       n
                                                                                         /   / \   \                |
                                                                                       the  aj aj   room          school
                                                                                           /    \
                                                                                        large   empty
*/

/*
b) The/ old/ woman/ and/ the/ old/ man/ gave/ the/ poor/ young/ man/ whom/ they/ liked/ a/ white/ envelope/ in/ the/ shed/ behind/ the/ building/. ✓
c) Every/ boy/ quickly/ climbed/ some/ big/ tree/ while/ every/ girl/ secretly/ watched/ some/ boy/. ✓
d) Some/ brilliant/ students/ and/ many/ professors/ watched/ and/ admired/ talented/ lecturers/ and/ appreciated/ bright/ scientists/ and/ researchers/. ✓
*/
/*
| ?- s(PT, [the,old,woman,and,the,old,man,gave,the,poor,young,man,whom,they,liked,a,white,envelope,in,the,shed,behind,the,building], []).
PT = s(nps(np(d(the), aj(old), n(woman)), cnj(and), np(d(the), aj(old), n(man))), vp(v(gave), obj(d(the), ajs(aj(poor), aj(young)), n(man), rpn(whom), s(np(pr(they)), vp(v(liked)))), obj(d(a), aj(white), n(envelope)), preposes(prepp(prep(in), obj(d(the), n(shed))), prepp(prep(behind), obj(d(the), n(building))))))
| ?- s(PT, [every,boy,quickly,climbed,some,big,tree,while,every,girl,secretly,watched,some,boy], []).
PT = ss(s(np(d(every), n(boy)), vp(av(quickly), v(climbed), obj(d(some), aj(big), n(tree)))), cnj(while), s(np(d(every), n(girl)), vp(av(secretly), v(watched), obj(d(some), n(boy)))))
| ?- s(PT, [some,brilliant,students,and,many,professors,watched,and,admired,talented,lecturers,and,appreciated,bright,scientists,and,researchers], []).
PT = s(nps(np(d(some), aj(brilliant), n(students)), cnj(and), np(d(many), n(professors))), vps(vp(v(watched)), cnj(and), vps(vp(v(admired), obj(aj(talented), n(lecturers))), cnj(and), vp(v(appreciated), objs(obj(aj(bright), n(scientists)), cnj(and), obj(n(researchers)))))))
*/