/* - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~
Author      :   Abdelrahman Gharib El-Hamahmi
ID          :   37-15881
Tutorial    :   T-16

------------------------------------------------
Disclaimer  :   Base code taken from (Wikipedia)
[https://en.wikipedia.org/wiki/Definite_clause_grammar#Parsing_with_DCGs]
The rest of the code is completely written by the author
Most of the calssifications were made according to
[https://en.wikipedia.org/wiki/English_grammar]
------------------------------------------------

Parser for a definite clause grammar (DCG) of English-Light.
- ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - ~ - */


%%%%%%%%%%%%%%%%%%%%%%% Sentence %%%%%%%%%%%%%%%%%%%%%%%
sentence(s(NP,VP))      --> noun_phrase(NP), verb_phrase(VP).
sentence(s(NP,CP,VP))   --> noun_phrase(NP), connective_phrase(CP), verb_phrase(VP).

%%%%%%%%%%%%%%%%%%%%%%% Phrases %%%%%%%%%%%%%%%%%%%%%%%
noun_phrase(np(D,N))    --> det(D), noun(N).
noun_phrase(np(D,A,N))  --> det(D), adj(A), noun(N).

verb_phrase(vp(V,NP))   --> verb(V), noun_phrase(NP).

connective_phrase(cp(RP,VP))    --> relpronoun(RP), verb_phrase(VP).

% adv alwas before v
% conj between sentences only
% prepos phrase
% noun phrase N only also with and without d
% zero, one or more adv before v
% proposensional phrases before and after each other


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
adv(av(secretly))   --> [secretly].
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
pronoun(pr(me))         --> [me].
pronoun(pr(my))         --> [my].
pronoun(pr(he))         --> [he].
pronoun(pr(it))         --> [it].
pronoun(pr(we))         --> [we].
pronoun(pr(us))         --> [us].
pronoun(pr(you))        --> [you].
pronoun(pr(his))        --> [his].
pronoun(pr(she))        --> [she].
pronoun(pr(her))        --> [her].
pronoun(pr(him))        --> [him].
pronoun(pr(its))        --> [its].
pronoun(pr(our))        --> [our].
pronoun(pr(ours))       --> [ours].
pronoun(pr(hers))       --> [hers].
pronoun(pr(them))       --> [them].
pronoun(pr(mine))       --> [mine].
pronoun(pr(they))       --> [they].
pronoun(pr(your))       --> [your].
pronoun(pr(yours))      --> [yours].
pronoun(pr(their))      --> [their].
pronoun(pr(theirs))     --> [theirs].
pronoun(pr(myself))     --> [myself].
pronoun(pr(itself))     --> [itself].
pronoun(pr(herself))    --> [herself].
pronoun(pr(himself))    --> [himself].
pronoun(pr(themself))   --> [themself].
pronoun(pr(yourself))   --> [yourself].
pronoun(pr(ourselves))  --> [ourselves].
pronoun(pr(yourselves)) --> [yourselves].
pronoun(pr(themselves)) --> [themselves].


%%%%%%%%%%%%%%%%%%%%%%% Relative Pronouns %%%%%%%%%%%%%%%%%%%%%%%
relpronoun(rpn(who))    --> [who].
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


% sentence(PT, [the,bat,ate,a,cat], []).
% PT = s(np(d(the), n(bat)), vp(v(ate), np(d(a), n(cat)))).
%                       s
%                     /   \
%                    /     \
%                   /       \
%                 np         vp
%               /    \      /   \
%              d      n    v    np
%              |      |    |     |
%             the    bat  ate   np
%                             /    \
%                            d      n
%                            |      |
%                            a     cat


/*
a) The/ young/ boy/ who/ worked/ for/ the/ old/ man/ pushed/ and/ stored/ a/ big/ box/ in/ the/ large/ empty/ room/ after/ school/. ✓
b) The/ old/ woman/ and/ the/ old/ man/ gave/ the/ poor/ young/ man/ whom/ they/ liked/ a/ white/ envelope/ in/ the/ shed/ behind/ the/ building/. ✓
c) Every/ boy/ quickly/ climbed/ some/ big/ tree/ while/ every/ girl/ secretly/ watched/ some/ boy/. ✓
d) Some/ brilliant/ students/ and/ many/ professors/ watched/ and/ admired/ talented/ lecturers/ and/ appreciated/ bright/ scientists/ and/ researchers/. ✓
*/