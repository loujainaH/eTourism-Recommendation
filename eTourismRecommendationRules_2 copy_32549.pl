
getRelevance(_, accommodation(A), B, 0):-
                A \= B.
getRelevance(Customer, accommodation(A), A, R):-
                customerPreferredAccommodation(Customer, A , R).
getRelevance(_, means(A), B, 0):-
                A \= B.
getRelevance(Customer, means(A), A, R):-
                customerPreferredMean(Customer, A , R).
getRelevance(Customer, activity(A1), A2, R):-
                getActivitiesRelevance(Customer, A1, A2, R).

getActivitiesRelevance(_, [], _, 0).
getActivitiesRelevance(Customer, [H|T], A2, R):-
                member(H, A2),
                customerPreferredActivity(Customer, H, R1),
                getActivitiesRelevance(Customer, T, A2, R2),
                R is R1 + R2.
getActivitiesRelevance(Customer, [H|T], A2, R):-
                \+member(H, A2),
                getActivitiesRelevance(Customer, T, A2, R).

preferenceSatisfaction(_, _, [], 0).
preferenceSatisfaction(Offer, Customer, [accommodation(A1)|T], S):-
                offerAccommodation(Offer, A2),
                getRelevance(Customer, accommodation(A1), A2, R),
                preferenceSatisfaction(Offer, Customer, T, S1),
                S is S1 + R.
preferenceSatisfaction(Offer, Customer, [means(M1)|T], S):-
                offerMean(Offer, M2),
                getRelevance(Customer, means(M1), M2, R),
                preferenceSatisfaction(Offer, Customer, T, S1),
                S is S1 + R.
preferenceSatisfaction(Offer, Customer, [activity(A1)|T], S):-
                Offer = offer(_, A2, _, _, _, _, _, _), % edited removed activity(A2) here not mentioned in the description that we should query the offer differently than in the knowledge base
                getRelevance(Customer, activity(A1), A2, R),
                preferenceSatisfaction(Offer, Customer, T, S1),
                S is S1 + R.
preferenceSatisfaction(Offer, Customer, [X|T], S):-
                X \= activity(_),
                X \= means(_),
                X \= accommodation(_),
                preferenceSatisfaction(Offer, Customer, T, S).

%%%%%%%%%%%%%%%%%%%%%%%%


possibleSubset(L,R):-
	chooseActivities(L,[],R1), permutation(R1,R).

chooseActivities([], Acc, Acc).
chooseActivities([H|T], Acc, R):-
        append(Acc, [H], NAcc),
        chooseActivities(T, NAcc, R).
chooseActivities([_|T], Acc, R):-
        chooseActivities(T, Acc, R).

choosePreferences(Prefs, ChosenPrefs):-
        choosePreferencesH(Prefs, [], ChosenPrefs).
choosePreferencesH([], Acc, Acc).
choosePreferencesH([H|T], Acc, P):-
        H \= activity(_),
        append(Acc, [H], NAcc),
        choosePreferencesH(T, NAcc, P).
choosePreferencesH([activity(A)|T], Acc, P):-
        chooseActivities(A, [], A1),
        A1 \= [],
        append(Acc, [activity(A1)], NAcc),
        choosePreferencesH(T, NAcc, P).
choosePreferencesH([activity(A)|T], Acc, P):-
        chooseActivities(A, [], []),
        choosePreferencesH(T, Acc, P).

choosePreferencesH([_|T], Acc, P):-
        choosePreferencesH(T, Acc, P).

overlapPeriod(period(D1, D2), period(D11, D22)):-
        D22 @>= D1,
        D11 @=< D2.

satisfiesPreference(activity(L), offer(Des, A, Cost, ValidFrom, ValidTill, OfferPeriod, MaxDuration, NoOfGuests)):- % removed Acitivity struct here again because no reason to add a struction on the argument since the activity list is always the second argument and its not mentioned in the description.
        subset(L, A).
satisfiesPreference(period(D1, D2), offer(Des, ActivityList, Cost, ValidFrom, ValidTill, OfferPeriod, MaxDuration, NoOfGuests)):-
        offerMean(offer(Des, ActivityList, Cost, ValidFrom, ValidTill, OfferPeriod, MaxDuration, NoOfGuests), _),
        overlapPeriod(period(D1, D2), OfferPeriod).

satisfiesPreference(dest(D), offer(D, ActivityList, Cost, ValidFrom, ValidTill, OfferPeriod, MaxDuration, NoOfGuests)).

satisfiesPreference(budget(B), offer(Des, ActivityList, Cost, ValidFrom, ValidTill, OfferPeriod, MaxDuration, NoOfGuests)):-
        offerMean(offer(Des, ActivityList, Cost, ValidFrom, ValidTill, OfferPeriod, MaxDuration, NoOfGuests), _),
        B >= Cost.

satisfiesPreference(means(M), Offer):-
        offerMean(Offer, M).

satisfiesPreference(accommodation(A), Offer):-
        offerAccommodation(Offer, A).

getOffer([], offer(Des, ActivityList, Cost, ValidFrom, ValidTill, Period, MaxDuration, NoOfGuests)):-
        offerMean(offer(Des, ActivityList, Cost, ValidFrom, ValidTill, Period, MaxDuration, NoOfGuests), _).
getOffer([Pref|T], offer(Des, ActivityList, Cost, ValidFrom, ValidTill, Period, MaxDuration, NoOfGuests)):-
        satisfiesPreference(Pref, offer(Des, ActivityList, Cost, ValidFrom, ValidTill, Period, MaxDuration, NoOfGuests)),
        getOffer(T, offer(Des, ActivityList, Cost, ValidFrom, ValidTill, Period, MaxDuration, NoOfGuests)).

recommendOfferForCustomer(Prefs, ChosenPrefs, Offer):-
        offerMean(Offer, _),
        getPrefs(Prefs, Offer, ChosenPrefs).

getPrefs([], _, []).
getPrefs([H|T], O, [H|T1]):-
        H \= activity(_),
        satisfiesPreference(H, O), 
        getPrefs(T, O, T1).
getPrefs([H|T], O, T1):-
        H \= activity(_),
        \+satisfiesPreference(H, O), 
        getPrefs(T, O, T1).
getPrefs([H|T], O, [activity(A)|T1]):-
        H = activity(_),
        satisfiesActivity(H, O, A),
        A \= [], 
        getPrefs(T, O, T1).
getPrefs([H|T], O, T1):-
        H = activity(_),
        satisfiesActivity(H, O, A), 
        A = [],
        getPrefs(T, O, T1).

satisfiesActivity(activity(L), offer(_, A, _, _, _, _, _, _), R):-
        intersection(L, A, R).

calculateSatisfactionForCustomers([], [], _, []).
calculateSatisfactionForCustomers([Customer|T], [CustomerPrefs|T1], Offer, [(Sat, Customer)|T2]):-
        preferenceSatisfaction(Offer, Customer, CustomerPrefs, Sat),
        calculateSatisfactionForCustomers(T, T1, Offer, T2).

chooseNCustomers(_, 0, []).
chooseNCustomers([], _, []).
chooseNCustomers([(S, H)|T], NoOfGuests, [H|T1]):-
        NoOfGuests > 0,
        N1 is NoOfGuests - 1,
        chooseNCustomers(T, N1, T1). 

recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
        offerMean(Offer, _),
        calculateSatisfactionForCustomers(Customers, PreferenceList, Offer, Satisfactions),
        sort(0,@>=,Satisfactions, S),
        Offer = offer(_, _, _, _, _, _, _, NoOfGuests),
        chooseNCustomers(S, NoOfGuests, CustomersChosen).
