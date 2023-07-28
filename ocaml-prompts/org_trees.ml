(* An OrgChart is one of:
- person [name title reports] 
- group [name members]
Interpretation: an organizational chart *)

type orgchart =
    | Person of string * string * orgchart list
    | Group of string * orgchart list


(* full_title: Adds the name of the organization in parenthesis
to every person in the org chart. *)
let rec full_title (org: orgchart) (name: string) =
  let
      f = fun (o: orgchart) -> full_title o name
  in
  match org with
  | Person (pname, title, reports) ->
      Person (pname, 
              String.cat title (String.cat " (" (String.cat name ")")),
                List.map f reports)
  | Group (gname, members) ->
      Group (gname,
             List.map f members)

(* <tests> *)

let org_provost = 
    Person ("David Madigan",
            "Provost and Senior Vice President for Academic Affairs",
            [
                Group ("Administration",
                    [
                        Person ("Thomas Sheahan",
                                "Senior Vice Provost, Curriculum and Programs",
                                [])
                    ]);
                Group ("Academic Deans",
                    [
                        Person ("Alan Mislove",
                                "Interim Dean, Khoury College of Computer Sciences",
                                []);
                        Person ("Carmen Sceppa",
                                "Dean, Bouvé College of Health Sciences",
                                []);
                        Person ("Uta G. Poiger",
                                "Dean, College of Social Sciences & Humanities",
                                []);
                    ])
            ])

let org_cabinet = 
    Group ("Cabinet",
            [
                Person ("Karl Reid",
                        "Senior Vice Provost and Inclusion Officer",
                        []);
                Person ("Madeleine Estabrook",
                        "Senior Advisor for Global Student Experience",
                        []);
                org_provost
            ])

let org_nu = 
    Person ("Joseph E. Aoun", "President", [org_cabinet]);;

let assertions () = 
  assert (full_title org_nu "Northeastern University" = 
          Person ("Joseph E. Aoun", "President (Northeastern University)", [
              Group ("Cabinet",
              [
                  Person ("Karl Reid",
                          "Senior Vice Provost and Inclusion Officer (Northeastern University)",
                          []);
                  Person ("Madeleine Estabrook",
                          "Senior Advisor for Global Student Experience (Northeastern University)",
                          []);
                          Person ("David Madigan",
                                  "Provost and Senior Vice President for Academic Affairs (Northeastern University)",
                                  [
                                      Group ("Administration",
                                          [
                                              Person ("Thomas Sheahan",
                                                      "Senior Vice Provost, Curriculum and Programs (Northeastern University)",
                                                      [])
                                          ]);
                                      Group ("Academic Deans",
                                          [
                                              Person ("Alan Mislove",
                                                      "Interim Dean, Khoury College of Computer Sciences (Northeastern University)",
                                                      []);
                                              Person ("Carmen Sceppa",
                                                      "Dean, Bouvé College of Health Sciences (Northeastern University)",
                                                      []);
                                              Person ("Uta G. Poiger",
                                                      "Dean, College of Social Sciences & Humanities (Northeastern University)",
                                                      []);
                                          ])
                                  ])
              ])
          ]))
;;

assertions()