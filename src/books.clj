(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])
(def authors #{china, felleisen, octavia, friedman})