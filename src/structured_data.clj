(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [[x1 x2 x3] v]
    (+ x1 x3)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x1 x2 x3] v]
    (+ x1 x3)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y2 y1))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (width rectangle) (height rectangle))))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [col] (get col 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn increasing? [a-seq]
  (apply >= a-seq))

(defn decreasing? [a-seq]
  (apply <= a-seq))

(defn monotonic? [a-seq]
  (or (increasing? a-seq) (decreasing? a-seq)))

(defn stars [n]
  (clojure.string/join (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (apply distinct? a-seq)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names-old [books]
  (let [author-names
        (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn birth-year [author]
  (if (contains? author :birth-year) (str " (" (:birth-year author) " - ") ""))

(defn death-year [author]
  (let [by (birth-year author)]
    (contains? author :death-year) (str (birth-year author) (:death-year author) (if (not-empty by) ")" ""))))

(defn author->string [author]
  (let [author-name (:name author)
        author-years (fn [author]
                       (let [dy (death-year author)]
                         (if (not-empty dy) dy (birth-year author))))]
    (str author-name (author-years author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books-count-as-string [books]
  (let [books-count (count books)]
    (str books-count " book" (if (< 1 books-count) "s" ""))))

;; Exercise 29
(defn books->string [books]
  (if (empty? books) "No books."
      (str (books-count-as-string books) ". " (apply str (interpose ". " (map book->string books))) ".")))

;; Exercise 30
(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

;; Exercise 31
(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

;; Exercise 32
(defn living-authors [authors]
  (filter alive? authors))

;; Exercise 33
(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

;; Exercise 34
(defn books-by-living-authors [books]
  (filter has-a-living-author? books))