;;;;
;;;; W::IRISH
;;;;

(define-words :pos W::n
 :words (
  ((W::IRISH w::MOSS)
  (senses
	   ((LF-PARENT ONT::VEGETABLE)
	    (TEMPL MASS-PRED-TEMPL)
	    (syntax (W::morph (:forms (-none))))
	    )
	   )
)
))

(define-words :pos W::n
 :words (
  ((W::IRISH W::SODA W::BREAD)
  (senses
	   ((LF-PARENT ONT::BREAD)
	    (syntax (W::morph (:forms (-none))))
	    (TEMPL MASS-PRED-TEMPL)
	    )
	   )
)
))

(define-words :pos w::adj 
 :words (
  (w::irish
  (senses((LF-parent ONT::nationality-val) 
	    (templ central-adj-templ)
	    (meta-data :origin calo-ontology :entry-date 20060128 :change-date nil :comments caloy3)
	    ))
)
))

(define-words :pos W::name
 :words (
;; removing names since Parser gets them from TT now
;(define-list-of-words :pos W::name
;  :senses (((LF-PARENT ONT::country)
;    (TEMPL nname-templ)
;    )
;   )
; :words (w::canada w::china w::japan w::australia (w::new w::zealand)  (w::great w::britain) w::england w::britain w::scotland w::germany w::france w::india w::pakistan w::afghanistan w::turkmenistan w::azerbaijan w::russia w::portugal w::spain w::italy w::yugoslavia w::serbia w::croatia w::iceland w::greenland w::mexico w::argentina w::brazil w::venezuela w::ireland w::wales w::turkey w::iraq w::iran W::ONTARIO w::cosovo w::antarctica))
;(define-words :POS w::name :templ nname-templ
;  :WORDS (
;  (w::america
;   (SENSES
;    ((lf-parent ont::country)
;     (lf-form w::usa)
;     (meta-data :origin plow-travel :entry-date 20070925 :change-date nil
;      :comments gsa-dialogues))))
;  (w::usa
;   (SENSES
;    ((lf-parent ont::country)
;     (lf-form w::usa)
;     (meta-data :origin plow-travel :entry-date 20070925 :change-date nil
;      :comments gsa-dialogues))))
;))
;(define-words :POS w::name :templ name-templ
;  :WORDS (
;    ((w::the w::usa)
;   (SENSES
;    ((lf-parent ont::country)
;     (preference .96) ;; don't compete with the article
;     (lf-form w::usa)
;     (meta-data :origin plow-travel :entry-date 20070925 :change-date nil
;      :comments gsa-dialogues))))
;
;    ((w::the w::us)
;   (SENSES
;    ((lf-parent ont::country)
;     (preference .96) ;; don't compete with the article
;     (lf-form w::usa)
;     (meta-data :origin plow-travel :entry-date 20070925 :change-date nil
;                :comments gsa-dialogues))))
;
;    ((w::the w::u w::punc-period w::s w::punc-period)
;   (SENSES
;    ((lf-parent ont::country)
;     (preference .96) ;; don't compete with the article
;     (lf-form w::usa)
;       (meta-data :origin step :entry-date 20080530 :change-date nil
;                                                    :comments nil))))
;
;  ((w::the w::u w::punc-period w::s w::punc-period w::a w::punc-period)
;   (SENSES
;    ((lf-parent ont::country)
;     (preference .96) ;; don't compete with the article
;     (lf-form w::usa)
;     (meta-data :origin step :entry-date 20080530 :change-date nil
;                :comments nil))))
;
;   ((w::the w::states)
;   (SENSES
;    ((lf-parent ont::country)
;     (preference .96) ;; don't compete with the article
;     (lf-form w::usa)
;     (meta-data :origin plow-travel :entry-date 20070925 :change-date nil
;                :comments gsa-dialogues))))
;   
;   ((w::the w::united w::states)
;   (SENSES
;    ((lf-parent ont::country)
;     (preference .96) ;; don't compete with the article
;     (lf-form w::usa)
;     (meta-data :origin plow-travel :entry-date 20070925 :change-date nil
;                :comments gsa-dialogues))))
;
;   ((w::the w::united w::states w::of w::america)
;   (SENSES
;    ((lf-parent ont::country)
;     (preference .96) ;; don't compete with the article
;     (lf-form w::usa)
;     (meta-data :origin plow-travel :entry-date 20070925 :change-date nil
;                :comments gsa-dialogues))))
;   ))
;  
;(define-list-of-words :pos W::name
;  :senses (((LF-PARENT ONT::political-region)
;    (TEMPL name-templ)))
; :words ((w::british w::columbia) w::manitoba (w::northwest w::territories) (w::nova w::scotia) w::nunavut
;         W::ONTARIO w::quebec (w::prince w::edward w::island) (w::washington w::d w::c) (w::hong w::kong) w::jalisco w::europe w::africa 
;         w::asia (w::north w::america) (w::south w::america)
;                     ))
;(define-list-of-words :pos W::name
;  :senses (((LF-PARENT ONT::city)
;    (TEMPL nname-templ)))
; :words (w::paris (w::new w::york) w::toronto w::london w::dakar w::chicago w::tokyo w::boston w::berlin w::munich w::copenhagen w::redmond w::stockholm (w::san w::francisco) w::seattle w::vancouver
;                  w::moscow w::beijing w::kabul w::rochester))
;(define-list-of-words :pos W::name
;  :senses (((LF-PARENT ONT::us-state)
;    (TEMPL nname-templ)))
; :words (
;  W::ALABAMA  w::alaska w::arizona w::arkansas w::california w::colorado W::CONNECTICUT
;  W::DELAWARE w::florida W::GEORGIA w::hawaii w::idaho W::ILLINOIS W::INDIANA w::iowa
;  w::kansas W::KENTUCKY w::louisiana W::MAINE W::MARYLAND W::MASSACHUSETTS W::MICHIGAN w::minnesota
;  W::MISSISSIPPI W::MISSOURI w::montana w::nebraska   w::nevada (w::new w::hampshire) (w::new w::jersey)
;  (w::new w::mexico) (w::new w::york) (w::north W::CAROLINA)(w::north w::dakota) W::OHIO w::oklahoma w::oregon
;  W::PENNSYLVANIA (w::rhode w::island) (w::south w::carolina) (w::south w::dakota) W::TENNESSEE w::texas            
;  w::utah W::VERMONT W::VIRGINIA w::washington (w::west w::virginia) W::WISCONSIN w::wyoming))
;(define-list-of-words :pos W::name
;  :senses (((LF-PARENT ONT::geographic-region)
;            (TEMPL nname-templ)
;            )
;           )
; :words (w::pacific w::atlantic  w::arctic (w::north w::pole) (w::south w::pole) (w::arctic w::circle)))
  (w::irish
  (senses((LF-PARENT ONT::language)
    (TEMPL nname-templ)
    )
   )
)
))

