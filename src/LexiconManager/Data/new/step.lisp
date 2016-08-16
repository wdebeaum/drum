;;;;
;;;; w::step
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
	 (w::step
	  ; nom
	  (senses ;((lf-parent ont::action)
;                   (meta-data :origin bee :entry-date 20040407 :change-date nil :comments test-s :wn ("step%1:04:02" "step%1:04:00" "step%1:04:03"))
;                   )
		  ((lf-parent ont::step)
		   (example "the bottom step of the staircase")
		   (meta-data :origin cardiac :entry-date 20070814 :change-date nil :comments nil)
		   )
                  ((lf-parent ont::information-function-object)
		   (example "delete this step")
		   (meta-data :origin plow :entry-date 20080718 :change-date nil :comments task-editing)
		   )
 		  ))
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
 ((W::step w::aerobic)
  (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::cardiopulmonary-exercise)
     ; changed to new type cardiopulmonary-exercise for asma
     (meta-data :origin asma :entry-date 20111003 :wn ("workout%1:04:00"))
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  ((W::step w::aerobics)
  (wordfeats (W::morph (:forms (-none))))
   (SENSES
    ((LF-PARENT ONT::cardiopulmonary-exercise)
     ; changed to new type cardiopulmonary-exercise for asma
     (meta-data :origin asma :entry-date 20111003 :wn ("workout%1:04:00"))
     (templ mass-pred-templ)
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :tags (:base500)
 :words (
  (W::step
;    (wordfeats (W::morph (:forms (-vb) :nom W::step)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("run-51.3.2"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like roll,scoot,walk,stomp,dart,roam,nip,parade,mosey,clamber,slither,troop,speed,leap,promenade,totter,trot,jog,stride,lope,hike,wander,somersault,cavort,tear,lurch,pad,canter,scramble,prance,plod,hobble,trudge,hurry,slog,slide,clump,saunter,skip,gallop,slink,skulk,prowl,gambol,carom,scurry,dash,journey,meander,swim,travel,slouch,file,romp,waddle,crawl,inch,creep,skitter,limp,dodder,mince,trek,stumble,race,climb,amble,rush,stray,hurtle,vault,charge,toddle,strut,stroll,sneak,trundle,shamble,backpack,tiptoe,tramp,whiz,scud,perambulate,scamper,scuttle,sashay,lollop,stump,rove,flit,wade,drift,stagger,float,sweep,traipse,run,hasten,coast,bolt,zigzag,ramble,sidle,sleepwalk,streak,frolic,march,swagger,glide,goose-step,bound,scutter,shuffle,lumber,bowl
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ((W::step w::by w::step)
   (SENSES
    ((lf-parent ont::incremental-val)
     (meta-data :origin plot :entry-date 20080529 :change-date nil :comments nil)
     (example "a step by step plan")
     )
    )
   )
))

(define-words :pos W::adv :templ DISC-PRE-TEMPL
 :words (
    ((W::step w::by w::step)
   (SENSES
    ((lf-parent ont::incremental-val)
     (meta-data :origin plot :entry-date 20080529 :change-date nil :comments nil)
     (example "execute the procedure step by step")
     (templ pred-vp-templ)
     )
    )
   )
))

