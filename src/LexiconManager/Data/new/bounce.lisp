;;;;
;;;; W::bounce
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  (W::bounce
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("run-51.3.2") :wn ("bounce%2:35:03" "bounce%2:38:00" "bounce%2:38:02"))
     (LF-PARENT ONT::self-locomote)
     (TEMPL agent-templ) ; like roll,scoot,walk,stomp,dart,roam,nip,parade,mosey,clamber,slither,troop,speed,leap,promenade,totter,trot,jog,stride,lope,hike,wander,somersault,cavort,tear,lurch,pad,canter,scramble,prance,plod,hobble,trudge,hurry,slog,slide,clump,saunter,skip,gallop,slink,skulk,prowl,gambol,carom,scurry,dash,journey,meander,swim,travel,slouch,file,romp,waddle,crawl,inch,creep,skitter,limp,dodder,mince,trek,stumble,race,climb,amble,rush,stray,hurtle,vault,charge,toddle,strut,stroll,sneak,trundle,shamble,backpack,tiptoe,tramp,whiz,scud,perambulate,scamper,scuttle,sashay,lollop,stump,rove,flit,wade,drift,stagger,float,sweep,traipse,run,hasten,coast,bolt,zigzag,ramble,sidle,sleepwalk,streak,frolic,march,swagger,glide,goose-step,bound,scutter,shuffle,lumber,bowl
     (PREFERENCE 0.96)
     )
    ((meta-data :origin calo :entry-date 20040916 :change-date nil :comments caloy2 :vn ("roll-51.3.1") :wn ("bounce%2:35:03" "bounce%2:38:00" "bounce%2:38:02"))
     (LF-PARENT ONT::bounce-reflect)
     (example "the ball bounced")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL affected-unaccusative-templ)
     )
    ((LF-PARENT ONT::bounce-reflect)
     (example "he bounced the ball")
     (TEMPL AGENT-AFFECTED-XP-TEMPL)
     )
    )
   )
))

