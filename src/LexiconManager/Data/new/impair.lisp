;;;;
;;;; W::impair
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::impair
    (wordfeats (W::morph (:forms (-vb) :nom w::impairment)))
   (SENSES
    ;((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date 20090512 :comments nil :vn ("amuse-31.1"))
     ;(LF-PARENT ONT::evoke-confusion)
     ;(TEMPL agent-affected-xp-templ) 
; like wound,weary,revolt,disgruntle,dumbfound,entrance,horrify,stagger,fluster,amuse,floor,offend,discourage,disturb,tire,insult,annoy,disillusion,titillate,miff,incense,enlighten,amaze,hurt,tantalize,shock,stir,disappoint,exhaust,nettle,dissatisfy,afflict,enrapture,irk,cow,repulse,madden,alienate,dishearten,uplift,stupefy,please,perplex,excite,astound,revitalize,interest,gratify,enliven,stun,elate,sting,captivate,placate,entertain,infuriate,refresh,stimulate,demoralize,preoccupy,electrify,rankle,perturb,distract,bewitch,content,depress,demolish,irritate,embolden,disgust,pain,hearten,stump,inspire,surprise,anger,daunt,unnerve,repel,terrorize,discomfit,gall,spellbind,frighten,disgrace,startle,dismay,intimidate,bore,invigorate,mystify,disconcert,assuage,peeve,disarm,affront,embarrass,mortify,outrage,numb,arouse,roil,rile,strike,confound,agonize,provoke,gladden,vex,disquiet,deject,flabbergast,daze,touch,flatter,lull,overawe,sober,bug,awe,muddle,tempt,plague,hypnotize,solace,convince,chagrin,bewilder,overwhelm,threaten,affect,engross,tease,satisfy,impress,spook,discombobulate,move,baffle,obsess,nauseate,reassure,charm,beguile,engage,sadden,console,cheer,distress,astonish,enchant,shake,jar,scandalize,terrify,jolt,enrage,encourage,exhilarate,entice,alarm,devastate,enthrall,discompose,boggle,comfort,tickle,trouble,scare,faze,appall,relieve,antagonize,haunt,sicken,dazzle,delight,intrigue,concern,abash,upset,pique,dispirit,thrill,appease,jollify,torment,puzzle,try,unsettle,harass,frustrate,ruffle,aggravate,fascinate,galvanize,grieve,mollify,bother,displease,enthuse,exasperate,agitate,wow,shame,humiliate,mesmerize,pacify,intoxicate,humble
     ;(PREFERENCE 0.96)
;     )
    #||((LF-PARENT ONT::BREAK-OBJECT)
     (meta-data :origin calo-ontology :entry-date 20051209 :change-date nil :comments Impair)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "he impaired the process")
     )||#
    (;(LF-PARENT ont::break-object)
     (LF-PARENT ont::hindering)
     (meta-data :origin calo-ontology :entry-date 20051209 :change-date nil :comments Impair)
     ;;(SEM (F::Cause F::Phenomenal) (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "it impaired the process")
     )
    )
   )
))

