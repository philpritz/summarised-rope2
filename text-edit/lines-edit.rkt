#lang racket

;; Lines: an optic that splits the cursor's focus into its lines and back. lines-iso is the
;; rope <-> (listof rope) bijection -- split at each newline, every line owning its trailing \n
;; (no phantom final line). focus-lines composes it onto the zipper's focus, so the lines of the
;; focus become one focus to view/edit through the value-stream optics (list-of, lref, ...).
;; Built on summaries.rkt's linecol metric and the experimental multisect*/newline-guide*.

(require "../rope-core.rkt"
         (submod "../rope-core.rkt" experimental)              ; multisect*
         "../summaries/summaries.rkt"                           ; linecol-smr
         (submod "../summaries/summaries.rkt" experimental)     ; newline-guide*
         "../toolbox/main.rkt"                               ; iso, iso->stage
         "../zipper-core.rkt")                                  ; zipper-focus/g, compose-stage

(provide lines-iso focus-lines)

;; rope <-> list of lines; the join applies the linecol rope constructor to the pieces.
(define lines-iso (iso (multisect* newline-guide*) (curry apply (make-rope linecol-smr))))

;; split the cursor's focus into its lines (one focus = the list of line ropes).
(define focus-lines (compose-stage zipper-focus/g (iso->stage lines-iso)))
