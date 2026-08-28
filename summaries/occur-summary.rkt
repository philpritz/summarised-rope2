#lang racket

;; ============================================================================
;; occur-summary.rkt  --  a word / substring-search SUMMARY for the
;; summarised rope.
;;
;; The summary value at a node is a word-AGNOSTIC function
;;     node -> (Word -> wm)
;; so ONE build answers any word; the word is supplied at query time.  For a
;; word W (length m) over a chunk the object is
;;
;;     (wm count posns len)
;;       count -- occurrences of W lying WHOLLY inside the chunk
;;       len   -- chunk length: the ruler that lets combine re-base one side's
;;                positions across a seam
;;       posns -- the start offset p of every LIVE candidate alignment, measured
;;                relative to the chunk's start, that still crosses an edge.  A
;;                candidate is an alignment of W at offset p whose window agrees
;;                with the chunk on the overlap [max(0,p), min(len,p+m)).  Only
;;                edge-crossers are stored (interior complete matches go to
;;                `count` -- listing them would be O(len)):
;;                  p < 0        -- starts BEFORE the chunk (a left partial:
;;                                  e.g. -2, two chars of W already to the left)
;;                  p+m > len    -- ends AFTER the chunk  (a right partial)
;;                  both at once -- only possible when len < m: a CONDUIT, a
;;                                  match threading clean through a chunk shorter
;;                                  than W.
;;
;; Why ONE signed list beats the old prefix-STRING + right int-list: both sides
;; of a seam were scanned against the SAME W, so two candidates agree by POSITION
;; alone -- combine never re-reads a character, it only shifts and intersects
;; integers.  The conduit -- a partial threading a chunk shorter than W -- is a
;; single position that is at once < 0 and p+m > len; the old two-ended shape had
;; no cell for it and mis-counted any match straddling three or more chunks.
;;
;; combine is a pure monoid on the object.  The caching lives in ONE global
;; capped trie (toolbox/trie.rkt): word -> cell -> wm, 64 warm words with 10000
;; cells each.  A cell keys itself (procedure identity -- equal? on procedures is
;; reference equality, no structural compare).  Because the rope is persistent,
;; an unchanged subtree after an edit is the SAME cell object and its cached wm
;; is reused -- a re-query costs only the changed root-to-edit path.  The queries
;; enter through the STORED root cell (rope-summary), never through the smr's
;; coercion -- (occur-smr rope) builds a fresh wrapper cell per call, and a fresh
;; closure is a fresh KEY: junk in the shared map.
;; SIZING RULE (measured, not guessed): the per-word cap must exceed the total
;; live cells of the ropes actively re-queried for that word (~ chars/16 per
;; rope), else the ropes cycle the sub-map and evict each other wholesale --
;; LRU ping-pong, every re-query a full recompute.  Within one over-cap rope the
;; root still hits (a query fills post-order, root last), but an edit's re-query
;; loses its unchanged-subtree reuse.  10000 holds one ~150KB document or dozens
;; of small ones per word.  Evicting a cold word drops its whole cell sub-map at
;; once.  The trade accepted: cached cells are RETAINED until they age out
;; (bounded at 64x10000).  The interface stays referentially transparent.
;; ============================================================================

(require "../rope-core.rkt"
         (submod "../rope-core.rkt" internal)   ; leaf?/leaf-text/branch-left/-right/rope-summary
         "../toolbox/trie.rkt")

(provide (struct-out wm)
         occur-smr      ; the summary algebra: (make-summary leaf combine)
         occur-prop     ; root W   -> wm      (the summary object; the "prop" query)
         occur-count    ; root W   -> exact   (full occurrences of W)
         occur-select)  ; root W k -> exact   (absolute offset of the k-th match)

;; ---- the monoid on the data object -------------------------------------------

(struct wm (count posns len) #:transparent)

;; does W, aligned so W[0] sits at chunk offset p, agree with s where they overlap?
(define (agrees? W s p)
  (define m (string-length W)) (define L (string-length s))
  (define lo (max 0 p)) (define hi (min L (+ p m)))
  (and (< lo hi)                                          ; non-empty overlap
       (for/and ([i (in-range lo hi)])
         (char=? (string-ref s i) (string-ref W (- i p))))))

(define (leaf-wm W s)
  (define m (string-length W)) (define L (string-length s))
  (let loop ([p (- 1 m)] [count 0] [posns '()])          ; p ranges over every overlapping shift
    (cond
      [(>= p L) (wm count (reverse posns) L)]
      [(agrees? W s p)
       (if (and (>= p 0) (<= (+ p m) L))
           (loop (add1 p) (add1 count) posns)             ; wholly inside -> count
           (loop (add1 p) count (cons p posns)))]         ; crosses an edge -> store
      [else (loop (add1 p) count posns)])))

;; A candidate at position p (relative to the combined chunk's start) survives iff
;; it agrees with BOTH sides where it overlaps them.  Where its overlap with a
;; side is empty that side imposes nothing (vacuously true); otherwise the side
;; must carry it as one of its stored crossers.
(define (combine-wm W A B)
  (define m (string-length W))
  (define lenA (wm-len A)) (define lenB (wm-len B)) (define len (+ lenA lenB))
  (define PB* (map (lambda (q) (+ q lenA)) (wm-posns B)))     ; B's posns, re-based to A's start
  (define (okA? p) (or (= lenA 0) (>= p lenA) (<= (+ p m) 0)  ; empty overlap with A
                       (memv p (wm-posns A))))
  (define (okB? p) (or (= lenB 0) (<= (+ p m) lenA) (>= p len) ; empty overlap with B
                       (memv p PB*)))
  (define pool (remove-duplicates (append (wm-posns A) PB*)))
  (define kept (filter (lambda (p) (and (okA? p) (okB? p))) pool))
  (define-values (done crossing)                              ; now-complete vs still-crossing
    (partition (lambda (p) (and (>= p 0) (<= (+ p m) len))) kept))
  (wm (+ (wm-count A) (wm-count B) (length done)) (sort crossing <) len))

;; ---- the summary value: word -> wm, cached in ONE global capped trie ---------
;; word -> cell -> wm.  A cell keys ITSELF (procedure identity).

(define missing (string->uninterned-symbol "missing"))
(define-values (cache-put! cache-ref cache-clear! cache-walk)
  (make-trie #:caps '(64 10000)))

(define (cached cell W compute)
  (define hit (cache-ref missing W cell))
  (if (eq? hit missing)
      (let ([v (compute)]) (cache-put! v W cell) v)
      hit))

(define (leaf-cell s)
  (define (cell W) (cached cell W (lambda () (leaf-wm W s))))
  cell)

(define (combine-cell A B)
  (define (cell W) (cached cell W (lambda () (combine-wm W (A W) (B W)))))
  cell)

(define occur-smr (make-summary leaf-cell combine-cell))

;; ---- queries -----------------------------------------------------------------

(define (occur-prop root W) ((rope-summary root) W))   ; the STORED cell -- stable key, no wrapper
(define (occur-count root W) (wm-count (occur-prop root W)))

;; the complete-match start offsets inside a leaf string (a fresh local scan)
(define (leaf-match-starts W s)
  (define m (string-length W)) (define L (string-length s))
  (for/list ([p (in-range 0 (add1 (- L m)))]
             #:when (for/and ([i (in-range m)])
                      (char=? (string-ref s (+ p i)) (string-ref W i))))
    p))

;; the seam matches at a branch L*R: alignments that start in L, cross the L|R
;; seam, and END within R -- returned as start offsets relative to the node start
;; (deepest-in-L first would need a reverse; ascending is what select wants).
(define (seam-starts W wl wr lenL lenR)
  (define m (string-length W))
  (sort
   (for/list ([p (in-list (wm-posns wl))]
              #:when (and (> (+ p m) lenL)                    ; crosses into R
                          (<= (+ p m) (+ lenL lenR))          ; ends within R
                          (memv (- p lenL) (wm-posns wr))))   ; R carries the continuation
     p)
   <))

;; the k-th match's absolute offset, by an order-statistics descent on `count`.
;; Order within a node: the cL matches inside L, then the seam matches (starts in
;; L, ends in R), then the matches inside R.
(define (occur-select root W k)
  (let descend ([node root] [k k] [base 0])
    (cond
      [(leaf? node)
       (+ base (list-ref (leaf-match-starts W (leaf-text node)) k))]
      [else
       (define L (branch-left node)) (define R (branch-right node))
       (define wl ((rope-summary L) W)) (define wr ((rope-summary R) W))
       (define cL (wm-count wl)) (define lenL (wm-len wl)) (define lenR (wm-len wr))
       (cond
         [(< k cL) (descend L k base)]                         ; k-th match is inside the left
         [else
          (define seam (seam-starts W wl wr lenL lenR))
          (define k* (- k cL))
          (if (< k* (length seam))
              (+ base (list-ref seam k*))                      ; ... straddles the seam
              (descend R (- k* (length seam)) (+ base lenL)))])]))) ; ... in the right

;; ============================================================================
(module+ test
  (require rackunit)
  (define build (make-rope occur-smr))

  (define (brute text W)
    (for/list ([i (in-range 0 (add1 (- (string-length text) (string-length W))))]
               #:when (string=? (substring text i (+ i (string-length W))) W))
      i))

  ;; one word-agnostic build answers many words; count + select agree with brute
  (define text "a needle. a needled fox. needle box needle")
  (define doc (build text))
  (for ([W '("needle" "needled" "eedl" "ee" "box" "zzz" "e")])
    (check-equal? (occur-count doc W) (length (brute text W)) (format "count ~s" W))
    (for ([k (in-range (occur-count doc W))])
      (check-equal? (occur-select doc W k) (list-ref (brute text W) k) (format "select ~s ~a" W k))))

  ;; seam-spanning across chunks the build keeps as branches (match splits at the seam)
  (define seam (build (make-string 40 #\.) "nee" "dle" (make-string 40 #\.)))
  (check-equal? (occur-count seam "needle") 1)
  (check-equal? (occur-select seam "needle" 0) 40)

  ;; THE CONDUIT: a match threading a chunk shorter than W -- the case the old
  ;; two-ended shape mis-counted.  "ed" (= W[2..4)) is swallowed whole.
  (define thread (build "ne" "ed" "le"))
  (check-equal? (occur-count thread "needle") 1)
  (check-equal? (occur-select thread "needle" 0) 0)
  (define thread2 (build "xx a " "ne" "ed" "le" " yy"))     ; conduit with flanks, at an offset
  (check-equal? (occur-count thread2 "needle") 1)
  (check-equal? (occur-select thread2 "needle" 0) 5)

  ;; the object at a chunk ending mid-word carries the right partial as a START
  (define m (occur-prop (build "aa need") "needle"))
  (check-equal? (wm-count m) 0)
  (check-equal? (wm-posns m) '(3))          ; "needle" would start at offset 3 ("need" seen so far)
  (check-equal? (wm-len m) 7)

  ;; the cache is warm after a query and enumerable: the word leads its sub-map
  (define warm '())
  (cache-walk (lambda (path v) (set! warm (cons (car path) warm))))
  (check-not-false (member "needle" warm) "needle cached")

  ;; combine is associative on the object (a lawful monoid)
  (define A (leaf-wm "aba" "ab")) (define B (leaf-wm "aba" "aab")) (define C (leaf-wm "aba" "a"))
  (check-equal? (combine-wm "aba" (combine-wm "aba" A B) C)
                (combine-wm "aba" A (combine-wm "aba" B C)))

  ;; empty chunk is the identity (the short-chunk case that sinks a two-list form)
  (define e (leaf-wm "aa" ""))
  (define x (leaf-wm "aa" "aaa"))
  (check-equal? (combine-wm "aa" e x) x)
  (check-equal? (combine-wm "aa" x e) x)

  ;; overlapping W across chunks: both straddlers counted, both starts reachable
  (define adoc (build "xx" "aaaa" "yy"))
  (check-equal? (occur-count adoc "aaa") 2)
  (check-equal? (occur-select adoc "aaa" 0) 2)
  (check-equal? (occur-select adoc "aaa" 1) 3))
