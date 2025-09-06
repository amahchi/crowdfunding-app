;; Crowdfunding Smart Contract
;; This contract allows users to create and contribute to crowdfunding campaigns.
;; The campaign creator can withdraw funds if the goal is met by the deadline.
;; Contributors can claim refunds if the campaign fails to meet its goal by the deadline.

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-campaign-exists (err u101))
(define-constant err-campaign-not-found (err u102))
(define-constant err-deadline-passed (err u103))
(define-constant err-deadline-not-passed (err u104))
(define-constant err-goal-not-met (err u105))
(define-constant err-already-claimed (err u106))
(define-constant err-unauthorized (err u107))
(define-constant err-zero-contribution (err u108))

;; Data structures
(define-map campaigns
  { campaign-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    goal: uint,
    deadline: uint,
    raised: uint,
    claimed: bool
  }
)

(define-map contributions
  { campaign-id: uint, contributor: principal }
  { amount: uint, refunded: bool }
)

(define-data-var campaign-count uint u0)

;; Read-only functions
(define-read-only (get-campaign-count)
  (var-get campaign-count)
)

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (default-to
    { amount: u0, refunded: false }
    (map-get? contributions { campaign-id: campaign-id, contributor: contributor })
  )
)

(define-read-only (get-campaign-raised (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign (get raised campaign)
    u0
  )
)

(define-read-only (is-campaign-successful (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign (and
      (>= (get raised campaign) (get goal campaign)) 
      (> block-height (get deadline campaign))
    )
    false
  )
)

(define-read-only (is-campaign-failed (campaign-id uint))
  (match (map-get? campaigns { campaign-id: campaign-id })
    campaign (and
      (< (get raised campaign) (get goal campaign)) 
      (> block-height (get deadline campaign))
    )
    false
  )
)

;; Public functions
(define-public (create-campaign (title (string-utf8 100)) (description (string-utf8 500)) (goal uint) (deadline uint))
  (let
    (
      (new-id (+ (var-get campaign-count) u1))
    )
    (asserts! (> goal u0) (err err-zero-contribution))
    (asserts! (> deadline block-height) (err err-deadline-passed))
    
    (map-set campaigns
      { campaign-id: new-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        goal: goal,
        deadline: deadline,
        raised: u0,
        claimed: false
      }
    )
    
    (var-set campaign-count new-id)
    (ok new-id)
  )
)

(define-public (contribute (campaign-id uint) (amount uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-campaign-not-found)))
      (current-contribution (get-contribution campaign-id tx-sender))
    )
    
    ;; Check that contribution is greater than zero
    (asserts! (> amount u0) (err err-zero-contribution))
    
    ;; Check that the deadline hasn't passed
    (asserts! (<= block-height (get deadline campaign)) (err err-deadline-passed))
    
    ;; Update the campaign's raised amount
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { raised: (+ (get raised campaign) amount) })
    )
    
    ;; Update contributor's contributions
    (map-set contributions
      { campaign-id: campaign-id, contributor: tx-sender }
      { 
        amount: (+ (get amount current-contribution) amount), 
        refunded: false 
      }
    )
    
    ;; Transfer STX from contributor to contract
    (stx-transfer? amount tx-sender (as-contract tx-sender))
  )
)

(define-public (claim-funds (campaign-id uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-campaign-not-found)))
    )
    
    ;; Check that sender is the campaign creator
    (asserts! (is-eq (get creator campaign) tx-sender) (err err-unauthorized))
    
    ;; Check that the deadline has passed
    (asserts! (> block-height (get deadline campaign)) (err err-deadline-not-passed))
    
    ;; Check that the goal was met
    (asserts! (>= (get raised campaign) (get goal campaign)) (err err-goal-not-met))
    
    ;; Check that funds haven't already been claimed
    (asserts! (not (get claimed campaign)) (err err-already-claimed))
    
    ;; Mark the campaign as claimed
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { claimed: true })
    )
    
    ;; Transfer STX from contract to campaign creator
    (as-contract (stx-transfer? (get raised campaign) tx-sender (get creator campaign)))
  )
)

(define-public (claim-refund (campaign-id uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-campaign-not-found)))
      (contribution (get-contribution campaign-id tx-sender))
    )
    
    ;; Check that the deadline has passed
    (asserts! (> block-height (get deadline campaign)) (err err-deadline-not-passed))
    
    ;; Check that the goal was not met
    (asserts! (< (get raised campaign) (get goal campaign)) (err err-goal-not-met))
    
    ;; Check that the contribution exists and hasn't been refunded
    (asserts! (and (> (get amount contribution) u0) (not (get refunded contribution))) (err err-already-claimed))
    
    ;; Mark the contribution as refunded
    (map-set contributions
      { campaign-id: campaign-id, contributor: tx-sender }
      (merge contribution { refunded: true })
    )
    
    ;; Transfer STX from contract back to contributor
    (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender))
  )
)