;; Community Insurance against 
;; A decentralized insurance pool for protection against cyber threats

;; Define the insurance pool token
(define-fungible-token insurance-pool-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-claim-already-exists (err u103))
(define-constant min-contribution u1000000) ;; 1 STX minimum contribution

;; Data Variables
(define-data-var total-pool-balance uint u0)
(define-data-var claim-counter uint u0)

;; Data Maps
(define-map member-contributions principal uint)
(define-map member-claim-history principal (list 10 uint))
(define-map claim-requests uint {
    claimant: principal,
    amount: uint,
    incident-type: (string-ascii 50),
    description: (string-ascii 500),
    timestamp: uint,
    status: (string-ascii 20)
})

;; Events for off-chain tracking
(define-data-var last-event-id uint u0)

;; Function 1: Contribute to Insurance Pool
;; Members can stake STX tokens as insurance premiums
(define-public (contribute-to-pool (amount uint))
  (let (
    (current-contribution (default-to u0 (map-get? member-contributions tx-sender)))
  )
    ;; Validate minimum contribution
    (asserts! (>= amount min-contribution) err-invalid-amount)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update member's total contribution
    (map-set member-contributions tx-sender (+ current-contribution amount))
    
    ;; Update total pool balance
    (var-set total-pool-balance (+ (var-get total-pool-balance) amount))
    
    ;; Emit contribution event
    (var-set last-event-id (+ (var-get last-event-id) u1))
    (print {
      event: "pool-contribution",
      event-id: (var-get last-event-id),
      contributor: tx-sender,
      amount: amount,
      new-total-balance: (var-get total-pool-balance),
      timestamp: stacks-block-height
    })
    
    (ok {
      contributor: tx-sender,
      contribution-amount: amount,
      total-contribution: (+ current-contribution amount),
      pool-balance: (var-get total-pool-balance)
    })))

;; Function 2: Request Insurance Claim
;; Members can submit claims for cyber attack incidents
(define-public (request-claim (amount uint) (incident-type (string-ascii 50)) (description (string-ascii 500)))
  (let (
    (claim-id (+ (var-get claim-counter) u1))
    (member-contribution (default-to u0 (map-get? member-contributions tx-sender)))
    (current-claims (default-to (list) (map-get? member-claim-history tx-sender)))
  )
    ;; Validate that user is a contributing member
    (asserts! (> member-contribution u0) err-insufficient-balance)
    
    ;; Validate claim amount is reasonable (not more than 10x their contribution)
    (asserts! (<= amount (* member-contribution u10)) err-invalid-amount)
    
    ;; Validate claim amount is positive
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Create claim request
    (map-set claim-requests claim-id {
      claimant: tx-sender,
      amount: amount,
      incident-type: incident-type,
      description: description,
      timestamp: stacks-block-height,
      status: "pending"
    })
    
    ;; Update claim counter
    (var-set claim-counter claim-id)
    
    ;; Add claim ID to member's history (if list isn't full)
    (if (< (len current-claims) u10)
      (map-set member-claim-history tx-sender (unwrap-panic (as-max-len? (append current-claims claim-id) u10)))
      true ;; Skip if history is full
    )
    
    ;; Emit claim request event
    (var-set last-event-id (+ (var-get last-event-id) u1))
    (print {
      event: "claim-requested",
      event-id: (var-get last-event-id),
      claim-id: claim-id,
      claimant: tx-sender,
      amount: amount,
      incident-type: incident-type,
      description: description,
      timestamp: stacks-block-height,
      pool-balance: (var-get total-pool-balance)
    })
    
    (ok {
      claim-id: claim-id,
      claimant: tx-sender,
      amount: amount,
      incident-type: incident-type,
      status: "pending",
      timestamp: stacks-block-height
    })))

;; Read-only functions for querying contract state

;; Get total pool balance
(define-read-only (get-pool-balance)
  (ok (var-get total-pool-balance)))

;; Get member's contribution
(define-read-only (get-member-contribution (member principal))
  (ok (default-to u0 (map-get? member-contributions member))))

;; Get claim details
(define-read-only (get-claim-details (claim-id uint))
  (ok (map-get? claim-requests claim-id)))

;; Get member's claim history
(define-read-only (get-member-claims (member principal))
  (ok (default-to (list) (map-get? member-claim-history member))))

;; Get total number of claims
(define-read-only (get-total-claims)
  (ok (var-get claim-counter)))

;; Get contract statistics
(define-read-only (get-contract-stats)
  (ok {
    total-pool-balance: (var-get total-pool-balance),
    total-claims: (var-get claim-counter),
    min-contribution: min-contribution
  }))