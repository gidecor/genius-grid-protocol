;; Genius Grid - Skill Exchange Network
;; Decentralized Knowledge Exchange Platform and system for peer exchange of knowledge hours facilitated through crypto token payments
;; This smart contract enables a marketplace for expertise trading

;; Platform Configuration Variables
(define-data-var hourly-compensation-base uint u10) ;; Base compensation per hour (in microstacks)
(define-data-var network-commission uint u10) ;; Commission percentage taken by the platform (e.g., 10%)
(define-data-var expertise-hour-capacity-total uint u0) ;; Current network capacity of expertise hours
(define-data-var expertise-hour-capacity-maximum uint u1000) ;; Maximum allowed network capacity
(define-data-var expertise-hour-limit-per-member uint u100) ;; Maximum contributions per member

;; Administrative Constants
(define-constant admin-address tx-sender)
(define-constant error-admin-restricted (err u300))
(define-constant error-funds-depleted (err u301))
(define-constant error-expertise-invalid (err u302))
(define-constant error-compensation-invalid (err u303))
(define-constant error-network-capacity-exceeded (err u304))
(define-constant error-forbidden-action (err u305))

;; Storage Structures
(define-map member-expertise-holdings principal uint) ;; Member's expertise balance in hours
(define-map member-token-holdings principal uint) ;; Member's token balance
(define-map expertise-offerings {member: principal} {hours-available: uint, compensation-rate: uint})

;; Helper Functions

;; Calculate the platform's commission on a transaction
(define-private (compute-network-commission (transaction-value uint))
  (/ (* transaction-value (var-get network-commission)) u100))

;; Update the network's available expertise capacity
(define-private (modify-expertise-capacity (change-amount int))
  (let (
    (current-capacity (var-get expertise-hour-capacity-total))
    (adjusted-capacity (if (< change-amount 0)
                     (if (>= current-capacity (to-uint (- 0 change-amount)))
                         (- current-capacity (to-uint (- 0 change-amount)))
                         u0)
                     (+ current-capacity (to-uint change-amount))))
  )
    (asserts! (<= adjusted-capacity (var-get expertise-hour-capacity-maximum)) error-network-capacity-exceeded)
    (var-set expertise-hour-capacity-total adjusted-capacity)
    (ok true)))

;; Core Transaction Functions

;; List expertise hours on the marketplace
(define-public (list-expertise-hours (hours uint) (rate uint))
  (let (
    (current-holdings (default-to u0 (map-get? member-expertise-holdings tx-sender)))
    (current-listing (get hours-available (default-to {hours-available: u0, compensation-rate: u0} 
                                           (map-get? expertise-offerings {member: tx-sender}))))
    (total-listed (+ hours current-listing))
  )
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (> rate u0) error-compensation-invalid) ;; Rate must be positive
    (asserts! (>= current-holdings total-listed) error-funds-depleted)
    (try! (modify-expertise-capacity (to-int hours)))
    (map-set expertise-offerings {member: tx-sender} {hours-available: total-listed, compensation-rate: rate})
    (ok true)))
