(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-not-verified (err u105))
(define-constant err-collection-not-found (err u106))
(define-constant err-already-verified (err u107))
(define-constant err-invalid-location (err u108))

(define-data-var token-name (string-ascii 12) "Plastix")
(define-data-var token-symbol (string-ascii 3) "PLX")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)
(define-data-var collection-id-nonce uint u0)
(define-data-var min-collection-weight uint u100)
(define-data-var base-reward-rate uint u10)

(define-map token-balances principal uint)
(define-map collection-records 
  uint 
  {
    collector: principal,
    weight: uint,
    location: (string-ascii 64),
    timestamp: uint,
    verified: bool,
    reward-amount: uint
  }
)
(define-map user-profiles 
  principal 
  {
    total-collections: uint,
    total-weight: uint,
    total-rewards: uint,
    registration-block: uint,
    verified-collector: bool
  }
)
(define-map verified-collectors principal bool)
(define-map location-stats 
  (string-ascii 64) 
  {
    total-collections: uint,
    total-weight: uint
  }
)

(define-read-only (get-name)
  (ok (var-get token-name))
)

(define-read-only (get-symbol)
  (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
  (ok (var-get token-decimals))
)

(define-read-only (get-total-supply)
  (ok (var-get total-supply))
)

(define-read-only (get-balance (who principal))
  (ok (default-to u0 (map-get? token-balances who)))
)

(define-read-only (get-collection-record (collection-id uint))
  (map-get? collection-records collection-id)
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user)
)

(define-read-only (get-location-stats (location (string-ascii 64)))
  (map-get? location-stats location)
)

(define-read-only (is-verified-collector (user principal))
  (default-to false (map-get? verified-collectors user))
)

(define-read-only (get-next-collection-id)
  (ok (+ (var-get collection-id-nonce) u1))
)

(define-read-only (calculate-reward (weight uint))
  (ok (* weight (var-get base-reward-rate)))
)

(define-private (mint-tokens (recipient principal) (amount uint))
  (begin
    (var-set total-supply (+ (var-get total-supply) amount))
    (map-set token-balances 
      recipient 
      (+ (default-to u0 (map-get? token-balances recipient)) amount)
    )
    (ok amount)
  )
)

(define-private (transfer-tokens (sender principal) (recipient principal) (amount uint))
  (let 
    ((sender-balance (default-to u0 (map-get? token-balances sender))))
    (if (>= sender-balance amount)
      (begin
        (map-set token-balances sender (- sender-balance amount))
        (map-set token-balances 
          recipient 
          (+ (default-to u0 (map-get? token-balances recipient)) amount)
        )
        (ok amount)
      )
      err-insufficient-balance
    )
  )
)

(define-private (update-location-stats (location (string-ascii 64)) (weight uint))
  (let 
    ((current-stats (default-to {total-collections: u0, total-weight: u0} 
                     (map-get? location-stats location))))
    (map-set location-stats location
      {
        total-collections: (+ (get total-collections current-stats) u1),
        total-weight: (+ (get total-weight current-stats) weight)
      }
    )
    (ok true)
  )
)

(define-public (register-collector)
  (let 
    ((current-profile (map-get? user-profiles tx-sender)))
    (if (is-none current-profile)
      (begin
        (map-set user-profiles tx-sender
          {
            total-collections: u0,
            total-weight: u0,
            total-rewards: u0,
            registration-block: stacks-block-height,
            verified-collector: false
          }
        )
        (ok true)
      )
      err-already-exists
    )
  )
)

(define-public (submit-collection (weight uint) (location (string-ascii 64)))
  (let 
    ((collection-id (+ (var-get collection-id-nonce) u1))
     (current-profile (map-get? user-profiles tx-sender)))
    (if (and (> weight u0) (>= weight (var-get min-collection-weight)))
      (if (is-some current-profile)
        (begin
          (var-set collection-id-nonce collection-id)
          (map-set collection-records collection-id
            {
              collector: tx-sender,
              weight: weight,
              location: location,
              timestamp: stacks-block-height,
              verified: false,
              reward-amount: u0
            }
          )
          (unwrap-panic (update-location-stats location weight))
          (ok collection-id)
        )
        err-not-found
      )
      err-invalid-amount
    )
  )
)

(define-public (verify-collection (collection-id uint))
  (let 
    ((collection-record (map-get? collection-records collection-id)))
    (if (is-eq tx-sender contract-owner)
      (if (is-some collection-record)
        (let 
          ((record (unwrap-panic collection-record))
           (collector (get collector record))
           (weight (get weight record))
           (reward-amount (unwrap-panic (calculate-reward weight)))
           (current-profile (unwrap-panic (map-get? user-profiles collector))))
          (if (not (get verified record))
            (begin
              (map-set collection-records collection-id
                (merge record {verified: true, reward-amount: reward-amount})
              )
              (map-set user-profiles collector
                {
                  total-collections: (+ (get total-collections current-profile) u1),
                  total-weight: (+ (get total-weight current-profile) weight),
                  total-rewards: (+ (get total-rewards current-profile) reward-amount),
                  registration-block: (get registration-block current-profile),
                  verified-collector: true
                }
              )
              (map-set verified-collectors collector true)
              (unwrap-panic (mint-tokens collector reward-amount))
              (ok reward-amount)
            )
            err-already-verified
          )
        )
        err-collection-not-found
      )
      err-owner-only
    )
  )
)

(define-public (transfer (amount uint) (recipient principal))
  (transfer-tokens tx-sender recipient amount)
)

(define-public (set-min-collection-weight (new-weight uint))
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set min-collection-weight new-weight)
      (ok new-weight)
    )
    err-owner-only
  )
)

(define-public (set-base-reward-rate (new-rate uint))
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set base-reward-rate new-rate)
      (ok new-rate)
    )
    err-owner-only
  )
)

;; 

(define-private (verify-collection-internal (collection-id uint))
  (match (verify-collection collection-id)
    success success
    error u0
  )
)



(define-private (get-collection-entry-by-id (id uint))
  {id: id, value: (default-to 
                   {collector: contract-owner, weight: u0, location: "", 
                    timestamp: u0, verified: false, reward-amount: u0}
                   (map-get? collection-records id))}
)





(define-read-only (get-total-collections)
  (ok (var-get collection-id-nonce))
)

(define-read-only (get-contract-stats)
  (ok {
    total-supply: (var-get total-supply),
    total-collections: (var-get collection-id-nonce),
    min-collection-weight: (var-get min-collection-weight),
    base-reward-rate: (var-get base-reward-rate)
  })
)
