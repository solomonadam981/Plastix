;; title: Carbon Credits
;; version: 1.0.0
;; summary: Carbon credit system for plastic waste collection environmental impact
;; description: Generate and trade carbon credits based on environmental impact of verified plastic collections

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-not-found (err u401))
(define-constant err-unauthorized (err u402))
(define-constant err-invalid-amount (err u403))
(define-constant err-insufficient-balance (err u404))
(define-constant err-credit-retired (err u405))
(define-constant err-marketplace-closed (err u406))
(define-constant err-listing-not-found (err u407))
(define-constant err-self-trade (err u408))

;; Carbon credit calculation constants
(define-constant co2-per-kg-pet u2800) ;; kg CO2 saved per kg PET recycled
(define-constant co2-per-kg-hdpe u2100) 
(define-constant co2-per-kg-pvc u1900)
(define-constant co2-per-kg-ldpe u2000)
(define-constant co2-per-kg-pp u2200)
(define-constant co2-per-kg-ps u2600)
(define-constant co2-per-kg-other u1500)
(define-constant credits-per-ton-co2 u1000) ;; 1000 credits per ton CO2 saved
(define-constant marketplace-fee u250) ;; 2.5% marketplace fee

;; data vars
(define-data-var next-credit-id uint u1)
(define-data-var total-credits-issued uint u0)
(define-data-var total-credits-traded uint u0)
(define-data-var total-co2-impact uint u0)
(define-data-var marketplace-active bool true)

;; data maps
(define-map carbon-credits
  uint
  {
    collector: principal,
    plastic-collection-id: uint,
    co2-impact: uint,
    credit-amount: uint,
    issue-block: uint,
    plastic-type: (string-ascii 16),
    retired: bool,
    retirement-block: uint,
    retirement-purpose: (string-ascii 100)
  }
)

(define-map credit-balances
  principal
  uint
)

(define-map collector-impact-stats
  principal
  {
    total-co2-saved: uint,
    total-credits-earned: uint,
    credits-traded: uint,
    credits-retired: uint,
    impact-score: uint
  }
)

(define-map credit-marketplace
  uint
  {
    seller: principal,
    price-per-credit: uint,
    credits-available: uint,
    listing-block: uint,
    is-active: bool
  }
)

(define-map retired-credits
  uint
  {
    credit-id: uint,
    retired-by: principal,
    retirement-purpose: (string-ascii 100),
    retirement-block: uint,
    co2-offset: uint
  }
)

;; public functions
(define-public (generate-credits-from-collection (collection-id uint))
  (let ((collection-data (contract-call? .Plastix get-collection-record collection-id)))
    (match collection-data
      collection (let ((collector (get collector collection))
                       (weight (get weight collection))
                       (plastic-type (get plastic-type collection))
                       (verified (get verified collection))
                       (credit-id (var-get next-credit-id))
                       (co2-saved (calculate-co2-impact weight plastic-type))
                       (credit-amount (/ (* co2-saved credits-per-ton-co2) u1000000))) ;; Convert to credits
        
        (asserts! verified err-unauthorized)
        (asserts! (> credit-amount u0) err-invalid-amount)
        
        (map-set carbon-credits credit-id {
          collector: collector,
          plastic-collection-id: collection-id,
          co2-impact: co2-saved,
          credit-amount: credit-amount,
          issue-block: stacks-block-height,
          plastic-type: plastic-type,
          retired: false,
          retirement-block: u0,
          retirement-purpose: ""
        })
        
        ;; Update collector's credit balance
        (map-set credit-balances collector 
          (+ (default-to u0 (map-get? credit-balances collector)) credit-amount))
        
        ;; Update stats
        (update-collector-impact-stats collector co2-saved credit-amount)
        (var-set total-credits-issued (+ (var-get total-credits-issued) credit-amount))
        (var-set total-co2-impact (+ (var-get total-co2-impact) co2-saved))
        (var-set next-credit-id (+ credit-id u1))
        
        (ok credit-id))
      err-not-found)
  )
)

(define-public (list-credits-for-sale (credit-amount uint) (price-per-credit uint))
  (let ((seller-balance (default-to u0 (map-get? credit-balances tx-sender)))
        (listing-id (var-get next-credit-id)))
    
    (asserts! (var-get marketplace-active) err-marketplace-closed)
    (asserts! (> credit-amount u0) err-invalid-amount)
    (asserts! (> price-per-credit u0) err-invalid-amount)
    (asserts! (>= seller-balance credit-amount) err-insufficient-balance)
    
    ;; Lock credits in marketplace
    (map-set credit-balances tx-sender (- seller-balance credit-amount))
    
    (map-set credit-marketplace listing-id {
      seller: tx-sender,
      price-per-credit: price-per-credit,
      credits-available: credit-amount,
      listing-block: stacks-block-height,
      is-active: true
    })
    
    (var-set next-credit-id (+ listing-id u1))
    (ok listing-id)
  )
)

(define-public (buy-credits (listing-id uint) (credits-to-buy uint))
  (let ((listing (unwrap! (map-get? credit-marketplace listing-id) err-listing-not-found))
        (total-cost (* credits-to-buy (get price-per-credit listing)))
        (marketplace-fee-amount (/ (* total-cost marketplace-fee) u10000))
        (seller-payment (- total-cost marketplace-fee-amount))
        (buyer-balance (default-to u0 (map-get? credit-balances tx-sender))))
    
    (asserts! (get is-active listing) err-listing-not-found)
    (asserts! (not (is-eq tx-sender (get seller listing))) err-self-trade)
    (asserts! (<= credits-to-buy (get credits-available listing)) err-invalid-amount)
    (asserts! (> credits-to-buy u0) err-invalid-amount)
    
    ;; Transfer STX payment
    (try! (stx-transfer? seller-payment tx-sender (get seller listing)))
    (try! (stx-transfer? marketplace-fee-amount tx-sender contract-owner))
    
    ;; Transfer credits to buyer
    (map-set credit-balances tx-sender (+ buyer-balance credits-to-buy))
    
    ;; Update listing
    (let ((remaining-credits (- (get credits-available listing) credits-to-buy)))
      (if (is-eq remaining-credits u0)
        (map-set credit-marketplace listing-id (merge listing {is-active: false}))
        (map-set credit-marketplace listing-id (merge listing {credits-available: remaining-credits}))
      )
    )
    
    (var-set total-credits-traded (+ (var-get total-credits-traded) credits-to-buy))
    (ok credits-to-buy)
  )
)

(define-public (retire-credits (credit-amount uint) (purpose (string-ascii 100)))
  (let ((user-balance (default-to u0 (map-get? credit-balances tx-sender)))
        (retirement-id (var-get next-credit-id)))
    
    (asserts! (>= user-balance credit-amount) err-insufficient-balance)
    (asserts! (> credit-amount u0) err-invalid-amount)
    
    ;; Burn credits from circulation
    (map-set credit-balances tx-sender (- user-balance credit-amount))
    
    (map-set retired-credits retirement-id {
      credit-id: retirement-id,
      retired-by: tx-sender,
      retirement-purpose: purpose,
      retirement-block: stacks-block-height,
      co2-offset: (/ (* credit-amount u1000000) credits-per-ton-co2)
    })
    
    ;; Update collector stats
    (match (map-get? collector-impact-stats tx-sender)
      stats (map-set collector-impact-stats tx-sender (merge stats {
        credits-retired: (+ (get credits-retired stats) credit-amount)
      }))
      true
    )
    
    (var-set next-credit-id (+ retirement-id u1))
    (ok retirement-id)
  )
)

;; read-only functions
(define-read-only (get-carbon-credit (credit-id uint))
  (map-get? carbon-credits credit-id)
)

(define-read-only (get-credit-balance (holder principal))
  (default-to u0 (map-get? credit-balances holder))
)

(define-read-only (get-marketplace-listing (listing-id uint))
  (map-get? credit-marketplace listing-id)
)

(define-read-only (get-collector-impact-stats (collector principal))
  (map-get? collector-impact-stats collector)
)

(define-read-only (get-carbon-credit-stats)
  {
    total-credits-issued: (var-get total-credits-issued),
    total-credits-traded: (var-get total-credits-traded),
    total-co2-impact: (var-get total-co2-impact),
    marketplace-active: (var-get marketplace-active),
    next-credit-id: (var-get next-credit-id)
  }
)

(define-read-only (calculate-co2-impact-for-collection (weight uint) (plastic-type (string-ascii 16)))
  (some (calculate-co2-impact weight plastic-type))
)

;; private functions
(define-private (calculate-co2-impact (weight uint) (plastic-type (string-ascii 16)))
  ;; Calculate CO2 saved based on plastic type and weight (in grams to CO2 in grams)
  (let ((weight-kg (/ weight u1000))) ;; Convert grams to kg
    (if (is-eq plastic-type "PET")
      (* weight-kg co2-per-kg-pet)
      (if (is-eq plastic-type "HDPE")
        (* weight-kg co2-per-kg-hdpe)
        (if (is-eq plastic-type "PVC")
          (* weight-kg co2-per-kg-pvc)
          (if (is-eq plastic-type "LDPE")
            (* weight-kg co2-per-kg-ldpe)
            (if (is-eq plastic-type "PP")
              (* weight-kg co2-per-kg-pp)
              (if (is-eq plastic-type "PS")
                (* weight-kg co2-per-kg-ps)
                (* weight-kg co2-per-kg-other) ;; default for OTHER/unknown
              ))))))
  )
)

(define-private (update-collector-impact-stats (collector principal) (co2-saved uint) (credits-earned uint))
  (match (map-get? collector-impact-stats collector)
    stats (map-set collector-impact-stats collector (merge stats {
      total-co2-saved: (+ (get total-co2-saved stats) co2-saved),
      total-credits-earned: (+ (get total-credits-earned stats) credits-earned),
      impact-score: (calculate-impact-score (+ (get total-co2-saved stats) co2-saved) (+ (get total-credits-earned stats) credits-earned))
    }))
    (map-set collector-impact-stats collector {
      total-co2-saved: co2-saved,
      total-credits-earned: credits-earned,
      credits-traded: u0,
      credits-retired: u0,
      impact-score: (calculate-impact-score co2-saved credits-earned)
    })
  )
)

(define-private (calculate-impact-score (total-co2 uint) (total-credits uint))
  ;; Simple scoring: 1 point per 100g CO2 saved + 10 points per credit
  (+ (/ total-co2 u100) (* total-credits u10))
)
