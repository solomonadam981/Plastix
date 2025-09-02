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
(define-constant err-leaderboard-not-found (err u109))
(define-constant err-invalid-timeframe (err u110))
(define-constant err-achievement-exists (err u111))
(define-constant err-invalid-plastic-type (err u112))
(define-constant err-plastic-type-not-found (err u113))
(define-constant err-insufficient-verification-level (err u114))
(define-constant err-plastic-type-exists (err u115))

(define-data-var token-name (string-ascii 12) "Plastix")
(define-data-var token-symbol (string-ascii 3) "PLX")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)
(define-data-var collection-id-nonce uint u0)
(define-data-var min-collection-weight uint u100)
(define-data-var base-reward-rate uint u10)
(define-data-var leaderboard-season uint u1)
(define-data-var season-duration uint u10080)
(define-data-var current-season-start uint u0)
(define-data-var plastic-type-counter uint u0)

(define-map token-balances principal uint)
(define-map collection-records 
  uint 
  {
    collector: principal,
    weight: uint,
    location: (string-ascii 64),
    timestamp: uint,
    verified: bool,
    reward-amount: uint,
    plastic-type: (string-ascii 16),
    recycling-score: uint
  }
)
(define-map user-profiles 
  principal 
  {
    total-collections: uint,
    total-weight: uint,
    total-rewards: uint,
    registration-block: uint,
    verified-collector: bool,
    specialization-type: (string-ascii 16),
    verification-level: uint
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
(define-map leaderboard-entries 
  {season: uint, rank: uint}
  {
    collector: principal,
    total-weight: uint,
    total-collections: uint,
    reputation-points: uint
  }
)
(define-map reputation-scores 
  principal 
  {
    total-points: uint,
    current-season-points: uint,
    achievements: (list 10 (string-ascii 32)),
    consecutive-seasons: uint,
    highest-rank: uint
  }
)
(define-map season-participants 
  {season: uint, collector: principal}
  {
    weight-collected: uint,
    collections-count: uint,
    final-rank: uint,
    points-earned: uint
  }
)
(define-map achievement-definitions 
  (string-ascii 32)
  {
    name: (string-ascii 64),
    description: (string-ascii 128),
    points-value: uint,
    requirement-type: (string-ascii 16),
    requirement-value: uint
  }
)
(define-map plastic-type-definitions 
  (string-ascii 16)
  {
    name: (string-ascii 64),
    reward-multiplier: uint,
    min-verification-level: uint,
    recycling-difficulty: uint,
    market-value: uint,
    environmental-impact: uint
  }
)
(define-map plastic-type-stats 
  (string-ascii 16)
  {
    total-collections: uint,
    total-weight: uint,
    total-recycling-score: uint,
    average-market-value: uint
  }
)
(define-map collector-plastic-specialization 
  {collector: principal, plastic-type: (string-ascii 16)}
  {
    collections-count: uint,
    total-weight: uint,
    expertise-level: uint,
    certification-earned: bool
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

(define-read-only (get-plastic-type-definition (plastic-type (string-ascii 16)))
  (map-get? plastic-type-definitions plastic-type)
)

(define-read-only (get-plastic-type-stats (plastic-type (string-ascii 16)))
  (default-to 
    {total-collections: u0, total-weight: u0, total-recycling-score: u0, average-market-value: u0}
    (map-get? plastic-type-stats plastic-type)
  )
)

(define-read-only (get-collector-specialization (collector principal) (plastic-type (string-ascii 16)))
  (default-to 
    {collections-count: u0, total-weight: u0, expertise-level: u0, certification-earned: false}
    (map-get? collector-plastic-specialization {collector: collector, plastic-type: plastic-type})
  )
)

(define-read-only (calculate-plastic-reward (weight uint) (plastic-type (string-ascii 16)))
  (let 
    ((plastic-def (unwrap! (get-plastic-type-definition plastic-type) err-plastic-type-not-found))
     (base-reward (* weight (var-get base-reward-rate)))
     (multiplier (get reward-multiplier plastic-def)))
    (ok (* base-reward multiplier))
  )
)

(define-private (calculate-recycling-score (weight uint) (plastic-type (string-ascii 16)))
  (match (get-plastic-type-definition plastic-type)
    plastic-def (let 
                  ((difficulty (get recycling-difficulty plastic-def))
                   (market-value (get market-value plastic-def))
                   (environmental-impact (get environmental-impact plastic-def)))
                  (+ (* weight u10) (* difficulty u5) (* market-value u3) (* environmental-impact u2))
                )
    u0
  )
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
            verified-collector: false,
            specialization-type: "GENERAL",
            verification-level: u1
          }
        )
        (ok true)
      )
      err-already-exists
    )
  )
)

(define-public (submit-collection (weight uint) (location (string-ascii 64)) (plastic-type (string-ascii 16)))
  (let 
    ((collection-id (+ (var-get collection-id-nonce) u1))
     (current-profile (map-get? user-profiles tx-sender))
     (plastic-def (map-get? plastic-type-definitions plastic-type)))
    (if (and (> weight u0) (>= weight (var-get min-collection-weight)))
      (if (and (is-some current-profile) (is-some plastic-def))
        (let 
          ((profile (unwrap-panic current-profile))
           (plastic-info (unwrap-panic plastic-def))
           (recycling-score (calculate-recycling-score weight plastic-type)))
          (if (>= (get verification-level profile) (get min-verification-level plastic-info))
            (begin
              (var-set collection-id-nonce collection-id)
              (map-set collection-records collection-id
                {
                  collector: tx-sender,
                  weight: weight,
                  location: location,
                  timestamp: stacks-block-height,
                  verified: false,
                  reward-amount: u0,
                  plastic-type: plastic-type,
                  recycling-score: recycling-score
                }
              )
              (unwrap-panic (update-location-stats location weight))
              (unwrap-panic (update-season-participation tx-sender weight))
              (unwrap-panic (update-plastic-type-stats plastic-type weight recycling-score))
              (unwrap-panic (update-collector-specialization tx-sender plastic-type weight))
              (ok collection-id)
            )
            err-insufficient-verification-level
          )
        )
        (if (is-none current-profile) err-not-found err-plastic-type-not-found)
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
                  verified-collector: true,
                  specialization-type: (get specialization-type current-profile),
                  verification-level: (get verification-level current-profile)
                }
              )
              (map-set verified-collectors collector true)
              (unwrap-panic (mint-tokens collector reward-amount))
              (unwrap-panic (update-reputation-for-collection collector weight))
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

(define-read-only (get-leaderboard-entry (season uint) (rank uint))
  (map-get? leaderboard-entries {season: season, rank: rank})
)

(define-read-only (get-reputation-score (collector principal))
  (default-to 
    {total-points: u0, current-season-points: u0, achievements: (list), consecutive-seasons: u0, highest-rank: u999}
    (map-get? reputation-scores collector)
  )
)

(define-read-only (get-season-participant (season uint) (collector principal))
  (map-get? season-participants {season: season, collector: collector})
)

(define-read-only (get-achievement-definition (achievement-id (string-ascii 32)))
  (map-get? achievement-definitions achievement-id)
)

(define-read-only (get-current-season)
  (ok (var-get leaderboard-season))
)

(define-read-only (is-season-active)
  (let 
    ((season-start (var-get current-season-start))
     (duration (var-get season-duration)))
    (if (is-eq season-start u0)
      (ok false)
      (ok (< (- stacks-block-height season-start) duration))
    )
  )
)

(define-read-only (calculate-reputation-points (weight uint) (rank uint))
  (let 
    ((base-points (* weight u2))
     (rank-bonus (if (<= rank u10) (- u110 (* rank u10)) u0)))
    (ok (+ base-points rank-bonus))
  )
)

(define-private (initialize-achievements)
  (begin
    (map-set achievement-definitions "FIRST_COLLECTION"
      {name: "First Steps", description: "Complete your first verified collection", 
       points-value: u50, requirement-type: "collections", requirement-value: u1})
    (map-set achievement-definitions "WEIGHT_MILESTONE_1"
      {name: "Lightweight", description: "Collect 1000kg of plastic waste", 
       points-value: u100, requirement-type: "weight", requirement-value: u1000})
    (map-set achievement-definitions "WEIGHT_MILESTONE_2"
      {name: "Heavyweight", description: "Collect 5000kg of plastic waste", 
       points-value: u250, requirement-type: "weight", requirement-value: u5000})
    (map-set achievement-definitions "STREAK_WARRIOR"
      {name: "Streak Warrior", description: "Participate in 5 consecutive seasons", 
       points-value: u300, requirement-type: "seasons", requirement-value: u5})
    (map-set achievement-definitions "TOP_PERFORMER"
      {name: "Top Performer", description: "Achieve rank 1 in any season", 
       points-value: u500, requirement-type: "rank", requirement-value: u1})
    (ok true)
  )
)

(define-private (award-achievement (collector principal) (achievement-id (string-ascii 32)))
  (let 
    ((current-reputation (get-reputation-score collector))
     (achievement-def (unwrap! (get-achievement-definition achievement-id) err-not-found))
     (current-achievements (get achievements current-reputation)))
    (if (is-none (index-of current-achievements achievement-id))
      (let 
        ((new-achievements (unwrap! (as-max-len? (append current-achievements achievement-id) u10) err-invalid-amount)))
        (map-set reputation-scores collector
          (merge current-reputation 
            {
              total-points: (+ (get total-points current-reputation) (get points-value achievement-def)),
              achievements: new-achievements
            }
          )
        )
        (ok (get points-value achievement-def))
      )
      err-achievement-exists
    )
  )
)

(define-private (check-and-award-achievements (collector principal))
  (let 
    ((profile (unwrap! (get-user-profile collector) err-not-found))
     (reputation (get-reputation-score collector)))
    (begin
      (if (and (>= (get total-collections profile) u1) 
               (is-none (index-of (get achievements reputation) "FIRST_COLLECTION")))
        (unwrap-panic (award-achievement collector "FIRST_COLLECTION"))
        u0)
      (if (and (>= (get total-weight profile) u1000) 
               (is-none (index-of (get achievements reputation) "WEIGHT_MILESTONE_1")))
        (unwrap-panic (award-achievement collector "WEIGHT_MILESTONE_1"))
        u0)
      (if (and (>= (get total-weight profile) u5000) 
               (is-none (index-of (get achievements reputation) "WEIGHT_MILESTONE_2")))
        (unwrap-panic (award-achievement collector "WEIGHT_MILESTONE_2"))
        u0)
      (if (and (>= (get consecutive-seasons reputation) u5) 
               (is-none (index-of (get achievements reputation) "STREAK_WARRIOR")))
        (unwrap-panic (award-achievement collector "STREAK_WARRIOR"))
        u0)
      (if (and (<= (get highest-rank reputation) u1) 
               (is-none (index-of (get achievements reputation) "TOP_PERFORMER")))
        (unwrap-panic (award-achievement collector "TOP_PERFORMER"))
        u0)
      (ok true)
    )
  )
)

(define-private (update-season-participation (collector principal) (weight uint))
  (let 
    ((current-season (var-get leaderboard-season))
     (current-participation (default-to 
                             {weight-collected: u0, collections-count: u0, final-rank: u999, points-earned: u0}
                             (get-season-participant current-season collector))))
    (map-set season-participants {season: current-season, collector: collector}
      {
        weight-collected: (+ (get weight-collected current-participation) weight),
        collections-count: (+ (get collections-count current-participation) u1),
        final-rank: (get final-rank current-participation),
        points-earned: (get points-earned current-participation)
      }
    )
    (ok true)
  )
)

(define-private (update-reputation-for-collection (collector principal) (weight uint))
  (let 
    ((current-reputation (get-reputation-score collector))
     (base-points (* weight u1)))
    (map-set reputation-scores collector
      (merge current-reputation 
        {
          current-season-points: (+ (get current-season-points current-reputation) base-points),
          total-points: (+ (get total-points current-reputation) base-points)
        }
      )
    )
    (unwrap-panic (check-and-award-achievements collector))
    (ok base-points)
  )
)

(define-public (start-new-season)
  (if (is-eq tx-sender contract-owner)
    (begin
      (var-set leaderboard-season (+ (var-get leaderboard-season) u1))
      (var-set current-season-start stacks-block-height)
      (unwrap-panic (initialize-achievements))
      (ok (var-get leaderboard-season))
    )
    err-owner-only
  )
)

(define-public (end-season-and-rank)
  (if (is-eq tx-sender contract-owner)
    (let 
      ((current-season (var-get leaderboard-season)))
      (begin
        (unwrap-panic (finalize-season-rankings current-season))
        (ok current-season)
      )
    )
    err-owner-only
  )
)

(define-private (finalize-season-rankings (season uint))
  (begin
    (ok true)
  )
)

(define-public (submit-leaderboard-entry (season uint) (rank uint) (collector principal) (total-weight uint) (total-collections uint))
  (if (is-eq tx-sender contract-owner)
    (let 
      ((reputation-points (unwrap-panic (calculate-reputation-points total-weight rank)))
       (current-reputation (get-reputation-score collector)))
      (begin
        (map-set leaderboard-entries {season: season, rank: rank}
          {
            collector: collector,
            total-weight: total-weight,
            total-collections: total-collections,
            reputation-points: reputation-points
          }
        )
        (map-set season-participants {season: season, collector: collector}
          (merge (default-to 
                   {weight-collected: u0, collections-count: u0, final-rank: u999, points-earned: u0}
                   (get-season-participant season collector))
                 {final-rank: rank, points-earned: reputation-points}
          )
        )
        (map-set reputation-scores collector
          (merge current-reputation 
            {
              total-points: (+ (get total-points current-reputation) reputation-points),
              highest-rank: (if (< rank (get highest-rank current-reputation)) rank (get highest-rank current-reputation)),
              consecutive-seasons: (if (is-eq season (+ (get-previous-season-participation collector) u1))
                                     (+ (get consecutive-seasons current-reputation) u1)
                                     u1),
              current-season-points: u0
            }
          )
        )
        (unwrap-panic (check-and-award-achievements collector))
        (ok reputation-points)
      )
    )
    err-owner-only
  )
)

(define-private (get-previous-season-participation (collector principal))
  (let 
    ((current-season (var-get leaderboard-season)))
    (if (> current-season u1)
      (if (is-some (get-season-participant (- current-season u1) collector))
        (- current-season u1)
        u0)
      u0
    )
  )
)

(define-public (get-leaderboard-range (season uint) (start-rank uint) (count uint))
  (ok (map get-leaderboard-entry-by-rank 
          (generate-range start-rank count)))
)

(define-private (get-leaderboard-entry-by-rank (rank uint))
  {rank: rank, entry: (get-leaderboard-entry (var-get leaderboard-season) rank)}
)

(define-private (generate-range (start uint) (count uint))
  (if (<= count u10)
    (list start (+ start u1) (+ start u2) (+ start u3) (+ start u4) 
          (+ start u5) (+ start u6) (+ start u7) (+ start u8) (+ start u9))
    (list start)
  )
)

(define-private (update-plastic-type-stats (plastic-type (string-ascii 16)) (weight uint) (recycling-score uint))
  (let 
    ((current-stats (get-plastic-type-stats plastic-type)))
    (map-set plastic-type-stats plastic-type
      {
        total-collections: (+ (get total-collections current-stats) u1),
        total-weight: (+ (get total-weight current-stats) weight),
        total-recycling-score: (+ (get total-recycling-score current-stats) recycling-score),
        average-market-value: (if (> (get total-collections current-stats) u0)
                                (/ (get total-recycling-score current-stats) (get total-collections current-stats))
                                u0)
      }
    )
    (ok true)
  )
)

(define-private (update-collector-specialization (collector principal) (plastic-type (string-ascii 16)) (weight uint))
  (let 
    ((current-spec (get-collector-specialization collector plastic-type))
     (new-collections (+ (get collections-count current-spec) u1))
     (new-weight (+ (get total-weight current-spec) weight))
     (new-expertise (+ (get expertise-level current-spec) u1)))
    (map-set collector-plastic-specialization {collector: collector, plastic-type: plastic-type}
      {
        collections-count: new-collections,
        total-weight: new-weight,
        expertise-level: new-expertise,
        certification-earned: (or (get certification-earned current-spec) (>= new-collections u10))
      }
    )
    (ok true)
  )
)

(define-public (create-plastic-type (plastic-type (string-ascii 16)) (name (string-ascii 64)) (reward-multiplier uint) (min-verification-level uint) (recycling-difficulty uint) (market-value uint) (environmental-impact uint))
  (if (is-eq tx-sender contract-owner)
    (if (is-none (get-plastic-type-definition plastic-type))
      (begin
        (map-set plastic-type-definitions plastic-type
          {
            name: name,
            reward-multiplier: reward-multiplier,
            min-verification-level: min-verification-level,
            recycling-difficulty: recycling-difficulty,
            market-value: market-value,
            environmental-impact: environmental-impact
          }
        )
        (map-set plastic-type-stats plastic-type
          {
            total-collections: u0,
            total-weight: u0,
            total-recycling-score: u0,
            average-market-value: u0
          }
        )
        (ok plastic-type)
      )
      err-plastic-type-exists
    )
    err-owner-only
  )
)

(define-public (update-plastic-type-multiplier (plastic-type (string-ascii 16)) (new-multiplier uint))
  (if (is-eq tx-sender contract-owner)
    (let 
      ((plastic-def (unwrap! (get-plastic-type-definition plastic-type) err-plastic-type-not-found)))
      (map-set plastic-type-definitions plastic-type
        (merge plastic-def {reward-multiplier: new-multiplier})
      )
      (ok new-multiplier)
    )
    err-owner-only
  )
)

(define-public (set-collector-verification-level (collector principal) (new-level uint))
  (if (is-eq tx-sender contract-owner)
    (let 
      ((profile (unwrap! (get-user-profile collector) err-not-found)))
      (map-set user-profiles collector
        (merge profile {verification-level: new-level})
      )
      (ok new-level)
    )
    err-owner-only
  )
)

(define-public (set-collector-specialization (collector principal) (plastic-type (string-ascii 16)))
  (if (is-eq tx-sender contract-owner)
    (let 
      ((profile (unwrap! (get-user-profile collector) err-not-found)))
      (if (is-some (get-plastic-type-definition plastic-type))
        (begin
          (map-set user-profiles collector
            (merge profile {specialization-type: plastic-type})
          )
          (ok plastic-type)
        )
        err-plastic-type-not-found
      )
    )
    err-owner-only
  )
)

(define-public (initialize-default-plastic-types)
  (if (is-eq tx-sender contract-owner)
    (begin
      (unwrap-panic (create-plastic-type "PET" "Polyethylene Terephthalate" u150 u1 u3 u8 u7))
      (unwrap-panic (create-plastic-type "HDPE" "High-Density Polyethylene" u130 u1 u2 u7 u6))
      (unwrap-panic (create-plastic-type "PVC" "Polyvinyl Chloride" u200 u2 u8 u5 u9))
      (unwrap-panic (create-plastic-type "LDPE" "Low-Density Polyethylene" u110 u1 u4 u4 u5))
      (unwrap-panic (create-plastic-type "PP" "Polypropylene" u120 u1 u3 u6 u5))
      (unwrap-panic (create-plastic-type "PS" "Polystyrene" u180 u2 u7 u3 u8))
      (unwrap-panic (create-plastic-type "OTHER" "Other Plastics" u90 u3 u9 u2 u6))
      (ok true)
    )
    err-owner-only
  )
)

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





