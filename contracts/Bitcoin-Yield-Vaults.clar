;; Bitcoin Yield Vaults Contract
;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-VAULT-LOCKED (err u102))
(define-constant ERR-MIN-DEPOSIT (err u103))
(define-constant ERR-POSITION-LOCKED (err u104))
(define-constant ERR-INVALID-TIER (err u105))

;; Configuration constants
(define-constant MIN-DEPOSIT u1000000)

;; Tier constants
(define-constant TIER-FLEXIBLE u0)
(define-constant TIER-SHORT u1)
(define-constant TIER-MEDIUM u2)
(define-constant TIER-LONG u3)

;; Lock period constants (in blocks)
(define-constant LOCK-PERIOD-SHORT u4320)    ;; ~30 days
(define-constant LOCK-PERIOD-MEDIUM u12960)  ;; ~90 days
(define-constant LOCK-PERIOD-LONG u25920)    ;; ~180 days

;; Reward multipliers (basis points)
(define-constant MULTIPLIER-FLEXIBLE u100)   ;; 1x
(define-constant MULTIPLIER-SHORT u150)      ;; 1.5x
(define-constant MULTIPLIER-MEDIUM u200)     ;; 2x
(define-constant MULTIPLIER-LONG u300)       ;; 3x

;; Data variables
(define-data-var total-deposits uint u0)
(define-data-var total-rewards uint u0)
(define-data-var vault-locked bool false)

;; Data maps
(define-map user-positions principal 
  {
    amount: uint,
    tier: uint,
    start-height: uint,
    last-claim: uint,
    lock-end-height: uint
  }
)

(define-map user-rewards principal uint)

;; Private functions
(define-private (get-lock-period (tier uint))
  (if (is-eq tier TIER-FLEXIBLE)
    u0
    (if (is-eq tier TIER-SHORT)
      LOCK-PERIOD-SHORT
      (if (is-eq tier TIER-MEDIUM)
        LOCK-PERIOD-MEDIUM
        LOCK-PERIOD-LONG))))

(define-private (get-tier-multiplier (tier uint))
  (if (is-eq tier TIER-FLEXIBLE)
    MULTIPLIER-FLEXIBLE
    (if (is-eq tier TIER-SHORT)
      MULTIPLIER-SHORT
      (if (is-eq tier TIER-MEDIUM)
        MULTIPLIER-MEDIUM
        MULTIPLIER-LONG))))

(define-private (is-valid-tier (tier uint))
  (or (is-eq tier TIER-FLEXIBLE)
      (is-eq tier TIER-SHORT)
      (is-eq tier TIER-MEDIUM)
      (is-eq tier TIER-LONG)))

(define-private (calculate-tier-rewards (position {amount: uint, tier: uint, start-height: uint, last-claim: uint, lock-end-height: uint}))
  (let
    (
      (blocks-staked (- burn-block-height (get last-claim position)))
      (stake-amount (get amount position))
      (multiplier (get-tier-multiplier (get tier position)))
      (base-reward (* stake-amount (/ blocks-staked u10000)))
    )
    (/ (* base-reward multiplier) u100)))

(define-private (calculate-epoch-yield (user-amount uint) (epoch uint))
  (match (map-get? epoch-data epoch)
    epoch-info (let
      (
        (total-staked (get total-staked epoch-info))
        (yield-distributed (get yield-distributed epoch-info))
      )
      (if (> total-staked u0)
        (/ (* user-amount yield-distributed) total-staked)
        u0))
    u0))

(define-private (calculate-accumulated-yield (user-amount uint) (from-epoch uint) (to-epoch uint))
  (if (>= from-epoch to-epoch)
    u0
    (if (is-eq (+ from-epoch u1) to-epoch)
      (calculate-epoch-yield user-amount from-epoch)
      (+ (calculate-epoch-yield user-amount from-epoch)
         (calculate-epoch-yield user-amount (+ from-epoch u1))))))

(define-private (calculate-referral-bonus (amount uint))
  (/ (* amount (var-get referral-bonus-rate)) u10000))

(define-private (process-referral-bonus (user principal) (amount uint))
  (match (map-get? user-referrer user)
    referrer (let
      (
        (bonus (calculate-referral-bonus amount))
        (current-stats (default-to 
          { total-referrals: u0, total-referral-rewards: u0, pending-rewards: u0 }
          (map-get? referrer-stats referrer)))
      )
      (map-set referrer-stats
        referrer
        {
          total-referrals: (+ (get total-referrals current-stats) u1),
          total-referral-rewards: (+ (get total-referral-rewards current-stats) bonus),
          pending-rewards: (+ (get pending-rewards current-stats) bonus)
        })
      (var-set total-referral-rewards (+ (var-get total-referral-rewards) bonus))
      bonus)
    u0))

;; Public functions
(define-public (deposit-with-tier (amount uint) (tier uint))
  (let
    (
      (sender tx-sender)
      (lock-period (get-lock-period tier))
      (lock-end (+ burn-block-height lock-period))
    )
    (asserts! (>= amount MIN-DEPOSIT) ERR-MIN-DEPOSIT)
    (asserts! (not (var-get vault-locked)) ERR-VAULT-LOCKED)
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (is-valid-tier tier) ERR-INVALID-TIER)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (process-referral-bonus sender amount)
    (map-set user-positions
      sender
      {
        amount: amount,
        tier: tier,
        start-height: burn-block-height,
        last-claim: burn-block-height,
        lock-end-height: lock-end
      })
    (var-set total-deposits (+ (var-get total-deposits) amount))
    (ok true)))

(define-public (withdraw-from-tier (amount uint))
  (let
    (
      (sender tx-sender)
      (position (unwrap! (map-get? user-positions sender) ERR-NOT-AUTHORIZED))
      (current-amount (get amount position))
      (lock-end (get lock-end-height position))
    )
    (asserts! (>= current-amount amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (not (var-get vault-locked)) ERR-VAULT-LOCKED)
    (asserts! (>= burn-block-height lock-end) ERR-POSITION-LOCKED)
    (try! (as-contract (stx-transfer? amount (as-contract tx-sender) sender)))
    (if (is-eq amount current-amount)
      (map-delete user-positions sender)
      (map-set user-positions
        sender
        (merge position { amount: (- current-amount amount) })))
    (var-set total-deposits (- (var-get total-deposits) amount))
    (ok true)))

(define-public (claim-tier-rewards)
  (let
    (
      (sender tx-sender)
      (position (unwrap! (map-get? user-positions sender) ERR-NOT-AUTHORIZED))
      (rewards (calculate-tier-rewards position))
    )
    (map-set user-positions
      sender
      (merge position { last-claim: burn-block-height }))
    (map-set user-rewards
      sender
      (+ (default-to u0 (map-get? user-rewards sender)) rewards))
    (var-set total-rewards (+ (var-get total-rewards) rewards))
    (ok rewards)))

(define-public (compound-rewards)
  (let
    (
      (sender tx-sender)
      (rewards (default-to u0 (map-get? user-rewards sender)))
      (position (unwrap! (map-get? user-positions sender) ERR-NOT-AUTHORIZED))
      (current-amount (get amount position))
    )
    (asserts! (> rewards u0) ERR-INSUFFICIENT-BALANCE)
    (map-set user-positions
      sender
      (merge position { amount: (+ current-amount rewards) }))
    (map-set user-rewards sender u0)
    (var-set total-deposits (+ (var-get total-deposits) rewards))
    (ok rewards)))

;; Read-only functions
(define-read-only (get-user-position (user principal))
  (ok (map-get? user-positions user)))

(define-read-only (get-user-rewards (user principal))
  (ok (default-to u0 (map-get? user-rewards user))))

(define-read-only (get-total-deposits)
  (ok (var-get total-deposits)))

(define-read-only (get-total-rewards)
  (ok (var-get total-rewards)))

(define-read-only (get-lock-remaining (user principal))
  (match (map-get? user-positions user)
    position (let
      (
        (lock-end (get lock-end-height position))
        (current-height burn-block-height)
      )
      (ok (if (> lock-end current-height)
        (- lock-end current-height)
        u0)))
    (ok u0)))

(define-read-only (get-vault-status)
  (ok {
    total-deposits: (var-get total-deposits),
    total-rewards: (var-get total-rewards),
    vault-locked: (var-get vault-locked)
  }))



(define-constant ERR-NO-YIELD-AVAILABLE (err u106))
(define-constant ERR-DISTRIBUTION-ACTIVE (err u107))
(define-constant ERR-CONTRACT-PAUSED (err u108))
(define-constant ERR-INVALID-REFERRER (err u109))
(define-constant ERR-SELF-REFERRAL (err u110))

(define-data-var contract-owner principal tx-sender)
(define-data-var total-yield-pool uint u0)
(define-data-var last-distribution-height uint u0)
(define-data-var distribution-interval uint u1440)
(define-data-var current-epoch uint u0)
(define-data-var contract-paused bool false)
(define-data-var referral-bonus-rate uint u500)
(define-data-var total-referral-rewards uint u0)

(define-map user-stakes principal 
  {
    amount: uint,
    deposit-height: uint,
    last-distribution-epoch: uint,
    pending-yield: uint
  }
)

(define-map epoch-data uint
  {
    total-staked: uint,
    yield-distributed: uint,
    distribution-height: uint
  }
)

(define-map user-referrer principal principal)
(define-map referrer-stats principal 
  {
    total-referrals: uint,
    total-referral-rewards: uint,
    pending-rewards: uint
  }
)

(define-public (deposit-for-yield (amount uint))
  (let
    (
      (sender tx-sender)
      (current-stake (default-to 
        { amount: u0, deposit-height: u0, last-distribution-epoch: u0, pending-yield: u0 }
        (map-get? user-stakes sender)))
      (new-amount (+ (get amount current-stake) amount))
    )
    (asserts! (>= amount MIN-DEPOSIT) ERR-MIN-DEPOSIT)
    (asserts! (not (var-get vault-locked)) ERR-VAULT-LOCKED)
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (process-referral-bonus sender amount)
    (map-set user-stakes
      sender
      {
        amount: new-amount,
        deposit-height: burn-block-height,
        last-distribution-epoch: (var-get current-epoch),
        pending-yield: (get pending-yield current-stake)
      })
    (var-set total-deposits (+ (var-get total-deposits) amount))
    (ok true)))

(define-public (withdraw-stake (amount uint))
  (let
    (
      (sender tx-sender)
      (stake (unwrap! (map-get? user-stakes sender) ERR-NOT-AUTHORIZED))
      (current-amount (get amount stake))
      (user-amount (get amount stake))
      (last-epoch (get last-distribution-epoch stake))
      (current-epoch-num (var-get current-epoch))
      (accumulated-yield (calculate-accumulated-yield user-amount last-epoch current-epoch-num))
      (updated-pending (+ (get pending-yield stake) accumulated-yield))
    )
    (asserts! (>= current-amount amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (not (var-get vault-locked)) ERR-VAULT-LOCKED)
    (try! (as-contract (stx-transfer? amount (as-contract tx-sender) sender)))
    (if (is-eq amount current-amount)
      (map-delete user-stakes sender)
      (map-set user-stakes
        sender
        (merge stake 
          { 
            amount: (- current-amount amount),
            last-distribution-epoch: current-epoch-num,
            pending-yield: updated-pending
          })))
    (var-set total-deposits (- (var-get total-deposits) amount))
    (ok true)))

(define-public (fund-yield-pool (amount uint))
  (let
    (
      (sender tx-sender)
    )
    (asserts! (is-eq sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (var-set total-yield-pool (+ (var-get total-yield-pool) amount))
    (ok true)))

(define-public (distribute-yield)
  (let
    (
      (current-height burn-block-height)
      (last-distribution (var-get last-distribution-height))
      (interval (var-get distribution-interval))
      (yield-pool (var-get total-yield-pool))
      (total-staked (var-get total-deposits))
      (current-epoch-num (var-get current-epoch))
    )
    (asserts! (>= (- current-height last-distribution) interval) ERR-DISTRIBUTION-ACTIVE)
    (asserts! (> yield-pool u0) ERR-NO-YIELD-AVAILABLE)
    (asserts! (> total-staked u0) ERR-INSUFFICIENT-BALANCE)
    (map-set epoch-data
      current-epoch-num
      {
        total-staked: total-staked,
        yield-distributed: yield-pool,
        distribution-height: current-height
      })
    (var-set last-distribution-height current-height)
    (var-set current-epoch (+ current-epoch-num u1))
    (var-set total-yield-pool u0)
    (ok yield-pool)))

(define-public (claim-distributed-yield)
  (let
    (
      (sender tx-sender)
      (stake (unwrap! (map-get? user-stakes sender) ERR-NOT-AUTHORIZED))
      (user-amount (get amount stake))
      (last-epoch (get last-distribution-epoch stake))
      (current-epoch-num (var-get current-epoch))
      (accumulated-yield (calculate-accumulated-yield user-amount last-epoch current-epoch-num))
      (total-pending (+ (get pending-yield stake) accumulated-yield))
    )
    (asserts! (> total-pending u0) ERR-NO-YIELD-AVAILABLE)
    (try! (as-contract (stx-transfer? total-pending (as-contract tx-sender) sender)))
    (map-set user-stakes
      sender
      (merge stake 
        { 
          last-distribution-epoch: current-epoch-num,
          pending-yield: u0
        }))
    (ok total-pending)))

(define-public (update-user-yield (user principal))
  (match (map-get? user-stakes user)
    stake (let
      (
        (user-amount (get amount stake))
        (last-epoch (get last-distribution-epoch stake))
        (current-epoch-num (var-get current-epoch))
        (accumulated-yield (calculate-accumulated-yield user-amount last-epoch current-epoch-num))
      )
      (map-set user-stakes
        user
        (merge stake 
          { 
            last-distribution-epoch: current-epoch-num,
            pending-yield: (+ (get pending-yield stake) accumulated-yield)
          }))
      (ok accumulated-yield))
    (ok u0)))

(define-read-only (get-user-stake (user principal))
  (ok (map-get? user-stakes user)))

(define-read-only (get-yield-pool-balance)
  (ok (var-get total-yield-pool)))

(define-read-only (get-next-distribution-height)
  (ok (+ (var-get last-distribution-height) (var-get distribution-interval))))

(define-read-only (get-epoch-info (epoch uint))
  (ok (map-get? epoch-data epoch)))

(define-public (pause-contract)
  (let
    (
      (sender tx-sender)
    )
    (asserts! (is-eq sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (var-set contract-paused true)
    (ok true)))

(define-public (unpause-contract)
  (let
    (
      (sender tx-sender)
    )
    (asserts! (is-eq sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (var-get contract-paused) ERR-CONTRACT-PAUSED)
    (var-set contract-paused false)
    (ok true)))

(define-read-only (is-contract-paused)
  (ok (var-get contract-paused)))

(define-public (set-referrer (referrer principal))
  (let
    (
      (sender tx-sender)
    )
    (asserts! (not (is-eq sender referrer)) ERR-SELF-REFERRAL)
    (asserts! (is-none (map-get? user-referrer sender)) ERR-INVALID-REFERRER)
    (asserts! (or (is-some (map-get? user-positions referrer))
                  (is-some (map-get? user-stakes referrer))) ERR-INVALID-REFERRER)
    (map-set user-referrer sender referrer)
    (ok true)))

(define-public (claim-referral-rewards)
  (let
    (
      (sender tx-sender)
      (stats (unwrap! (map-get? referrer-stats sender) ERR-NOT-AUTHORIZED))
      (pending (get pending-rewards stats))
    )
    (asserts! (> pending u0) ERR-INSUFFICIENT-BALANCE)
    (try! (as-contract (stx-transfer? pending (as-contract tx-sender) sender)))
    (map-set referrer-stats
      sender
      (merge stats { pending-rewards: u0 }))
    (ok pending)))

(define-read-only (get-referrer-stats (user principal))
  (ok (map-get? referrer-stats user)))

(define-read-only (get-user-referrer (user principal))
  (ok (map-get? user-referrer user)))

(define-read-only (get-referral-bonus-rate)
  (ok (var-get referral-bonus-rate)))

(define-read-only (get-total-referral-rewards)
  (ok (var-get total-referral-rewards)))