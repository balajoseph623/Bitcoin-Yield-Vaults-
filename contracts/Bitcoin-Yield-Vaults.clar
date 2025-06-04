(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-VAULT-LOCKED (err u102))
(define-constant ERR-MIN-DEPOSIT (err u103))
(define-constant MIN-DEPOSIT u1000000)

(define-data-var total-deposits uint u0)
(define-data-var total-rewards uint u0)
(define-data-var last-compound-height uint u0)
(define-data-var vault-locked bool false)

(define-map user-deposits principal uint)
(define-map user-rewards principal uint)
(define-map staking-positions principal 
  {
    amount: uint,
    height: uint,
    last-claim: uint
  }
)

(define-public (deposit (amount uint))
  (let
    (
      (sender tx-sender)
      (current-deposit (default-to u0 (map-get? user-deposits sender)))
    )
    (asserts! (>= amount MIN-DEPOSIT) ERR-MIN-DEPOSIT)
    (asserts! (not (var-get vault-locked)) ERR-VAULT-LOCKED)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (map-set user-deposits
      sender
      (+ current-deposit amount))
    (var-set total-deposits (+ (var-get total-deposits) amount))
    (map-set staking-positions
      sender
      {
        amount: amount,
        height: burn-block-height,
        last-claim: burn-block-height
      })
    (ok true)))
(define-public (withdraw (amount uint))
  (let
    (
      (sender tx-sender)
      (current-deposit (default-to u0 (map-get? user-deposits sender)))
    )
    (asserts! (>= current-deposit amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (not (var-get vault-locked)) ERR-VAULT-LOCKED)
    (try! (as-contract (stx-transfer? amount (as-contract tx-sender) sender)))
    (map-set user-deposits
      sender
      (- current-deposit amount))
    (var-set total-deposits (- (var-get total-deposits) amount))
    (ok true)))
(define-public (claim-rewards)
  (let
    (
      (sender tx-sender)
      (position (unwrap! (map-get? staking-positions sender) ERR-NOT-AUTHORIZED))
      (rewards (calculate-rewards position))
    )
    (map-set staking-positions
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
      (current-deposit (default-to u0 (map-get? user-deposits sender)))
    )
    (map-set user-deposits
      sender
      (+ current-deposit rewards))
    (map-set user-rewards sender u0)
    (var-set last-compound-height burn-block-height)
    (ok rewards)))
(define-read-only (get-user-deposit (user principal))
  (ok (default-to u0 (map-get? user-deposits user))))

(define-read-only (get-user-rewards (user principal))
  (ok (default-to u0 (map-get? user-rewards user))))

(define-read-only (get-total-deposits)
  (ok (var-get total-deposits)))

(define-private (calculate-rewards (position {amount: uint, height: uint, last-claim: uint}))
  (let
    (
      (blocks-staked (- burn-block-height (get last-claim position)))
      (stake-amount (get amount position))
    )
    (* stake-amount (/ blocks-staked u10000))))
