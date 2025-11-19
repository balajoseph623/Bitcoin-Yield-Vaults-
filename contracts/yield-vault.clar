;; Bitcoin Yield Vaults - Smart Contract
;; A vault system for generating yield on Bitcoin holdings through Stacks
;; Includes automated compounding feature for maximizing returns

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u1000))
(define-constant ERR_VAULT_NOT_FOUND (err u1001))
(define-constant ERR_INSUFFICIENT_BALANCE (err u1002))
(define-constant ERR_INVALID_AMOUNT (err u1003))
(define-constant ERR_VAULT_LOCKED (err u1004))
(define-constant ERR_MINIMUM_STAKE_NOT_MET (err u1005))
(define-constant ERR_ALREADY_EXISTS (err u1006))
(define-constant ERR_COMPOUND_TOO_EARLY (err u1007))

;; Contract constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant MIN_STAKE_AMOUNT u1000000) ;; 1 STX minimum
(define-constant YIELD_RATE u500) ;; 5% annual yield (500 basis points)
(define-constant COMPOUND_COOLDOWN u144) ;; ~24 hours in blocks
(define-constant BLOCKS_PER_YEAR u52560) ;; Approximate blocks per year

;; Data structures
(define-map vaults
    { owner: principal }
    {
        balance: uint,
        last-yield-claim: uint,
        yield-accumulated: uint,
        is-locked: bool,
        auto-compound: bool,
        last-compound: uint,
        compound-count: uint
    }
)

(define-map vault-tiers
    { owner: principal }
    {
        tier-level: uint,
        total-compounds: uint,
        avg-balance: uint,
        tier-updated-block: uint
    }
)

(define-map vault-stats
    { vault-id: uint }
    {
        total-deposited: uint,
        total-yield-paid: uint,
        creation-block: uint,
        last-activity: uint
    }
)

;; Global state
(define-data-var total-vaults uint u0)
(define-data-var total-locked-value uint u0)
(define-data-var protocol-fee-rate uint u100) ;; 1% protocol fee
(define-data-var tier1-threshold uint u5000000) ;; 5 STX
(define-data-var tier2-threshold uint u25000000) ;; 25 STX
(define-data-var tier3-threshold uint u100000000) ;; 100 STX

;; Events
(define-data-var vault-created-event (string-ascii 50) "vault-created")
(define-data-var deposit-event (string-ascii 50) "deposit-made")
(define-data-var yield-claimed-event (string-ascii 50) "yield-claimed")
(define-data-var compound-event (string-ascii 50) "yield-compounded")

;; Public functions

;; Create a new vault
(define-public (create-vault (initial-deposit uint) (enable-auto-compound bool))
    (let (
        (existing-vault (map-get? vaults { owner: tx-sender }))
    )
    (asserts! (is-none existing-vault) ERR_ALREADY_EXISTS)
    (asserts! (>= initial-deposit MIN_STAKE_AMOUNT) ERR_MINIMUM_STAKE_NOT_MET)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? initial-deposit tx-sender (as-contract tx-sender)))
    
    ;; Create vault entry
    (map-set vaults
        { owner: tx-sender }
        {
            balance: initial-deposit,
            last-yield-claim: block-height,
            yield-accumulated: u0,
            is-locked: false,
            auto-compound: enable-auto-compound,
            last-compound: block-height,
            compound-count: u0
        }
    )
    
    ;; Initialize vault tier
    (map-set vault-tiers
        { owner: tx-sender }
        {
            tier-level: u0,
            total-compounds: u0,
            avg-balance: initial-deposit,
            tier-updated-block: block-height
        }
    )
    
    ;; Update global stats
    (var-set total-vaults (+ (var-get total-vaults) u1))
    (var-set total-locked-value (+ (var-get total-locked-value) initial-deposit))
    
    (ok true)
    )
)

;; Deposit additional funds to existing vault
(define-public (deposit (amount uint))
    (let (
        (vault-data (unwrap! (map-get? vaults { owner: tx-sender }) ERR_VAULT_NOT_FOUND))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get is-locked vault-data)) ERR_VAULT_LOCKED)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update vault balance
    (map-set vaults
        { owner: tx-sender }
        (merge vault-data {
            balance: (+ (get balance vault-data) amount)
        })
    )
    
    ;; Update global locked value
    (var-set total-locked-value (+ (var-get total-locked-value) amount))
    
    (ok amount)
    )
)

;; Calculate yield for a vault
(define-private (calculate-yield (vault-data {balance: uint, last-yield-claim: uint, yield-accumulated: uint, is-locked: bool, auto-compound: bool, last-compound: uint, compound-count: uint}))
    (let (
        (blocks-since-claim (- block-height (get last-yield-claim vault-data)))
        (annual-yield (/ (* (get balance vault-data) YIELD_RATE) u10000))
        (yield-per-block (/ annual-yield BLOCKS_PER_YEAR))
        (new-yield (* yield-per-block blocks-since-claim))
    )
    (+ (get yield-accumulated vault-data) new-yield)
    )
)

;; Claim accumulated yield
(define-public (claim-yield)
    (let (
        (vault-data (unwrap! (map-get? vaults { owner: tx-sender }) ERR_VAULT_NOT_FOUND))
        (total-yield (calculate-yield vault-data))
        (protocol-fee (/ (* total-yield (var-get protocol-fee-rate)) u10000))
        (user-yield (- total-yield protocol-fee))
    )
    (asserts! (> total-yield u0) ERR_INSUFFICIENT_BALANCE)
    
    ;; Transfer yield to user (minus protocol fee)
    (try! (as-contract (stx-transfer? user-yield tx-sender tx-sender)))
    
    ;; Update vault data
    (map-set vaults
        { owner: tx-sender }
        (merge vault-data {
            last-yield-claim: block-height,
            yield-accumulated: u0
        })
    )
    
    (ok user-yield)
    )
)

;; NEW FEATURE: Compound yield automatically
(define-public (compound-yield)
    (let (
        (vault-data (unwrap! (map-get? vaults { owner: tx-sender }) ERR_VAULT_NOT_FOUND))
        (blocks-since-compound (- block-height (get last-compound vault-data)))
        (total-yield (calculate-yield vault-data))
        (protocol-fee (/ (* total-yield (var-get protocol-fee-rate)) u10000))
        (compound-amount (- total-yield protocol-fee))
    )
    ;; Check cooldown period
    (asserts! (>= blocks-since-compound COMPOUND_COOLDOWN) ERR_COMPOUND_TOO_EARLY)
    (asserts! (> compound-amount u0) ERR_INSUFFICIENT_BALANCE)
    
    ;; Add yield to principal balance (compounding)
    (let (
        (new-balance (+ (get balance vault-data) compound-amount))
        (tier-data (unwrap! (map-get? vault-tiers { owner: tx-sender }) ERR_VAULT_NOT_FOUND))
        (new-compound-count (+ (get total-compounds tier-data) u1))
    )
    (map-set vaults
        { owner: tx-sender }
        (merge vault-data {
            balance: new-balance,
            last-yield-claim: block-height,
            yield-accumulated: u0,
            last-compound: block-height,
            compound-count: (+ (get compound-count vault-data) u1)
        })
    )
    
    (map-set vault-tiers
        { owner: tx-sender }
        (merge tier-data {
            total-compounds: new-compound-count,
            avg-balance: (/ (+ (get avg-balance tier-data) new-balance) u2),
            tier-updated-block: block-height
        })
    )
    
    ;; Update global locked value
    (var-set total-locked-value (+ (var-get total-locked-value) compound-amount))
    )
    
    (ok compound-amount)
    )
)

;; Auto-compound for eligible vaults (can be called by anyone)
(define-public (trigger-auto-compound (vault-owner principal))
    (let (
        (vault-data (unwrap! (map-get? vaults { owner: vault-owner }) ERR_VAULT_NOT_FOUND))
        (blocks-since-compound (- block-height (get last-compound vault-data)))
        (total-yield (calculate-yield vault-data))
        (protocol-fee (/ (* total-yield (var-get protocol-fee-rate)) u10000))
        (compound-amount (- total-yield protocol-fee))
        (tier-data (unwrap! (map-get? vault-tiers { owner: vault-owner }) ERR_VAULT_NOT_FOUND))
    )
    ;; Check if auto-compound is enabled and cooldown met
    (asserts! (get auto-compound vault-data) ERR_NOT_AUTHORIZED)
    (asserts! (>= blocks-since-compound COMPOUND_COOLDOWN) ERR_COMPOUND_TOO_EARLY)
    (asserts! (> compound-amount u0) ERR_INSUFFICIENT_BALANCE)
    
    ;; Compound the yield
    (let (
        (new-balance (+ (get balance vault-data) compound-amount))
        (new-compound-count (+ (get total-compounds tier-data) u1))
    )
    (map-set vaults
        { owner: vault-owner }
        (merge vault-data {
            balance: new-balance,
            last-yield-claim: block-height,
            yield-accumulated: u0,
            last-compound: block-height,
            compound-count: (+ (get compound-count vault-data) u1)
        })
    )
    
    (map-set vault-tiers
        { owner: vault-owner }
        (merge tier-data {
            total-compounds: new-compound-count,
            avg-balance: (/ (+ (get avg-balance tier-data) new-balance) u2),
            tier-updated-block: block-height
        })
    )
    
    ;; Update global locked value
    (var-set total-locked-value (+ (var-get total-locked-value) compound-amount))
    
    (ok compound-amount)
    )
    )
)

;; Toggle auto-compound setting
(define-public (set-auto-compound (enabled bool))
    (let (
        (vault-data (unwrap! (map-get? vaults { owner: tx-sender }) ERR_VAULT_NOT_FOUND))
    )
    (map-set vaults
        { owner: tx-sender }
        (merge vault-data { auto-compound: enabled })
    )
    (ok enabled)
    )
)

;; Withdraw from vault
(define-public (withdraw (amount uint))
    (let (
        (vault-data (unwrap! (map-get? vaults { owner: tx-sender }) ERR_VAULT_NOT_FOUND))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (get balance vault-data)) ERR_INSUFFICIENT_BALANCE)
    (asserts! (not (get is-locked vault-data)) ERR_VAULT_LOCKED)
    
    ;; Update vault balance
    (map-set vaults
        { owner: tx-sender }
        (merge vault-data {
            balance: (- (get balance vault-data) amount)
        })
    )
    
    ;; Transfer STX back to user
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update global locked value
    (var-set total-locked-value (- (var-get total-locked-value) amount))
    
    (ok amount)
    )
)

;; Read-only functions

;; Get vault information
(define-read-only (get-vault-info (owner principal))
    (map-get? vaults { owner: owner })
)

;; Get vault balance with current yield
(define-read-only (get-vault-balance-with-yield (owner principal))
    (match (map-get? vaults { owner: owner })
        vault-data 
        (let (
            (current-yield (calculate-yield vault-data))
        )
        (some {
            balance: (get balance vault-data),
            current-yield: current-yield,
            total-value: (+ (get balance vault-data) current-yield),
            compound-count: (get compound-count vault-data),
            auto-compound-enabled: (get auto-compound vault-data)
        }))
        none
    )
)

;; Get protocol statistics
(define-read-only (get-protocol-stats)
    {
        total-vaults: (var-get total-vaults),
        total-locked-value: (var-get total-locked-value),
        protocol-fee-rate: (var-get protocol-fee-rate)
    }
)

;; Calculate current tier level based on balance
(define-private (calculate-tier-level (balance uint))
    (if (>= balance (var-get tier3-threshold))
        u3
        (if (>= balance (var-get tier2-threshold))
            u2
            (if (>= balance (var-get tier1-threshold))
                u1
                u0
            )
        )
    )
)

;; Update vault tier
(define-public (update-vault-tier)
    (let (
        (vault-data (unwrap! (map-get? vaults { owner: tx-sender }) ERR_VAULT_NOT_FOUND))
        (tier-data (unwrap! (map-get? vault-tiers { owner: tx-sender }) ERR_VAULT_NOT_FOUND))
        (current-tier (calculate-tier-level (get balance vault-data)))
    )
    (map-set vault-tiers
        { owner: tx-sender }
        (merge tier-data {
            tier-level: current-tier,
            avg-balance: (/ (+ (get avg-balance tier-data) (get balance vault-data)) u2),
            tier-updated-block: block-height
        })
    )
    (ok current-tier)
    )
)

;; Check if compound is available
(define-read-only (can-compound (owner principal))
    (match (map-get? vaults { owner: owner })
        vault-data
        (let (
            (blocks-since-compound (- block-height (get last-compound vault-data)))
            (total-yield (calculate-yield vault-data))
        )
        {
            can-compound: (and (>= blocks-since-compound COMPOUND_COOLDOWN) (> total-yield u0)),
            blocks-until-next: (if (>= blocks-since-compound COMPOUND_COOLDOWN) u0 (- COMPOUND_COOLDOWN blocks-since-compound)),
            pending-yield: total-yield
        })
        { can-compound: false, blocks-until-next: u0, pending-yield: u0 }
    )
)

;; Get vault tier information
(define-read-only (get-vault-tier (owner principal))
    (map-get? vault-tiers { owner: owner })
)

;; Admin functions (only contract owner)

;; Update protocol fee rate
(define-public (set-protocol-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (<= new-rate u1000) ERR_INVALID_AMOUNT) ;; Max 10% fee
        (var-set protocol-fee-rate new-rate)
        (ok new-rate)
    )
)

;; Emergency lock/unlock vault
(define-public (emergency-lock-vault (owner principal) (lock bool))
    (let (
        (vault-data (unwrap! (map-get? vaults { owner: owner }) ERR_VAULT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set vaults
        { owner: owner }
        (merge vault-data { is-locked: lock })
    )
    (ok lock)
    )
)
