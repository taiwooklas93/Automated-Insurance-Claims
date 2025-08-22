;; =============================================================================
;; AUTOMATED INSURANCE CLAIMS CONTRACT
;; A parametric insurance contract that automatically processes claims 
;; based on predefined weather conditions using oracle data
;; =============================================================================

;; =============================================================================
;; ERROR CONSTANTS
;; =============================================================================
(define-constant ERR-NOT-AUTHORIZED u1)
(define-constant ERR-POLICY-NOT-FOUND u2)
(define-constant ERR-POLICY-EXPIRED u3)
(define-constant ERR-POLICY-NOT-ACTIVE u4)
(define-constant ERR-INSUFFICIENT-PAYMENT u5)
(define-constant ERR-INVALID-RISK-PROFILE u6)
(define-constant ERR-INVALID-COVERAGE-AMOUNT u7)
(define-constant ERR-ALREADY-CLAIMED u8)
(define-constant ERR-CLAIM-NOT-FOUND u9)
(define-constant ERR-INVALID-ORACLE-DATA u10)
(define-constant ERR-CLAIM-CONDITION-NOT-MET u11)
(define-constant ERR-ORACLE-NOT-REGISTERED u12)
(define-constant ERR-NO-ORACLE-DATA u13)
(define-constant ERR-INVALID-PARAMETERS u14)
(define-constant ERR-NOT-CLAIMABLE-YET u15)
(define-constant ERR-PAYMENT-FAILED u16)
(define-constant ERR-POLICY-NOT-EXPIRED u17)

;; =============================================================================
;; STATUS CONSTANTS
;; =============================================================================

;; Policy Status Values
(define-constant POLICY-STATUS-ACTIVE u1)
(define-constant POLICY-STATUS-EXPIRED u2)
(define-constant POLICY-STATUS-CANCELED u3)
(define-constant POLICY-STATUS-CLAIMED u4)

;; Claim Status Values
(define-constant CLAIM-STATUS-PENDING u1)
(define-constant CLAIM-STATUS-APPROVED u2)
(define-constant CLAIM-STATUS-REJECTED u3)
(define-constant CLAIM-STATUS-PAID u4)

;; Weather Event Type Constants
(define-constant WEATHER-RAINFALL u1)
(define-constant WEATHER-TEMPERATURE u2)
(define-constant WEATHER-WIND-SPEED u3)
(define-constant WEATHER-HUMIDITY u4)
(define-constant WEATHER-HURRICANE u5)
(define-constant WEATHER-FLOOD u6)
(define-constant WEATHER-DROUGHT u7)

;; Condition Operator Constants
(define-constant OPERATOR-GREATER-THAN u1)
(define-constant OPERATOR-LESS-THAN u2)
(define-constant OPERATOR-EQUAL-TO u3)
(define-constant OPERATOR-GREATER-THAN-OR-EQUAL u4)
(define-constant OPERATOR-LESS-THAN-OR-EQUAL u5)

;; =============================================================================
;; STATE VARIABLES
;; =============================================================================
(define-data-var contract-owner principal tx-sender)
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var treasury-balance uint u0)
(define-data-var total-premiums-collected uint u0)
(define-data-var total-claims-paid uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

;; Oracle Registry - Stores registered oracle information
(define-map oracle-registry
  { oracle-id: (string-ascii 36) }
  {
    oracle-principal: principal,
    oracle-name: (string-utf8 100),
    oracle-type: uint,
    is-active: bool,
    registration-block: uint
  }
)

;; Oracle Data - Stores weather data from oracles
(define-map oracle-data
  { oracle-id: (string-ascii 36), data-block: uint }
  {
    weather-type: uint,
    location: (string-utf8 100),
    value: uint,
    timestamp: uint
  }
)

;; Risk Profiles - Defines different insurance risk categories
(define-map risk-profiles
  { profile-id: uint }
  {
    profile-name: (string-utf8 100),
    base-premium-rate: uint,      ;; basis points (1/100 of 1%)
    coverage-multiplier: uint,    ;; multiplier for coverage calculation
    risk-factor: uint,           ;; additional risk factor (basis points)
    min-coverage: uint,
    max-coverage: uint,
    description: (string-utf8 500)
  }
)

;; Insurance Policies - Core policy information
(define-map policies
  { policy-id: uint }
  {
    policyholder: principal,
    risk-profile-id: uint,
    coverage-amount: uint,
    premium-amount: uint,
    start-block: uint,
    end-block: uint,
    policy-status: uint,
    renewal-count: uint,
    auto-renew: bool,
    location: (string-utf8 100),
    created-at: uint,
    last-updated: uint
  }
)

;; Policy Conditions - Defines claim triggers for each policy
(define-map policy-conditions
  { policy-id: uint, condition-index: uint }
  {
    weather-type: uint,
    operator: uint,
    threshold-value: uint,
    payout-percentage: uint,      ;; percentage of coverage to pay out (basis points)
    oracle-id: (string-ascii 36)
  }
)

;; Claims - Stores claim information
(define-map claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    claim-status: uint,
    claim-amount: uint,
    weather-event-type: uint,
    weather-event-value: uint,
    condition-index: uint,
    submitted-block: uint,
    processed-block: (optional uint),
    paid-block: (optional uint),
    oracle-data-block: uint
  }
)

;; Policy Claims Index - Links claims to policies
(define-map policy-claims
  { policy-id: uint, claim-index: uint }
  { claim-id: uint }
)

;; User Policy Management - Tracks user's policies
(define-map user-policy-count
  { user: principal }
  { count: uint }
)

(define-map user-policies
  { user: principal, index: uint }
  { policy-id: uint }
)

;; =============================================================================
;; READ-ONLY FUNCTIONS - Data Retrieval
;; =============================================================================

;; Get complete policy information by ID
(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

;; Get complete claim information by ID
(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

;; Get risk profile information by ID
(define-read-only (get-risk-profile (profile-id uint))
  (map-get? risk-profiles { profile-id: profile-id })
)

;; Get oracle registration information
(define-read-only (get-oracle (oracle-id (string-ascii 36)))
  (map-get? oracle-registry { oracle-id: oracle-id })
)

;; Get oracle data for a specific block height
(define-read-only (get-oracle-data (oracle-id (string-ascii 36)) (data-block uint))
  (map-get? oracle-data { oracle-id: oracle-id, data-block: data-block })
)

;; Get the most recent oracle data
(define-read-only (get-latest-oracle-data (oracle-id (string-ascii 36)))
  (get-oracle-data oracle-id block-height)
)

;; Get policy condition by policy ID and condition index
(define-read-only (get-policy-condition (policy-id uint) (condition-index uint))
  (map-get? policy-conditions { policy-id: policy-id, condition-index: condition-index })
)

;; =============================================================================
;; READ-ONLY FUNCTIONS - Business Logic
;; =============================================================================

;; Calculate premium for given parameters
(define-read-only (calculate-premium (profile-id uint) (coverage-amount uint) (location (string-utf8 100)))
  (match (get-risk-profile profile-id)
    profile
    (let
      (
        (base-rate (get base-premium-rate profile))
        (risk-factor (get risk-factor profile))
        (min-coverage (get min-coverage profile))
        (max-coverage (get max-coverage profile))
      )
      ;; Validate coverage amount is within bounds
      (asserts! (and (>= coverage-amount min-coverage) 
                     (<= coverage-amount max-coverage)) 
                (err ERR-INVALID-COVERAGE-AMOUNT))
      
      ;; Calculate premium: (coverage * (base-rate + risk-factor)) / 10000
      (let ((premium (/ (* coverage-amount (+ base-rate risk-factor)) u10000)))
        (ok premium)
      )
    )
    (err ERR-INVALID-RISK-PROFILE)
  )
)

;; Check if a policy is currently active
(define-read-only (is-policy-active (policy-id uint))
  (match (get-policy policy-id)
    policy
    (and
      (is-eq (get policy-status policy) POLICY-STATUS-ACTIVE)
      (>= block-height (get start-block policy))
      (<= block-height (get end-block policy))
    )
    false
  )
)

;; Check if a policy is eligible for claims based on current conditions
(define-read-only (is-policy-claimable (policy-id uint))
  (match (get-policy policy-id)
    policy
    (if (is-policy-active policy-id)
      (check-policy-conditions-met policy-id)
      false
    )
    false
  )
)

;; Check if any policy conditions are met (simplified to check condition 0)
(define-read-only (check-policy-conditions-met (policy-id uint))
  (match (get-policy-condition policy-id u0)
    condition
    (let
      (
        (oracle-id (get oracle-id condition))
        (weather-type (get weather-type condition))
        (operator (get operator condition))
        (threshold (get threshold-value condition))
      )
      (match (get-latest-oracle-data oracle-id)
        oracle-data-entry
        (if (is-eq (get weather-type oracle-data-entry) weather-type)
          (let ((current-value (get value oracle-data-entry)))
            (evaluate-condition operator current-value threshold)
          )
          false
        )
        false
      )
    )
    false
  )
)

;; Evaluate condition based on operator type
(define-read-only (evaluate-condition (operator uint) (current-value uint) (threshold uint))
  (if (is-eq operator OPERATOR-GREATER-THAN)
    (> current-value threshold)
    (if (is-eq operator OPERATOR-LESS-THAN)
      (< current-value threshold)
      (if (is-eq operator OPERATOR-EQUAL-TO)
        (is-eq current-value threshold)
        (if (is-eq operator OPERATOR-GREATER-THAN-OR-EQUAL)
          (>= current-value threshold)
          (if (is-eq operator OPERATOR-LESS-THAN-OR-EQUAL)
            (<= current-value threshold)
            false ;; Invalid operator
          )
        )
      )
    )
  )
)

;; Get claims associated with a policy (simplified to check index 0)
(define-read-only (get-policy-claims (policy-id uint))
  (map-get? policy-claims { policy-id: policy-id, claim-index: u0 })
)

;; Get treasury and contract statistics
(define-read-only (get-treasury-stats)
  {
    balance: (var-get treasury-balance),
    total-premiums: (var-get total-premiums-collected),
    total-claims-paid: (var-get total-claims-paid),
    next-policy-id: (var-get next-policy-id),
    next-claim-id: (var-get next-claim-id)
  }
)

;; Get user's policy count
(define-read-only (get-user-policy-count (user principal))
  (default-to u0 (get count (map-get? user-policy-count { user: user })))
)

;; =============================================================================
;; PUBLIC FUNCTIONS - Oracle Management
;; =============================================================================

;; Register a new oracle (only contract owner)
(define-public (register-oracle
  (oracle-id (string-ascii 36))
  (oracle-name (string-utf8 100))
  (oracle-type uint)
)
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))
    
    ;; Validate oracle doesn't already exist
    (asserts! (is-none (map-get? oracle-registry { oracle-id: oracle-id })) 
              (err ERR-INVALID-PARAMETERS))
    
    ;; Register the oracle
    (map-set oracle-registry
      { oracle-id: oracle-id }
      {
        oracle-principal: tx-sender,
        oracle-name: oracle-name,
        oracle-type: oracle-type,
        is-active: true,
        registration-block: block-height
      }
    )
    
    (ok oracle-id)
  )
)

;; Submit oracle data (only registered oracles)
(define-public (submit-oracle-data
  (oracle-id (string-ascii 36))
  (weather-type uint)
  (location (string-utf8 100))
  (value uint)
)
  (let
    (
      (oracle-info (unwrap! (get-oracle oracle-id) (err ERR-ORACLE-NOT-REGISTERED)))
    )
    ;; Verify oracle is active and caller is authorized
    (asserts! (get is-active oracle-info) (err ERR-ORACLE-NOT-REGISTERED))
    (asserts! (is-eq tx-sender (get oracle-principal oracle-info)) (err ERR-NOT-AUTHORIZED))
    
    ;; Store oracle data
    (map-set oracle-data
      { oracle-id: oracle-id, data-block: block-height }
      {
        weather-type: weather-type,
        location: location,
        value: value,
        timestamp: block-height
      }
    )
    
    (ok true)
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - Risk Profile Management
;; =============================================================================

;; Create a new risk profile (only contract owner)
(define-public (create-risk-profile
  (profile-id uint)
  (profile-name (string-utf8 100))
  (base-premium-rate uint)
  (coverage-multiplier uint)
  (risk-factor uint)
  (min-coverage uint)
  (max-coverage uint)
  (description (string-utf8 500))
)
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))
    
    ;; Validate parameters
    (asserts! (> max-coverage min-coverage) (err ERR-INVALID-PARAMETERS))
    (asserts! (> coverage-multiplier u0) (err ERR-INVALID-PARAMETERS))
    
    ;; Create risk profile
    (map-set risk-profiles
      { profile-id: profile-id }
      {
        profile-name: profile-name,
        base-premium-rate: base-premium-rate,
        coverage-multiplier: coverage-multiplier,
        risk-factor: risk-factor,
        min-coverage: min-coverage,
        max-coverage: max-coverage,
        description: description
      }
    )
    
    (ok profile-id)
  )
)

;; =============================================================================
;; HELPER FUNCTIONS - Policy Management
;; =============================================================================

;; Helper function to update user policy tracking
(define-private (update-user-policy-tracking (user principal) (policy-id uint))
  (let
    (
      (current-count (get-user-policy-count user))
      (new-count (+ current-count u1))
    )
    ;; Update user policy count
    (map-set user-policy-count { user: user } { count: new-count })
    
    ;; Add policy to user's policy list
    (map-set user-policies 
      { user: user, index: current-count } 
      { policy-id: policy-id })
    
    new-count
  )
)