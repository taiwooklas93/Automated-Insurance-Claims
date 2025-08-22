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
(define-constant ERR-CONTRACT-PAUSED u18)

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
(define-data-var contract-paused bool false)

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
;; Submit oracle data
(define-public (submit-oracle-data
 (oracle-id (string-ascii 36))
 (weather-type uint)
 (location (string-utf8 100))
 (value uint)
 (timestamp uint)
)
 (let
   (
     (oracle (unwrap! (get-oracle oracle-id) (err ERR-ORACLE-NOT-REGISTERED)))
   )
  
   ;; Only the registered oracle principal can submit data
   (asserts! (is-eq tx-sender (get oracle-principal oracle)) (err ERR-NOT-AUTHORIZED))
  
   ;; Ensure oracle is active
   (asserts! (get is-active oracle) (err ERR-ORACLE-NOT-REGISTERED))
  
   ;; Store oracle data
   (map-set oracle-data
     { oracle-id: oracle-id, data-block: block-height }
     {
       weather-type: weather-type,
       location: location,
       value: value,
       timestamp: timestamp
     }
   )
  
   (ok true)
 )
)

;; Create a new insurance policy
(define-public (create-policy
 (risk-profile-id uint)
 (coverage-amount uint)
 (duration-blocks uint)
 (auto-renew bool)
 (location (string-utf8 100))
)
 (let
  (
     (policy-id (var-get next-policy-id))
     (risk-profile (unwrap! (get-risk-profile risk-profile-id) (err ERR-INVALID-RISK-PROFILE)))
     (premium-result (unwrap! (calculate-premium risk-profile-id coverage-amount location) (err ERR-INVALID-PARAMETERS)))
   )

   ;; Check if contract is paused
   (asserts! (not (var-get contract-paused)) (err ERR-CONTRACT-PAUSED))
  
   ;; Validate coverage amount
   (asserts! (and
              (>= coverage-amount (get min-coverage risk-profile))
              (<= coverage-amount (get max-coverage risk-profile))
             )
             (err ERR-INVALID-COVERAGE-AMOUNT))
  
   ;; Collect premium payment
   (try! (stx-transfer? premium-result tx-sender (as-contract tx-sender)))
  
   ;; Update treasury
   (var-set treasury-balance (+ (var-get treasury-balance) premium-result))
   (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-result))
  
   ;; Create policy
   (map-set policies
     { policy-id: policy-id }
     {
       policyholder: tx-sender,
       risk-profile-id: risk-profile-id,
       coverage-amount: coverage-amount,
       premium-amount: premium-result,
       start-block: block-height,
       end-block: (+ block-height duration-blocks),
       policy-status: POLICY-STATUS-ACTIVE,
       renewal-count: u0,
       auto-renew: auto-renew,
       location: location,
       created-at: block-height,
       last-updated: block-height
     }
   )
  
   ;; Update user policy tracking
   (match (map-get? user-policy-count { user: tx-sender })
     existing-count
     (let
       (
         (new-count (+ (get count existing-count) u1))
       )
       (map-set user-policy-count
         { user: tx-sender }
         { count: new-count }
       )
       (map-set user-policies
         { user: tx-sender, index: (- new-count u1) }
         { policy-id: policy-id }
       )
     )
     (begin
       (map-set user-policy-count
         { user: tx-sender }
         { count: u1 }
       )
       (map-set user-policies
         { user: tx-sender, index: u0 }
         { policy-id: policy-id }
       )
     )
   )
  
   ;; Increment policy ID counter
   (var-set next-policy-id (+ policy-id u1))
  
   (ok policy-id)
 )
)


;; Add a condition to policy
(define-public (add-policy-condition
 (policy-id uint)
 (weather-type uint)
 (operator uint)
 (threshold-value uint)
 (payout-percentage uint)
(oracle-id (string-ascii 36))
)
 (let
   (
     (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
     (condition-index u0) ;; For simplicity, we only allow one condition per policy
   )
  
   ;; Check if caller is policy holder
   (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))
  
   ;; Check if policy is active
   (asserts! (is-eq (get policy-status policy) POLICY-STATUS-ACTIVE) (err ERR-POLICY-NOT-ACTIVE))
  
   ;; Check if oracle exists
   (asserts! (is-some (get-oracle oracle-id)) (err ERR-ORACLE-NOT-REGISTERED))
  
   ;; Validate payout percentage (max 100%)
   (asserts! (<= payout-percentage u10000) (err ERR-INVALID-PARAMETERS))
  
   ;; Add condition
   (map-set policy-conditions
     { policy-id: policy-id, condition-index: condition-index }
     {
       weather-type: weather-type,
       operator: operator,
       threshold-value: threshold-value,
       payout-percentage: payout-percentage,
       oracle-id: oracle-id
     }
   )
  
   (ok true)
 )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - Claim Processing
;; =============================================================================

;; Submit a claim for a policy
(define-public (submit-claim
  (policy-id uint)
  (weather-event-type uint)
  (weather-event-value uint)
  (oracle-data-block uint)
)
  (let
    (
      (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
      (claim-id (var-get next-claim-id))
      (condition (unwrap! (get-policy-condition policy-id u0) (err ERR-INVALID-PARAMETERS)))
    )

    ;; Check if contract is paused
    (asserts! (not (var-get contract-paused)) (err ERR-CONTRACT-PAUSED))

    ;; Verify caller is the policyholder
    (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))

    ;; Verify policy is active
    (asserts! (is-policy-active policy-id) (err ERR-POLICY-NOT-ACTIVE))

    ;; Verify no existing claim for this policy
    (asserts! (is-none (map-get? policy-claims { policy-id: policy-id, claim-index: u0 }))
              (err ERR-ALREADY-CLAIMED))

    ;; Verify weather event matches policy condition
    (asserts! (is-eq weather-event-type (get weather-type condition)) (err ERR-CLAIM-CONDITION-NOT-MET))

    ;; Get oracle data for verification
    (let
      (
        (oracle-id (get oracle-id condition))
        (oracle-data-entry (unwrap! (get-oracle-data oracle-id oracle-data-block) (err ERR-NO-ORACLE-DATA)))
      )

      ;; Verify oracle data matches claim
      (asserts! (is-eq (get weather-type oracle-data-entry) weather-event-type) (err ERR-INVALID-ORACLE-DATA))
      (asserts! (is-eq (get value oracle-data-entry) weather-event-value) (err ERR-INVALID-ORACLE-DATA))

      ;; Calculate claim amount based on condition
      (let
        (
          (coverage-amount (get coverage-amount policy))
          (payout-percentage (get payout-percentage condition))
          (claim-amount (/ (* coverage-amount payout-percentage) u10000))
        )

        ;; Create the claim
        (map-set claims
          { claim-id: claim-id }
          {
            policy-id: policy-id,
            claimant: tx-sender,
            claim-status: CLAIM-STATUS-PENDING,
            claim-amount: claim-amount,
            weather-event-type: weather-event-type,
            weather-event-value: weather-event-value,
            condition-index: u0,
            submitted-block: block-height,
            processed-block: none,
            paid-block: none,
            oracle-data-block: oracle-data-block
          }
        )

        ;; Link claim to policy
        (map-set policy-claims
          { policy-id: policy-id, claim-index: u0 }
          { claim-id: claim-id }
        )

        ;; Increment claim ID counter
        (var-set next-claim-id (+ claim-id u1))

        (ok claim-id)
      )
    )
  )
)

;; Process a pending claim (automated evaluation)
(define-public (process-claim (claim-id uint))
  (let
    (
      (claim (unwrap! (get-claim claim-id) (err ERR-CLAIM-NOT-FOUND)))
      (policy-id (get policy-id claim))
      (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
      (condition (unwrap! (get-policy-condition policy-id (get condition-index claim)) (err ERR-INVALID-PARAMETERS)))
    )

    ;; Verify claim is pending
    (asserts! (is-eq (get claim-status claim) CLAIM-STATUS-PENDING) (err ERR-INVALID-PARAMETERS))

    ;; Get oracle data for evaluation
    (let
      (
        (oracle-id (get oracle-id condition))
        (oracle-data-block (get oracle-data-block claim))
        (oracle-data-entry (unwrap! (get-oracle-data oracle-id oracle-data-block) (err ERR-NO-ORACLE-DATA)))
        (threshold (get threshold-value condition))
        (operator (get operator condition))
        (actual-value (get weather-event-value claim))
      )

      ;; Evaluate if claim conditions are met
      (if (evaluate-condition operator actual-value threshold)
        ;; Approve and pay claim
        (begin
          (let ((claim-amount (get claim-amount claim)))
            ;; Verify treasury has sufficient funds
            (asserts! (>= (var-get treasury-balance) claim-amount) (err ERR-INSUFFICIENT-PAYMENT))

            ;; Transfer payment to claimant
            (try! (as-contract (stx-transfer? claim-amount tx-sender (get claimant claim))))

            ;; Update treasury and statistics
            (var-set treasury-balance (- (var-get treasury-balance) claim-amount))
            (var-set total-claims-paid (+ (var-get total-claims-paid) claim-amount))

            ;; Update claim status
            (map-set claims
              { claim-id: claim-id }
              (merge claim {
                claim-status: CLAIM-STATUS-PAID,
                processed-block: (some block-height),
                paid-block: (some block-height)
              })
            )

            ;; Update policy status
            (map-set policies
              { policy-id: policy-id }
              (merge policy {
                policy-status: POLICY-STATUS-CLAIMED,
                last-updated: block-height
              })
            )

            (ok true)
          )
        )
        ;; Reject claim
        (begin
          (map-set claims
            { claim-id: claim-id }
            (merge claim {
              claim-status: CLAIM-STATUS-REJECTED,
              processed-block: (some block-height)
            })
          )
          (ok false)
        )
      )
    )
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - Policy Management
;; =============================================================================

;; Renew an existing policy
(define-public (renew-policy
  (policy-id uint)
  (duration-blocks uint)
)
  (let
    (
      (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
      (risk-profile (unwrap! (get-risk-profile (get risk-profile-id policy)) (err ERR-INVALID-RISK-PROFILE)))
    )

    ;; Verify caller is the policyholder
    (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))

    ;; Verify policy is expired or about to expire (within 100 blocks)
    (asserts! (or
                (is-eq (get policy-status policy) POLICY-STATUS-EXPIRED)
                (<= (get end-block policy) (+ block-height u100))
              )
              (err ERR-POLICY-NOT-EXPIRED))

    ;; Calculate renewal premium
    (let
      (
        (coverage-amount (get coverage-amount policy))
        (location (get location policy))
        (premium-result (unwrap! (calculate-premium (get risk-profile-id policy) coverage-amount location) (err ERR-INVALID-PARAMETERS)))
      )

      ;; Collect premium payment
      (try! (stx-transfer? premium-result tx-sender (as-contract tx-sender)))

      ;; Update treasury
      (var-set treasury-balance (+ (var-get treasury-balance) premium-result))
      (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-result))

      ;; Update policy
      (map-set policies
        { policy-id: policy-id }
        (merge policy {
          start-block: block-height,
          end-block: (+ block-height duration-blocks),
          policy-status: POLICY-STATUS-ACTIVE,
          renewal-count: (+ (get renewal-count policy) u1),
          last-updated: block-height
        })
      )

      (ok true)
    )
  )
)

;; Cancel a policy (with partial refund if applicable)
(define-public (cancel-policy (policy-id uint))
  (let
    (
      (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
    )

    ;; Verify caller is the policyholder
    (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))

    ;; Verify policy is active
    (asserts! (is-eq (get policy-status policy) POLICY-STATUS-ACTIVE) (err ERR-POLICY-NOT-ACTIVE))

    ;; Calculate refund (50% if more than half the policy period remains)
    (let
      (
        (start-block (get start-block policy))
        (end-block (get end-block policy))
        (total-duration (- end-block start-block))
        (remaining-duration (- end-block block-height))
        (premium-amount (get premium-amount policy))
      )

      (if (> remaining-duration (/ total-duration u2))
        ;; Refund 50% of premium
        (let ((refund-amount (/ premium-amount u2)))
          (asserts! (>= (var-get treasury-balance) refund-amount) (err ERR-INSUFFICIENT-PAYMENT))

          ;; Transfer refund
          (try! (as-contract (stx-transfer? refund-amount tx-sender (get policyholder policy))))

          ;; Update treasury
          (var-set treasury-balance (- (var-get treasury-balance) refund-amount))

          ;; Update policy status
          (map-set policies
            { policy-id: policy-id }
            (merge policy {
              policy-status: POLICY-STATUS-CANCELED,
              last-updated: block-height
            })
          )

          (ok refund-amount)
        )
        ;; No refund
        (begin
          (map-set policies
            { policy-id: policy-id }
            (merge policy {
              policy-status: POLICY-STATUS-CANCELED,
              last-updated: block-height
            })
          )
          (ok u0)
        )
      )
    )
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - Advanced Oracle Management
;; =============================================================================

;; Deactivate an oracle (only contract owner)
(define-public (deactivate-oracle (oracle-id (string-ascii 36)))
  (let
    (
      (oracle (unwrap! (get-oracle oracle-id) (err ERR-ORACLE-NOT-REGISTERED)))
    )

    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Update oracle status
    (map-set oracle-registry
      { oracle-id: oracle-id }
      (merge oracle { is-active: false })
    )

    (ok true)
  )
)

;; Reactivate an oracle (only contract owner)
(define-public (reactivate-oracle (oracle-id (string-ascii 36)))
  (let
    (
      (oracle (unwrap! (get-oracle oracle-id) (err ERR-ORACLE-NOT-REGISTERED)))
    )

    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Update oracle status
    (map-set oracle-registry
      { oracle-id: oracle-id }
      (merge oracle { is-active: true })
    )

    (ok true)
  )
)

;; Update oracle information (only contract owner)
(define-public (update-oracle-info
  (oracle-id (string-ascii 36))
  (new-oracle-name (string-utf8 100))
  (new-oracle-type uint)
)
  (let
    (
      (oracle (unwrap! (get-oracle oracle-id) (err ERR-ORACLE-NOT-REGISTERED)))
    )

    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Update oracle information
    (map-set oracle-registry
      { oracle-id: oracle-id }
      (merge oracle {
        oracle-name: new-oracle-name,
        oracle-type: new-oracle-type
      })
    )

    (ok true)
  )
)

;; Transfer oracle ownership (only current oracle principal)
(define-public (transfer-oracle-ownership
  (oracle-id (string-ascii 36))
  (new-principal principal)
)
  (let
    (
      (oracle (unwrap! (get-oracle oracle-id) (err ERR-ORACLE-NOT-REGISTERED)))
    )

    ;; Verify caller is current oracle principal
    (asserts! (is-eq tx-sender (get oracle-principal oracle)) (err ERR-NOT-AUTHORIZED))

    ;; Update oracle principal
    (map-set oracle-registry
      { oracle-id: oracle-id }
      (merge oracle { oracle-principal: new-principal })
    )

    (ok true)
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS - Advanced Queries
;; =============================================================================

;; Get a user's policy by index
(define-read-only (get-user-policy (user principal) (index uint))
  (match (map-get? user-policies { user: user, index: index })
    policy-entry
    (get-policy (get policy-id policy-entry))
    none
  )
)

;; Get all active policies count
(define-read-only (get-active-policies-count)
  (var-get next-policy-id)
)

;; Check if a policy has any claims
(define-read-only (has-policy-claims (policy-id uint))
  (is-some (map-get? policy-claims { policy-id: policy-id, claim-index: u0 }))
)

;; Get policy claim by policy ID
(define-read-only (get-policy-claim (policy-id uint))
  (match (map-get? policy-claims { policy-id: policy-id, claim-index: u0 })
    claim-entry
    (get-claim (get claim-id claim-entry))
    none
  )
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-policies: (- (var-get next-policy-id) u1),
    total-claims: (- (var-get next-claim-id) u1),
    treasury-balance: (var-get treasury-balance),
    total-premiums-collected: (var-get total-premiums-collected),
    total-claims-paid: (var-get total-claims-paid),
    contract-owner: (var-get contract-owner)
  }
)

;; Check if policy is eligible for renewal
(define-read-only (is-policy-renewable (policy-id uint))
  (match (get-policy policy-id)
    policy
    (and
      (is-eq (get policy-status policy) POLICY-STATUS-ACTIVE)
      (<= (get end-block policy) (+ block-height u100))
    )
    false
  )
)

;; Get policy time remaining (in blocks)
(define-read-only (get-policy-time-remaining (policy-id uint))
  (match (get-policy policy-id)
    policy
    (if (> (get end-block policy) block-height)
      (ok (- (get end-block policy) block-height))
      (ok u0)
    )
    (err ERR-POLICY-NOT-FOUND)
  )
)

;; Calculate potential claim amount for a policy
(define-read-only (calculate-potential-claim (policy-id uint))
  (match (get-policy policy-id)
    policy
    (match (get-policy-condition policy-id u0)
      condition
      (let
        (
          (coverage-amount (get coverage-amount policy))
          (payout-percentage (get payout-percentage condition))
        )
        (ok (/ (* coverage-amount payout-percentage) u10000))
      )
      (err ERR-INVALID-PARAMETERS)
    )
    (err ERR-POLICY-NOT-FOUND)
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - Emergency & Administrative
;; =============================================================================

;; Pause the contract (only contract owner)
(define-public (pause-contract)
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Set contract to paused
    (var-set contract-paused true)

    (ok true)
  )
)

;; Unpause the contract (only contract owner)
(define-public (unpause-contract)
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Set contract to unpaused
    (var-set contract-paused false)

    (ok true)
  )
)

;; Emergency withdrawal (only contract owner)
(define-public (emergency-withdraw (amount uint))
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Verify sufficient balance
    (asserts! (<= amount (var-get treasury-balance)) (err ERR-INSUFFICIENT-PAYMENT))

    ;; Transfer funds to contract owner
    (try! (as-contract (stx-transfer? amount tx-sender (var-get contract-owner))))

    ;; Update treasury balance
    (var-set treasury-balance (- (var-get treasury-balance) amount))

    (ok amount)
  )
)

;; Transfer contract ownership (only current owner)
(define-public (transfer-ownership (new-owner principal))
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Transfer ownership
    (var-set contract-owner new-owner)

    (ok true)
  )
)

;; Check if contract is paused
(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

;; Get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)
