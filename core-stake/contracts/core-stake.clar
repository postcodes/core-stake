;; CoreStake - Decentralized Blockchain Mining Simulation Ecosystem
;; A comprehensive platform for transparent, skill-based digital resource extraction

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-MINER-NOT-FOUND (err u102))
(define-constant ERR-PLANET-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-RESOURCES (err u104))
(define-constant ERR-INVALID-CORE-LAYER (err u105))
(define-constant ERR-ALREADY-EXTRACTED (err u106))
(define-constant ERR-EXTRACTION-FAILED (err u107))
(define-constant ERR-VOTING-CLOSED (err u108))
(define-constant ERR-INVALID-TIER (err u109))
(define-constant ERR-FRAUD-DETECTED (err u110))
(define-constant ERR-GUILD-NOT-VERIFIED (err u111))
(define-constant ERR-MINER-ALREADY-EXISTS (err u112))
(define-constant ERR-CORE-LAYER-NOT-FOUND (err u113))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u114))
(define-constant ERR-EQUIPMENT-NOT-FOUND (err u115))
(define-constant ERR-INSUFFICIENT-DURABILITY (err u116))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant PLATFORM-FEE u250) ;; 2.5% platform fee
(define-constant MIN-STAKING-AMOUNT u1000000) ;; 1 STX minimum
(define-constant MAX-CORE-LAYERS u10)
(define-constant GOVERNANCE-THRESHOLD u1000)
(define-constant EQUIPMENT-DEGRADATION-RATE u5)

;; Data Variables
(define-data-var total-resources-extracted uint u0)
(define-data-var total-miners-active uint u0)
(define-data-var platform-treasury uint u0)
(define-data-var emergency-pause bool false)
(define-data-var core-token-supply uint u0)
(define-data-var next-miner-id uint u1)
(define-data-var next-planet-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var next-equipment-id uint u1)

;; Data Maps
(define-map miners 
  { miner-id: uint }
  {
    wallet: principal,
    total-resources: uint,
    completed-extractions: uint,
    skill-score: uint,
    equipment-tier: uint,
    active: bool
  }
)

(define-map mining-planets
  { planet-id: uint }
  {
    discoverer: principal,
    name: (string-ascii 100),
    resource-goal: uint,
    current-extraction: uint,
    core-layer-count: uint,
    guild-verified: bool,
    difficulty-score: uint,
    active: bool,
    sector: (string-ascii 50)
  }
)

(define-map staking-contributions
  { staker: principal, planet-id: uint }
  {
    amount: uint,
    stake-date: uint,
    hashrate-multiplier: uint
  }
)

(define-map core-layers
  { planet-id: uint, layer-id: uint }
  {
    description: (string-ascii 200),
    resource-amount: uint,
    extraction-difficulty: uint, ;; 1=surface, 2=deep, 3=core
    extracted: bool,
    geological-hash: (optional (buff 32)),
    extraction-date: (optional uint)
  }
)

(define-map miner-skill-scores
  { user: principal }
  {
    score: uint,
    core-tokens: uint,
    extraction-count: uint,
    equipment-reports: uint
  }
)

(define-map verified-guilds
  { guild-id: (string-ascii 50) }
  {
    verified: bool,
    reputation-score: uint,
    api-endpoint: (string-ascii 100),
    verification-date: uint
  }
)

(define-map mining-equipment
  { equipment-id: uint }
  {
    owner: principal,
    planet-id: uint,
    durability-amount: uint,
    expected-degradation-rate: uint,
    maintenance-layers: uint,
    current-efficiency: uint
  }
)

(define-map anti-fraud-checks
  { user: principal }
  {
    identity-verified: bool,
    risk-score: uint,
    last-verification: uint,
    multi-sig-required: bool
  }
)

(define-map governance-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    proposal-type: uint, ;; 1=planet, 2=parameter, 3=guild
    target-id: uint,
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    executed: bool
  }
)

(define-map sector-data
  { sector: (string-ascii 50) }
  {
    priority-score: uint,
    resource-multiplier: uint,
    active-planets: uint,
    extraction-rate: uint
  }
)

(define-map miner-equipment-assignments
  { miner-id: uint }
  {
    equipment-id: uint,
    assignment-date: uint,
    total-extractions: uint
  }
)

;; Private Functions
(define-private (calculate-dynamic-extraction (planet-id uint) (base-amount uint))
  (let (
    (planet (unwrap! (map-get? mining-planets { planet-id: planet-id }) u0))
    (sector-info (default-to 
      { priority-score: u100, resource-multiplier: u100, active-planets: u1, extraction-rate: u50 }
      (map-get? sector-data { sector: (get sector planet) })))
  )
    (/ (* base-amount (+ (get difficulty-score planet) (get resource-multiplier sector-info))) u200)
  )
)

(define-private (update-skill-score (user principal) (points uint))
  (let (
    (current-score (default-to 
      { score: u0, core-tokens: u0, extraction-count: u0, equipment-reports: u0 }
      (map-get? miner-skill-scores { user: user })))
    (new-score (+ (get score current-score) points))
    (new-tokens (/ new-score u10))
  )
    (map-set miner-skill-scores 
      { user: user }
      (merge current-score { 
        score: new-score, 
        core-tokens: new-tokens 
      }))
    (var-set core-token-supply (+ (var-get core-token-supply) (- new-tokens (get core-tokens current-score))))
  )
)

(define-private (verify-fraud-check (user principal))
  (let (
    (fraud-data (default-to
      { identity-verified: false, risk-score: u0, last-verification: u0, multi-sig-required: false }
      (map-get? anti-fraud-checks { user: user })))
  )
    (and 
      (get identity-verified fraud-data)
      (< (get risk-score fraud-data) u50)
    )
  )
)

(define-private (validate-geological-verification (extraction-difficulty uint) (geological-hash (optional (buff 32))))
  (if (is-eq extraction-difficulty u1)
    (is-some geological-hash)
    true
  )
)

(define-private (calculate-equipment-efficiency (equipment-id uint) (usage-count uint))
  (let (
    (equipment (unwrap! (map-get? mining-equipment { equipment-id: equipment-id }) u100))
    (degradation (/ (* usage-count EQUIPMENT-DEGRADATION-RATE) u100))
    (current-efficiency (get current-efficiency equipment))
  )
    (if (> degradation current-efficiency)
      u0
      (- current-efficiency degradation)
    )
  )
)

(define-private (degrade-equipment (equipment-id uint))
  (let (
    (equipment (unwrap! (map-get? mining-equipment { equipment-id: equipment-id }) false))
    (new-efficiency (calculate-equipment-efficiency equipment-id u1))
  )
    (map-set mining-equipment
      { equipment-id: equipment-id }
      (merge equipment { current-efficiency: new-efficiency })
    )
    true
  )
)

;; Admin Functions
(define-public (set-emergency-pause (pause bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-pause pause)
    (ok true)
  )
)

(define-public (verify-guild (guild-id (string-ascii 50)) (api-endpoint (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set verified-guilds 
      { guild-id: guild-id }
      {
        verified: true,
        reputation-score: u100,
        api-endpoint: api-endpoint,
        verification-date: block-height
      }
    )
    (ok true)
  )
)

(define-public (update-sector-data (sector (string-ascii 50)) (priority uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= priority u1000) ERR-INVALID-AMOUNT)
    (let (
      (current-data (default-to
        { priority-score: u100, resource-multiplier: u100, active-planets: u0, extraction-rate: u0 }
        (map-get? sector-data { sector: sector })))
    )
      (map-set sector-data 
        { sector: sector }
        (merge current-data {
          priority-score: priority,
          resource-multiplier: multiplier
        })
      )
      (ok true)
    )
  )
)

(define-public (set-fraud-verification (user principal) (verified bool) (risk-score uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= risk-score u100) ERR-INVALID-AMOUNT)
    (map-set anti-fraud-checks
      { user: user }
      {
        identity-verified: verified,
        risk-score: risk-score,
        last-verification: block-height,
        multi-sig-required: (> risk-score u75)
      }
    )
    (ok true)
  )
)

;; Public Functions
(define-public (create-mining-planet 
  (name (string-ascii 100)) 
  (resource-goal uint) 
  (sector (string-ascii 50))
  (core-layer-count uint))
  (let (
    (planet-id (var-get next-planet-id))
  )
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (>= resource-goal MIN-STAKING-AMOUNT) ERR-INVALID-AMOUNT)
    (asserts! (<= core-layer-count MAX-CORE-LAYERS) ERR-INVALID-CORE-LAYER)
    (asserts! (> core-layer-count u0) ERR-INVALID-CORE-LAYER)
    (asserts! (verify-fraud-check tx-sender) ERR-FRAUD-DETECTED)
    
    (map-set mining-planets 
      { planet-id: planet-id }
      {
        discoverer: tx-sender,
        name: name,
        resource-goal: resource-goal,
        current-extraction: u0,
        core-layer-count: core-layer-count,
        guild-verified: false,
        difficulty-score: u0,
        active: true,
        sector: sector
      }
    )
    (var-set next-planet-id (+ planet-id u1))
    (update-skill-score tx-sender u10)
    (ok planet-id)
  )
)

(define-public (stake-on-planet (planet-id uint) (amount uint))
  (begin
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (>= amount MIN-STAKING-AMOUNT) ERR-INVALID-AMOUNT)
    (asserts! (verify-fraud-check tx-sender) ERR-FRAUD-DETECTED)
    
    (let (
      (planet (unwrap! (map-get? mining-planets { planet-id: planet-id }) ERR-PLANET-NOT-FOUND))
      (platform-fee-amount (/ (* amount PLATFORM-FEE) u10000))
      (net-staking (- amount platform-fee-amount))
      (dynamic-amount (calculate-dynamic-extraction planet-id net-staking))
    )
      (asserts! (get active planet) ERR-PLANET-NOT-FOUND)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      (map-set mining-planets 
        { planet-id: planet-id }
        (merge planet { 
          current-extraction: (+ (get current-extraction planet) dynamic-amount)
        })
      )
      
      (map-set staking-contributions
        { staker: tx-sender, planet-id: planet-id }
        {
          amount: amount,
          stake-date: block-height,
          hashrate-multiplier: u100
        }
      )
      
      (var-set platform-treasury (+ (var-get platform-treasury) platform-fee-amount))
      (var-set total-resources-extracted (+ (var-get total-resources-extracted) dynamic-amount))
      (update-skill-score tx-sender u25)
      (ok dynamic-amount)
    )
  )
)

(define-public (register-miner (equipment-tier uint))
  (let (
    (miner-id (var-get next-miner-id))
  )
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (<= equipment-tier u3) ERR-INVALID-TIER)
    (asserts! (>= equipment-tier u1) ERR-INVALID-TIER)
    (asserts! (verify-fraud-check tx-sender) ERR-FRAUD-DETECTED)
    
    (map-set miners 
      { miner-id: miner-id }
      {
        wallet: tx-sender,
        total-resources: u0,
        completed-extractions: u0,
        skill-score: u0,
        equipment-tier: equipment-tier,
        active: true
      }
    )
    (var-set next-miner-id (+ miner-id u1))
    (var-set total-miners-active (+ (var-get total-miners-active) u1))
    (update-skill-score tx-sender u15)
    (ok miner-id)
  )
)

(define-public (create-core-layer 
  (planet-id uint) 
  (layer-id uint)
  (description (string-ascii 200))
  (resource-amount uint)
  (extraction-difficulty uint))
  (begin
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (<= extraction-difficulty u3) ERR-INVALID-TIER)
    (asserts! (>= extraction-difficulty u1) ERR-INVALID-TIER)
    
    (let (
      (planet (unwrap! (map-get? mining-planets { planet-id: planet-id }) ERR-PLANET-NOT-FOUND))
    )
      (asserts! (is-eq tx-sender (get discoverer planet)) ERR-NOT-AUTHORIZED)
      (asserts! (get active planet) ERR-PLANET-NOT-FOUND)
      (asserts! (< layer-id (get core-layer-count planet)) ERR-INVALID-CORE-LAYER)
      
      (map-set core-layers
        { planet-id: planet-id, layer-id: layer-id }
        {
          description: description,
          resource-amount: resource-amount,
          extraction-difficulty: extraction-difficulty,
          extracted: false,
          geological-hash: none,
          extraction-date: none
        }
      )
      (ok true)
    )
  )
)

(define-public (extract-core-layer 
  (planet-id uint) 
  (layer-id uint)
  (geological-hash (optional (buff 32))))
  (begin
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    
    (let (
      (planet (unwrap! (map-get? mining-planets { planet-id: planet-id }) ERR-PLANET-NOT-FOUND))
      (core-layer (unwrap! (map-get? core-layers { planet-id: planet-id, layer-id: layer-id }) ERR-CORE-LAYER-NOT-FOUND))
    )
      (asserts! (is-eq tx-sender (get discoverer planet)) ERR-NOT-AUTHORIZED)
      (asserts! (get active planet) ERR-PLANET-NOT-FOUND)
      (asserts! (not (get extracted core-layer)) ERR-ALREADY-EXTRACTED)
      (asserts! (validate-geological-verification (get extraction-difficulty core-layer) geological-hash) ERR-EXTRACTION-FAILED)
      
      (map-set core-layers
        { planet-id: planet-id, layer-id: layer-id }
        (merge core-layer {
          extracted: true,
          geological-hash: geological-hash,
          extraction-date: (some block-height)
        })
      )
      
      ;; Transfer extracted resources
      (try! (as-contract (stx-transfer? (get resource-amount core-layer) tx-sender (get discoverer planet))))
      (update-skill-score (get discoverer planet) u50)
      (ok true)
    )
  )
)

(define-public (create-mining-equipment 
  (planet-id uint) 
  (durability-amount uint)
  (expected-degradation-rate uint))
  (let (
    (equipment-id (var-get next-equipment-id))
  )
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (verify-fraud-check tx-sender) ERR-FRAUD-DETECTED)
    (asserts! (> durability-amount u0) ERR-INVALID-AMOUNT)
    
    (let (
      (planet (unwrap! (map-get? mining-planets { planet-id: planet-id }) ERR-PLANET-NOT-FOUND))
    )
      (asserts! (get active planet) ERR-PLANET-NOT-FOUND)
      
      (map-set mining-equipment
        { equipment-id: equipment-id }
        {
          owner: tx-sender,
          planet-id: planet-id,
          durability-amount: durability-amount,
          expected-degradation-rate: expected-degradation-rate,
          maintenance-layers: u0,
          current-efficiency: u100
        }
      )
      (var-set next-equipment-id (+ equipment-id u1))
      (update-skill-score tx-sender u20)
      (ok equipment-id)
    )
  )
)

(define-public (assign-equipment-to-miner (miner-id uint) (equipment-id uint))
  (begin
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    
    (let (
      (miner (unwrap! (map-get? miners { miner-id: miner-id }) ERR-MINER-NOT-FOUND))
      (equipment (unwrap! (map-get? mining-equipment { equipment-id: equipment-id }) ERR-EQUIPMENT-NOT-FOUND))
    )
      (asserts! (is-eq tx-sender (get wallet miner)) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq tx-sender (get owner equipment)) ERR-NOT-AUTHORIZED)
      (asserts! (get active miner) ERR-MINER-NOT-FOUND)
      (asserts! (> (get current-efficiency equipment) u0) ERR-INSUFFICIENT-DURABILITY)
      
      (map-set miner-equipment-assignments
        { miner-id: miner-id }
        {
          equipment-id: equipment-id,
          assignment-date: block-height,
          total-extractions: u0
        }
      )
      (ok true)
    )
  )
)

(define-public (perform-extraction-with-equipment (miner-id uint) (planet-id uint) (layer-id uint))
  (begin
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    
    (let (
      (miner (unwrap! (map-get? miners { miner-id: miner-id }) ERR-MINER-NOT-FOUND))
      (assignment (unwrap! (map-get? miner-equipment-assignments { miner-id: miner-id }) ERR-EQUIPMENT-NOT-FOUND))
      (equipment (unwrap! (map-get? mining-equipment { equipment-id: (get equipment-id assignment) }) ERR-EQUIPMENT-NOT-FOUND))
      (core-layer (unwrap! (map-get? core-layers { planet-id: planet-id, layer-id: layer-id }) ERR-CORE-LAYER-NOT-FOUND))
    )
      (asserts! (is-eq tx-sender (get wallet miner)) ERR-NOT-AUTHORIZED)
      (asserts! (get active miner) ERR-MINER-NOT-FOUND)
      (asserts! (not (get extracted core-layer)) ERR-ALREADY-EXTRACTED)
      (asserts! (> (get current-efficiency equipment) u10) ERR-INSUFFICIENT-DURABILITY)
      (asserts! (is-eq planet-id (get planet-id equipment)) ERR-PLANET-NOT-FOUND)
      
      ;; Calculate extraction success based on equipment efficiency and miner skill
      (let (
        (extraction-bonus (/ (* (get current-efficiency equipment) (get equipment-tier miner)) u100))
        (resource-extracted (/ (* (get resource-amount core-layer) extraction-bonus) u100))
      )
        ;; Mark layer as extracted
        (map-set core-layers
          { planet-id: planet-id, layer-id: layer-id }
          (merge core-layer {
            extracted: true,
            extraction-date: (some block-height)
          })
        )
        
        ;; Update miner stats
        (map-set miners
          { miner-id: miner-id }
          (merge miner {
            total-resources: (+ (get total-resources miner) resource-extracted),
            completed-extractions: (+ (get completed-extractions miner) u1)
          })
        )
        
        ;; Degrade equipment
        (degrade-equipment (get equipment-id assignment))
        
        ;; Update assignment stats
        (map-set miner-equipment-assignments
          { miner-id: miner-id }
          (merge assignment {
            total-extractions: (+ (get total-extractions assignment) u1)
          })
        )
        
        ;; Transfer resources to miner
        (try! (as-contract (stx-transfer? resource-extracted tx-sender (get wallet miner))))
        (update-skill-score (get wallet miner) u75)
        (ok resource-extracted)
      )
    )
  )
)

(define-public (create-governance-proposal 
  (proposal-type uint) 
  (target-id uint) 
  (voting-duration uint))
  (let (
    (proposal-id (var-get next-proposal-id))
    (user-score (default-to 
      { score: u0, core-tokens: u0, extraction-count: u0, equipment-reports: u0 }
      (map-get? miner-skill-scores { user: tx-sender })))
  )
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get core-tokens user-score) GOVERNANCE-THRESHOLD) ERR-NOT-AUTHORIZED)
    (asserts! (<= proposal-type u3) ERR-INVALID-TIER)
    (asserts! (>= proposal-type u1) ERR-INVALID-TIER)
    
    (map-set governance-proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        proposal-type: proposal-type,
        target-id: target-id,
        votes-for: u0,
        votes-against: u0,
        voting-deadline: (+ block-height voting-duration),
        executed: false
      }
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (begin
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    
    (let (
      (proposal (unwrap! (map-get? governance-proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
      (user-score (default-to 
        { score: u0, core-tokens: u0, extraction-count: u0, equipment-reports: u0 }
        (map-get? miner-skill-scores { user: tx-sender })))
      (voting-power (get core-tokens user-score))
    )
      (asserts! (< block-height (get voting-deadline proposal)) ERR-VOTING-CLOSED)
      (asserts! (> voting-power u0) ERR-NOT-AUTHORIZED)
      
      (if vote-for
        (map-set governance-proposals
          { proposal-id: proposal-id }
          (merge proposal { votes-for: (+ (get votes-for proposal) voting-power) }))
        (map-set governance-proposals
          { proposal-id: proposal-id }
          (merge proposal { votes-against: (+ (get votes-against proposal) voting-power) }))
      )
      (ok true)
    )
  )
)

(define-public (repair-equipment (equipment-id uint) (repair-amount uint))
  (begin
    (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
    (asserts! (> repair-amount u0) ERR-INVALID-AMOUNT)
    
    (let (
      (equipment (unwrap! (map-get? mining-equipment { equipment-id: equipment-id }) ERR-EQUIPMENT-NOT-FOUND))
      (repair-cost (/ (* repair-amount u1000) u100))
    )
      (asserts! (is-eq tx-sender (get owner equipment)) ERR-NOT-AUTHORIZED)
      (try! (stx-transfer? repair-cost tx-sender (as-contract tx-sender)))
      
      (let (
        (calculated-efficiency (+ (get current-efficiency equipment) repair-amount))
        (new-efficiency (if (> calculated-efficiency u100) u100 calculated-efficiency))
      )
        (map-set mining-equipment
          { equipment-id: equipment-id }
          (merge equipment { 
            current-efficiency: new-efficiency,
            maintenance-layers: (+ (get maintenance-layers equipment) u1)
          })
        )
        (update-skill-score tx-sender u10)
        (ok new-efficiency)
      )
    )
  )
)

;; Read-only Functions
(define-read-only (get-miner (miner-id uint))
  (map-get? miners { miner-id: miner-id })
)

(define-read-only (get-planet (planet-id uint))
  (map-get? mining-planets { planet-id: planet-id })
)

(define-read-only (get-core-layer (planet-id uint) (layer-id uint))
  (map-get? core-layers { planet-id: planet-id, layer-id: layer-id })
)

(define-read-only (get-equipment (equipment-id uint))
  (map-get? mining-equipment { equipment-id: equipment-id })
)

(define-read-only (get-miner-equipment (miner-id uint))
  (map-get? miner-equipment-assignments { miner-id: miner-id })
)

(define-read-only (get-user-skill-score (user principal))
  (map-get? miner-skill-scores { user: user })
)

(define-read-only (get-platform-stats)
  {
    total-resources-extracted: (var-get total-resources-extracted),
    total-miners-active: (var-get total-miners-active),
    platform-treasury: (var-get platform-treasury),
    core-token-supply: (var-get core-token-supply),
    emergency-pause: (var-get emergency-pause)
  }
)

(define-read-only (get-staking-contribution (staker principal) (planet-id uint))
  (map-get? staking-contributions { staker: staker, planet-id: planet-id })
)

(define-read-only (get-sector-data (sector (string-ascii 50)))
  (map-get? sector-data { sector: sector })
)

(define-read-only (get-guild-verification (guild-id (string-ascii 50)))
  (map-get? verified-guilds { guild-id: guild-id })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? governance-proposals { proposal-id: proposal-id })
)

(define-read-only (get-fraud-check (user principal))
  (map-get? anti-fraud-checks { user: user })
)