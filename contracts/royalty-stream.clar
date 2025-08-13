(define-data-var contract-owner principal tx-sender)

(define-map artists 
  { artist-id: uint }
  {
    artist-principal: principal,
    artist-name: (string-ascii 64),
    total-streams: uint,
    total-earnings: uint,
    active: bool
  }
)

(define-map songs
  { song-id: uint }
  {
    title: (string-ascii 64),
    artist-id: uint,
    price-per-stream: uint,
    total-streams: uint,
    active: bool
  }
)

(define-map listeners
  { listener-principal: principal }
  {
    total-streams: uint,
    total-spent: uint
  }
)

(define-map stream-history
  { stream-id: uint }
  {
    listener: principal,
    song-id: uint,
    artist-id: uint,
    timestamp: uint,
    amount-paid: uint
  }
)

(define-data-var next-artist-id uint u1)
(define-data-var next-song-id uint u1)
(define-data-var next-stream-id uint u1)
(define-data-var platform-fee-percent uint u5)
(define-data-var min-stream-price uint u1000)

(define-read-only (get-artist (artist-id uint))
  (map-get? artists { artist-id: artist-id })
)

(define-read-only (get-song (song-id uint))
  (map-get? songs { song-id: song-id })
)

(define-read-only (get-listener (listener-principal principal))
  (map-get? listeners { listener-principal: listener-principal })
)

(define-read-only (get-stream (stream-id uint))
  (map-get? stream-history { stream-id: stream-id })
)

(define-read-only (get-owner)
  (var-get contract-owner)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percent)
)

(define-read-only (get-min-stream-price)
  (var-get min-stream-price)
)

(define-public (register-artist (artist-name (string-ascii 64)))
  (let
    (
      (artist-id (var-get next-artist-id))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (map-set artists
      { artist-id: artist-id }
      {
        artist-principal: tx-sender,
        artist-name: artist-name,
        total-streams: u0,
        total-earnings: u0,
        active: true
      }
    )
    (var-set next-artist-id (+ artist-id u1))
    (ok artist-id)
  )
)

(define-public (register-artist-with-principal (artist-name (string-ascii 64)) (artist-principal principal))
  (let
    (
      (artist-id (var-get next-artist-id))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (map-set artists
      { artist-id: artist-id }
      {
        artist-principal: artist-principal,
        artist-name: artist-name,
        total-streams: u0,
        total-earnings: u0,
        active: true
      }
    )
    (var-set next-artist-id (+ artist-id u1))
    (ok artist-id)
  )
)

(define-public (add-song (title (string-ascii 64)) (artist-id uint) (price-per-stream uint))
  (let
    (
      (song-id (var-get next-song-id))
      (artist-info (unwrap! (get-artist artist-id) (err u404)))
    )
    (asserts! (is-eq tx-sender (get-owner)) (err u401))
    (asserts! (>= price-per-stream (var-get min-stream-price)) (err u403))
    (map-set songs
      { song-id: song-id }
      {
        title: title,
        artist-id: artist-id,
        price-per-stream: price-per-stream,
        total-streams: u0,
        active: true
      }
    )
    ;; Auto-enable dynamic pricing for new songs
    (initialize-song-pricing song-id price-per-stream)
    
    (var-set next-song-id (+ song-id u1))
    (ok song-id)
  )
)

(define-public (stream-song (song-id uint))
  (let
    (
      (song (unwrap! (get-song song-id) (err u404)))
      (artist-id (get artist-id song))
      (artist (unwrap! (get-artist artist-id) (err u404)))
      (stream-price (unwrap! (get-current-song-price song-id) (err u500)))
      (stream-id (var-get next-stream-id))
      (platform-fee (/ (* stream-price (var-get platform-fee-percent)) u100))
      (artist-payment (- stream-price platform-fee))
      (listener-info (default-to { total-streams: u0, total-spent: u0 } (get-listener tx-sender)))
    )
    (asserts! (get active song) (err u403))
    (asserts! (get active artist) (err u403))
    
    (try! (stx-transfer? stream-price tx-sender (var-get contract-owner)))
    (try! (stx-transfer? artist-payment (var-get contract-owner) (get artist-principal artist)))
    
    (map-set stream-history
      { stream-id: stream-id }
      {
        listener: tx-sender,
        song-id: song-id,
        artist-id: artist-id,
        timestamp: stacks-block-height,
        amount-paid: stream-price
      }
    )
    
    (map-set songs
      { song-id: song-id }
      (merge song { total-streams: (+ (get total-streams song) u1) })
    )
    
    (map-set artists
      { artist-id: artist-id }
      (merge artist { 
        total-streams: (+ (get total-streams artist) u1),
        total-earnings: (+ (get total-earnings artist) artist-payment)
      })
    )
    
    (map-set listeners
      { listener-principal: tx-sender }
      {
        total-streams: (+ (get total-streams listener-info) u1),
        total-spent: (+ (get total-spent listener-info) stream-price)
      }
    )
    
    ;; Update demand metrics for dynamic pricing if enabled
    (match (get-song-pricing-data song-id)
      pricing-data (update-song-demand song-id)
      true
    )
    
    (update-streams-leaderboard song-id (+ (get total-streams song) u1))
    (update-earnings-leaderboard song-id (* (+ (get total-streams song) u1) stream-price))
    
    (var-set next-stream-id (+ stream-id u1))
    (ok stream-id)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (<= new-fee u30) (err u403))
    (var-set platform-fee-percent new-fee)
    (ok new-fee)
  )
)

(define-public (set-min-stream-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (var-set min-stream-price new-price)
    (ok new-price)
  )
)

(define-public (deactivate-song (song-id uint))
  (let
    (
      (song (unwrap! (get-song song-id) (err u404)))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (map-set songs
      { song-id: song-id }
      (merge song { active: false })
    )
    (ok true)
  )
)

(define-public (deactivate-artist (artist-id uint))
  (let
    (
      (artist (unwrap! (get-artist artist-id) (err u404)))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (map-set artists
      { artist-id: artist-id }
      (merge artist { active: false })
    )
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (var-set contract-owner new-owner)
    (ok true)
  )
)



(define-map playlists
  { playlist-id: uint, owner: principal }
  {
    name: (string-ascii 64),
    songs: (list 100 uint),
    created-at: uint
  }
)

(define-data-var next-playlist-id uint u1)

(define-read-only (get-playlist (playlist-id uint) (owner principal))
  (map-get? playlists { playlist-id: playlist-id, owner: owner })
)

(define-public (create-playlist (name (string-ascii 64)))
  (let
    (
      (playlist-id (var-get next-playlist-id))
    )
    (map-set playlists
      { playlist-id: playlist-id, owner: tx-sender }
      {
        name: name,
        songs: (list),
        created-at: stacks-block-height
      }
    )
    (var-set next-playlist-id (+ playlist-id u1))
    (ok playlist-id)
  )
)

(define-public (add-song-to-playlist (playlist-id uint) (song-id uint))
  (let
    (
      (playlist (unwrap! (get-playlist playlist-id tx-sender) (err u404)))
      (song (unwrap! (get-song song-id) (err u404)))
    )
    (map-set playlists
      { playlist-id: playlist-id, owner: tx-sender }
      (merge playlist { songs: (unwrap! (as-max-len? (append (get songs playlist) song-id) u100) (err u403)) })
    )
    (ok true)
  )
)


(define-map artist-tips
  { artist-id: uint }
  {
    total-tips: uint,
    tip-count: uint
  }
)

(define-read-only (get-artist-tips (artist-id uint))
  (default-to
    { total-tips: u0, tip-count: u0 }
    (map-get? artist-tips { artist-id: artist-id })
  )
)

(define-public (tip-artist (artist-id uint) (amount uint))
  (let
    (
      (artist (unwrap! (get-artist artist-id) (err u404)))
      (current-tips (get-artist-tips artist-id))
    )
    (asserts! (> amount u0) (err u403))
    (try! (stx-transfer? amount tx-sender (get artist-principal artist)))
    
    (map-set artist-tips
      { artist-id: artist-id }
      {
        total-tips: (+ (get total-tips current-tips) amount),
        tip-count: (+ (get tip-count current-tips) u1)
      }
    )
    (ok true)
  )
)


(define-map artist-subscriptions
  { artist-id: uint }
  {
    monthly-price: uint,
    subscriber-count: uint,
    subscription-enabled: bool,
    total-subscription-revenue: uint
  }
)

(define-map user-subscriptions
  { subscriber: principal, artist-id: uint }
  {
    start-block: uint,
    end-block: uint,
    amount-paid: uint,
    active: bool,
    auto-renew: bool
  }
)

(define-data-var subscription-duration-blocks uint u4320)

(define-read-only (get-artist-subscription (artist-id uint))
  (map-get? artist-subscriptions { artist-id: artist-id })
)

(define-read-only (get-user-subscription (subscriber principal) (artist-id uint))
  (map-get? user-subscriptions { subscriber: subscriber, artist-id: artist-id })
)

(define-read-only (is-subscription-active (subscriber principal) (artist-id uint))
  (match (get-user-subscription subscriber artist-id)
    subscription (and 
      (get active subscription)
      (> (get end-block subscription) stacks-block-height)
    )
    false
  )
)

(define-read-only (get-subscription-duration)
  (var-get subscription-duration-blocks)
)

(define-public (enable-artist-subscription (artist-id uint) (monthly-price uint))
  (let
    (
      (artist (unwrap! (get-artist artist-id) (err u404)))
    )
    (asserts! (or 
      (is-eq tx-sender (var-get contract-owner))
      (is-eq tx-sender (get artist-principal artist))
    ) (err u401))
    (asserts! (> monthly-price u0) (err u403))
    
    (map-set artist-subscriptions
      { artist-id: artist-id }
      {
        monthly-price: monthly-price,
        subscriber-count: u0,
        subscription-enabled: true,
        total-subscription-revenue: u0
      }
    )
    (ok true)
  )
)

(define-public (subscribe-to-artist (artist-id uint))
  (let
    (
      (artist (unwrap! (get-artist artist-id) (err u404)))
      (subscription-info (unwrap! (get-artist-subscription artist-id) (err u404)))
      (monthly-price (get monthly-price subscription-info))
      (platform-fee (/ (* monthly-price (var-get platform-fee-percent)) u100))
      (artist-payment (- monthly-price platform-fee))
      (current-block stacks-block-height)
      (end-block (+ current-block (var-get subscription-duration-blocks)))
      (existing-sub (get-user-subscription tx-sender artist-id))
    )
    (asserts! (get subscription-enabled subscription-info) (err u403))
    (asserts! (get active artist) (err u403))
    (asserts! (not (is-subscription-active tx-sender artist-id)) (err u409))
    
    (try! (stx-transfer? monthly-price tx-sender (var-get contract-owner)))
    (try! (stx-transfer? artist-payment (var-get contract-owner) (get artist-principal artist)))
    
    (map-set user-subscriptions
      { subscriber: tx-sender, artist-id: artist-id }
      {
        start-block: current-block,
        end-block: end-block,
        amount-paid: monthly-price,
        active: true,
        auto-renew: false
      }
    )
    
    (map-set artist-subscriptions
      { artist-id: artist-id }
      (merge subscription-info {
        subscriber-count: (+ (get subscriber-count subscription-info) u1),
        total-subscription-revenue: (+ (get total-subscription-revenue subscription-info) artist-payment)
      })
    )
    
    (map-set artists
      { artist-id: artist-id }
      (merge artist {
        total-earnings: (+ (get total-earnings artist) artist-payment)
      })
    )
    
    (ok true)
  )
)

(define-public (stream-song-with-subscription (song-id uint))
  (let
    (
      (song (unwrap! (get-song song-id) (err u404)))
      (artist-id (get artist-id song))
      (artist (unwrap! (get-artist artist-id) (err u404)))
      (stream-id (var-get next-stream-id))
      (listener-info (default-to { total-streams: u0, total-spent: u0 } (get-listener tx-sender)))
    )
    (asserts! (get active song) (err u403))
    (asserts! (get active artist) (err u403))
    (asserts! (is-subscription-active tx-sender artist-id) (err u402))
    
    (map-set stream-history
      { stream-id: stream-id }
      {
        listener: tx-sender,
        song-id: song-id,
        artist-id: artist-id,
        timestamp: stacks-block-height,
        amount-paid: u0
      }
    )
    
    (map-set songs
      { song-id: song-id }
      (merge song { total-streams: (+ (get total-streams song) u1) })
    )
    
    (map-set artists
      { artist-id: artist-id }
      (merge artist { 
        total-streams: (+ (get total-streams artist) u1)
      })
    )
    
    (map-set listeners
      { listener-principal: tx-sender }
      {
        total-streams: (+ (get total-streams listener-info) u1),
        total-spent: (get total-spent listener-info)
      }
    )
    
    (var-set next-stream-id (+ stream-id u1))
    (ok stream-id)
  )
)

(define-public (cancel-subscription (artist-id uint))
  (let
    (
      (subscription (unwrap! (get-user-subscription tx-sender artist-id) (err u404)))
      (subscription-info (unwrap! (get-artist-subscription artist-id) (err u404)))
    )
    (asserts! (get active subscription) (err u403))
    
    (map-set user-subscriptions
      { subscriber: tx-sender, artist-id: artist-id }
      (merge subscription { active: false })
    )
    
    (map-set artist-subscriptions
      { artist-id: artist-id }
      (merge subscription-info {
        subscriber-count: (- (get subscriber-count subscription-info) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (set-subscription-duration (new-duration uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (> new-duration u0) (err u403))
    (var-set subscription-duration-blocks new-duration)
    (ok new-duration)
  )
)

(define-public (disable-artist-subscription (artist-id uint))
  (let
    (
      (artist (unwrap! (get-artist artist-id) (err u404)))
      (subscription-info (unwrap! (get-artist-subscription artist-id) (err u404)))
    )
    (asserts! (or 
      (is-eq tx-sender (var-get contract-owner))
      (is-eq tx-sender (get artist-principal artist))
    ) (err u401))
    
    (map-set artist-subscriptions
      { artist-id: artist-id }
      (merge subscription-info { subscription-enabled: false })
    )
    (ok true)
  )
)

(define-map leaderboard-by-streams
  { position: uint }
  {
    song-id: uint,
    stream-count: uint,
    last-updated: uint
  }
)

(define-map leaderboard-by-earnings
  { position: uint }
  {
    song-id: uint,
    total-earnings: uint,
    last-updated: uint
  }
)

(define-data-var leaderboard-size uint u10)

(define-read-only (get-streams-leaderboard (position uint))
  (map-get? leaderboard-by-streams { position: position })
)

(define-read-only (get-earnings-leaderboard (position uint))
  (map-get? leaderboard-by-earnings { position: position })
)

(define-read-only (get-leaderboard-size)
  (var-get leaderboard-size)
)

(define-private (update-streams-leaderboard (song-id uint) (stream-count uint))
  (let
    (
      (leaderboard-size-val (var-get leaderboard-size))
      (current-block stacks-block-height)
    )
    (map-set leaderboard-by-streams
      { position: u1 }
      {
        song-id: song-id,
        stream-count: stream-count,
        last-updated: current-block
      }
    )
    true
  )
)

(define-private (update-earnings-leaderboard (song-id uint) (earnings uint))
  (let
    (
      (leaderboard-size-val (var-get leaderboard-size))
      (current-block stacks-block-height)
    )
    (map-set leaderboard-by-earnings
      { position: u1 }
      {
        song-id: song-id,
        total-earnings: earnings,
        last-updated: current-block
      }
    )
    true
  )
)

(define-read-only (get-top-songs-by-streams (limit uint))
  (let
    (
      (max-limit (if (<= limit (var-get leaderboard-size)) limit (var-get leaderboard-size)))
    )
    (map get-streams-leaderboard (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))
  )
)

(define-read-only (get-top-songs-by-earnings (limit uint))
  (let
    (
      (max-limit (if (<= limit (var-get leaderboard-size)) limit (var-get leaderboard-size)))
    )
    (map get-earnings-leaderboard (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))
  )
)

(define-public (refresh-leaderboards)
  (let
    (
      (song-1 (get-song u1))
      (song-2 (get-song u2))
      (song-3 (get-song u3))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (match song-1
      song-data (update-streams-leaderboard u1 (get total-streams song-data))
      false
    )
    (match song-2
      song-data (update-streams-leaderboard u2 (get total-streams song-data))
      false
    )
    (match song-3
      song-data (update-streams-leaderboard u3 (get total-streams song-data))
      false
    )
    (ok true)
  )
)

(define-public (set-leaderboard-size (new-size uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (and (> new-size u0) (<= new-size u50)) (err u403))
    (var-set leaderboard-size new-size)
    (ok new-size)
  )
)

;; Dynamic Pricing Engine
(define-map song-pricing-data
  { song-id: uint }
  {
    base-price: uint,
    release-block: uint,
    recent-streams: uint,
    last-stream-block: uint,
    demand-multiplier: uint,
    max-price: uint
  }
)

(define-map pricing-config
  { config-key: (string-ascii 32) }
  { config-value: uint }
)

;; Pricing parameters with default values
(define-data-var demand-decay-rate uint u50) ;; Percentage decay per 144 blocks (~1 day)
(define-data-var recency-bonus-blocks uint u1008) ;; Blocks for recency bonus (~1 week)
(define-data-var demand-threshold uint u10) ;; Streams needed for price increase
(define-data-var max-price-multiplier uint u300) ;; Maximum 3x price increase
(define-data-var price-adjustment-interval uint u144) ;; Blocks between adjustments

(define-read-only (get-song-pricing-data (song-id uint))
  (map-get? song-pricing-data { song-id: song-id })
)

(define-read-only (get-pricing-config (config-key (string-ascii 32)))
  (map-get? pricing-config { config-key: config-key })
)

(define-read-only (get-demand-decay-rate)
  (var-get demand-decay-rate)
)

(define-read-only (get-recency-bonus-blocks)
  (var-get recency-bonus-blocks)
)

;; Initialize pricing data for a song
(define-private (initialize-song-pricing (song-id uint) (base-price uint))
  (let
    (
      (current-block stacks-block-height)
      (max-price (* base-price (/ (var-get max-price-multiplier) u100)))
    )
    (map-set song-pricing-data
      { song-id: song-id }
      {
        base-price: base-price,
        release-block: current-block,
        recent-streams: u0,
        last-stream-block: current-block,
        demand-multiplier: u100,
        max-price: max-price
      }
    )
    true
  )
)

;; Calculate current dynamic price for a song
(define-read-only (calculate-dynamic-price (song-id uint))
  (let
    (
      (song-data (unwrap! (get-song song-id) (err u404)))
      (pricing-data (unwrap! (get-song-pricing-data song-id) (err u404)))
      (current-block stacks-block-height)
      (base-price (get base-price pricing-data))
      (release-block (get release-block pricing-data))
      (recent-streams (get recent-streams pricing-data))
      (last-stream-block (get last-stream-block pricing-data))
      (demand-multiplier (get demand-multiplier pricing-data))
      (max-price (get max-price pricing-data))
      
      ;; Calculate recency bonus (newer songs cost more)
      (blocks-since-release (- current-block release-block))
      (recency-multiplier 
        (if (<= blocks-since-release (var-get recency-bonus-blocks))
          (+ u100 (/ (* u50 (- (var-get recency-bonus-blocks) blocks-since-release)) (var-get recency-bonus-blocks)))
          u100
        )
      )
      
      ;; Calculate demand decay
      (blocks-since-last-stream (- current-block last-stream-block))
      (decay-periods (/ blocks-since-last-stream (var-get price-adjustment-interval)))
      (decay-amount (* decay-periods (var-get demand-decay-rate)))
      (adjusted-demand-multiplier 
        (if (> demand-multiplier decay-amount)
          (- demand-multiplier decay-amount)
          u100
        )
      )
      
      ;; Calculate final price
      (price-with-demand (/ (* base-price adjusted-demand-multiplier) u100))
      (price-with-recency (/ (* price-with-demand recency-multiplier) u100))
      (final-price (if (> price-with-recency max-price) max-price price-with-recency))
    )
    (ok final-price)
  )
)

;; Update demand metrics when a song is streamed
(define-private (update-song-demand (song-id uint))
  (let
    (
      (current-pricing (unwrap! (get-song-pricing-data song-id) false))
      (current-block stacks-block-height)
      (blocks-since-last (- current-block (get last-stream-block current-pricing)))
      (recent-streams (get recent-streams current-pricing))
      (demand-multiplier (get demand-multiplier current-pricing))
      
      ;; Reset recent streams if too much time has passed
      (updated-recent-streams 
        (if (> blocks-since-last (var-get price-adjustment-interval))
          u1
          (+ recent-streams u1)
        )
      )
      
      ;; Increase demand multiplier if threshold is met
      (new-demand-multiplier
        (if (>= updated-recent-streams (var-get demand-threshold))
          (let
            (
              (increase-amount u20) ;; 20% increase
              (new-multiplier (+ demand-multiplier increase-amount))
              (max-multiplier (var-get max-price-multiplier))
            )
            (if (> new-multiplier max-multiplier) max-multiplier new-multiplier)
          )
          demand-multiplier
        )
      )
    )
    (map-set song-pricing-data
      { song-id: song-id }
      (merge current-pricing {
        recent-streams: updated-recent-streams,
        last-stream-block: current-block,
        demand-multiplier: new-demand-multiplier
      })
    )
    true
  )
)

;; Get current price for streaming (used by streaming functions)
(define-read-only (get-current-song-price (song-id uint))
  (match (get-song-pricing-data song-id)
    pricing-data (calculate-dynamic-price song-id)
    (match (get-song song-id)
      song-data (ok (get price-per-stream song-data))
      (err u404)
    )
  )
)

;; Admin function to set pricing parameters
(define-public (set-demand-decay-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (<= new-rate u100) (err u403)) ;; Max 100% decay
    (var-set demand-decay-rate new-rate)
    (ok new-rate)
  )
)

(define-public (set-recency-bonus-blocks (new-blocks uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (> new-blocks u0) (err u403))
    (var-set recency-bonus-blocks new-blocks)
    (ok new-blocks)
  )
)

(define-public (set-demand-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (> new-threshold u0) (err u403))
    (var-set demand-threshold new-threshold)
    (ok new-threshold)
  )
)

(define-public (set-max-price-multiplier (new-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (and (>= new-multiplier u100) (<= new-multiplier u500)) (err u403)) ;; 1x to 5x
    (var-set max-price-multiplier new-multiplier)
    (ok new-multiplier)
  )
)

;; Enable dynamic pricing for existing song
(define-public (enable-dynamic-pricing (song-id uint))
  (let
    (
      (song-data (unwrap! (get-song song-id) (err u404)))
      (base-price (get price-per-stream song-data))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (is-none (get-song-pricing-data song-id)) (err u409)) ;; Already enabled
    (initialize-song-pricing song-id base-price)
    (ok true)
  )
)

;; Disable dynamic pricing for a song
(define-public (disable-dynamic-pricing (song-id uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (is-some (get-song-pricing-data song-id)) (err u404))
    (map-delete song-pricing-data { song-id: song-id })
    (ok true)
  )
)


