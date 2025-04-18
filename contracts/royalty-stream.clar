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
      (stream-price (get price-per-stream song))
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